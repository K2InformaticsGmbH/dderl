-module(dderl_account).

-include_lib("eunit/include/eunit.hrl").

-include("dderl.hrl").

-export([ create_table/1
        , delete_table/1
        ]).

-export([ create/2
        , get/2
        , read/2
        , write/2
        , update/3
        , delete/2
        , exists/1
        , authenticate/1
        , authorize/2
        , authorize_all/2
        , authorize_some/2
        , lock/2
        , unlock/2
        ]).

create_table(_RequestorCredentials) ->
    imem_if:build_table(ddAccount, record_info(fields, ddAccount)).

delete_table(_RequestorCredentials) -> 
    imem_if:delete_table(ddAccount).

create(RequestorCredentials, #ddAccount{id=AccountId}=Account) -> 
    case imem_if:read(ddAccount, AccountId) of
        [] ->   ok = imem_if:write(ddAccount, Account),
                case dderl_role:create(RequestorCredentials,AccountId) of
                    ok  ->      ok;
                    Error ->    %% simple transaction rollback
                                delete(RequestorCredentials, Account),
                                Error
                end;    
        [_] ->  {error, {"Account already exists",AccountId}}
    end.

write(_RequestorCredentials, #ddAccount{}=Account) -> 
        imem_if:write(ddAccount, Account).

read(_RequestorCredentials, AccountId) -> 
    imem_if:read(ddAccount, AccountId).

get(_RequestorCredentials, AccountId) -> 
    case imem_if:read(ddAccount, AccountId) of
        [] -> {error, {"Account does not exist", AccountId} };
        [Account] -> Account
    end.

update(_RequestorCredentials, #ddAccount{id=AccountId}=Account, AccountNew) -> 
    case imem_if:read(ddAccount, AccountId) of
        [] -> {error, {"Account does not exist", AccountId}};
        [Account] -> imem_if:insert_into_table(ddAccount, AccountNew);
        [_] -> {error, {"Account is modified by someone else", AccountId}}
    end.

delete(RequestorCredentials, #ddAccount{id=AccountId}=Account) ->
    case imem_if:read(ddAccount, AccountId) of
        [] -> {error, {"Account does not exist", AccountId}};
        [Account] -> delete(RequestorCredentials, AccountId);
        [_] -> {error, {"Account is modified by someone else", AccountId}}
    end;
delete(RequestorCredentials, AccountId) -> 
    imem_if:delete(ddAccount, AccountId),
    dderl_role:delete(RequestorCredentials, AccountId).

lock(RequestorCredentials, #ddAccount{}=Account) -> 
    update(RequestorCredentials, Account, Account#ddAccount{isLocked=true});
lock(RequestorCredentials, AccountId) -> 
    Account = get(RequestorCredentials, AccountId),
    update(RequestorCredentials,  Account, Account#ddAccount{isLocked=true}).

unlock(RequestorCredentials, #ddAccount{}=Account) -> 
    update(RequestorCredentials, Account, Account#ddAccount{isLocked=false,lastFailureTime=undefined});
unlock(RequestorCredentials, AccountId) -> 
    Account = get(RequestorCredentials, AccountId),
    update(RequestorCredentials, Account, Account#ddAccount{isLocked=false,lastFailureTime=undefined}).

exists(#ddAccount{id=AccountId}=Account) ->         %% exists unchanged
    case imem_if:read(ddAccount, AccountId) of
        [] -> false;
        [Account] -> true;
        [_] -> false
    end;
exists(AccountId) ->                                %% exists, maybe in changed form
    case imem_if:read(ddAccount, AccountId) of
        [] -> false;
        [_] -> true
    end.

authenticate(_Credentials) -> ok.

authorize(_Credentials, _Permission) -> ok.

authorize_all(_Credentials, _Permissions) -> ok.

authorize_some(_Credentials, _Permissions) -> ok.

%% ----- TESTS ------------------------------------------------

setup() -> 
    application:start(imem).

teardown(_) -> 
    application:stop(imem).

account_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        {with, [
            fun test/1
            %%, fun test_create_account/1
        ]}}.    

    
test(_) ->
    io:format(user, "----TEST--~p:test_create_account_table~n", [?MODULE]),

    ?assertEqual({atomic,ok}, dderl_account:create_table(no_credentials)),
    io:format(user, "success ~p~n", [create_account_table]),
    ?assertMatch({aborted,{already_exists,ddAccount}}, dderl_account:create_table(no_credentials)),
    io:format(user, "success ~p~n", [create_account_table_already_exists]), 
    ?assertEqual({atomic,ok}, dderl_account:delete_table(no_credentials)),
    io:format(user, "success ~p~n", [delete_account_table]), 
    ?assertEqual({aborted,{no_exists,ddAccount}}, dderl_account:delete_table(no_credentials)),
    io:format(user, "success ~p~n", [delete_account_table_no_exists]), 
    ?assertEqual({atomic,ok}, dderl_account:create_table(no_credentials)),
    io:format(user, "success ~p~n", [create_account_table]), 

    io:format(user, "----TEST--~p:test_create_role_table~n", [?MODULE]),

    ?assertEqual({atomic,ok}, dderl_role:create_table(no_credentials)),
    io:format(user, "success ~p~n", [create_role_table]),
    ?assertMatch({aborted,{already_exists,ddRole}}, dderl_role:create_table(no_credentials)),
    io:format(user, "success ~p~n", [create_role_table_already_exists]), 
    ?assertEqual({atomic,ok}, dderl_role:delete_table(no_credentials)),
    io:format(user, "success ~p~n", [delete_role_table]), 
    ?assertEqual({aborted,{no_exists,ddRole}}, dderl_role:delete_table(no_credentials)),
    io:format(user, "success ~p~n", [delete_role_table_no_exists]), 
    ?assertEqual({atomic,ok}, dderl_role:create_table(no_credentials)),
    io:format(user, "success ~p~n", [create_role_table]), 

    io:format(user, "----TEST--~p:test_manage_accounts~n", [?MODULE]),

    AccountId = make_ref(),
    Account = #ddAccount{id=AccountId,name= <<"test">>,credentials=no_credentials,fullName= <<"FullName">>},
    Account1 = Account#ddAccount{credentials=new_credentials,fullName= <<"NewFullName">>,isLocked='true'},
    Account2 = Account#ddAccount{credentials=new_credentials,fullName= <<"OldFullName">>},

    ?assertEqual(ok, dderl_account:create(no_credentials, Account)),
    io:format(user, "success ~p~n", [account_create]),
    ?assertEqual({error, {"Account already exists",AccountId}}, dderl_account:create(no_credentials, Account)),
    io:format(user, "success ~p~n", [account_create_already_exists]), 
    ?assertEqual(Account, dderl_account:get(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [account_get]), 
    ?assertEqual(#ddRole{id=AccountId}, dderl_role:get(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [role_get]), 
    ?assertEqual(ok, dderl_account:delete(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [account_delete]), 
    ?assertEqual(ok, dderl_account:delete(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [account_delete_even_no_exists]), 
    ?assertEqual({error, {"Account does not exist", AccountId}}, dderl_account:delete(no_credentials, Account)),
    io:format(user, "success ~p~n", [account_delete_no_exists]), 
    ?assertEqual(false, dderl_account:exists(AccountId)),
    io:format(user, "success ~p~n", [account_no_exists]), 
    ?assertEqual({error, {"Account does not exist", AccountId}}, dderl_account:get(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [account_get_no_exists]), 
    ?assertEqual({error, {"Role does not exist", AccountId}}, dderl_role:get(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [role_get_no_exists]), 
    ?assertEqual(ok, dderl_account:create(no_credentials, Account)),
    io:format(user, "success ~p~n", [account_create]), 
    ?assertEqual({error, {"Account is modified by someone else", AccountId}}, dderl_account:delete(no_credentials, Account1)),
    io:format(user, "success ~p~n", [account_delete_wrong_version]), 
    ?assertEqual(ok, dderl_account:delete(no_credentials, Account)),
    io:format(user, "success ~p~n", [account_delete_with_check]), 
    ?assertEqual(ok, dderl_account:create(no_credentials, Account)),
    io:format(user, "success ~p~n", [account_create]), 
    ?assertEqual(true, dderl_account:exists(AccountId)),
    io:format(user, "success ~p~n", [account_exists]), 
    ?assertEqual(Account, dderl_account:get(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [account_get]), 
    ?assertEqual(#ddRole{id=AccountId}, dderl_role:get(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [role_get]), 
    ?assertEqual(ok, update(no_credentials, Account, Account1)),
    io:format(user, "success ~p~n", [update_account]), 
    ?assertEqual(Account1, dderl_account:get(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [account_get_modified]), 
    ?assertEqual({error, {"Account is modified by someone else",AccountId}}, update(no_credentials, Account, Account2)),
    io:format(user, "success ~p~n", [update_account_reject]), 
    ?assertEqual(Account1, dderl_account:get(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [account_get_unchanged]), 

    io:format(user, "----TEST--~p:test_manage_account_roles~n", [?MODULE]),

    ?assertEqual(true, dderl_role:has_role(no_credentials, AccountId, AccountId)),
    io:format(user, "success ~p~n", [role_has_own_role]), 
    ?assertEqual(false, dderl_role:has_role(no_credentials, AccountId, some_unknown_role)),
    io:format(user, "success ~p~n", [role_has_some_unknown_role]), 
    ?assertEqual({error, {"Role does not exist", some_unknown_role}}, dderl_role:grant_role(no_credentials, AccountId, some_unknown_role)),
    io:format(user, "success ~p~n", [role_grant_reject]), 
    ?assertEqual({error, {"Role does not exist", some_unknown_role}}, dderl_role:grant_role(no_credentials, some_unknown_role, AccountId)),
    io:format(user, "success ~p~n", [role_grant_reject]), 
    ?assertEqual(ok, dderl_role:create(no_credentials, admin)),
    io:format(user, "success ~p~n", [role_create_empty_role]), 
    ?assertEqual({error, {"Role already exists",admin}}, dderl_role:create(no_credentials, admin)),
    io:format(user, "success ~p~n", [role_create_existing_role]), 
    ?assertEqual(false, dderl_role:has_role(no_credentials, AccountId, admin)),
    io:format(user, "success ~p~n", [role_has_not_admin_role]), 
    ?assertEqual(ok, dderl_role:grant_role(no_credentials, AccountId, admin)),
    io:format(user, "success ~p~n", [role_grant_admin_role]), 
    ?assertEqual(true, dderl_role:has_role(no_credentials, AccountId, admin)),
    io:format(user, "success ~p~n", [role_has_admin_role]), 
    ?assertEqual(ok, dderl_role:grant_role(no_credentials, AccountId, admin)),
    io:format(user, "success ~p~n", [role_re_grant_admin_role]), 
    ?assertEqual(#ddRole{id=AccountId,roles=[admin]}, dderl_role:get(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [role_get]), 
    ?assertEqual(ok, dderl_role:revoke_role(no_credentials, AccountId, admin)),
    io:format(user, "success ~p~n", [role_revoke_admin_role]), 
    ?assertEqual(#ddRole{id=AccountId,roles=[]}, dderl_role:get(no_credentials, AccountId)),
    io:format(user, "success ~p~n", [role_get]),
    ?assertEqual(ok, dderl_role:grant_role(no_credentials, AccountId, admin)),
    io:format(user, "success ~p~n", [role_grant_admin_role]),      
    ?assertEqual(ok, dderl_role:create(no_credentials, #ddRole{id=test_role,roles=[],permissions=[perform_tests]})),
    io:format(user, "success ~p~n", [role_create_test_role]), 
    ?assertEqual(false, dderl_role:has_role(no_credentials, AccountId, test_role)),
    io:format(user, "success ~p~n", [role_has_test_role]), 
    ?assertEqual(ok, dderl_role:grant_role(no_credentials, admin, test_role)),
    io:format(user, "success ~p~n", [role_grant_test_role]), 
    ?assertEqual(true, dderl_role:has_role(no_credentials, AccountId, test_role)),
    io:format(user, "success ~p~n", [role_has_test_role]), 


    ?assertEqual({atomic,ok}, dderl_role:delete_table(no_credentials)),
    io:format(user, "success ~p~n", [delete_role_table]), 
    ?assertEqual({atomic,ok}, dderl_account:delete_table(no_credentials)),
    io:format(user, "success ~p~n", [delete_account_table]), 

    ok.

