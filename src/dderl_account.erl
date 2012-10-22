-module(dderl_account).

-include_lib("eunit/include/eunit.hrl").

-include("dderl.hrl").

-export([ create_table/1
        , delete_table/1
        ]).

-export([ create/2
        , get/2
        , get_by_name/2
        , update/3
        , delete/2
        , exists/2
        , authenticate/1
        , authorize/2
        , authorize_all/2
        , authorize_some/2
        , lock/2
        , unlock/2
        ]).


%% --Interface functions  (calling imem_if for now) ----------------------------------

if_create_table(_RequestorCredentials) ->
    imem_if:build_table(ddAccount, record_info(fields, ddAccount)).

if_delete_table(_RequestorCredentials) -> 
    imem_if:delete_table(ddAccount).

if_write(_RequestorCredentials, #ddAccount{}=Account) -> 
    imem_if:write(ddAccount, Account).

if_read(_RequestorCredentials, AccountId) -> 
    imem_if:read(ddAccount, AccountId).

if_select(_RequestorCredentials, MatchSpec) ->
    imem_if:select_rows(ddAccount, MatchSpec). 

if_delete(_RequestorCredentials, AccountId) ->
    imem_if:delete(ddAccount, AccountId).

%% --Implementation ------------------------------------------------------------------

create_table(RequestorCredentials) ->
    if_create_table(RequestorCredentials).

delete_table(RequestorCredentials) -> 
    if_delete_table(RequestorCredentials).

% create(RequestorCredentials, #ddAccount{id=AccountId, name=Name}) when is_atom(Name)->
%     create(RequestorCredentials, #ddAccount{id=AccountId, name=atom_to_list(Name)});
% create(RequestorCredentials, #ddAccount{id=AccountId, name=Name}) when is_list(Name)->
%     create(RequestorCredentials, #ddAccount{id=AccountId, name=list_to_binary(Name)});
create(RequestorCredentials, #ddAccount{id=AccountId, name=Name}=Account) when is_binary(Name) -> 
    case get(RequestorCredentials, AccountId) of
        {error, {"Account does not exist", AccountId}} ->   
            case get_by_name(RequestorCredentials, Name) of
                {error, {"Account does not exist", Name}} ->   
                    ok = if_write(RequestorCredentials, Account),
                    case dderl_role:create(RequestorCredentials,AccountId) of
                        ok  ->      ok;
                        Error ->    %% simple transaction rollback
                                    delete(RequestorCredentials, Account),
                                    Error
                    end;
                #ddAccount{} ->     {error, {"Account name already exists for",Name}};
                Error ->            Error
            end;
        #ddAccount{} ->  
            {error, {"Account already exists",AccountId}}
    end.

get(RequestorCredentials, AccountId) -> 
    case if_read(RequestorCredentials, AccountId) of
        [] -> {error, {"Account does not exist", AccountId}};
        [Account] -> Account
    end.

get_by_name(RequestorCredentials, Name) -> 
    MatchHead = #ddAccount{name='$1', _='_'},
    Guard = {'==', '$1', Name},
    Result = '$_',
    case if_select(RequestorCredentials, [{MatchHead, [Guard], [Result]}]) of
        [] ->           {error, {"Account does not exist", Name}};
        [Account] ->    Account
    end.

update(RequestorCredentials, #ddAccount{id=AccountId}=Account, AccountNew) -> 
    case if_read(RequestorCredentials, AccountId) of
        [] -> {error, {"Account does not exist", AccountId}};
        [Account] -> if_write(RequestorCredentials, AccountNew);
        [_] -> {error, {"Account is modified by someone else", AccountId}}
    end.

delete(RequestorCredentials, #ddAccount{id=AccountId}=Account) ->
    case if_read(RequestorCredentials, AccountId) of
        [] -> {error, {"Account does not exist", AccountId}};
        [Account] -> delete(RequestorCredentials, AccountId);
        [_] -> {error, {"Account is modified by someone else", AccountId}}
    end;
delete(RequestorCredentials, AccountId) -> 
    if_delete(RequestorCredentials, AccountId),
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

exists(RequestorCredentials, #ddAccount{id=AccountId}=Account) ->   %% exists unchanged
    case if_read(RequestorCredentials, AccountId) of
        [] -> false;
        [Account] -> true;
        [_] -> false
    end;
exists(RequestorCredentials, AccountId) ->                          %% exists, maybe in changed form
    case if_read(RequestorCredentials, AccountId) of
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
    AccountId0 = make_ref(),
    Account0 = #ddAccount{id=AccountId0,name= <<"test">>,credentials=no_credentials,fullName= <<"AnotherName">>},
    Account1 = Account#ddAccount{credentials=new_credentials,fullName= <<"NewFullName">>,isLocked='true'},
    Account2 = Account#ddAccount{credentials=new_credentials,fullName= <<"OldFullName">>},

    ?assertEqual(ok, dderl_account:create(no_credentials, Account)),
    io:format(user, "success ~p~n", [account_create]),
    ?assertEqual({error, {"Account already exists",AccountId}}, dderl_account:create(no_credentials, Account)),
    io:format(user, "success ~p~n", [account_create_already_exists]), 
    ?assertEqual({error, {"Account name already exists for",<<"test">>}}, dderl_account:create(no_credentials, Account0)),
    io:format(user, "success ~p~n", [account_create_name_already_exists]), 
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
    ?assertEqual(false, dderl_account:exists(no_credentials, AccountId)),
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
    ?assertEqual(true, dderl_account:exists(no_credentials, AccountId)),
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
    ?assertEqual(true, dderl_role:has_permission(no_credentials, test_role, perform_tests)),
    io:format(user, "success ~p~n", [role_has_test_permission]), 
    ?assertEqual(false, dderl_role:has_permission(no_credentials, test_role, stupid_permission)),
    io:format(user, "success ~p~n", [role_has_stupid_permission]), 
    ?assertEqual(false, dderl_role:has_role(no_credentials, AccountId, test_role)),
    io:format(user, "success ~p~n", [role_has_test_role]), 
    ?assertEqual(false, dderl_role:has_permission(no_credentials, AccountId, perform_tests)),
    io:format(user, "success ~p~n", [role_has_test_permission]), 
    ?assertEqual(ok, dderl_role:grant_role(no_credentials, admin, test_role)),
    io:format(user, "success ~p~n", [role_grant_test_role]), 
    ?assertEqual(true, dderl_role:has_role(no_credentials, AccountId, test_role)),
    io:format(user, "success ~p~n", [role_has_test_role]), 
    ?assertEqual(true, dderl_role:has_permission(no_credentials, AccountId, perform_tests)),
    io:format(user, "success ~p~n", [role_has_test_permission]), 

    io:format(user, "----TEST--~p:test_manage_account_permissions~n", [?MODULE]),
    ?assertEqual(ok, dderl_role:grant_permission(no_credentials, test_role, delete_tests)),
    io:format(user, "success ~p~n", [role_grant_test_role_delete_tests]), 
    ?assertEqual(ok, dderl_role:grant_permission(no_credentials, test_role, fake_tests)),
    io:format(user, "success ~p~n", [role_grant_test_role_fake_tests]), 
    ?assertEqual(true, dderl_role:has_permission(no_credentials, AccountId, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(true, dderl_role:has_permission(no_credentials, AccountId, fake_tests)),
    io:format(user, "success ~p~n", [role_has_fake_tests_permission]), 
    ?assertEqual(true, dderl_role:has_permission(no_credentials, admin, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(true, dderl_role:has_permission(no_credentials, admin, fake_tests)),
    io:format(user, "success ~p~n", [role_has_fake_tests_permission]), 
    ?assertEqual(true, dderl_role:has_permission(no_credentials, test_role, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(true, dderl_role:has_permission(no_credentials, test_role, fake_tests)),
    io:format(user, "success ~p~n", [role_has_fake_tests_permission]), 
    ?assertEqual(ok, dderl_role:revoke_permission(no_credentials, test_role, delete_tests)),
    io:format(user, "success ~p~n", [role_revoke_test_role_delete_tests]), 
    ?assertEqual(false, dderl_role:has_permission(no_credentials, AccountId, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(false, dderl_role:has_permission(no_credentials, admin, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(false, dderl_role:has_permission(no_credentials, test_role, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(ok, dderl_role:revoke_permission(no_credentials, test_role, delete_tests)),
    io:format(user, "success ~p~n", [role_revoket_test_role_delete_tests]), 


    %% Cleanup only if we arrive at this point
    dderl_role:delete_table(no_credentials),
    dderl_account:delete_table(no_credentials),
    ok.

