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
        , authenticate/3
        , authorize/2
        , authorize_all/2
        , authorize_some/2
        , lock/2
        , unlock/2
        ]).


%% --Interface functions  (calling imem_if for now, not exported) -------------------

if_create_table(_SeCo) ->
    imem_if:build_table(ddAccount, record_info(fields, ddAccount)).

if_delete_table(_SeCo) -> 
    imem_if:delete_table(ddAccount).

if_write(_SeCo, #ddAccount{}=Account) -> 
    imem_if:write(ddAccount, Account).

if_read(_SeCo, AccountId) -> 
    imem_if:read(ddAccount, AccountId).

if_select(_SeCo, MatchSpec) ->
    imem_if:select_rows(ddAccount, MatchSpec). 

if_delete(_SeCo, AccountId) ->
    imem_if:delete(ddAccount, AccountId).

% for test setup only, not exported
if_write(#ddAccount{}=Account) -> 
    imem_if:write(ddAccount, Account);
if_write(#ddRole{}=Role) -> 
    imem_if:write(ddRole, Role).


%% --Implementation ------------------------------------------------------------------

create_table(SeCo) ->
    if_create_table(SeCo).

delete_table(SeCo) -> 
    if_delete_table(SeCo).

% create(SeCo, #ddAccount{id=AccountId, name=Name}) when is_atom(Name)->
%     create(SeCo, #ddAccount{id=AccountId, name=atom_to_list(Name)});
% create(SeCo, #ddAccount{id=AccountId, name=Name}) when is_list(Name)->
%     create(SeCo, #ddAccount{id=AccountId, name=list_to_binary(Name)});
create(SeCo, #ddAccount{id=AccountId, name=Name}=Account) when is_binary(Name) -> 
    case get(SeCo, AccountId) of
        {error, {"Account does not exist", AccountId}} ->   
            case get_by_name(SeCo, Name) of
                {error, {"Account does not exist", Name}} ->   
                    ok = if_write(SeCo, Account),
                    case dderl_role:create(SeCo,AccountId) of
                        ok  ->      ok;
                        Error ->    %% simple transaction rollback
                                    delete(SeCo, Account),
                                    Error
                    end;
                #ddAccount{} ->     {error, {"Account name already exists for",Name}};
                Error ->            Error
            end;
        #ddAccount{} ->  
            {error, {"Account already exists",AccountId}}
    end.

get(SeCo, AccountId) -> 
    case if_read(SeCo, AccountId) of
        [] -> {error, {"Account does not exist", AccountId}};
        [Account] -> Account
    end.

get_by_name(SeCo, Name) -> 
    MatchHead = #ddAccount{name='$1', _='_'},
    Guard = {'==', '$1', Name},
    Result = '$_',
    case if_select(SeCo, [{MatchHead, [Guard], [Result]}]) of
        [] ->           {error, {"Account does not exist", Name}};
        [Account] ->    Account
    end.

update(SeCo, #ddAccount{id=AccountId}=Account, AccountNew) -> 
    case if_read(SeCo, AccountId) of
        [] -> {error, {"Account does not exist", AccountId}};
        [Account] -> if_write(SeCo, AccountNew);
        [_] -> {error, {"Account is modified by someone else", AccountId}}
    end.

delete(SeCo, #ddAccount{id=AccountId}=Account) ->
    case if_read(SeCo, AccountId) of
        [] -> {error, {"Account does not exist", AccountId}};
        [Account] -> delete(SeCo, AccountId);
        [_] -> {error, {"Account is modified by someone else", AccountId}}
    end;
delete(SeCo, AccountId) -> 
    if_delete(SeCo, AccountId),
    dderl_role:delete(SeCo, AccountId).

lock(SeCo, #ddAccount{}=Account) -> 
    update(SeCo, Account, Account#ddAccount{isLocked=true});
lock(SeCo, AccountId) -> 
    Account = get(SeCo, AccountId),
    update(SeCo,  Account, Account#ddAccount{isLocked=true}).

unlock(SeCo, #ddAccount{}=Account) -> 
    update(SeCo, Account, Account#ddAccount{isLocked=false,lastFailureTime=undefined});
unlock(SeCo, AccountId) -> 
    Account = get(SeCo, AccountId),
    update(SeCo, Account, Account#ddAccount{isLocked=false,lastFailureTime=undefined}).

exists(SeCo, #ddAccount{id=AccountId}=Account) ->   %% exists unchanged
    case if_read(SeCo, AccountId) of
        [] -> false;
        [Account] -> true;
        [_] -> false
    end;
exists(SeCo, AccountId) ->                          %% exists, maybe in changed form
    case if_read(SeCo, AccountId) of
        [] -> false;
        [_] -> true
    end.

authenticate(SeCo, Name, Credentials) -> 
    case get_by_name(SeCo, Name) of
        #ddAccount{id=Id, isLocked='false', lastFailureTime=undefined} -> Id;
        #ddAccount{isLocked='true'} -> {error,"Account is locked"};
        #ddAccount{lastFailureTime=LastFailureTime} -> {error,"Account is suspended after login failure, please retry later"}
    end.

authorize(_SeCo, _Permission) -> ok.

authorize_all(_SeCo, _Permissions) -> ok.

authorize_some(_SeCo, _Permissions) -> ok.

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

    ?assertEqual({atomic,ok}, dderl_account:create_table(none)),
    io:format(user, "success ~p~n", [create_account_table]),
    ?assertMatch({aborted,{already_exists,ddAccount}}, dderl_account:create_table(none)),
    io:format(user, "success ~p~n", [create_account_table_already_exists]),

    io:format(user, "----TEST--~p:test_create_role_table~n", [?MODULE]),

    ?assertEqual({atomic,ok}, dderl_role:create_table(none)),
    io:format(user, "success ~p~n", [create_role_table]),
    ?assertMatch({aborted,{already_exists,ddRole}}, dderl_role:create_table(none)),
    io:format(user, "success ~p~n", [create_role_table_already_exists]), 

    UserId = make_ref(),
    UserCred={pwdmd5, erlang:md5(<<"t1e2s3t4_5a6d7m8i9n">>)},
    User = #ddAccount{id=UserId,name= <<"test_admin">>,credentials=UserCred,fullName= <<"TestAdmin">>},

    ?assertEqual(ok, if_write(User)),
    io:format(user, "success ~p~n", [create_test_admin]), 
    ?assertEqual(ok, if_write(#ddRole{id=UserId,roles=[],permissions=[manage_accounts, manage_roles]})),
    io:format(user, "success ~p~n", [create_test_admin_role]), 
     
    io:format(user, "----TEST--~p:test_manage_accounts~n", [?MODULE]),

    AccountId = make_ref(),
    AccountCred={pwdmd5, erlang:md5(<<"TestPwd">>)},
    Account = #ddAccount{id=AccountId,name= <<"test">>,credentials=AccountCred,fullName= <<"FullName">>},
    AccountId0 = make_ref(),
    Account0 = #ddAccount{id=AccountId0,name= <<"test">>,credentials=AccountCred,fullName= <<"AnotherName">>},
    Account1 = Account#ddAccount{credentials=new_credentials,fullName= <<"NewFullName">>,isLocked='true'},
    Account2 = Account#ddAccount{credentials=new_credentials,fullName= <<"OldFullName">>},

    SeCo = {erlang:phash2({dderl_session, self()}), UserId},    %% Session Context (SessionId, UserId)

    ?assertEqual(ok, dderl_account:create(SeCo, Account)),
    io:format(user, "success ~p~n", [account_create]),
    ?assertEqual({error, {"Account already exists",AccountId}}, dderl_account:create(SeCo, Account)),
    io:format(user, "success ~p~n", [account_create_already_exists]), 
    ?assertEqual({error, {"Account name already exists for",<<"test">>}}, dderl_account:create(SeCo, Account0)),
    io:format(user, "success ~p~n", [account_create_name_already_exists]), 
    ?assertEqual(Account, dderl_account:get(SeCo, AccountId)),
    io:format(user, "success ~p~n", [account_get]), 
    ?assertEqual(#ddRole{id=AccountId}, dderl_role:get(SeCo, AccountId)),
    io:format(user, "success ~p~n", [role_get]), 
    ?assertEqual(ok, dderl_account:delete(SeCo, AccountId)),
    io:format(user, "success ~p~n", [account_delete]), 
    ?assertEqual(ok, dderl_account:delete(SeCo, AccountId)),
    io:format(user, "success ~p~n", [account_delete_even_no_exists]), 
    ?assertEqual({error, {"Account does not exist", AccountId}}, dderl_account:delete(SeCo, Account)),
    io:format(user, "success ~p~n", [account_delete_no_exists]), 
    ?assertEqual(false, dderl_account:exists(SeCo, AccountId)),
    io:format(user, "success ~p~n", [account_no_exists]), 
    ?assertEqual({error, {"Account does not exist", AccountId}}, dderl_account:get(SeCo, AccountId)),
    io:format(user, "success ~p~n", [account_get_no_exists]), 
    ?assertEqual({error, {"Role does not exist", AccountId}}, dderl_role:get(SeCo, AccountId)),
    io:format(user, "success ~p~n", [role_get_no_exists]), 
    ?assertEqual(ok, dderl_account:create(SeCo, Account)),
    io:format(user, "success ~p~n", [account_create]), 
    ?assertEqual({error, {"Account is modified by someone else", AccountId}}, dderl_account:delete(SeCo, Account1)),
    io:format(user, "success ~p~n", [account_delete_wrong_version]), 
    ?assertEqual(ok, dderl_account:delete(SeCo, Account)),
    io:format(user, "success ~p~n", [account_delete_with_check]), 
    ?assertEqual(ok, dderl_account:create(SeCo, Account)),
    io:format(user, "success ~p~n", [account_create]), 
    ?assertEqual(true, dderl_account:exists(SeCo, AccountId)),
    io:format(user, "success ~p~n", [account_exists]), 
    ?assertEqual(Account, dderl_account:get(SeCo, AccountId)),
    io:format(user, "success ~p~n", [account_get]), 
    ?assertEqual(#ddRole{id=AccountId}, dderl_role:get(SeCo, AccountId)),
    io:format(user, "success ~p~n", [role_get]), 
    ?assertEqual(ok, update(SeCo, Account, Account1)),
    io:format(user, "success ~p~n", [update_account]), 
    ?assertEqual(Account1, dderl_account:get(SeCo, AccountId)),
    io:format(user, "success ~p~n", [account_get_modified]), 
    ?assertEqual({error, {"Account is modified by someone else",AccountId}}, update(SeCo, Account, Account2)),
    io:format(user, "success ~p~n", [update_account_reject]), 
    ?assertEqual(Account1, dderl_account:get(SeCo, AccountId)),
    io:format(user, "success ~p~n", [account_get_unchanged]), 

    io:format(user, "----TEST--~p:test_manage_account_roles~n", [?MODULE]),

    ?assertEqual(true, dderl_role:has_role(SeCo, AccountId, AccountId)),
    io:format(user, "success ~p~n", [role_has_own_role]), 
    ?assertEqual(false, dderl_role:has_role(SeCo, AccountId, some_unknown_role)),
    io:format(user, "success ~p~n", [role_has_some_unknown_role]), 
    ?assertEqual({error, {"Role does not exist", some_unknown_role}}, dderl_role:grant_role(SeCo, AccountId, some_unknown_role)),
    io:format(user, "success ~p~n", [role_grant_reject]), 
    ?assertEqual({error, {"Role does not exist", some_unknown_role}}, dderl_role:grant_role(SeCo, some_unknown_role, AccountId)),
    io:format(user, "success ~p~n", [role_grant_reject]), 
    ?assertEqual(ok, dderl_role:create(SeCo, admin)),
    io:format(user, "success ~p~n", [role_create_empty_role]), 
    ?assertEqual({error, {"Role already exists",admin}}, dderl_role:create(SeCo, admin)),
    io:format(user, "success ~p~n", [role_create_existing_role]), 
    ?assertEqual(false, dderl_role:has_role(SeCo, AccountId, admin)),
    io:format(user, "success ~p~n", [role_has_not_admin_role]), 
    ?assertEqual(ok, dderl_role:grant_role(SeCo, AccountId, admin)),
    io:format(user, "success ~p~n", [role_grant_admin_role]), 
    ?assertEqual(true, dderl_role:has_role(SeCo, AccountId, admin)),
    io:format(user, "success ~p~n", [role_has_admin_role]), 
    ?assertEqual(ok, dderl_role:grant_role(SeCo, AccountId, admin)),
    io:format(user, "success ~p~n", [role_re_grant_admin_role]), 
    ?assertEqual(#ddRole{id=AccountId,roles=[admin]}, dderl_role:get(SeCo, AccountId)),
    io:format(user, "success ~p~n", [role_get]), 
    ?assertEqual(ok, dderl_role:revoke_role(SeCo, AccountId, admin)),
    io:format(user, "success ~p~n", [role_revoke_admin_role]), 
    ?assertEqual(#ddRole{id=AccountId,roles=[]}, dderl_role:get(SeCo, AccountId)),
    io:format(user, "success ~p~n", [role_get]),
    ?assertEqual(ok, dderl_role:grant_role(SeCo, AccountId, admin)),
    io:format(user, "success ~p~n", [role_grant_admin_role]),      
    ?assertEqual(ok, dderl_role:create(SeCo, #ddRole{id=test_role,roles=[],permissions=[perform_tests]})),
    io:format(user, "success ~p~n", [role_create_test_role]), 
    ?assertEqual(true, dderl_role:has_permission(SeCo, test_role, perform_tests)),
    io:format(user, "success ~p~n", [role_has_test_permission]), 
    ?assertEqual(false, dderl_role:has_permission(SeCo, test_role, stupid_permission)),
    io:format(user, "success ~p~n", [role_has_stupid_permission]), 
    ?assertEqual(false, dderl_role:has_role(SeCo, AccountId, test_role)),
    io:format(user, "success ~p~n", [role_has_test_role]), 
    ?assertEqual(false, dderl_role:has_permission(SeCo, AccountId, perform_tests)),
    io:format(user, "success ~p~n", [role_has_test_permission]), 
    ?assertEqual(ok, dderl_role:grant_role(SeCo, admin, test_role)),
    io:format(user, "success ~p~n", [role_grant_test_role]), 
    ?assertEqual(true, dderl_role:has_role(SeCo, AccountId, test_role)),
    io:format(user, "success ~p~n", [role_has_test_role]), 
    ?assertEqual(true, dderl_role:has_permission(SeCo, AccountId, perform_tests)),
    io:format(user, "success ~p~n", [role_has_test_permission]), 

    io:format(user, "----TEST--~p:test_manage_account_permissions~n", [?MODULE]),
    ?assertEqual(ok, dderl_role:grant_permission(SeCo, test_role, delete_tests)),
    io:format(user, "success ~p~n", [role_grant_test_role_delete_tests]), 
    ?assertEqual(ok, dderl_role:grant_permission(SeCo, test_role, fake_tests)),
    io:format(user, "success ~p~n", [role_grant_test_role_fake_tests]), 
    ?assertEqual(true, dderl_role:has_permission(SeCo, AccountId, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(true, dderl_role:has_permission(SeCo, AccountId, fake_tests)),
    io:format(user, "success ~p~n", [role_has_fake_tests_permission]), 
    ?assertEqual(true, dderl_role:has_permission(SeCo, admin, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(true, dderl_role:has_permission(SeCo, admin, fake_tests)),
    io:format(user, "success ~p~n", [role_has_fake_tests_permission]), 
    ?assertEqual(true, dderl_role:has_permission(SeCo, test_role, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(true, dderl_role:has_permission(SeCo, test_role, fake_tests)),
    io:format(user, "success ~p~n", [role_has_fake_tests_permission]), 
    ?assertEqual(ok, dderl_role:revoke_permission(SeCo, test_role, delete_tests)),
    io:format(user, "success ~p~n", [role_revoke_test_role_delete_tests]), 
    ?assertEqual(false, dderl_role:has_permission(SeCo, AccountId, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(false, dderl_role:has_permission(SeCo, admin, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(false, dderl_role:has_permission(SeCo, test_role, delete_tests)),
    io:format(user, "success ~p~n", [role_has_delete_tests_permission]), 
    ?assertEqual(ok, dderl_role:revoke_permission(SeCo, test_role, delete_tests)),
    io:format(user, "success ~p~n", [role_revoket_test_role_delete_tests]), 


    %% Cleanup only if we arrive at this point
    ?assertEqual({atomic,ok}, dderl_role:delete_table(SeCo)),
    io:format(user, "success ~p~n", [delete_role_table]), 
    ?assertEqual({aborted,{no_exists,ddRole}}, dderl_role:delete_table(SeCo)),
    io:format(user, "success ~p~n", [delete_role_table_no_exists]), 
    ?assertEqual({atomic,ok}, dderl_account:delete_table(SeCo)),
    io:format(user, "success ~p~n", [delete_account_table]), 
    ?assertEqual({aborted,{no_exists,ddAccount}}, dderl_account:delete_table(SeCo)),
    io:format(user, "success ~p~n", [delete_account_table_no_exists]), 
    ok.

