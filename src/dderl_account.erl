-module(dderl_account).

-include_lib("eunit/include/eunit.hrl").

-include("dderl.hrl").

-export([ create_tables/1
        , delete_tables/1
        ]).

-export([ create/2
        , get/2
        , get_by_name/2
        , update/3
        , delete/2
        , exists/2
        , authenticate/3
        , login/1
        , change_credentials/3
        , logout/1
        , authorize/2
        , authorize_all/2
        , authorize_some/2
        , lock/2
        , unlock/2
        ]).


%% --Interface functions  (calling imem_if for now, not exported) -------------------

if_create_tables(_SeCo) ->
    imem_if:build_table(ddAccount, record_info(fields, ddAccount)).

if_delete_tables(_SeCo) -> 
    imem_if:delete_table(ddAccount).

if_write(_SeCo, #ddAccount{}=Account) -> 
    imem_if:write(ddAccount, Account).

if_read(_SeCo, AccountId) -> 
    imem_if:read(ddAccount, AccountId).

if_get(SeCo, AccountId) -> 
    case if_read(SeCo, AccountId) of
        [] -> {error, {"Account does not exist", AccountId}};
        [Account] -> Account
    end.

if_select(_SeCo, MatchSpec) ->
    imem_if:select_rows(ddAccount, MatchSpec). 

if_get_by_name(SeCo, Name) -> 
    MatchHead = #ddAccount{name='$1', _='_'},
    Guard = {'==', '$1', Name},
    Result = '$_',
    case if_select(SeCo, [{MatchHead, [Guard], [Result]}]) of
        [] ->           {error, {"Account does not exist", Name}};
        [Account] ->    Account
    end.

if_delete(_SeCo, AccountId) ->
    imem_if:delete(ddAccount, AccountId).

% for test setup only, not exported
if_write(#ddAccount{}=Account) -> 
    imem_if:write(ddAccount, Account);
if_write(#ddRole{}=Role) -> 
    imem_if:write(ddRole, Role).


%% --Implementation ------------------------------------------------------------------

create_tables(SeCo) ->
    if_create_tables(SeCo).

delete_tables(SeCo) -> 
    case dderl_role:have_permission(SeCo, manage_accounts) of
        true ->     if_delete_tables(SeCo);
        false ->    {error, {"Delete account tables unauthorized",SeCo}}
    end.

create(SeCo, #ddAccount{id=AccountId, name=Name}=Account) when is_binary(Name) ->
    case dderl_role:have_permission(SeCo, manage_accounts) of
        true ->     case if_get(SeCo, AccountId) of
                        {error, {"Account does not exist", AccountId}} ->   
                            case if_get_by_name(SeCo, Name) of
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
                    end;
        false ->    {error, {"Create account unauthorized",SeCo}}
    end.

get(SeCo, AccountId) -> 
    case dderl_role:have_permission(SeCo, manage_accounts) of
        true ->     if_get(SeCo, AccountId);
        false ->    {error, {"Get account unauthorized",SeCo}}
    end.

get_by_name(SeCo, Name) -> 
    case dderl_role:have_permission(SeCo, manage_accounts) of
        true ->     if_get_by_name(SeCo, Name);
        false ->    {error, {"Get account unauthorized",SeCo}}
    end.

update(SeCo, #ddAccount{id=AccountId}=Account, AccountNew) -> 
    case dderl_role:have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, AccountId) of
                        [] -> {error, {"Account does not exist", AccountId}};
                        [Account] -> if_write(SeCo, AccountNew);
                        [_] -> {error, {"Account is modified by someone else", AccountId}}
                    end;
        false ->    {error, {"Update account unauthorized",SeCo}}
    end.    

delete(SeCo, #ddAccount{id=AccountId}=Account) ->
    case dderl_role:have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, AccountId) of
                        [] -> {error, {"Account does not exist", AccountId}};
                        [Account] -> delete(SeCo, AccountId);
                        [_] -> {error, {"Account is modified by someone else", AccountId}}
                    end;
        false ->    {error, {"Delete account unauthorized",SeCo}}
    end;        
delete(SeCo, AccountId) -> 
    case dderl_role:have_permission(SeCo, manage_accounts) of
        true ->     if_delete(SeCo, AccountId),
                    dderl_role:delete(SeCo, AccountId);
        false ->    {error, {"Delete account unauthorized",SeCo}}
    end.        

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
    case dderl_role:have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, AccountId) of
                        [] -> false;
                        [Account] -> true;
                        [_] -> false
                    end;
        false ->    {error, {"Exists account unauthorized",SeCo}}
    end;                    
exists(SeCo, AccountId) ->                          %% exists, maybe in changed form
    case dderl_role:have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, AccountId) of
                        [] -> false;
                        [_] -> true
                    end;
        false ->    {error, {"Exists account unauthorized",SeCo}}
    end.            

authenticate(SessionId, Name, Credentials) ->
    LocalTime = calendar:local_time(),
    SeCo = #ddSeCo{authenticationTime=erlang:now(), pid=self(), sessionId=SessionId},
    Result = if_get_by_name(SeCo, Name),
    case Result of
        #ddAccount{isLocked='true'} ->
            {error,{"Account is locked. Contact a system administrator", Name}};
        #ddAccount{lastFailureTime=LocalTime} ->
            %% lie a bit, don't show a fast attacker that this attempt might have worked
            if_write(SeCo, Result#ddAccount{lastFailureTime=calendar:local_time(), isLocked='true'}),
            {error,{"Invalid account credentials. Please retry", Name}};
        #ddAccount{id=AccountId, credentials=CredList} -> 
            case lists:member(Credentials,CredList) of
                false ->    if_write(SeCo, Result#ddAccount{lastFailureTime=calendar:local_time()}),
                            {error,{"Invalid account credentials. Please retry", Name}};
                true ->     if_write(SeCo, Result#ddAccount{lastFailureTime=undefined}),
                             %% ToDo: Create entry in seco table here value = {authenticated, method}
                            SeCo#ddSeCo{accountId=AccountId}
            end;
        Error -> 
            Error
    end.

unauthenticate(_SeCo) ->
     %% ToDo: Delete entry in seco table if it exists
     ok.

login(#ddSeCo{pid=Pid, accountId=AccountId} = SeCo) when Pid == self() ->
    %% ToDo: Check for entry in seco table here , must exist and be recent, otherwise reject
    LocalTime = calendar:local_time(),
    AuthenticationMethod = pwdmd5,      %% ToDo: get it from seco table value
    PwdExpireSecs = calendar:datetime_to_gregorian_seconds(LocalTime),
    PwdExpireDate = calendar:gregorian_seconds_to_datetime(PwdExpireSecs-24*3600*?PASSWORD_VALIDITY),
    case {Result=if_get(SeCo, AccountId), AuthenticationMethod} of
        {#ddAccount{lastPasswordChangeTime=undefined}, pwdmd5} -> 
            unauthenticate(SeCo),
            {error,{"Password expired. Please change it", AccountId}};
        {#ddAccount{lastPasswordChangeTime=LastChange}, pwdmd5} when LastChange < PwdExpireDate -> 
            unauthenticate(SeCo),
            {error,{"Password expired. Please change it", AccountId}};
        {#ddAccount{}, _} ->
            if_write(SeCo, Result#ddAccount{lastLoginTime=calendar:local_time()}),
            SeCo;            
        {Error, _} ->                    
            unauthenticate(SeCo),
            Error
    end;
login(#ddSeCo{} = SeCo) ->
    {error,{"Unauthorized session context", SeCo}}.


change_credentials(#ddSeCo{pid=Pid, accountId=AccountId}=SeCo, {pwdmd5,_}=OldCred, {pwdmd5,_}=NewCred) when Pid == self() ->
    %% ToDo: Check for entry in seco table here , must exist and be recent, otherwise reject
    LocalTime = calendar:local_time(),
    #ddAccount{credentials=CredList} = Account = if_get(SeCo, AccountId),
    if_write(SeCo, Account#ddAccount{lastPasswordChangeTime=LocalTime, credentials=[NewCred|lists:delete(OldCred,CredList)]}),
    login(SeCo);
change_credentials(#ddSeCo{pid=Pid, accountId=AccountId}=SeCo, {CredType,_}=OldCred, {CredType,_}=NewCred) when Pid == self() ->
    %% ToDo: Check for entry in seco table here , must exist and be recent, otherwise reject
    #ddAccount{credentials=CredList} = Account = if_get(SeCo, AccountId),
    if_write(SeCo, Account#ddAccount{credentials=[NewCred|lists:delete(OldCred,CredList)]}),
    login(SeCo).

logout(#ddSeCo{pid=Pid} = SeCo) when Pid == self() ->
    %% ToDo: Check for entry in seco table here , must exist and be recent, otherwise reject
    unauthenticate(SeCo),
    %% ToDo: Delete entry in cache tables here (ddPerm ...)
    ok;
logout(#ddSeCo{} = SeCo) ->
    {error,{"Unauthorized session context", SeCo}}.

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

    ?assertEqual({atomic,ok}, dderl_account:create_tables(none)),
    io:format(user, "success ~p~n", [create_account_tables]),
    ?assertMatch({aborted,{already_exists,ddAccount}}, dderl_account:create_tables(none)),
    io:format(user, "success ~p~n", [create_account_table_already_exists]),

    io:format(user, "----TEST--~p:test_create_role_table~n", [?MODULE]),

    ?assertEqual({atomic,ok}, dderl_role:create_tables(none)),
    io:format(user, "success ~p~n", [create_role_tables]),
    ?assertMatch({aborted,{already_exists,ddRole}}, dderl_role:create_tables(none)),
    io:format(user, "success ~p~n", [create_role_table_already_exists]), 

    UserId = make_ref(),
    UserName= <<"test_admin">>,
    UserCred={pwdmd5, erlang:md5(<<"t1e2s3t4_5a6d7m8i9n">>)},
    UserCredNew={pwdmd5, erlang:md5(<<"test_5a6d7m8i9n">>)},
    User = #ddAccount{id=UserId,name=UserName,credentials=[UserCred],fullName= <<"TestAdmin">>},

    ?assertEqual(ok, if_write(User)),
    io:format(user, "success ~p~n", [create_test_admin]), 
    ?assertEqual(ok, if_write(#ddRole{id=UserId,roles=[],permissions=[manage_accounts]})),
    io:format(user, "success ~p~n", [create_test_admin_role]),
    ?assertEqual([User], if_read(#ddSeCo{}, UserId)),
    io:format(user, "success ~p~n", [if_read]),
    ?assertEqual(User, if_get(#ddSeCo{}, UserId)),
    io:format(user, "success ~p~n", [if_get]),
    ?assertEqual(User, if_get_by_name(#ddSeCo{}, UserName)),
    io:format(user, "success ~p~n", [if_get_by_name]),
 
    io:format(user, "----TEST--~p:test_authentification~n", [?MODULE]),

    SeCo0=authenticate(someSessionId, UserName, UserCred),
    ?assertMatch(#ddSeCo{}, SeCo0),
    ?assertEqual(UserId, SeCo0#ddSeCo.accountId),
    ?assertEqual(self(), SeCo0#ddSeCo.pid),
    ?assertEqual(someSessionId, SeCo0#ddSeCo.sessionId),
    io:format(user, "success ~p~n", [test_admin_authentification]), 
    ?assertEqual({error,{"Password expired. Please change it", UserId}}, login(SeCo0)),
    io:format(user, "success ~p~n", [new_password]),
    SeCo1=authenticate(someSessionId, UserName, UserCred), 
    ?assertMatch(#ddSeCo{}, SeCo1),
    io:format(user, "success ~p~n", [test_admin_authentification]), 
    ?assertEqual(SeCo1, change_credentials(SeCo1, UserCred, UserCredNew)),
    io:format(user, "success ~p~n", [password_changed]), 
    ?assertEqual(ok, logout(SeCo1)),
    io:format(user, "success ~p~n", [logout]), 
    SeCo2=authenticate(someSessionId, UserName, UserCredNew),
    ?assertEqual(UserId, SeCo2#ddSeCo.accountId),
    ?assertEqual(self(), SeCo2#ddSeCo.pid),
    ?assertEqual(someSessionId, SeCo2#ddSeCo.sessionId),
    io:format(user, "success ~p~n", [test_admin_reauthentification]),
    ?assertEqual(true, dderl_role:have_permission(SeCo2, manage_accounts)), 
    ?assertEqual(false, dderl_role:have_permission(SeCo2, manage_bananas)), 
    io:format(user, "success ~p~n", [have_permission]),

    io:format(user, "----TEST--~p:test_manage_accounts~n", [?MODULE]),

    AccountId = make_ref(),
    AccountCred={pwdmd5, erlang:md5(<<"TestPwd">>)},
    AccountCredNew={pwdmd5, erlang:md5(<<"TestPwd1">>)},
    AccountName= <<"test">>,
    Account = #ddAccount{id=AccountId,name=AccountName,credentials=[AccountCred],fullName= <<"FullName">>},
    AccountId0 = make_ref(),
    Account0 = #ddAccount{id=AccountId0,name=AccountName,credentials=[AccountCred],fullName= <<"AnotherName">>},
    Account1 = Account#ddAccount{credentials=[AccountCredNew],fullName= <<"NewFullName">>,isLocked='true'},
    Account2 = Account#ddAccount{credentials=[AccountCredNew],fullName= <<"OldFullName">>},

    SeCo = SeCo2, %% belonging to user <<"test_admin">>

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
    ?assertEqual(false, dderl_role:has_permission(SeCo, AccountId, manage_accounts)), 
    ?assertEqual(false, dderl_role:has_permission(SeCo, AccountId, manage_bananas)), 
    io:format(user, "success ~p~n", [has_permission]),

    ?assertEqual({error,{"Account is locked. Contact a system administrator",<<"test">>}},authenticate(someSessionId, AccountName, AccountCredNew)), 
    io:format(user, "success ~p~n", [is_locked]),
    ?assertEqual(ok, dderl_account:unlock(SeCo, AccountId)),
    io:format(user, "success ~p~n", [unlock]),
    SeCo3=authenticate(someSessionId, AccountName, AccountCredNew),
    ?assertMatch(#ddSeCo{}, SeCo3),
    ?assertEqual(AccountId, SeCo3#ddSeCo.accountId),
    ?assertEqual(self(), SeCo3#ddSeCo.pid),
    ?assertEqual(someSessionId, SeCo3#ddSeCo.sessionId),
    io:format(user, "success ~p~n", [test_authentification]),
    ?assertEqual({error,{"Password expired. Please change it", AccountId}}, login(SeCo3)),
    io:format(user, "success ~p~n", [new_password]),
    SeCo4=authenticate(someSessionId, AccountName, AccountCredNew), 
    ?assertMatch(#ddSeCo{}, SeCo4),
    io:format(user, "success ~p~n", [test_authentification]), 
    ?assertEqual(SeCo4, change_credentials(SeCo4, AccountCredNew, AccountCred)),
    io:format(user, "success ~p~n", [password_changed]), 
    ?assertEqual(true, dderl_role:have_role(SeCo4, AccountId)), 
    ?assertEqual(false, dderl_role:have_role(SeCo4, some_unknown_role)), 
    ?assertEqual(false, dderl_role:have_permission(SeCo4, manage_accounts)), 
    ?assertEqual(false, dderl_role:have_permission(SeCo4, manage_bananas)), 
    io:format(user, "success ~p~n", [have_permission]),

    io:format(user, "----TEST--~p:test_manage_account_rejectss~n", [?MODULE]),

    ?assertEqual({error, {"Delete role tables unauthorized",SeCo4}}, dderl_role:delete_tables(SeCo4)),
    io:format(user, "success ~p~n", [delete_role_tables_rejected]), 
    ?assertEqual({error, {"Delete account tables unauthorized",SeCo4}}, dderl_account:delete_tables(SeCo4)),
    io:format(user, "success ~p~n", [delete_account_tables_rejected]), 
    ?assertEqual({error, {"Create account unauthorized",SeCo4}}, dderl_account:create(SeCo4, Account)),
    ?assertEqual({error, {"Create account unauthorized",SeCo4}}, dderl_account:create(SeCo4, Account0)),
    ?assertEqual({error, {"Get account unauthorized",SeCo4}}, dderl_account:get(SeCo4, AccountId)),
    ?assertEqual({error, {"Delete account unauthorized",SeCo4}}, dderl_account:delete(SeCo4, AccountId)),
    ?assertEqual({error, {"Delete account unauthorized",SeCo4}}, dderl_account:delete(SeCo4, Account)),
    ?assertEqual({error, {"Exists account unauthorized",SeCo4}}, dderl_account:exists(SeCo4, AccountId)),
    ?assertEqual({error, {"Get role unauthorized",SeCo4}}, dderl_role:get(SeCo4, AccountId)),
    ?assertEqual({error, {"Delete account unauthorized",SeCo4}}, dderl_account:delete(SeCo4, Account1)),
    ?assertEqual({error, {"Update account unauthorized",SeCo4}}, dderl_account:update(SeCo4, Account, Account1)),
    ?assertEqual({error, {"Update account unauthorized",SeCo4}}, dderl_account:update(SeCo4, Account, Account2)),
    io:format(user, "success ~p~n", [manage_accounts_rejected]),


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

    io:format(user, "----TEST--~p:test_manage_account_role rejects~n", [?MODULE]),

    ?assertEqual({error, {"Create role unauthorized",SeCo4}}, dderl_role:create(SeCo4, #ddRole{id=test_role,roles=[],permissions=[perform_tests]})),
    ?assertEqual({error, {"Create role unauthorized",SeCo4}}, dderl_role:create(SeCo4, admin)),
    ?assertEqual({error, {"Get role unauthorized",SeCo4}}, dderl_role:get(SeCo4, AccountId)),
    ?assertEqual({error, {"Grant role unauthorized",SeCo4}}, dderl_role:grant_role(SeCo4, AccountId, admin)),
    ?assertEqual({error, {"Grant role unauthorized",SeCo4}}, dderl_role:grant_role(SeCo4, AccountId, some_unknown_role)),
    ?assertEqual({error, {"Grant role unauthorized",SeCo4}}, dderl_role:grant_role(SeCo4, admin, test_role)),
    ?assertEqual({error, {"Has role unauthorized",SeCo4}}, dderl_role:has_role(SeCo4, AccountId, AccountId)),
    ?assertEqual({error, {"Has role unauthorized",SeCo4}}, dderl_role:has_role(SeCo4, AccountId, admin)),
    ?assertEqual({error, {"Revoke role unauthorized",SeCo4}}, dderl_role:revoke_role(SeCo4, AccountId, admin)),
    io:format(user, "success ~p~n", [manage_account_roles_rejects]), 

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

    io:format(user, "----TEST--~p:test_manage_account_permission_rejects~n", [?MODULE]),

    ?assertEqual({error, {"Has permission unauthorized",SeCo4}}, dderl_role:has_permission(SeCo4, UserId, manage_accounts)), 
    ?assertEqual({error, {"Has permission unauthorized",SeCo4}}, dderl_role:has_permission(SeCo4, AccountId, perform_tests)),
    ?assertEqual({error, {"Grant permission unauthorized",SeCo4}}, dderl_role:grant_permission(SeCo4, test_role, delete_tests)),
    ?assertEqual({error, {"Revoke permission unauthorized",SeCo4}}, dderl_role:revoke_permission(SeCo4, test_role, delete_tests)),
    io:format(user, "success ~p~n", [test_manage_account_permission_rejects]), 


    %% Cleanup only if we arrive at this point
    ?assertEqual({atomic,ok}, dderl_role:delete_tables(SeCo)),
    io:format(user, "success ~p~n", [delete_role_tables]), 
    %% ?assertEqual({aborted,{no_exists,ddRole}}, dderl_role:delete_tables(SeCo)),
    %% io:format(user, "success ~p~n", [delete_role_tables_no_exists]), 
    ?assertEqual({atomic,ok}, if_delete_tables(SeCo)),
    io:format(user, "success ~p~n", [delete_account_tables]), 
    ?assertEqual({aborted,{no_exists,ddAccount}}, if_delete_tables(SeCo)),
    io:format(user, "success ~p~n", [delete_account_tables_no_exists]), 
    ok.

