-module(dderl_account).

-include_lib("eunit/include/eunit.hrl").

-include("dderl.hrl").

-export([ create_table/1
        , delete_table/1
        , create/2
        , read/2
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

create(_RequestorCredentials,  #ddAccount{id=AccountId}=Account) -> 
    case imem_if:read(ddAccount, AccountId) of
        [] -> imem_if:write(ddAccount, Account);
        [Account|_] -> {error, {"Account already exists",AccountId}}
    end.

write(_RequestorCredentials,  #ddAccount{id=AccountId}=Account) -> 
        imem_if:write(ddAccount, Account).

read(_RequestorCredentials, AccountId) -> 
    imem_if:read(ddAccount, AccountId).

get(_RequestorCredentials, AccountId) -> 
    case imem_if:read(ddAccount, AccountId) of
        [] -> {error, {"Account does not exist", AccountId} };
        [Account] -> Account
    end.

update(_RequestorCredentials,  #ddAccount{id=AccountId}=Account, AccountNew) -> 
    case imem_if:read(ddAccount, AccountId) of
        [] -> {error, "Account does not exist" };
        [Account|_] -> imem_if:insert_into_table(ddAccount, AccountNew);
        [_|_] -> {error, "Account is modified by someone else" }
    end.

delete(_RequestorCredentials, AccountId) -> 
        imem_if:delete(ddAccount, AccountId).

lock(RequestorCredentials, AccountId) -> 
        Account = read(RequestorCredentials, AccountId),
        update(RequestorCredentials,  Account, Account#ddAccount{isLocked=true}).

unlock(RequestorCredentials, AccountId) -> 
        Account = read(RequestorCredentials, AccountId),
        update(RequestorCredentials,  Account, Account#ddAccount{isLocked=false}).

exists(AccountId) -> 
    case imem_if:read(ddAccount, AccountId) of
        [] -> false;
        [_Account|_] -> true
    end.

authenticate(_Credentials) -> ok.

authorize(_Credentials, _Permission) -> ok.

authorize_all(_Credentials, _Permissions) -> ok.

authorize_some(_Credentials, _Permissions) -> ok.

%% ----- TESTS ------------------------------------------------

setup() -> 
    io:format(user, "building imem cluster with ~p~n", [nodes()]),
    build_cluster(nodes()),
    dderl:ensure_started(imem),
    ok.

build_cluster([]) ->
    io:format(user, "imem cluster build up done~n", []);
build_cluster([N|Nodes]) ->
    case net_adm:ping(N) of
        pong -> io:format(user, "found clustering node ~p~n", [N]);
        pang -> io:format(user, "clustering node ~p not found~n", [N])
    end,
    build_cluster(Nodes).

teardown(_) -> 
    %% dderl_account:delete_table(no_credentials),
    ok.

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
    io:format(user, "----TEST--~p:test_create_table~n", [?MODULE]),
    ?assertEqual({atomic,ok}, dderl_account:create_table(no_credentials)),
    io:format(user, "~p~n", [create_table]), 
    ?assertMatch({aborted,{already_exists,ddAccount}}, dderl_account:create_table(no_credentials)),
    io:format(user, "~p~n", [already_exists]), 
    ?assertEqual({atomic,ok}, dderl_account:delete_table(no_credentials)),
    io:format(user, "~p~n", [delete_table]), 
    ?assertEqual({atomic,ok}, dderl_account:delete_table(no_credentials)),
    io:format(user, "~p~n", [delete_nonexisting_table]), 
    %% ?assertEqual({aborted,{no_exists,ddAccount}}, dderl_account:remove_table(no_credentials)),
    %% io:format(user, "~p~n", [no_exists]), 
    ?assertEqual({atomic,ok}, dderl_account:create_table(no_credentials)),
    io:format(user, "~p~n", [create_table]), 
    io:format(user, "----TEST--~p:test_create_account~n", [?MODULE]),
    AccountId = make_ref(),
    Account = #ddAccount{id=AccountId,name= <<"test">>,credentials=no_credentials,fullName= <<"FullName">>},
    ?assertEqual(ok, dderl_account:create(no_credentials, Account)),
    io:format(user, "~p~n", [create]), 
    ?assertEqual({error, {"Account already exists",AccountId}}, dderl_account:create(no_credentials, Account)),
    io:format(user, "~p~n", [already_exists]), 
    ?assertEqual(ok, dderl_account:delete(no_credentials, AccountId)),
    io:format(user, "~p~n", [delete]), 
    ?assertEqual(ok, dderl_account:delete(no_credentials, AccountId)),
    io:format(user, "~p~n", [re_delete]), 
    ?assertEqual({error,{no_exists,ddAccount}}, dderl_account:remove(no_credentials, AccountId)),
    io:format(user, "~p~n", [remove]), 
    ?assertEqual(ok, dderl_account:create(no_credentials, Account)),
    io:format(user, "~p~n", [create]), 
    ok.
    

