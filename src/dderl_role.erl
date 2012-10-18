-module(dderl_role).

-include("dderl.hrl").

-export([ create_table/1
        , delete_table/1
        ]).

-export([ create/2
        , get/2
        , read/2
        , update/2
        , delete/2
        , exists/1
        , add_role/3
        , remove_role/3
        , grant_permission/3
        , revoke_permission/3
        , grant_conn/3
        , revoke_conn/3
        , grant_cmd/3
        , revoke_cmd/3
        ]).



create_table(_RequestorCredentials) ->
    imem_if:build_table(ddRole, record_info(fields, ddRole)).

delete_table(_RequestorCredentials) -> 
    imem_if:delete_table(ddRole).


create(_RequestorCredentials, #ddRole{id=RoleId}=Role) -> 
    case imem_if:read(ddRole, RoleId) of
        [] ->   imem_if:write(ddRole, Role);
        [_] -> {error, {"Role already exists",RoleId}}
    end;
create(_RequestorCredentials, RoleId) -> 
    case imem_if:read(ddRole, RoleId) of
        [] ->   imem_if:write(ddRole, #ddRole{id=RoleId});
        [_] -> {error, {"Role already exists",RoleId}}
    end.

read(_RequestorCredentials, RoleId) -> 
    imem_if:read(ddRole, RoleId).

get(_RequestorCredentials, RoleId) -> 
    case imem_if:read(ddRole, RoleId) of
        [] -> {error, {"Role does not exist", RoleId} };
        [Role] -> Role
    end.

update(_RequestorCredentials,_Role) -> ok.

delete(RequestorCredentials, #ddRole{id=RoleId}=Role) ->
    case imem_if:read(ddRole, RoleId) of
        [] -> {error, {"Role does not exist", RoleId}};
        [Role] -> delete(RequestorCredentials, RoleId);
        [_] -> {error, {"Role is modified by someone else", RoleId}}
    end;
delete(_RequestorCredentials, RoleId) -> 
    imem_if:delete(ddRole, RoleId).

exists(_RoleId) -> ok.

add_role(_RequestorCredentials, _ToRole, _RoleId) -> ok.

remove_role(_RequestorCredentials, _FromRole, _RoleId) -> ok.

grant_permission(_RequestorCredentials, _ToRole, _PermissionId) -> ok.

revoke_permission(_RequestorCredentials, _FromRole, _PermissionId) -> ok.

grant_conn(_RequestorCredentials, _ToRole, _ConnId) -> ok.

revoke_conn(_RequestorCredentials, _FromRole, _ConnId) -> ok.

grant_cmd(_RequestorCredentials, _ToRole, _CmdId) -> ok.

revoke_cmd(_RequestorCredentials, _FromRole, _CmdId) -> ok.

