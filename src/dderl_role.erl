-module(dderl_role).

-include("dderl.hrl").

-export([ create/2
        , read/1
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



create(_RequestorCredentials,_Role) -> ok.

read(_RoleId) -> ok.

update(_RequestorCredentials,_Role) -> ok.

delete(_RequestorCredentials,_RoleId) -> ok.

exists(_RoleId) -> ok.

add_role(_RequestorCredentials, _ToRole, _RoleId) -> ok.

remove_role(_RequestorCredentials, _FromRole, _RoleId) -> ok.

grant_permission(_RequestorCredentials, _ToRole, _PermissionId) -> ok.

revoke_permission(_RequestorCredentials, _FromRole, _PermissionId) -> ok.

grant_conn(_RequestorCredentials, _ToRole, _ConnId) -> ok.

revoke_conn(_RequestorCredentials, _FromRole, _ConnId) -> ok.

grant_cmd(_RequestorCredentials, _ToRole, _CmdId) -> ok.

revoke_cmd(_RequestorCredentials, _FromRole, _CmdId) -> ok.

