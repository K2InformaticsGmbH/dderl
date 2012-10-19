-module(dderl_role).

-include("dderl.hrl").

-export([ create_table/1
        , delete_table/1
        ]).

-export([ create/2
        , get/2
        , read/2
        , update/3
        , delete/2
        , exists/1
        , grant_role/3
        , revoke_role/3
        , grant_permission/3
        , revoke_permission/3
        , grant_conn/3
        , revoke_conn/3
        , grant_cmd/3
        , revoke_cmd/3
        ]).

-export([ has_role/3
%        , has_permission/3
%        , has_conn/3
%        , has_cmd/3
        ]).

has_role(_RequestorCredentials, _RootRoleId, _RootRoleId) ->
    true;
has_role(RequestorCredentials, RootRoleId, RoleId) ->
    case get(RequestorCredentials, RootRoleId) of
        {error, Error} ->               {error, Error};
        #ddRole{roles=[]} ->            false;
        #ddRole{roles=ChildRoles} ->    has_child_role(RequestorCredentials,  ChildRoles, RoleId)
    end.

has_child_role(_RequestorCredentials, [], _RoleId) -> false;
has_child_role(RequestorCredentials, [RootRoleId|OtherRoles], RoleId) ->
    case has_role(RequestorCredentials, RootRoleId, RoleId) of
        {error, Error} ->               {error, Error};
        true ->                         true;
        false ->                        has_child_role(RequestorCredentials, OtherRoles, RoleId)
    end.

create_table(_RequestorCredentials) ->
    imem_if:build_table(ddRole, record_info(fields, ddRole)).

delete_table(_RequestorCredentials) -> 
    imem_if:delete_table(ddRole).


create(_RequestorCredentials, #ddRole{id=RoleId}=Role) -> 
    case imem_if:read(ddRole, RoleId) of
        [] ->   %% ToDo: Check if roles contained in Role are all defined $$$$$$$$$$$$$$
                imem_if:write(ddRole, Role);
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
        [] -> {error, {"Role does not exist", RoleId}};
        [Role] -> Role
    end.

update(_RequestorCredentials, #ddRole{id=RoleId}=Role, RoleNew) -> 
    case imem_if:read(ddRole, RoleId) of
        [] -> {error, {"Role does not exist", RoleId}};
        [Role] -> imem_if:insert_into_table(ddRole, RoleNew);
        [_] -> {error, {"Role is modified by someone else", RoleId}}
    end.

delete(RequestorCredentials, #ddRole{id=RoleId}=Role) ->
    case imem_if:read(ddRole, RoleId) of
        [] -> {error, {"Role does not exist", RoleId}};
        [Role] -> delete(RequestorCredentials, RoleId);
        [_] -> {error, {"Role is modified by someone else", RoleId}}
    end;
delete(_RequestorCredentials, RoleId) -> 
    imem_if:delete(ddRole, RoleId).

exists(#ddRole{id=RoleId}=Role) ->               %% exists unchanged
    case imem_if:read(ddRole, RoleId) of
        [] -> false;
        [Role] -> true;
        [_] -> false
    end;
exists(RoleId) ->                                %% exists, maybe in changed form
    case imem_if:read(ddRole, RoleId) of
        [] -> false;
        [_] -> true
    end.

grant_role(RequestorCredentials, #ddRole{id=ToRoleId}=ToRole, RoleId) -> 
    case imem_if:read(ddRole, ToRoleId) of
        [] -> {error, {"Role does not exist", ToRoleId}};
        [ToRole] -> grant_role(RequestorCredentials, ToRoleId, RoleId);
        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
    end;
grant_role(RequestorCredentials, ToRoleId, RoleId) ->
    case exists(RoleId) of
        false ->  
            {error, {"Role does not exist", RoleId}};
        true ->   
            case get(RequestorCredentials, ToRoleId) of
                #ddRole{roles=Roles}=ToRole ->   
                    NewRoles = case lists:member(RoleId, Roles) of
                        true ->     Roles;
                        false ->    lists:append(Roles, [RoleId])       %% append because newer = seldom used
                    end,
                    update(RequestorCredentials,ToRole,ToRole#ddRole{roles=NewRoles});   
                Error ->    Error    
            end
    end.

revoke_role(RequestorCredentials, #ddRole{id=FromRoleId}=FromRole, RoleId) -> 
    case imem_if:read(ddRole, FromRoleId) of
        [] -> {error, {"Role does not exist", FromRoleId}};
        [FromRole] -> revoke_role(RequestorCredentials, FromRoleId, RoleId);
        [_] -> {error, {"Role is modified by someone else", FromRoleId}}
    end;
revoke_role(RequestorCredentials, FromRoleId, RoleId) -> 
    case get(RequestorCredentials, FromRoleId) of
        #ddRole{roles=Roles}=FromRole ->   
            update(RequestorCredentials,FromRole,FromRole#ddRole{roles=lists:delete(RoleId, Roles)});   
        Error ->    Error    
    end.

grant_permission(_RequestorCredentials, _ToRole, _PermissionId) -> ok.

revoke_permission(_RequestorCredentials, _FromRole, _PermissionId) -> ok.

grant_conn(_RequestorCredentials, _ToRole, _ConnId) -> ok.

revoke_conn(_RequestorCredentials, _FromRole, _ConnId) -> ok.

grant_cmd(_RequestorCredentials, _ToRole, _CmdId) -> ok.

revoke_cmd(_RequestorCredentials, _FromRole, _CmdId) -> ok.

