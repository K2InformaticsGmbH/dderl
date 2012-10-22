-module(dderl_role).

-include("dderl.hrl").

-export([ create_table/1
        , delete_table/1
        ]).

-export([ create/2
        , get/2
        , update/3
        , delete/2
        , exists/2
        , grant_role/3
        , revoke_role/3
        , grant_permission/3
        , revoke_permission/3
        , grant_connection/3
        , revoke_connection/3
        , grant_command/3
        , revoke_command/3
        ]).

-export([ has_role/3
        , has_permission/3
        , has_connection/3
        , has_command/3
        ]).


%% --Interface functions  (calling imem_if for now) ----------------------------------

if_create_table(_RequestorCredentials) ->
    imem_if:build_table(ddRole, record_info(fields, ddRole)).

if_delete_table(_RequestorCredentials) -> 
    imem_if:delete_table(ddRole).

if_write(_RequestorCredentials, #ddRole{}=Role) -> 
    imem_if:write(ddRole, Role).

if_read(_RequestorCredentials, RoleId) -> 
    imem_if:read(ddRole, RoleId).

% if_select(_RequestorCredentials, MatchSpec) ->
%    imem_if:select_rows(ddRole, MatchSpec). 

if_delete(_RequestorCredentials, RoleId) ->
    imem_if:delete(ddRole, RoleId).

%% --Implementation ------------------------------------------------------------------

create_table(RequestorCredentials) ->
    if_create_table(RequestorCredentials).

delete_table(RequestorCredentials) -> 
    if_delete_table(RequestorCredentials).

create(RequestorCredentials, #ddRole{id=RoleId}=Role) -> 
    case if_read(RequestorCredentials, RoleId) of
        [] ->   %% ToDo: Check if roles contained in Role are all defined $$$$$$$$$$$$$$
                if_write(RequestorCredentials, Role);
        [_] -> {error, {"Role already exists",RoleId}}
    end;
create(RequestorCredentials, RoleId) -> 
    case if_read(RequestorCredentials, RoleId) of
        [] ->   if_write(RequestorCredentials, #ddRole{id=RoleId});
        [_] -> {error, {"Role already exists",RoleId}}
    end.


get(RequestorCredentials, RoleId) -> 
    case if_read(RequestorCredentials, RoleId) of
        [] -> {error, {"Role does not exist", RoleId}};
        [Role] -> Role
    end.

update(RequestorCredentials, #ddRole{id=RoleId}=Role, RoleNew) -> 
    case if_read(RequestorCredentials, RoleId) of
        [] -> {error, {"Role does not exist", RoleId}};
        [Role] -> if_write(RequestorCredentials, RoleNew);
        [_] -> {error, {"Role is modified by someone else", RoleId}}
    end.

delete(RequestorCredentials, #ddRole{id=RoleId}=Role) ->
    case if_read(RequestorCredentials, RoleId) of
        [] -> {error, {"Role does not exist", RoleId}};
        [Role] -> delete(RequestorCredentials, RoleId);
        [_] -> {error, {"Role is modified by someone else", RoleId}}
    end;
delete(RequestorCredentials, RoleId) -> 
    if_delete(RequestorCredentials, RoleId).

exists(RequestorCredentials, #ddRole{id=RoleId}=Role) ->    %% exists unchanged
    case if_read(RequestorCredentials, RoleId) of
        [] -> false;
        [Role] -> true;
        [_] -> false
    end;
exists(RequestorCredentials, RoleId) ->                     %% exists, maybe in changed form
    case if_read(RequestorCredentials, RoleId) of
        [] -> false;
        [_] -> true
    end.

grant_role(RequestorCredentials, #ddRole{id=ToRoleId}=ToRole, RoleId) -> 
    case if_read(RequestorCredentials, ToRoleId) of
        [] -> {error, {"Role does not exist", ToRoleId}};
        [ToRole] -> grant_role(RequestorCredentials, ToRoleId, RoleId);
        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
    end;
grant_role(RequestorCredentials, ToRoleId, RoleId) ->
    case exists(RequestorCredentials, RoleId) of
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
    case if_read(RequestorCredentials, FromRoleId) of
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

grant_permission(RequestorCredentials, #ddRole{id=ToRoleId}=ToRole, PermissionId) -> 
    case if_read(RequestorCredentials, ToRoleId) of
        [] -> {error, {"Role does not exist", ToRoleId}};
        [ToRole] -> grant_permission(RequestorCredentials, ToRoleId, PermissionId);
        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
    end;
grant_permission(RequestorCredentials, ToRoleId, PermissionId) ->
    case get(RequestorCredentials, ToRoleId) of
        #ddRole{permissions=Permissions}=ToRole ->   
            NewPermissions = case lists:member(PermissionId, Permissions) of
                true ->     Permissions;
                false ->    lists:append(Permissions, [PermissionId])   %% append because newer = seldom used
            end,
            update(RequestorCredentials,ToRole,ToRole#ddRole{permissions=NewPermissions});   
        Error ->    Error    
    end.

revoke_permission(RequestorCredentials, #ddRole{id=FromRoleId}=FromRole, PermissionId) -> 
    case if_read(RequestorCredentials, FromRoleId) of
        [] -> {error, {"Role does not exist", FromRoleId}};
        [FromRole] -> revoke_permission(RequestorCredentials, FromRoleId, PermissionId);
        [_] -> {error, {"Role is modified by someone else", FromRoleId}}
    end;
revoke_permission(RequestorCredentials, FromRoleId, PermissionId) -> 
    case get(RequestorCredentials, FromRoleId) of
        #ddRole{permissions=Permissions}=FromRole ->   
            update(RequestorCredentials,FromRole,FromRole#ddRole{permissions=lists:delete(PermissionId, Permissions)});   
        Error ->    Error    
    end.

grant_connection(RequestorCredentials, #ddRole{id=ToRoleId}=ToRole, ConnectionId) -> 
    case if_read(RequestorCredentials, ToRoleId) of
        [] -> {error, {"Role does not exist", ToRoleId}};
        [ToRole] -> grant_connection(RequestorCredentials, ToRoleId, ConnectionId);
        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
    end;
grant_connection(RequestorCredentials, ToRoleId, ConnectionId) ->
    case get(RequestorCredentials, ToRoleId) of
        #ddRole{dbConns=Connections}=ToRole ->   
            NewConnections = case lists:member(ConnectionId, Connections) of
                true ->     Connections;
                false ->    lists:append(Connections, [ConnectionId])   %% append because newer = seldom used
            end,
            update(RequestorCredentials,ToRole,ToRole#ddRole{dbConns=NewConnections});   
        Error ->    Error    
    end.

revoke_connection(RequestorCredentials, #ddRole{id=FromRoleId}=FromRole, ConnectionId) -> 
    case if_read(RequestorCredentials, FromRoleId) of
        [] -> {error, {"Role does not exist", FromRoleId}};
        [FromRole] -> revoke_connection(RequestorCredentials, FromRoleId, ConnectionId);
        [_] -> {error, {"Role is modified by someone else", FromRoleId}}
    end;
revoke_connection(RequestorCredentials, FromRoleId, ConnectionId) -> 
    case get(RequestorCredentials, FromRoleId) of
        #ddRole{dbConns=Connections}=FromRole ->   
            update(RequestorCredentials,FromRole,FromRole#ddRole{dbConns=lists:delete(ConnectionId, Connections)});   
        Error ->    Error    
    end.

grant_command(RequestorCredentials, #ddRole{id=ToRoleId}=ToRole, CommandId) -> 
    case if_read(RequestorCredentials, ToRoleId) of
        [] -> {error, {"Role does not exist", ToRoleId}};
        [ToRole] -> grant_command(RequestorCredentials, ToRoleId, CommandId);
        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
    end;
grant_command(RequestorCredentials, ToRoleId, CommandId) ->
    case get(RequestorCredentials, ToRoleId) of
        #ddRole{dbCmds=Commands}=ToRole ->   
            NewCommands = case lists:member(CommandId, Commands) of
                true ->     Commands;
                false ->    lists:append(Commands, [CommandId])   %% append because newer = seldom used
            end,
            update(RequestorCredentials,ToRole,ToRole#ddRole{dbCmds=NewCommands});   
        Error ->    Error    
    end.

revoke_command(RequestorCredentials, #ddRole{id=FromRoleId}=FromRole, CommandId) -> 
    case if_read(RequestorCredentials, FromRoleId) of
        [] -> {error, {"Role does not exist", FromRoleId}};
        [FromRole] -> revoke_command(RequestorCredentials, FromRoleId, CommandId);
        [_] -> {error, {"Role is modified by someone else", FromRoleId}}
    end;
revoke_command(RequestorCredentials, FromRoleId, CommandId) -> 
    case get(RequestorCredentials, FromRoleId) of
        #ddRole{dbCmds=Commands}=FromRole ->   
            update(RequestorCredentials,FromRole,FromRole#ddRole{dbCmds=lists:delete(CommandId, Commands)});   
        Error ->    Error    
    end.

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

has_permission(RequestorCredentials, RootRoleId, PermissionId) ->
    case get(RequestorCredentials, RootRoleId) of
        {error, Error} ->                       
            {error, Error};
        #ddRole{permissions=[],roles=[]} ->     
            false;
        #ddRole{permissions=Permissions, roles=[]} -> 
            lists:member(PermissionId, Permissions);
        #ddRole{permissions=Permissions, roles=ChildRoles} ->
            case lists:member(PermissionId, Permissions) of
                true ->     true;
                false ->    has_child_permission(RequestorCredentials,  ChildRoles, PermissionId)
            end            
    end.

has_child_permission(_RequestorCredentials, [], _PermissionId) -> false;
has_child_permission(RequestorCredentials, [RootRoleId|OtherRoles], PermissionId) ->
    case has_permission(RequestorCredentials, RootRoleId, PermissionId) of
        {error, Error} ->               {error, Error};
        true ->                         true;
        false ->                        has_child_permission(RequestorCredentials, OtherRoles, PermissionId)
    end.


has_connection(RequestorCredentials, RootRoleId, ConnectionId) ->
    case get(RequestorCredentials, RootRoleId) of
        {error, Error} ->                       
            {error, Error};
        #ddRole{dbConns=[],roles=[]} ->     
            false;
        #ddRole{dbConns=Connections, roles=[]} -> 
             lists:member('any', Connections) orelse lists:member(ConnectionId, Connections);
        #ddRole{dbConns=Connections, roles=ChildRoles} ->
            case lists:member('any', Connections) orelse lists:member(ConnectionId, Connections) of
                true ->     true;
                false ->    has_child_connection(RequestorCredentials,  ChildRoles, ConnectionId)
            end            
    end.

has_child_connection(_RequestorCredentials, [], _ConnectionId) -> false;
has_child_connection(RequestorCredentials, [RootRoleId|OtherRoles], ConnectionId) ->
    case has_connection(RequestorCredentials, RootRoleId, ConnectionId) of
        {error, Error} ->   {error, Error};
        true ->             true;
        false ->            has_child_connection(RequestorCredentials, OtherRoles, ConnectionId)
    end.

has_command(RequestorCredentials, RootRoleId, CommandId) ->
    case get(RequestorCredentials, RootRoleId) of
        {error, Error} ->                       
            {error, Error};
        #ddRole{dbCmds=[],roles=[]} ->     
            false;
        #ddRole{dbCmds=Commands, roles=[]} -> 
             lists:member('any', Commands) orelse lists:member(CommandId, Commands);
        #ddRole{dbCmds=Commands, roles=ChildRoles} ->
            case lists:member('any', Commands) orelse lists:member(CommandId, Commands) of
                true ->     true;
                false ->    has_child_command(RequestorCredentials,  ChildRoles, CommandId)
            end            
    end.

has_child_command(_RequestorCredentials, [], _CommandId) -> false;
has_child_command(RequestorCredentials, [RootRoleId|OtherRoles], CommandId) ->
    case has_command(RequestorCredentials, RootRoleId, CommandId) of
        {error, Error} ->   {error, Error};
        true ->             true;
        false ->            has_child_command(RequestorCredentials, OtherRoles, CommandId)
    end.

%% ----- TESTS ---(implemented in dderl_account)------------------------------
