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

if_create_table(_SeCo) ->
    imem_if:build_table(ddRole, record_info(fields, ddRole)).

if_delete_table(_SeCo) -> 
    imem_if:delete_table(ddRole).

if_write(_SeCo, #ddRole{}=Role) -> 
    imem_if:write(ddRole, Role).

if_read(_SeCo, RoleId) -> 
    imem_if:read(ddRole, RoleId).

% if_select(_SeCo, MatchSpec) ->
%    imem_if:select_rows(ddRole, MatchSpec). 

if_delete(_SeCo, RoleId) ->
    imem_if:delete(ddRole, RoleId).

%% --Implementation ------------------------------------------------------------------

create_table(SeCo) ->
    if_create_table(SeCo).

delete_table(SeCo) -> 
    if_delete_table(SeCo).

create(SeCo, #ddRole{id=RoleId}=Role) -> 
    case if_read(SeCo, RoleId) of
        [] ->   %% ToDo: Check if roles contained in Role are all defined $$$$$$$$$$$$$$
                if_write(SeCo, Role);
        [_] -> {error, {"Role already exists",RoleId}}
    end;
create(SeCo, RoleId) -> 
    case if_read(SeCo, RoleId) of
        [] ->   if_write(SeCo, #ddRole{id=RoleId});
        [_] -> {error, {"Role already exists",RoleId}}
    end.


get(SeCo, RoleId) -> 
    case if_read(SeCo, RoleId) of
        [] -> {error, {"Role does not exist", RoleId}};
        [Role] -> Role
    end.

update(SeCo, #ddRole{id=RoleId}=Role, RoleNew) -> 
    case if_read(SeCo, RoleId) of
        [] -> {error, {"Role does not exist", RoleId}};
        [Role] -> if_write(SeCo, RoleNew);
        [_] -> {error, {"Role is modified by someone else", RoleId}}
    end.

delete(SeCo, #ddRole{id=RoleId}=Role) ->
    case if_read(SeCo, RoleId) of
        [] -> {error, {"Role does not exist", RoleId}};
        [Role] -> delete(SeCo, RoleId);
        [_] -> {error, {"Role is modified by someone else", RoleId}}
    end;
delete(SeCo, RoleId) -> 
    if_delete(SeCo, RoleId).

exists(SeCo, #ddRole{id=RoleId}=Role) ->    %% exists unchanged
    case if_read(SeCo, RoleId) of
        [] -> false;
        [Role] -> true;
        [_] -> false
    end;
exists(SeCo, RoleId) ->                     %% exists, maybe in changed form
    case if_read(SeCo, RoleId) of
        [] -> false;
        [_] -> true
    end.

grant_role(SeCo, #ddRole{id=ToRoleId}=ToRole, RoleId) -> 
    case if_read(SeCo, ToRoleId) of
        [] -> {error, {"Role does not exist", ToRoleId}};
        [ToRole] -> grant_role(SeCo, ToRoleId, RoleId);
        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
    end;
grant_role(SeCo, ToRoleId, RoleId) ->
    case exists(SeCo, RoleId) of
        false ->  
            {error, {"Role does not exist", RoleId}};
        true ->   
            case get(SeCo, ToRoleId) of
                #ddRole{roles=Roles}=ToRole ->   
                    NewRoles = case lists:member(RoleId, Roles) of
                        true ->     Roles;
                        false ->    lists:append(Roles, [RoleId])       %% append because newer = seldom used
                    end,
                    update(SeCo,ToRole,ToRole#ddRole{roles=NewRoles});   
                Error ->    Error    
            end
    end.

revoke_role(SeCo, #ddRole{id=FromRoleId}=FromRole, RoleId) -> 
    case if_read(SeCo, FromRoleId) of
        [] -> {error, {"Role does not exist", FromRoleId}};
        [FromRole] -> revoke_role(SeCo, FromRoleId, RoleId);
        [_] -> {error, {"Role is modified by someone else", FromRoleId}}
    end;
revoke_role(SeCo, FromRoleId, RoleId) -> 
    case get(SeCo, FromRoleId) of
        #ddRole{roles=Roles}=FromRole ->   
            update(SeCo,FromRole,FromRole#ddRole{roles=lists:delete(RoleId, Roles)});   
        Error ->    Error    
    end.

grant_permission(SeCo, #ddRole{id=ToRoleId}=ToRole, PermissionId) -> 
    case if_read(SeCo, ToRoleId) of
        [] -> {error, {"Role does not exist", ToRoleId}};
        [ToRole] -> grant_permission(SeCo, ToRoleId, PermissionId);
        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
    end;
grant_permission(SeCo, ToRoleId, PermissionId) ->
    case get(SeCo, ToRoleId) of
        #ddRole{permissions=Permissions}=ToRole ->   
            NewPermissions = case lists:member(PermissionId, Permissions) of
                true ->     Permissions;
                false ->    lists:append(Permissions, [PermissionId])   %% append because newer = seldom used
            end,
            update(SeCo,ToRole,ToRole#ddRole{permissions=NewPermissions});   
        Error ->    Error    
    end.

revoke_permission(SeCo, #ddRole{id=FromRoleId}=FromRole, PermissionId) -> 
    case if_read(SeCo, FromRoleId) of
        [] -> {error, {"Role does not exist", FromRoleId}};
        [FromRole] -> revoke_permission(SeCo, FromRoleId, PermissionId);
        [_] -> {error, {"Role is modified by someone else", FromRoleId}}
    end;
revoke_permission(SeCo, FromRoleId, PermissionId) -> 
    case get(SeCo, FromRoleId) of
        #ddRole{permissions=Permissions}=FromRole ->   
            update(SeCo,FromRole,FromRole#ddRole{permissions=lists:delete(PermissionId, Permissions)});   
        Error ->    Error    
    end.

grant_connection(SeCo, #ddRole{id=ToRoleId}=ToRole, ConnectionId) -> 
    case if_read(SeCo, ToRoleId) of
        [] -> {error, {"Role does not exist", ToRoleId}};
        [ToRole] -> grant_connection(SeCo, ToRoleId, ConnectionId);
        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
    end;
grant_connection(SeCo, ToRoleId, ConnectionId) ->
    case get(SeCo, ToRoleId) of
        #ddRole{dbConns=Connections}=ToRole ->   
            NewConnections = case lists:member(ConnectionId, Connections) of
                true ->     Connections;
                false ->    lists:append(Connections, [ConnectionId])   %% append because newer = seldom used
            end,
            update(SeCo,ToRole,ToRole#ddRole{dbConns=NewConnections});   
        Error ->    Error    
    end.

revoke_connection(SeCo, #ddRole{id=FromRoleId}=FromRole, ConnectionId) -> 
    case if_read(SeCo, FromRoleId) of
        [] -> {error, {"Role does not exist", FromRoleId}};
        [FromRole] -> revoke_connection(SeCo, FromRoleId, ConnectionId);
        [_] -> {error, {"Role is modified by someone else", FromRoleId}}
    end;
revoke_connection(SeCo, FromRoleId, ConnectionId) -> 
    case get(SeCo, FromRoleId) of
        #ddRole{dbConns=Connections}=FromRole ->   
            update(SeCo,FromRole,FromRole#ddRole{dbConns=lists:delete(ConnectionId, Connections)});   
        Error ->    Error    
    end.

grant_command(SeCo, #ddRole{id=ToRoleId}=ToRole, CommandId) -> 
    case if_read(SeCo, ToRoleId) of
        [] -> {error, {"Role does not exist", ToRoleId}};
        [ToRole] -> grant_command(SeCo, ToRoleId, CommandId);
        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
    end;
grant_command(SeCo, ToRoleId, CommandId) ->
    case get(SeCo, ToRoleId) of
        #ddRole{dbCmds=Commands}=ToRole ->   
            NewCommands = case lists:member(CommandId, Commands) of
                true ->     Commands;
                false ->    lists:append(Commands, [CommandId])   %% append because newer = seldom used
            end,
            update(SeCo,ToRole,ToRole#ddRole{dbCmds=NewCommands});   
        Error ->    Error    
    end.

revoke_command(SeCo, #ddRole{id=FromRoleId}=FromRole, CommandId) -> 
    case if_read(SeCo, FromRoleId) of
        [] -> {error, {"Role does not exist", FromRoleId}};
        [FromRole] -> revoke_command(SeCo, FromRoleId, CommandId);
        [_] -> {error, {"Role is modified by someone else", FromRoleId}}
    end;
revoke_command(SeCo, FromRoleId, CommandId) -> 
    case get(SeCo, FromRoleId) of
        #ddRole{dbCmds=Commands}=FromRole ->   
            update(SeCo,FromRole,FromRole#ddRole{dbCmds=lists:delete(CommandId, Commands)});   
        Error ->    Error    
    end.

has_role(_SeCo, _RootRoleId, _RootRoleId) ->
    true;
has_role(SeCo, RootRoleId, RoleId) ->
    case get(SeCo, RootRoleId) of
        {error, Error} ->               {error, Error};
        #ddRole{roles=[]} ->            false;
        #ddRole{roles=ChildRoles} ->    has_child_role(SeCo,  ChildRoles, RoleId)
    end.

has_child_role(_SeCo, [], _RoleId) -> false;
has_child_role(SeCo, [RootRoleId|OtherRoles], RoleId) ->
    case has_role(SeCo, RootRoleId, RoleId) of
        {error, Error} ->               {error, Error};
        true ->                         true;
        false ->                        has_child_role(SeCo, OtherRoles, RoleId)
    end.

has_permission(SeCo, RootRoleId, PermissionId) ->
    case get(SeCo, RootRoleId) of
        {error, Error} ->                       
            {error, Error};
        #ddRole{permissions=[],roles=[]} ->     
            false;
        #ddRole{permissions=Permissions, roles=[]} -> 
            lists:member(PermissionId, Permissions);
        #ddRole{permissions=Permissions, roles=ChildRoles} ->
            case lists:member(PermissionId, Permissions) of
                true ->     true;
                false ->    has_child_permission(SeCo,  ChildRoles, PermissionId)
            end            
    end.

has_child_permission(_SeCo, [], _PermissionId) -> false;
has_child_permission(SeCo, [RootRoleId|OtherRoles], PermissionId) ->
    case has_permission(SeCo, RootRoleId, PermissionId) of
        {error, Error} ->               {error, Error};
        true ->                         true;
        false ->                        has_child_permission(SeCo, OtherRoles, PermissionId)
    end.


has_connection(SeCo, RootRoleId, ConnectionId) ->
    case get(SeCo, RootRoleId) of
        {error, Error} ->                       
            {error, Error};
        #ddRole{dbConns=[],roles=[]} ->     
            false;
        #ddRole{dbConns=Connections, roles=[]} -> 
             lists:member('any', Connections) orelse lists:member(ConnectionId, Connections);
        #ddRole{dbConns=Connections, roles=ChildRoles} ->
            case lists:member('any', Connections) orelse lists:member(ConnectionId, Connections) of
                true ->     true;
                false ->    has_child_connection(SeCo,  ChildRoles, ConnectionId)
            end            
    end.

has_child_connection(_SeCo, [], _ConnectionId) -> false;
has_child_connection(SeCo, [RootRoleId|OtherRoles], ConnectionId) ->
    case has_connection(SeCo, RootRoleId, ConnectionId) of
        {error, Error} ->   {error, Error};
        true ->             true;
        false ->            has_child_connection(SeCo, OtherRoles, ConnectionId)
    end.

has_command(SeCo, RootRoleId, CommandId) ->
    case get(SeCo, RootRoleId) of
        {error, Error} ->                       
            {error, Error};
        #ddRole{dbCmds=[],roles=[]} ->     
            false;
        #ddRole{dbCmds=Commands, roles=[]} -> 
             lists:member('any', Commands) orelse lists:member(CommandId, Commands);
        #ddRole{dbCmds=Commands, roles=ChildRoles} ->
            case lists:member('any', Commands) orelse lists:member(CommandId, Commands) of
                true ->     true;
                false ->    has_child_command(SeCo,  ChildRoles, CommandId)
            end            
    end.

has_child_command(_SeCo, [], _CommandId) -> false;
has_child_command(SeCo, [RootRoleId|OtherRoles], CommandId) ->
    case has_command(SeCo, RootRoleId, CommandId) of
        {error, Error} ->   {error, Error};
        true ->             true;
        false ->            has_child_command(SeCo, OtherRoles, CommandId)
    end.

%% ----- TESTS ---(implemented in dderl_account)------------------------------
