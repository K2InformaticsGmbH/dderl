-module(dderl_role).

-include("dderl.hrl").

-export([ create_tables/1
        , drop_tables/1
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
        , grant_quota/4
        , revoke_quota/3
        ]).

-export([ has_role/3
        , has_permission/3
        , have_role/2
        , have_permission/2
%%        , my_quota/2
        ]).


%% --Interface functions  (calling imem_if for now) ----------------------------------

if_create_tables(_SeCo) ->
    imem_if:create_cluster_table(ddRole, record_info(fields, ddRole),[]).

if_drop_tables(_SeCo) -> 
    imem_if:drop_table(ddRole).

if_write(_SeCo, #ddRole{}=Role) -> 
    imem_if:write(ddRole, Role).

if_read(_SeCo, RoleId) -> 
    imem_if:read(ddRole, RoleId).

if_get(SeCo, RoleId) -> 
    case if_read(SeCo, RoleId) of
        [] -> {error, {"Role does not exist", RoleId}};
        [Role] -> Role
    end.

% if_select(_SeCo, MatchSpec) ->
%     imem_if:select_rows(ddRole, MatchSpec). 

if_delete(_SeCo, RoleId) ->
    imem_if:delete(ddRole, RoleId).

if_has_role(_SeCo, _RootRoleId, _RootRoleId) ->
    true;
if_has_role(SeCo, RootRoleId, RoleId) ->
    case if_get(SeCo, RootRoleId) of
        {error, Error} ->               {error, Error};
        #ddRole{roles=[]} ->            false;
        #ddRole{roles=ChildRoles} ->    if_has_child_role(SeCo,  ChildRoles, RoleId)
    end.

if_has_child_role(_SeCo, [], _RoleId) -> false;
if_has_child_role(SeCo, [RootRoleId|OtherRoles], RoleId) ->
    case if_has_role(SeCo, RootRoleId, RoleId) of
        {error, Error} ->               {error, Error};
        true ->                         true;
        false ->                        if_has_child_role(SeCo, OtherRoles, RoleId)
    end.

if_has_permission(SeCo, RootRoleId, PermissionId) ->
    case if_get(SeCo, RootRoleId) of
        {error, Error} ->                       
            {error, Error};
        #ddRole{permissions=[],roles=[]} ->     
            false;
        #ddRole{permissions=Permissions, roles=[]} -> 
            lists:member(PermissionId, Permissions);
        #ddRole{permissions=Permissions, roles=ChildRoles} ->
            case lists:member(PermissionId, Permissions) of
                true ->     true;
                false ->    if_has_child_permission(SeCo,  ChildRoles, PermissionId)
            end            
    end.

if_has_child_permission(_SeCo, [], _PermissionId) -> false;
if_has_child_permission(SeCo, [RootRoleId|OtherRoles], PermissionId) ->
    case if_has_permission(SeCo, RootRoleId, PermissionId) of
        {error, Error} ->               {error, Error};
        true ->                         true;
        false ->                        if_has_child_permission(SeCo, OtherRoles, PermissionId)
    end.


%% --Implementation ------------------------------------------------------------------

create_tables(SeCo) ->
    if_create_tables(SeCo).

drop_tables(SeCo) -> 
    case have_permission(SeCo, manage_accounts) of
        true ->     if_drop_tables(SeCo);
        false ->    {error, {"Drop role tables unauthorized",SeCo}}
    end.

create(SeCo, #ddRole{id=RoleId}=Role) -> 
    case have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, RoleId) of
                        [] ->   %% ToDo: Check if roles contained in Role are all defined $$$$$$$$$$$$$$
                                if_write(SeCo, Role);
                        [_] -> {error, {"Role already exists",RoleId}}
                    end;
        false ->    {error, {"Create role unauthorized",SeCo}}
    end;        
create(SeCo, RoleId) -> 
    case have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, RoleId) of
                        [] ->   if_write(SeCo, #ddRole{id=RoleId});
                        [_] -> {error, {"Role already exists",RoleId}}
                    end;
        false ->    {error, {"Create role unauthorized",SeCo}}
    end.

get(SeCo, RoleId) -> 
    case have_permission(SeCo, manage_accounts) of
        true ->     if_get(SeCo, RoleId);
        false ->    {error, {"Get role unauthorized",SeCo}}
    end.            

update(SeCo, #ddRole{id=RoleId}=Role, RoleNew) -> 
    case have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, RoleId) of
                        [] -> {error, {"Role does not exist", RoleId}};
                        [Role] -> if_write(SeCo, RoleNew);
                        [_] -> {error, {"Role is modified by someone else", RoleId}}
                    end;
        false ->    {error, {"Update role unauthorized",SeCo}}
    end.

delete(SeCo, #ddRole{id=RoleId}=Role) ->
    case have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, RoleId) of
                        [] -> {error, {"Role does not exist", RoleId}};
                        [Role] -> delete(SeCo, RoleId);
                        [_] -> {error, {"Role is modified by someone else", RoleId}}
                    end;
        false ->    {error, {"Delete role unauthorized",SeCo}}
    end;
delete(SeCo, RoleId) -> 
    case have_permission(SeCo, manage_accounts) of
        true ->     if_delete(SeCo, RoleId);
        false ->    {error, {"Delete role unauthorized",SeCo}}
    end.

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
   case have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, ToRoleId) of
                        [] -> {error, {"Role does not exist", ToRoleId}};
                        [ToRole] -> grant_role(SeCo, ToRoleId, RoleId);
                        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
                    end;
        false ->    {error, {"Grant role unauthorized",SeCo}}
    end;
grant_role(SeCo, ToRoleId, RoleId) ->
   case have_permission(SeCo, manage_accounts) of
        true ->     case exists(SeCo, RoleId) of
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
                    end;
        false ->    {error, {"Grant role unauthorized",SeCo}}
    end.            

revoke_role(SeCo, #ddRole{id=FromRoleId}=FromRole, RoleId) -> 
   case have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, FromRoleId) of
                        [] -> {error, {"Role does not exist", FromRoleId}};
                        [FromRole] -> revoke_role(SeCo, FromRoleId, RoleId);
                        [_] -> {error, {"Role is modified by someone else", FromRoleId}}
                    end;
        false ->    {error, {"Revoke role unauthorized",SeCo}}
    end;            
revoke_role(SeCo, FromRoleId, RoleId) -> 
   case have_permission(SeCo, manage_accounts) of
        true ->     case get(SeCo, FromRoleId) of
                        #ddRole{roles=Roles}=FromRole ->   
                            update(SeCo,FromRole,FromRole#ddRole{roles=lists:delete(RoleId, Roles)});   
                        Error ->    Error    
                    end;
        false ->    {error, {"Revoke role unauthorized",SeCo}}
    end.            

grant_permission(SeCo, #ddRole{id=ToRoleId}=ToRole, PermissionId) -> 
   case have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, ToRoleId) of
                        [] -> {error, {"Role does not exist", ToRoleId}};
                        [ToRole] -> grant_permission(SeCo, ToRoleId, PermissionId);
                        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
                    end;
        false ->    {error, {"Grant permission unauthorized",SeCo}}
    end;
grant_permission(SeCo, ToRoleId, PermissionId) ->
   case have_permission(SeCo, manage_accounts) of
        true ->     case get(SeCo, ToRoleId) of
                        #ddRole{permissions=Permissions}=ToRole ->   
                            NewPermissions = case lists:member(PermissionId, Permissions) of
                                true ->     Permissions;
                                false ->    lists:append(Permissions, [PermissionId])   %% append because newer = seldom used
                            end,
                            update(SeCo,ToRole,ToRole#ddRole{permissions=NewPermissions});   
                        Error ->    Error    
                    end;
        false ->    {error, {"Grant permission unauthorized",SeCo}}
    end.

revoke_permission(SeCo, #ddRole{id=FromRoleId}=FromRole, PermissionId) -> 
   case have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, FromRoleId) of
                        [] -> {error, {"Role does not exist", FromRoleId}};
                        [FromRole] -> revoke_permission(SeCo, FromRoleId, PermissionId);
                        [_] -> {error, {"Role is modified by someone else", FromRoleId}}
                    end;
        false ->    {error, {"Revoke permission unauthorized",SeCo}}
    end;        
revoke_permission(SeCo, FromRoleId, PermissionId) -> 
   case have_permission(SeCo, manage_accounts) of
        true ->     case get(SeCo, FromRoleId) of
                        #ddRole{permissions=Permissions}=FromRole ->   
                            update(SeCo,FromRole,FromRole#ddRole{permissions=lists:delete(PermissionId, Permissions)});   
                        Error ->    Error    
                    end;
        false ->    {error, {"Revoke permission unauthorized",SeCo}}
    end.

grant_quota(SeCo, #ddRole{id=ToRoleId}=ToRole, QuotaId, Value) -> 
   case have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, ToRoleId) of
                        [] -> {error, {"Role does not exist", ToRoleId}};
                        [ToRole] -> grant_quota(SeCo, ToRoleId, QuotaId, Value);
                        [_] -> {error, {"Role is modified by someone else", ToRoleId}}
                    end;
        false ->    {error, {"Grant quota unauthorized",SeCo}}
    end;
grant_quota(SeCo, ToRoleId, QuotaId, Value) ->
   case have_permission(SeCo, manage_accounts) of
        true ->     case get(SeCo, ToRoleId) of
                        #ddRole{quotas=Quotas}=ToRole ->   
                            OtherQuotas = lists:key_delete(QuotaId, 1, Quotas),
                            update(SeCo,ToRole,ToRole#ddRole{quotas=lists:append(OtherQuotas, [{QuotaId,Value}])});   
                        Error ->    Error    
                    end;
        false ->    {error, {"Grant quota unauthorized",SeCo}}
    end.

revoke_quota(SeCo, #ddRole{id=FromRoleId}=FromRole, QuotaId) -> 
   case have_permission(SeCo, manage_accounts) of
        true ->     case if_read(SeCo, FromRoleId) of
                        [] -> {error, {"Role does not exist", FromRoleId}};
                        [FromRole] -> revoke_quota(SeCo, FromRoleId, QuotaId);
                        [_] -> {error, {"Role is modified by someone else", FromRoleId}}
                    end;
        false ->    {error, {"Revoke quota unauthorized",SeCo}}
    end;        
revoke_quota(SeCo, FromRoleId, QuotaId) -> 
    case have_permission(SeCo, manage_accounts) of
        true ->     case get(SeCo, FromRoleId) of
                        #ddRole{quotas=Quotas}=FromRole ->   
                            update(SeCo,FromRole,FromRole#ddRole{quotas=lists:key_delete(QuotaId, 1, Quotas)});   
                        Error ->    Error    
                    end;
        false ->    {error, {"Revoke quota unauthorized",SeCo}}
    end.


have_role(#ddSeCo{phid=Pid} = SeCo, RoleId) when Pid == self()->
    if_has_role(SeCo, SeCo#ddSeCo.accountId, RoleId);
have_role(#ddSeCo{}, _) -> false.

has_role(SeCo, RootRoleId, RoleId) ->
    case have_permission(SeCo, manage_accounts) of
        true ->     if_has_role(SeCo, RootRoleId, RoleId); 
        false ->    {error, {"Has role unauthorized",SeCo}}
    end.

have_permission(#ddSeCo{phid=Pid} = SeCo, PermissionId)  when Pid == self()  ->
    if_has_permission(SeCo, SeCo#ddSeCo.accountId, PermissionId);
have_permission(#ddSeCo{}, _)   -> false.


has_permission(SeCo, RootRoleId, PermissionId) ->
    case have_permission(SeCo, manage_accounts) of
        true ->     if_has_permission(SeCo, RootRoleId, PermissionId); 
        false ->    {error, {"Has permission unauthorized",SeCo}}
    end.

% has_connection(SeCo, RootRoleId, ConnectionId) ->
%     case if_get(SeCo, RootRoleId) of
%         {error, Error} ->                       
%             {error, Error};
%         #ddRole{dbConns=[],roles=[]} ->     
%             false;
%         #ddRole{dbConns=Connections, roles=[]} -> 
%              lists:member('any', Connections) orelse lists:member(ConnectionId, Connections);
%         #ddRole{dbConns=Connections, roles=ChildRoles} ->
%             case lists:member('any', Connections) orelse lists:member(ConnectionId, Connections) of
%                 true ->     true;
%                 false ->    has_child_connection(SeCo,  ChildRoles, ConnectionId)
%             end            
%     end.

% has_child_connection(_SeCo, [], _ConnectionId) -> false;
% has_child_connection(SeCo, [RootRoleId|OtherRoles], ConnectionId) ->
%     case has_connection(SeCo, RootRoleId, ConnectionId) of
%         {error, Error} ->   {error, Error};
%         true ->             true;
%         false ->            has_child_connection(SeCo, OtherRoles, ConnectionId)
%     end.


%% ----- TESTS ---(implemented in dderl_account)------------------------------
