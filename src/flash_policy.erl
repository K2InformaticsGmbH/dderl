-module(flash_policy).
-export([start_link/4, init/3]).

start_link(ListenerPid, Socket, Transport, _Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport) ->
    Policy = <<"<?xml version=\"1.0\"?>"
               "<!DOCTYPE cross-domain-policy SYSTEM \"http://www.macromedia.com/xml/dtds/cross-domain-policy.dtd\">"
               "<cross-domain-policy>"
               "<allow-access-from domain=\"*\" secure=\"false\" to-ports=\"*\"/>"
               "</cross-domain-policy>">>,
    io:format("flash socket accept~n", []),
    ok = ranch:accept_ack(ListenerPid),
    Transport:send(Socket, Policy),
    Transport:close(Socket),
    ok.
