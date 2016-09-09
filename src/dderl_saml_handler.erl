-module(dderl_saml_handler).

-include_lib("esaml/include/esaml.hrl").
-include("dderl.hrl").

-export([init/3, info/3, terminate/3]).

-export([fwdUrl/3]).

-record(state, {sp, idp}).

init(_Transport, Req, _Args) ->
    {HostUrl, Req} = cowboy_req:host_url(Req),
    {Url, Req} = cowboy_req:url(Req),
    {SP, IdpMeta} = initialize(HostUrl, Url),
    {Method, Req1} = cowboy_req:method(Req),
    process_req(Method, Req1, #state{sp = SP, idp = IdpMeta}).

process_req(<<"POST">>, Req, S = #state{sp = SP}) ->
    case esaml_cowboy:validate_assertion(SP, fun esaml_util:check_dupe_ets/2, Req) of
        {ok, Assertion, RelayState, Req1} ->
            Fun = binary_to_term(base64:decode(http_uri:decode(binary_to_list(RelayState)))),
            Fun(Req1, Assertion#esaml_assertion.attributes);
        {error, Reason, Req2} ->
            {ok, Req3} = cowboy_req:reply(403, [{<<"content-type">>, <<"text/plain">>}],
                ["Access denied, assertion failed validation:\n", io_lib:format("~p\n", [Reason])],
                Req2),
            {ok, Req3, S}
    end.

info({reply, {saml, UrlSuffix}}, Req, State) ->
    {Url, Req1} = cowboy_req:host_url(Req),
    TargetUrl = list_to_binary([Url, UrlSuffix, "/"]),
    {ok, Req2} = cowboy_req:reply(302, [
            {<<"Cache-Control">>, <<"no-cache">>},
            {<<"Pragma">>, <<"no-cache">>},
            {<<"Location">>, TargetUrl}
        ], <<"Redirecting...">>, Req1),
    {ok, Req2, State};
info({reply, Body}, Req, State) ->
    ?Debug("reply ~n~p to ~p", [Body, State]),
    BodyEnc = if is_binary(Body) -> Body;
                 true -> jsx:encode(Body)
              end,
    {ok, Req1} =  cowboy_req:reply(200, [
          {<<"content-encoding">>, <<"utf-8">>}
        , {<<"content-type">>, <<"application/json">>}
        ], BodyEnc, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) -> ok.

initialize(HostUrl, ConsumeUrl) ->
    % Load the certificate and private key for the SP
    % PrivKey = esaml_util:load_private_key("priv/test.key"),
    % Cert = esaml_util:load_certificate("priv/test.crt"),
    % We build all of our URLs (in metadata, and in requests) based on this
    % Certificate fingerprints to accept from our IDP
    NewHostUrl = re:replace(HostUrl, ":[0-9]+", "", [{return, list}]) ++ "/",
    NewConsumerUrl = re:replace(ConsumeUrl, ":[0-9]+", "", [{return, list}]),
    FPs = ["6b:d1:24:4b:38:cf:6c:1f:4e:53:56:c5:c8:90:63:68:55:5e:27:28"],
    SP = esaml_sp:setup(#esaml_sp{
        % key = PrivKey,
        % certificate = Cert,
        sp_sign_requests = false,
        trusted_fingerprints = FPs,
        consume_uri = NewConsumerUrl,
        metadata_uri = NewHostUrl
    }),
    IdpMeta = #esaml_idp_metadata{org = #esaml_org{name = [],
                                     displayname = [],url = []},
                    tech = #esaml_contact{name = [],email = []},
                    signed_requests = true,
                    login_location = ?IDPLOGINURL,
                    name_format = unknown},
    {SP, IdpMeta}.

fwdUrl(HostUrl, ConsumeUrl, RelayStateCbFun) when is_function(RelayStateCbFun) ->
    fwdUrl(HostUrl, ConsumeUrl, base64:encode(term_to_binary(RelayStateCbFun)));
fwdUrl(HostUrl, ConsumeUrl, RelayState) when is_binary(RelayState) ->
    {SP, #esaml_idp_metadata{login_location = IDP}} = initialize(HostUrl, ConsumeUrl),
    esaml_binding:encode_http_redirect(IDP, SP:generate_authn_request(IDP), RelayState).
