-module(dderl_saml_handler).

-include_lib("esaml/include/esaml.hrl").
-include("dderl.hrl").

-export([init/3, info/3, terminate/3]).

-export([fwdUrl/1]).

-record(state, {sp, idp}).

init(_Transport, Req, _Args) ->
    ?Info("host       ~p~n", [element(1,cowboy_req:host(Req))]),
    ?Info("host_info  ~p~n", [element(1,cowboy_req:host_info(Req))]),
    ?Info("path       ~p~n", [element(1,cowboy_req:path(Req))]),
    ?Info("path_info  ~p~n", [element(1,cowboy_req:path_info(Req))]),
    ?Info("host_url   ~p~n", [element(1,cowboy_req:host_url(Req))]),
    ?Info("url        ~p~n", [element(1,cowboy_req:url(Req))]),    
    {SP, IdpMeta} = initialize(),
    {Operation, Req2} = cowboy_req:binding(operation, Req),
    {Method, Req3} = cowboy_req:method(Req2),
    process_req(Method, Operation, Req3, #state{sp = SP, idp = IdpMeta}).

% Handles HTTP-POST bound assertions coming back from the IDP.
% handle(<<"POST">>, ?AUTHRESPURLSUFFIX, Req, S = #state{sp = SP}) ->
process_req(<<"POST">>, <<"consume">>, Req, S = #state{sp = SP}) ->
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
    TargetUrl = list_to_binary([Url,UrlSuffix]),
    {ok, Req2} = cowboy_req:reply(302, [
            {<<"Cache-Control">>, <<"no-cache">>},
            {<<"Pragma">>, <<"no-cache">>},
            {<<"Location">>, TargetUrl}
        ], <<"Redirecting...">>, Req1),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) -> ok.

initialize() ->
    % Load the certificate and private key for the SP
    % PrivKey = esaml_util:load_private_key("priv/test.key"),
    % Cert = esaml_util:load_certificate("priv/test.crt"),
    % We build all of our URLs (in metadata, and in requests) based on this
    Base = "https://127.0.0.1:8081/saml",
    % Certificate fingerprints to accept from our IDP
    FPs = ["6b:d1:24:4b:38:cf:6c:1f:4e:53:56:c5:c8:90:63:68:55:5e:27:28"],

    SP = esaml_sp:setup(#esaml_sp{
        % key = PrivKey,
        % certificate = Cert,
        sp_sign_requests = false,
        
        % %% temporary
        % idp_signs_envelopes = false,
        % idp_signs_assertions = false,
        % %%

        trusted_fingerprints = FPs,
        consume_uri = Base ++ "/" ++ "consume",
        metadata_uri = Base ++ "/" ++ "metadata",
        org = #esaml_org{
            % example of multi-lingual data -- only works in #esaml_org{}
            name = [{en, "Foo Bar"}, {de, "Das Foo Bar"}],
            displayname = "Foo Bar",
            url = "http://some.hostname.com"
        },
        tech = #esaml_contact{
            name = "Foo Bar",
            email = "foo@bar.com"
        }
    }),
    % IdpMeta = esaml_util:load_metadata(?IDPMETAURL),
    IdpMeta = esaml_util:load_metadata("https://sts.testswisscom.com/FederationMetadata/2007-06/FederationMetadata.xml"),
    {SP, IdpMeta}.
    

fwdUrl(RelayStateCbFun) when is_function(RelayStateCbFun) ->
    fwdUrl(base64:encode(term_to_binary(RelayStateCbFun)));
fwdUrl(RelayState) when is_binary(RelayState) ->
    {SP, #esaml_idp_metadata{login_location = IDP}} = initialize(),
    esaml_binding:encode_http_redirect(IDP, SP:generate_authn_request(IDP), RelayState).
