-module(dderl_saml_handler).

-include_lib("esaml/include/esaml.hrl").
-include("dderl.hrl").

-export([init/3, handle/2, terminate/3]).

-record(state, {sp, idp}).

init(_Transport, Req, _Args) ->
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
        trusted_fingerprints = FPs,
        consume_uri = Base ++ "/" ++ binary_to_list(?AUTHRESPURLSUFFIX),
        metadata_uri = Base ++ "/" ++ binary_to_list(?METAURLSUFFIX),
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
    IdpMeta = esaml_util:load_metadata(?IDPMETAURL),
    {ok, Req, #state{sp = SP, idp = IdpMeta}}.

handle(Req, S = #state{}) ->
    {Operation, Req2} = cowboy_req:binding(operation, Req),
    {Method, Req3} = cowboy_req:method(Req2),
    handle(Method, Operation, Req3, S).

% Return our SP metadata as signed XML
% handle(<<"GET">>, ?METAURLSUFFIX, Req, S = #state{sp = SP}) ->
handle(<<"GET">>, <<"metadata">>, Req, S = #state{sp = SP}) ->
    {ok, Req2} = esaml_cowboy:reply_with_metadata(SP, Req),
    {ok, Req2, S};

% Visit /saml/auth to start the authentication process -- we will make an AuthnRequest
% and send it to our IDP
handle(<<"GET">>, <<"auth">>, Req, S = #state{sp = SP,
    idp = #esaml_idp_metadata{login_location = IDP}}) ->
    {ok, Req2} = esaml_cowboy:reply_with_authnreq(SP, IDP, <<"foo">>, Req),
    {ok, Req2, S};

% Handles HTTP-POST bound assertions coming back from the IDP.
% handle(<<"POST">>, ?AUTHRESPURLSUFFIX, Req, S = #state{sp = SP}) ->
handle(<<"POST">>, <<"consume">>, Req, S = #state{sp = SP}) ->
    case esaml_cowboy:validate_assertion(SP, fun esaml_util:check_dupe_ets/2, Req) of
        {ok, Assertion, RelayState, Req2} ->
            Attrs = Assertion#esaml_assertion.attributes,
            Uid = proplists:get_value(uid, Attrs),
            Output = io_lib:format("<html><head><title>SAML SP demo</title></head><body><h1>Hi there!</h1><p>This is the <code>esaml_sp_default</code> demo SP callback module from eSAML.</p><table><tr><td>Your name:</td><td>\n~p\n</td></tr><tr><td>Your UID:</td><td>\n~p\n</td></tr></table><hr /><p>RelayState:</p><pre>\n~p\n</pre><p>The assertion I got was:</p><pre>\n~p\n</pre></body></html>", [Assertion#esaml_assertion.subject#esaml_subject.name, Uid, RelayState, Assertion]),
            {ok, Req3} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}], Output, Req2),
            {ok, Req3, S};

        {error, Reason, Req2} ->
            {ok, Req3} = cowboy_req:reply(403, [{<<"content-type">>, <<"text/plain">>}],
                ["Access denied, assertion failed validation:\n", io_lib:format("~p\n", [Reason])],
                Req2),
            {ok, Req3, S}
    end;

handle(_, _, Req, S = #state{}) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<"Not found">>, Req),
    {ok, Req2, S}.

terminate(_Reason, _Req, _State) -> ok.
