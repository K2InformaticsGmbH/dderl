-module(dderl_saml_handler).

-include_lib("esaml/include/esaml.hrl").
-include("dderl.hrl").

-export([init/3, info/3, terminate/3]).

-export([fwdUrl/3]).

-record(state, {sp, idp}).

-define(CERTKEYCACHE, samlCertKey).

init(_Transport, Req, _Args) ->
    {HostUrl, Req} = cowboy_req:host_url(Req),
    {Url, Req} = cowboy_req:url(Req),
    {SP, IdpMeta} = initialize(HostUrl, Url),
    {Method, Req1} = cowboy_req:method(Req),
    process_req(Method, Req1, #state{sp = SP, idp = IdpMeta}).

process_req(<<"POST">>, Req, S = #state{sp = #esaml_sp{metadata_uri = Url} = SP}) ->
    case esaml_cowboy:validate_assertion(SP, fun esaml_util:check_dupe_ets/2, Req) of
        {ok, Assertion, RelayState, Req1} ->
            Fun = binary_to_term(base64:decode(http_uri:decode(binary_to_list(RelayState)))),
            Fun(Req1, Assertion#esaml_assertion.attributes),
            {loop, Req1, S, 50000, hibernate};
        {error, Reason, Req2} ->
            {ok, Req3} = unauthorized(Req2, Url),
            ?Error("SAML - Auth error : ~p", [Reason]),
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
info({reply, Body}, Req, #state{sp = #esaml_sp{metadata_uri = Url}} = State) ->
    ?Error("logging in via saml ~s ", [Body]),
    {ok, Req1} = unauthorized(Req, Url),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) -> ok.

initialize(HostUrl, ConsumeUrl) ->
    % Load the certificate and private key for the SP
    #{cert := Cert, key := PrivKey} = fetch_cert_key(),
    NewHostUrl = re:replace(HostUrl, ":[0-9]+", "", [{return, list}]) ++ "/",
    NewConsumerUrl = re:replace(ConsumeUrl, ":[0-9]+", "", [{return, list}]),
    VerifyResponseSignature = ?VERIFYRESPONSESIGN,
    FingerPrints = 
        case {?SAMLFINGERPRINT, VerifyResponseSignature} of 
            {_, false} -> [<<"none">>];
            {'$none', true} -> error("No Certificate Thumbprint configured");
            {FPs, _} -> 
                try esaml_util:convert_fingerprints(FPs)
                catch
                    _:Error ->
                        ?Error("No valid Certificate Thumbprints configured : ~p ~p", [Error, erlang:get_stacktrace()]),
                        error("No valid Certificate Thumbprints configured")
                end
        end,
    SP = esaml_sp:setup(#esaml_sp{
        key = PrivKey,
        certificate = Cert,
        sp_sign_requests = ?SAMLSIGNREQUEST,
        trusted_fingerprints = FingerPrints,
        consume_uri = NewConsumerUrl,
        metadata_uri = NewHostUrl,
        idp_signs_envelopes = false,
        idp_signs_assertions = VerifyResponseSignature,
        encrypt_mandatory = ?ISENCRYPTMANDATORY
    }),
    IdpMeta = #esaml_idp_metadata{org = #esaml_org{name = [],
                                     displayname = [],url = []},
                    tech = #esaml_contact{name = [],email = []},
                    login_location = ?IDPLOGINURL,
                    name_format = unknown},
    {SP, IdpMeta}.

fwdUrl(HostUrl, ConsumeUrl, RelayStateCbFun) when is_function(RelayStateCbFun) ->
    fwdUrl(HostUrl, ConsumeUrl, base64:encode(term_to_binary(RelayStateCbFun)));
fwdUrl(HostUrl, ConsumeUrl, RelayState) when is_binary(RelayState) ->
    {SP, #esaml_idp_metadata{login_location = IDP}} = initialize(HostUrl, ConsumeUrl),
    esaml_binding:encode_http_post(IDP, SP:generate_authn_request(IDP), RelayState).

fetch_cert_key() ->
    case imem_cache:read({?MODULE, ?CERTKEYCACHE}) of
        [] -> fetch_cert_key(?SAMLCERTKEY);
        [CertKey] -> CertKey
    end.

fetch_cert_key('$no_cert_key') ->
    {CertBin, KeyBin} = 
    case file:read_file(priv_cert_file("saml.crt")) of
        {ok, CertB} -> 
            case file:read_file(priv_cert_file("saml.key")) of
                {ok, KeyB} -> {CertB, KeyB};
                _ -> error("No SAML Private key configured")
            end;
        _ -> error("No SAML Certificate configured")
    end,
    [{cert, Cert}] = imem_server:get_cert_key(CertBin),
    Key = get_priv_key(KeyBin),
    SamlSslCache = #{cert => Cert, key => Key},
    SamlSslConfig = #{cert => CertBin, key => KeyBin},
    ?Info("Installing SAML SSL ~p", [SamlSslConfig]),
    ?PUT_CONFIG(?CERTKEYCACHE, [], SamlSslConfig,
                list_to_binary(
                  io_lib:format(
                    "Installed at ~p on ~s",
                    [node(), imem_datatype:timestamp_to_io(
                               os:timestamp())]))),
    imem_cache:write({?MODULE, ?CERTKEYCACHE}, SamlSslCache),
    SamlSslCache;
fetch_cert_key(#{cert := CertBin, key := KeyBin}) ->
    CertFile = priv_cert_file("saml.crt"),
    KeyFile = priv_cert_file("saml.key"),
    case file:read_file(CertFile) of
        {ok, CertBin} -> nop;
        _ -> ok = file:write_file(CertFile, CertBin)
    end,
    case file:read_file(KeyFile) of
        {ok, KeyBin} -> nop;
        _ -> ok = file:write_file(KeyFile, KeyBin)
    end,
    [{cert, Cert}] = imem_server:get_cert_key(CertBin),
    Key = get_priv_key(KeyBin),
    SamlSsl = #{cert => Cert, key => Key},
    imem_cache:write({?MODULE, ?CERTKEYCACHE}, SamlSsl),
    SamlSsl.
    
priv_cert_file(FileName) -> filename:join([code:priv_dir("dderl"), "certs", FileName]).

get_priv_key(KeyBin) ->
    [KeyEntry] = public_key:pem_decode(KeyBin),
    case public_key:pem_entry_decode(KeyEntry) of
        #'PrivateKeyInfo'{privateKey = KeyData} ->
            KeyDataBin = if is_list(KeyData) -> list_to_binary(KeyData);
                            true -> KeyData
                         end,
            public_key:der_decode('RSAPrivateKey', KeyDataBin);
        Other -> Other
    end.

unauthorized(Req, MetaUrl) ->
    TargetUrl = MetaUrl ++ string:strip(dderl:get_url_suffix(), both, $/) ++ "/unauthorized.html",
    cowboy_req:reply(302, [
            {<<"Cache-Control">>, <<"no-cache">>},
            {<<"Pragma">>, <<"no-cache">>},
            {<<"Location">>, TargetUrl}
        ], <<"Redirecting...">>, Req).
