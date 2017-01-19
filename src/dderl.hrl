-ifndef(DDERL_HRL).
-define(DDERL_HRL, true).

-include_lib("imem/include/imem_meta.hrl").
-include_lib("imem/include/imem_exports.hrl").

-define(DEFAULT_ROW_SIZE, 100).
-record(viewstate, { table_layout = []
                   , column_layout = []
       }).

-record(ddAdapter,                          %% DDerl adapter (connect to databases)              
                  { id                      :: atom()             %% oci | imem | ets | os_text | dfs_text | hdfs_text
                  , fullName                :: binary()           %% displayed in drop down box
                  }
       ). 
-define(ddAdapter, [atom, binstr]).

-record(ddInterface,                        %% DDerl client interface (connect to ui / applications)               
                  { id                      :: atom()             %% ddjson
                  , fullName                :: binary()           %% displayed in drop down box
                  }
       ).
-define(ddInterface, [atom, binstr]).

-record(ddConn,                             %% DB connection    
                  { id                      ::ddEntityId()       
                  , name                    ::binary()          %% connection name (mutable)
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , adapter                 ::atom()            %% oci | imem | ets | os_text | dfs_text | hdfs_text
                  , access                  ::any()             %% erlang term depending on adapter (e.g. ip+service or tns)
                  , schm                    ::any()             %% erlang term depending on adapter (e.g. name or uri or data root path)
                  }
       ).
-define(ddConn, [integer, binstr, userid, atom, term, term]).
      
-record(ddCmd,                              %% DB command     
                  { id                      ::ddEntityId()       
                  , name                    ::binary()          %% command template name (mutable)
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , adapters                ::[atom()]          %% can be used for this list of ddAdap
                  , conns = local           ::list() | local    %% can be local or a list with remote connections, empty list for all remote connections.
                  , command                 ::binary()          %% erlang term depending on adapter (e.g. SQL text)
                  , opts                    ::any()             %% command options ()
                  }
       ).
-define(ddCmd, [integer, binstr, userid, list, list, binstr, term]).

-record(ddView,                             %% user representation of a db command including rendering parameters
                  { id                      ::ddEntityId()
                  , interface               ::atom()            %% interface plugin (ddjson for now)  
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , name                    ::binary()          %% should default to command name
                  , cmd                     ::ddEntityId()      %% db command id
                  , state                   ::#viewstate{}      %% transparent viewstate (managed by client application)
                  }
       ).
-define(ddView, [integer, atom, userid, binstr, integer, term]).

-record(ddDash,                             %% user representation of a dashboard (collection of views)
                  { id                      ::ddEntityId()
                  , interface               ::atom()            %% interface plugin (ddjson for now)  
                  , owner                   ::ddEntityId()      %% account.id of creator / owner
                  , name                    ::binary()          %% should default to command name
                  , views                   ::list()            %% array of proplists with view layout
                  }
       ).

-define(ddDash, [integer, atom, userid, binstr, list]).


%% Definition taken from imem_seco.hrl
-type ddIdentity()   :: binary().           %% Account name
-type ddCredential() :: {pwdmd5, binary()}. %% {pwdmd5, md5(password)} for now
-type ddPermission() :: atom() | tuple().   %% e.g. manage_accounts, {table,ddDash,select}
-type ddQuota()      :: {atom(),any()}.     %% e.g. {max_memory, 1000000000}

-record(ddAccount,                          %% imem cluster account (shared by application)
                  { id                      ::ddEntityId()
                  , name                    ::ddIdentity()        %% unique login id (mutable)
                  , type='user'             ::atom()              %% user | driver | deamon | application
                  , credentials             ::[ddCredential()]
                  , fullName                ::binary()
                  , lastLoginTime           ::ddDatetime()        %% erlang time of last login success
                  , lastFailureTime         ::ddDatetime()        %% erlang time of last login failure (for existing account name)
                  , lastPasswordChangeTime  ::ddDatetime()        %% change time (undefined or too old  => must change it now and reconnect)
                  , locked='false'          ::'true' | 'false'
                  }
       ).

-define(ddAccount, [userid,binstr,atom,list,binstr,datetime,datetime,datetime,boolean]).

-record(ddRole,                             %% hierarchy of roles with permissions and access privileges to connections and commands  
                  { id                      ::ddEntityId()            %% lookup starts with ddAccount.id, other roles are atoms
                  , roles=[]                ::[atom()]                %% granted roles
                  , permissions=[]          ::[ddPermission()]        %% granted permissions
                  , quotas=[]               ::[ddQuota()]             %% granted quotas
                  }
       ).

-define(ddRole, [userid,list,list,list]).

-ifndef(LOG_TAG).
-define(LOG_TAG, "_DDRL_").
-endif.

-define(Access(__Access), lager:debug([{type,dderl_access}], __Access)).

-define(NoDbLog(__L,__M,__F,__A),
        lager:__L(__M, "["++?LOG_TAG++"] ~p "++__F, [{?MODULE,?LINE}|__A])).
-define(Log(__L,__M,__F,__A,__S),
(fun(_S) ->
         __Ln = lager_util:level_to_num(__L),
         __LM = __Ln band element(1,lager_config:get(loglevel)),
         _LEn = lager_util:level_to_num(error),
         __ST = if __LM /= 0 andalso length(_S) == 0 andalso __Ln =< _LEn ->
                       erlang:get_stacktrace();
                   true ->
                       _S
                end,
         lager:__L([{stacktrace,__ST}|__M], "["++?LOG_TAG++"] ~p "++__F,
                   [{?MODULE,?LINE}|__A])
 end)(__S)).

-define(Debug(__M,__F,__A),        ?Log(debug,__M,__F,__A,[])).
-define(Info(__M,__F,__A),         ?Log(info,__M,__F,__A,[])).
-define(Note(__M,__F,__A),         ?Log(notice,__M,__F,__A,[])).
-define(Warn(__M,__F,__A),         ?Log(warning,__M,__F,__A,[])).
-define(Error(__M,__F,__A,__S),    ?Log(error,__M,__F,__A,__S)).
-define(Crit(__M,__F,__A,__S),     ?Log(critical,__M,__F,__A,__S)).
-define(Alert(__M,__F,__A,__S),    ?Log(alert,__M,__F,__A,__S)).
-define(Emergency(__M,__F,__A,__S),?Log(emergency,__M,__F,__A,__S)).


% helper macro extension
-define(Debug(__F,__A),         ?Debug([],__F,__A)).
-define(Debug(__F),             ?Debug(__F,[])).
-define(Info(__F,__A),          ?Info([],__F,__A)).
-define(Info(__F),              ?Info(__F,[])).
-define(Note(__F, __A),         ?Note([], __F, __A)).
-define(Note(__F),              ?Note(__F, [])).
-define(Warn(__F,__A),          ?Warn([],__F,__A)).
-define(Warn(__F),              ?Warn(__F,[])).
-define(Error(__F,__A,__S),     ?Error([],__F,__A,__S)).
-define(Error(__F,__A),         ?Error([],__F,__A,[])).
-define(Error(__F),             ?Error(__F,[])).
-define(Crit(__F,__A, __S),     ?Crit([],__F,__A,__S)).
-define(Crit(__F,__A),          ?Crit([],__F,__A,[])).
-define(Crit(__F),              ?Crit(__F,[])).
-define(Alert(__F,__A, __S),    ?Alert([],__F,__A,__S)).
-define(Alert(__F,__A),         ?Alert([],__F,__A,[])).
-define(Alert(__F),             ?Alert(__F,[])).
-define(Emergency(__F,__A, __S),?Emergency([],__F,__A,__S)).
-define(Emergency(__F,__A),     ?Emergency([],__F,__A,[])).
-define(Emergency(__F),         ?Emergency(__F,[])).

% Function shortcuts
-define(Encrypt(__T), dderl:encrypt(__T)).
-define(Decrypt(__S), dderl:decrypt(__S)).
-define(Hash(__T), erlang:phash2(dderl:encrypt(__T))).

% CSV Export
-define(CSV_FIELD_SEP, ";").

-define(URLSUFFIX, ?GET_CONFIG(urlsuffix,[],"/dderl","Suffix for access URL")).
-define(MAXACCEPTORS, ?GET_CONFIG(maxNumberOfAcceptors, [], 100, "Maximum number of TCP acceptors")).
-define(MAXCONNS, ?GET_CONFIG(maxNumberOfSockets, [], 5000, "Maximum number of simulteneous connections")).
-define(LOGTABLE, ?GET_CONFIG(dderlLogTable,[],'dderlLog_86400@',"Rolling log table name")).
-define(SSLOPTS, ?GET_CONFIG(dderlSslOpts,[],'$no_ssl_conf',"SSL listen socket options")).
-define(RESTARTAPPS(__CurrApp), ?GET_CONFIG(restartApplications, [], [__CurrApp], "Erlang applicationns to restart inside VM")).

-define(CONNECT_TIMEOUT, ?GET_CONFIG(connectTimeout,[],100000,"Connect timeout for data sender")).
%% TODO: Change this maybe to a receiver parameter ?.
-define(BLOCK_SIZE, ?GET_CONFIG(commitBlockSize,[],10,"Commit block size of data sender")).
%% TODO: Timeout should be defined by options
-define(RESPONSE_TIMEOUT, ?GET_CONFIG(responseTimeout,[],100000,"Response timeout of data receiver")).

%% OCI Adapter configs
-define(NLSLANG, ?GET_CONFIG(nls_lang, [], #{languange   => <<"GERMAN">>,
                                            territory   => <<"SWITZERLAND">>,
                                            charset     => <<"AL32UTF8">>}, "OCI NSL Language connect option")).
%% CSV Configs
-define(COL_SEP_CHAR(__UserId, __Adapter),
        ?GET_CONFIG(csvExportDelimiter, [__UserId, __Adapter], ";",
                    "Character to seperate each column of a CSV export")).
-define(ROW_SEP_CHAR(__UserId, __Adapter),
        ?GET_CONFIG(csvExportDelimiterNewLine, [__UserId, __Adapter], "\r\n",
                    "Character to seperate each rows of a CSV export")).
-define(CSV_ENC(__UserId, __Adapter),
        ?GET_CONFIG(csvExportEncoding, [__UserId, __Adapter], utf8,
                    "Character encoding for the content see unicode::encoding() for supported values")).

-define(CSV_BOM(__UserId, __Adapter),
        ?GET_CONFIG(csvExportBomPrefix, [__UserId, __Adapter], <<239, 187, 191>>,
                    "Byte Order Mark (BOM) matching the encoding")).

%% DDErl Activity Logging
-define(ACTLOGLEVEL,  ?GET_CONFIG(activityLogLevel, [], 0, "Loglevel parameter, all activity log with loglevel >= must be logged")).
-define(PROXY,        ?GET_CONFIG(proxyAddress, [], {0,0,0,0}, "Proxy Address")).

%% SAML configs
-define(IDPLOGINURL,        ?GET_CONFIG(samlIdpLoginUrl, [], "https://missing.saml_idp.host/", "IDP Login URL")).
-define(SPURLPREFIX,        ?GET_CONFIG(samlSpBaseUrl, [], "/saml", "URL Prefix for all SP links")).
-define(SAMLCERTKEY,        ?GET_CONFIG(samlCertKey,[],'$no_cert_key',"SAML cert and key options for encryption and signature")).
-define(SAMLFINGERPRINT,    ?GET_CONFIG(samlSignThumbPrint,[], '$nofp',"SAML - Fingerprint of the certificate used by idp to sign the response")).
-define(SAMLSIGNREQUEST,    ?GET_CONFIG(samlSignRequest,[], true,"SAML - flag to sign the requests or not")).
-define(ISENCRYPTMANDATORY, ?GET_CONFIG(samlEncryptIsMandatory,[], true,"SAML - Expect encrypted data")).
-define(VERIFYRESPONSESIGN, ?GET_CONFIG(samlVerifyResponseSignature,[], true,"SAML - flag to verify response signature")).
-define(UNAUTHORIZEDPAGE,   ?GET_CONFIG(samlUnauthorizedPage,[], <<"<!DOCTYPE html><html lang=\"en\"> <head> <meta charset=\"utf-8\"> <title>Unauthorized!</title> <style>::-moz-selection{background: #b3d4fc; text-shadow: none;}::selection{background: #b3d4fc; text-shadow: none;}html{padding: 30px 10px; font-size: 20px; line-height: 1.4; color: #737373; background: #f0f0f0; -webkit-text-size-adjust: 100%; -ms-text-size-adjust: 100%;}html, input{font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif;}body{max-width: 500px; _width: 500px; padding: 30px 20px 50px; border: 1px solid #b3b3b3; border-radius: 4px; margin: 0 auto; box-shadow: 0 1px 10px #a7a7a7, inset 0 1px 0 #fff; background: #fcfcfc;}h1{margin: 0 10px; font-size: 50px; text-align: center;}h1 span{color: #bbb;}.container{max-width: 380px; _width: 380px; margin: 0 auto;}</style> </head> <body> <div class=\"container\"> <h1>You are unauthorized to access the page</h1> </div></body></html>">>, "SAML - Unauthorized Page")).

%% Screensaver config
-define(SCREEN_SAVER_TIMEOUT, ?GET_CONFIG(screenSaverTimeout, [], 0, "Screen saver activation timeout in minutes, 0 = infinity")).

%% Access Log levels
-define(LOGIN_CONNECT,  1).
-define(CMD_NOARGS,     2).
-define(CMD_WITHARGS,   3).
-define(CUST_SQL,       4).

%% Cookie info
-define(SESSION_COOKIE, <<"DDERL-SESSION">>).
-define(XSRF_COOKIE, <<"DDERL-XSRF-TOKEN">>).
-define(XSRF_HEADER, <<"x-xsrf-token">>).
-define(COOKIE_OPTS(__Domain, __Path), [{path, __Path}, {secure, true}, {domain, __Domain}]).
-define(HTTP_ONLY_COOKIE_OPTS(__Domain, __Path), [{http_only, true}|?COOKIE_OPTS(__Domain, __Path)]).

% IMEM REST interface
-define(IMEMREST,               ?GET_CONFIG(active,         [], true,                   "Enable disable IMEM REST service")).
-define(IMEMREST_IPS,           ?GET_CONFIG(listenIntfs,    [], [{{127,0,0,1}, element(2, application:get_env(dderl, port)) + 1000}],
                                            "Listen IPs and TCP ports")).
-define(IMEMREST_SSLOPTS,       ?GET_CONFIG(ssl,            [], '$no_ssl_conf',         "SSL listen socket options")).
-define(IMEMREST_IPWHITELIST,   ?GET_CONFIG(ipWhiteLists,   [], [{127,0,0,1}],          "White listed IP address")).

-endif.
