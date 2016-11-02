@ECHO OFF

REM Parameters:  NodeID ClusterID IMEMPort DDERLPort

SET nid=%1
SET cid=%2
SET port=%3
SET dderlport=%4

IF "%4" == "" (
   SET nid=1
   SET cid=2
   SET port=1236
   SET dderlport=443
)

IF NOT "%5" == "" (
   SET nid=1
   SET cid=2
   SET port=1236
   SET dderlport=443
)

SET unamestr=%USERNAME%
SET host=Hostname
REM SET hostArrayIn=(${host//./ })
REM SET host=${hostArrayIn[0]}
SET host=127.0.0.1

SET name=dderl%nid%@%host%
SET cmname=dderl%cid%@%host%
SET imemtyp=disc
SET ck=dderl

REM Node name
SET node_name=-name %name%

REM Cookie
SET cookie=-setcookie %ck%

REM PATHS
SET paths=-pa
SET paths=%paths% %cd%\ebin
SET paths=%paths% %cd%\deps\cowboy\ebin
SET paths=%paths% %cd%\deps\cowlib\ebin
SET paths=%paths% %cd%\deps\dderloci\ebin
SET paths=%paths% %cd%\deps\edown\ebin
SET paths=%paths% %cd%\deps\erlimem\ebin
SET paths=%paths% %cd%\deps\erloci\ebin
SET paths=%paths% %cd%\deps\erlpkg\ebin
SET paths=%paths% %cd%\deps\erlscrypt\ebin
SET paths=%paths% %cd%\deps\goldrush\ebin
SET paths=%paths% %cd%\deps\imem\ebin
SET paths=%paths% %cd%\deps\jpparse\ebin
SET paths=%paths% %cd%\deps\jsx\ebin
SET paths=%paths% %cd%\deps\lager\ebin
SET paths=%paths% %cd%\deps\mimetypes\ebin
SET paths=%paths% %cd%\deps\ranch\ebin
SET paths=%paths% %cd%\deps\sext\ebin
SET paths=%paths% %cd%\deps\sqlparse\ebin

REM Proto dist module
SET dist_opts=-proto_dist
SET dist_opts=%dist_opts% imem_inet_tcp

REM Kernel Opts
SET kernel_opts=-kernel
SET kernel_opts=%kernel_opts% inet_dist_listen_min 7000
SET kernel_opts=%kernel_opts% inet_dist_listen_max 7020

SET Imem Opts
SET imem_opts=-imem
SET imem_opts=%imem_opts% mnesia_node_type %imemtyp%
SET imem_opts=%imem_opts% erl_cluster_mgrs ['%cmname%']
SET imem_opts=%imem_opts% mnesia_schema_name dderl
SET imem_opts=%imem_opts% tcp_port %port%

REM dderl opts
SET dderl_opts=-dderl
SET dderl_opts=%dderl_opts% port %dderlport%

SET start_opts=%paths% %cookie% %node_name% %dist_opts% %kernel_opts% %imem_opts% %dderl_opts%

REM DDERL start options
ECHO ------------------------------------------
ECHO Starting DDERL (Opts)
ECHO ------------------------------------------
ECHO Node Name : %node_name%
ECHO Cookie    : %cookie%
ECHO EBIN Path : %paths%
ECHO Dist      : %dist_opts%
ECHO Kernel    : %kernel_opts%
ECHO IMEM      : %imem_opts%
ECHO DDERL     : %dderl_opts%
ECHO ------------------------------------------

REM Starting dderl
START /MAX werl %start_opts% -s dderl
