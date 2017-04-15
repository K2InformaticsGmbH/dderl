git clone https://github.com/K2InformaticsGmbH/rebar
git clone https://github.com/K2InformaticsGmbH/dderl

cd /home/vagrant/rebar/
./bootstrap
cp -p /home/vagrant/rebar/rebar /home/vagrant/dderl

# echoc -e "begin\nDeps = proplists:get_value(deps, CONFIG),\nNewDeps = lists:keydelete(erloci, 1, Deps),\nlists:keyreplace(deps, 1, CONFIG, {deps, NewDeps})\nend." > /home/vagrant/dderl/rebar.config.script

# removing Oracle dependency (erloci) by manipulating rebar.config.script
echo "begin" > /home/vagrant/dderl/rebar.config.script
echo "    Deps = proplists:get_value(deps, CONFIG)," >> /home/vagrant/dderl/rebar.config.script
echo "    NewDeps = lists:keydelete(erloci, 1, Deps)," >> /home/vagrant/dderl/rebar.config.script
echo "    {ok, [{application, dderl, Props}]}" >> /home/vagrant/dderl/rebar.config.script
echo "    = file:consult(\"src/dderl.app.src\")," >> /home/vagrant/dderl/rebar.config.script
echo "    Apps = proplists:get_value(applications, Props) -- [erloci]," >> /home/vagrant/dderl/rebar.config.script
echo "    NewAppSrc = {application, dderl, lists:keyreplace(applications, 1, Props, {applications, Apps})}," >> /home/vagrant/dderl/rebar.config.script
echo "    file:write_file(\"src/dderl.app.src\", list_to_binary(io_lib:format(\"~p.\",[NewAppSrc])))," >> /home/vagrant/dderl/rebar.config.script
echo "    lists:keyreplace(deps, 1, CONFIG, {deps, NewDeps})" >> /home/vagrant/dderl/rebar.config.script
echo "end." >> /home/vagrant/dderl/rebar.config.script

cd /home/vagrant/dderl/
./rebar get-deps
./rebar compile

cd /home/vagrant/dderl/priv
npm install
npm run build
