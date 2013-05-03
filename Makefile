REPO 		?= dderl

all: deps compile

compile: 
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

generate: compile
	(cd rel && rebar generate target_dir=dev overlay_vars=vars/dev_vars.config)

rel: deps compile generate

rel_agustin: deps compile
	(cd rel && rebar generate target_dir=agustin overlay_vars=vars/agustin_vars.config)
rel_bikram: deps compile
	(cd rel && rebar generate target_dir=bikram overlay_vars=vars/bikram_vars.config)
rel_stefan: deps compile
	(cd rel && rebar generate target_dir=stefan overlay_vars=vars/stefan_vars.config)
rel_olt: deps compile
	(cd rel && rebar generate target_dir=olt overlay_vars=vars/olt_vars.config)
rel_zhh: deps compile
	(cd rel && rebar generate target_dir=zhh overlay_vars=vars/zhh_vars.config)

rel_all: rel rel_agustin rel_bikram rel_stafen rel_olt rel_zhh

relclean:
	rm -rf rel/agustin rel/bikram rel/stefan rel/olt rel/zhh

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	   xmerl webtool snmp public_key mnesia eunit syntax_tools compiler

COMBO_PLT = $(HOME)/.$(REPO)_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin apps/*/ebin
