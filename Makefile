all:
	rebar compile
	cp priv/cnsl_cpu_util.escript ebin/
