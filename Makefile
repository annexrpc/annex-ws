PROJECT = annex_ws

# dependencies

DEPS = websocket_client fast_key

SHELL_DEPS = rl annex annex_marshal_msgpack

dep_websocket_client = git https://github.com/jeremyong/websocket_client master
dep_fast_key = git https://github.com/camshaft/fast_key master
dep_rl = git https://github.com/camshaft/rl master
dep_annex = git https://github.com/annexrpc/annex master
dep_annex_marshal_msgpack = git https://github.com/annexrpc/annex-marshal-msgpack master

include erlang.mk

repl: all bin/start
	@bin/start annex_ws -s rl make

bin/start:
	@mkdir -p bin
	@curl https://gist.githubusercontent.com/camshaft/372cc332241ac95ae335/raw/start -o $@
	@chmod a+x $@

.PHONY: repl
