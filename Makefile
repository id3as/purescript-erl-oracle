.PHONY: all test erl ci clean cleandist

PS_SRC = src
TEST_SRC = test
PROJECT_ROOT = $(shell pwd)

PS_SOURCEFILES = $(shell find ${PS_SRC} -type f -name \*.purs)
PS_ERL_FFI = $(shell find ${PS_SRC} -type f -name \*.erl)
PS_TEST_SOURCEFILES = $(shell find ${TEST_SRC} -type f -name \*.purs)
PS_TEST_ERL_FFI = $(shell find ${TEST_SRC} -type f -name \*.erl)

.DEFAULT_GOAL := erl

all: erl docs

ci: all test

output/.complete: .spago $(PS_SOURCEFILES) $(PS_ERL_FFI)
	spago build
	touch output/.complete

testoutput/.complete: .spago $(PS_SOURCEFILES) $(PS_ERL_FFI) $(PS_TEST_SOURCEFILES) $(PS_TEST_ERL_FFI)
	# Should be able to just use the below, but spago does not pass the testouput directory through to the purs backend
	# spago -x test.dhall build --purs-args "-o testoutput"
	# Start of workaround ------------------------------------
	spago sources | xargs purs compile -o testoutput --codegen corefn
	purerl -o testoutput
	# End  of workaround -------------------------------------
	touch testoutput/.complete

docs: output/.complete
	mkdir -p docs
	spago docs --format markdown
	cp generated-docs/md/Erl.Oci*.md docs

.spago: spago.yaml
	spago install
	# spago -x test.yaml install
	touch .spago

erl: output/.complete
	rebar3 as dist compile

test: testoutput/.complete
	# rebar3 as test compile
	# erl -pa ebin -pa _build/test/lib/*/ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'
	ERL_FLAGS="-config $(PROJECT_ROOT)/test/config/sys.config" rebar3 as test eunit -m "test_main@ps" -v

shell: output/.complete
	rebar3 as dist shell

clean:
	rebar3 as dist clean
	rebar3 as test clean
	rm -rf output testoutput

distclean: clean
	rm -rf .spago _build
