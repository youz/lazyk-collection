BINDIR = bin

all: allbin

$(BINDIR):
	mkdir $@


CL_FILES = $(wildcard commonlisp/*.lisp)
CL_FASL = commonlisp/lazyk.fasl
CL_LAZYK = sbcl --script commonlisp/run-lazyk.lisp
$(CL_FASL): $(CL_FILES)
	$(CL_LAZYK) make
.PHONY: commonlisp test-unit-commonlisp test-commonlisp
commonlisp: $(CL_FASL)
test-unit-commonlisp: $(CL_FASL)
	$(CL_LAZYK) test
test-commonlisp: $(CL_FASL)
	./run_tests $(CL_LAZYK) run


CS_FILES = $(wildcard csharp/lazyk-cs/*.cs*)
CS_BIN = $(BINDIR)/cslazyk
$(CS_BIN): $(BINDIR) $(CS_FILES)
	cd csharp && make && cp publish/lazyk-cs ../$(CS_BIN)
.PHONY: csharp test-unit-csharp test-csharp
csharp: $(CS_BIN)
test-unit-csharp:
	cd csharp && make test
test-csharp: $(CS_BIN)
	./run_tests $(CS_BIN)


GO_FILES = $(wildcard go/*.go) $(wildcard go/cmd/*.go)
GO_BIN = $(BINDIR)/golazyk
$(GO_BIN): $(GO_FILES)
	cd go && make && cp golazyk ../$(BINDIR)/
.PHONY: go test-unit-go test-go
go: $(GO_BIN)
test-unit-go:
	cd go && go test -v
test-go: $(GO_BIN)
	./run_tests $(GO_BIN)


OCAML_FILES = $(wildcard ocaml/*.ml) ocaml/Makefile
OCAML_BIN = $(BINDIR)/mllazyk
$(OCAML_BIN): $(BINDIR) $(OCAML_FILES)
	cd ocaml && make && cp mllazyk ../$(BINDIR)/
.PHONY: ocaml test-unit-ocaml test-ocaml
ocaml: $(OCAML_BIN)
test-unit-ocaml: $(OCAML_FILES)
	cd ocaml && make test
test-ocaml: $(OCAML_BIN)
	./run_tests $(OCAML_BIN)


PY_FILES = $(wildcard python/*.py)
.PHONY: test-unit-python test-python
test-unit-python: $(PY_FILES)
	cd python && python3 -m lazyk_test
test-python: $(PY_FILES)
	./run_tests python3 python/lazyk.py


RUBY_FILES = $(wildcard ruby/*.rb)
.PHONY: test-unit-ruby test-ruby
test-unit-ruby: $(RUBY_FILES)
	ruby ruby/lazyk_test.rb
test-ruby:
	./run_tests ruby ruby/lazyk.rb


SCHEME_FILES = $(wildcard scheme/*.scm)
.PHONY: test-unit-scheme test-scheme
test-unit-scheme:
	gosh scheme/lazyk-test.scm
test-scheme:
	./run_tests gosh scheme/lazyk.scm


TS_FILES = $(wildcard typescript/*.ts)
.PHONY: test-unit-typescript test-unit
test-unit-typescript: $(TS_FILES)
	cd typescript && deno test
test-typescript: $(TS_FILES)
	./run_tests deno run --allow-read typescript/lazyk.ts


.PHONY: test-all
test-all: test-commonlisp test-csharp test-go test-ocaml test-python test-ruby test-scheme test-typescript

ALLBIN = $(CL_FASL) $(CS_BIN) $(GO_BIN) $(OCAML_BIN)

.PHONY: clean
clean:
	rm $(ALLBIN)

.PHONY: allbin
allbin: $(ALLBIN)

.PHONY: bench
bench:
	sh ./bench_all.sh -n 5 >bench.log 2>&1
