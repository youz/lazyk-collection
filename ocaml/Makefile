BIN = mllazyk
TESTBIN = lazyk_test

OO = ocamlopt
OF = ocamlfind
OOFLAGS = -O3

BINSRC = lazyk.ml main.ml
TESTSRC = lazyk.ml lazyk_test.ml
TESTLIB = alcotest

$(BIN) : $(BINSRC)
	$(OO) $(OOFLAGS) -o $@ $^

$(TESTBIN) : $(TESTSRC)
	$(OF) $(OO) -package $(TESTLIB) -linkpkg $(OOFLAGS) -o $@ $^

test : $(TESTBIN)
	./$(TESTBIN)

all: $(BIN)

clean:
	rm -f $(BIN) $(TESTBIN) *.cm* *.o


