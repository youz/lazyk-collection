
BIN = golazyk

$(BIN): $(wildcard *.go) $(wildcard cmd/*.go)
	go build -o $(BIN) ./cmd

.PHONY: test
test:
	go test -v

.PHONY: clean
clean:
	rm $(BIN)
