BIN = publish/lazyk-cs
SRC = $(wildcard lazyk-cs/*.cs*)
TESTSRC = $(SRC) $(wildcar lazyk-csTests/*.cs*)

$(BIN): $(SRC)
	cd lazyk-cs && \
	dotnet publish lazyk-cs.csproj --no-self-contained -c Release --os linux -o ../publish/

test: $(TESTSRC)
	cd lazyk-cs && \
	dotnet test lazyk-cs.sln

all: $(BIN)

clean:
	rm publish/*

