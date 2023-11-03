# Lazy K Collection

Writing Lazy K interpreters in various languages


## tested compiler / interpreter versions

| language    | compiler / interpreter version             |
|-------------|--------------------------------------------|
| Common Lisp | sbcl (Steel Bank Common Lisp) 2.11.1       |
| C#          | Visual Studio 2022, .NET SDK 7.0 for linux |
| Go          | Go 1.21.3                                  |
| OCaml       | OCaml native-compiler (ocamlopt) 4.14.1    |
| Python      | Python 3.12.0, PyPy 7.3.13                 |
| Ruby        | Ruby 3.2.2                                 |
| Scheme      | Gauche 0.9.12                              |
| TypeScript  | deno 1.38.0                                |


## build binaries

Run `make` at the root directory of this repository, then following files will be generated.

- bin/cslazyk (C#)
- bin/golazyk (Go)
- bin/mllazyk (OCaml)
- commonlisp/lazyk.fasl


## run tests

### unit test

```
$ make test-unit-(language-name)
```

You need external libraries or support program to run unit tests in some languages.

- Common Lisp need [quicklisp](https://www.quicklisp.org/beta/) installed.
- OCaml need [Alcotest](https://github.com/mirage/alcotest) installed.


### common test cases

```
$ make test-(language-name)
```

runs tests using test data in `test` directory.


## usage of each implementations

### Common Lisp

```
$ sbcl --script commonlisp/run-lazyk.lisp
usage: sbcl --script run-lazyk.lisp <command> [arg]

commands:
  run FILENAME  : run Lazy K program file
  make          : make lazyk.fasl
  test          : run tests for lazyk.lisp (requires quicklisp)
```


### C#

#### compile with VisualStudio 2022

1. Open `csharp/lazyk-cs/lazyk-cs.sln`
2. Select "Publish" from the context menu of the "lazyk-cs" project.

#### compile with dotnet-sdk-7.0 on linux

Use Makefile in the root directory of this repository.

```
$ make csharp
```

makes `csharp/publish/cslazyk` and copies it to `bin/cslazyk`.


#### usage

```
$ bin/cslazyk -h
Usage: lazyk-cs.exe [options] [programfile]
Options:
  -t   text input mode (read CRLF as LF)
  -h   show this help
  If no program file is specified, the program is read from the standard input.
```


### Go

#### compile

```
$ make go
```

makes `go/golazyk` and copies it to `bin/golazyk`

#### usage

```
$ bin/golazyk
Usage: golazyk [-t] srcfile.lazy
Options
   -t  : read CRLF as LF
```


### OCaml

#### compile

```
$ make ocaml
```

makes `ocaml/mllazyk` and copies it ot `bin/mllazyk`

#### usage

```
$ bin/mllazyk
usage: mllazyk <source.lazy>
```


### Python

```
$ python3 python/lazyk.py
Usage: python lazyk.py [-t] srcfile
Option
   -t : text mode (read crlf as lf)
```

### Ruby

```
$ ruby ruby/lazyk.rb
Usage: ruby [-t] lazyk.rb prog.rb
Options
  -t : text mode (read crlf as lf)
```

### Scheme (Gauche)

```
$ gosh scheme/lazyk.scm
usage: gosh scheme/lazyk.scm src.lazyk
```

### TypeScript (Deno)

```
$ deno run --allow-read typescript/lazyk.ts
usage: deno run --allow-read lazyk.ts <source.lazy>
```

