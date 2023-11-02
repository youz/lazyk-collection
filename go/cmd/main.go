package main

import (
	"os"

	lazyk "github.com/youz/lazyk-collection/go"
)

func usage() {
	println("Usage: golazyk [-t] srcfile.lazy")
	println("Options")
	println("   -t  : read CRLF as LF")
	os.Exit(1)
}

func main() {
	textmode := false
	args := os.Args[1:]
	for len(args) > 0 && args[0][0] == '-' {
		switch args[0] {
		case "-t":
			textmode = true
		default:
			println("unknown option: " + args[0])
			os.Exit(1)
		}
		args = args[1:]
	}
	if len(args) == 0 {
		usage()
	}
	lk := lazyk.NewLazyK(os.Stdin, os.Stdout, textmode)
	exitCode := lk.RunFile(args[0])
	os.Exit(exitCode)
}
