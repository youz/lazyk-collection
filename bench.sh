#!/bin/sh

llisp=lambdalisp.lazy
llisp_url=https://raw.githubusercontent.com/woodrush/lambdalisp/main/bin/lambdalisp.lazy

if ! multitime -h > /dev/null 2>&1; then
    echo "Can't find multitime on this machine."
    exit 1
fi


if [ ! -e "$llisp" ]; then
    echo "Get lambdalisp.lazy from ${llisp_url}"
    echo "ok? (y/N)"
    read ans < /dev/tty
    case "$ans" in
	[yY]*) ;;
	*) echo aborted
	   exit 1;;
    esac
    if ! curl --version > /dev/null 2>&1; then
	echo "Can't find curl on this machine."
	exit 1
    fi
    if ! curl -o "${llisp}" "${llisp_url}"; then
	exit 1
    fi
fi

if [ "$1" = "-n" ]; then
    numruns="$1 $2"
    shift 2
fi

multitime $numruns -v -i 'cat fib10.lisp' -q $@ lambdalisp.lazy
