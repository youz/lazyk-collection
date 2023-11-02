#!/usr/bin/env python3

import sys
import io
from enum import Enum
from typing import BinaryIO, TextIO

EType = Enum(
    "EType", [
        "App", "S", "K", "I", "Iota", "CFalse",
        "S1", "S2", "K1", "I1",
        "CNum", "CNum1", "Inc", "Num", "Read",
    ],
)

class Expr:
    def __init__(
        self,
        type: EType,
        arg1: type["Expr"] | int | None = None,
        arg2: type["Expr"] | None = None,
    ) -> None:
        self.type = type
        self.arg1 = arg1
        self.arg2 = arg2

    def __str__(self):
        match (self.type):
            case EType.App:
                return "(%s %s)" % (self.arg1, self.arg2)
            case EType.S1 | EType.K1 | EType.I1:
                return "(%s %s)" % (self.type.name, self.arg1)
            case EType.S2:
                return "(%s %s %s)" % (self.type.name, self.arg1, self.arg2)
            case EType.CNum | EType.Num:
                return "%s#%d" % (self.type.name, self.arg1)
            case EType.CNum1:
                return "(%s#%d %s)" % (self.type.name, self.arg1, self.arg2)
            case _:
                return self.type.name

    def apply(self, arg: "Expr") -> "Expr":
        return Expr(EType.App, self, arg)

    def drop_i1(self) -> "Expr":
        cur = self
        while cur.type == EType.I1:
            cur = cur.arg1
        return cur


class Parser:
    def __init__(self, src: str) -> None:
        self.__input = io.StringIO(src)
        self.__unread = []

    def nextc(self) -> str:
        if len(self.__unread) > 0:
            return self.__unread.pop()
        while True:
            c = self.__input.read(1)
            while c == "#":
                self.__input.readline()
                c = self.__input.read(1)
            if c == "" or not c.isspace():
                return c

    def unreadc(self, c):
        if c != "":
            self.__unread.append(c)

    def read_cc(self, closingchar: str) -> Expr:
        c = self.nextc()
        if c == closingchar:
            return LazyK.I
        self.unreadc(c)
        e = self.read_one(False)
        while True:
            c = self.nextc()
            if c == closingchar:
                return e
            else:
                self.unreadc(c)
                e = e.apply(self.read_one(False))

    def read_one(self, i_is_iota: bool) -> Expr:
        c = self.nextc()
        match c:
            case "`" | "*":
                return self.read_app(c)
            case "(":
                return self.read_cc(")")
            case "S" | "s":
                return LazyK.S
            case "K" | "k":
                return LazyK.K
            case "I":
                return LazyK.I
            case "i":
                return LazyK.IOTA if i_is_iota else LazyK.I
            case "0" | "1":
                return self.read_jot(c)
            case "":
                raise SyntaxError("unexpected EOF")
            case _:
                raise SyntaxError(f'unexpected char "{c}"')

    def read_app(self, c0: str) -> Expr:
        stack = []
        buf = []
        is_iota = c0 == "*"
        while True:
            c = self.nextc()
            match c:
                case "`" | "*":
                    stack.append((is_iota, buf))
                    buf = []
                    is_iota = c == "*"
                case _:
                    self.unreadc(c)
                    buf.append(self.read_one(is_iota))
            while len(buf) == 2:
                e = buf[0].apply(buf[1])
                if len(stack) == 0:
                    return e
                else:
                    (is_iota, buf) = stack.pop()
                    buf.append(e)

    def read_jot(self, c0: str) -> Expr:
        c = c0
        e = LazyK.I
        while c == "0" or c == "1":
            if c == "0":
                e = e.apply(LazyK.S).apply(LazyK.K)
            else:
                e = LazyK.S.apply(LazyK.K.apply(e))
            c = self.nextc()
        self.unreadc(c)
        return e

    @staticmethod
    def parse(src: str) -> Expr:
        return Parser(src).read_cc("")


class LazyK:
    S = Expr(EType.S)
    K = Expr(EType.K)
    I = Expr(EType.I)
    CTRUE = K
    CFALSE = Expr(EType.CFalse)
    IOTA = Expr(EType.Iota)
    INC = Expr(EType.Inc)
    NUM0 = Expr(EType.Num, 0)
    CNUMS = [Expr(EType.CNum, i) for i in range(257)]

    def cn_to_int(self, e: Expr) -> int:
        result = self.eval_expr(e.apply(LazyK.INC).apply(LazyK.NUM0))
        if result.type != EType.Num:
            raise ValueError(f"invalid output format: {result}")
        else:
            return result.arg1

    def eval_expr(self, e: Expr) -> Expr:
        stack = []
        cur = e
        while True:
            cur = cur.drop_i1()
            while cur.type == EType.App:
                stack.append(cur)
                cur = cur.arg1.drop_i1()
            if len(stack) == 0:
                return cur
            a = cur
            cur = stack.pop()
            cur.arg1 = a
            self.eval_primitive(cur)

    def eval_primitive(self, e: Expr):
        lhs: Expr = e.arg1
        rhs: Expr = e.arg2.drop_i1()
        e.arg1 = e.arg2 = None
        match lhs.type:
            case EType.I:
                e.type = EType.I1
                e.arg1 = rhs
            case EType.CFalse:
                e.type = EType.I
            case EType.K:
                e.type = EType.K1
                e.arg1 = rhs
            case EType.K1:
                e.type = EType.I1
                e.arg1 = lhs.arg1
            case EType.S:
                e.type = EType.S1
                e.arg1 = rhs
            case EType.S1:
                e.type = EType.S2
                e.arg1 = lhs.arg1
                e.arg2 = rhs
            case EType.S2:
                e.arg1 = lhs.arg1.apply(rhs)
                e.arg2 = lhs.arg2.apply(rhs)
            case EType.Iota:
                e.arg1 = rhs.apply(LazyK.S)
                e.arg2 = LazyK.K
            case EType.Read:
                ch = LazyK.CNUMS[self.readc()]
                readnext = Expr(EType.Read)
                e.arg1 = rhs.apply(ch)
                e.arg2 = readnext
                lhs.type = EType.S2
                lhs.arg1 = Expr(EType.S2, LazyK.I, Expr(EType.K1, ch))
                lhs.arg2 = Expr(EType.K1, readnext)
            case EType.CNum:
                e.type = EType.CNum1
                e.arg1 = lhs.arg1
                e.arg2 = rhs
            case EType.CNum1:
                if lhs.arg2 == LazyK.INC:
                    if rhs.type != EType.Num:
                        rhs = self.eval_expr(rhs)
                    if rhs.type == EType.Num:
                        e.type = EType.Num
                        e.arg1 = lhs.arg1 + rhs.arg1
                    else:
                        raise ValueError(f"invalid output format: {rhs}")
                else:
                    f = lhs.arg2
                    x = rhs
                    for i in range(lhs.arg1):
                        x = f.apply(x)
                    e.type = x.type
                    e.arg1 = x.arg1
                    e.arg2 = x.arg2
            case EType.Inc:
                if rhs.type != EType.Num:
                    rhs = self.eval_expr(rhs)
                if rhs.type == EType.Num:
                    e.type = EType.Num
                    e.arg1 = rhs.arg1 + 1
                else:
                    raise ValueError(
                        f"invalid output format (attempted to apply inc to a non-number: {rhs})"
                    )
            case EType.Num:
                raise ValueError(f"invalid output format ({lhs} is not applicable)")
            case _:
                raise RuntimeError(f"unexpected state: lhs={lhs} rhs={rhs}")

    def readc(self) -> int:
        self.output.flush()
        b = self.input.read(1)
        if len(b) == 0:
            return 256
        c = b[0]
        if c == 13 and self.textmode:
            return self.readc()
        else:
            return c

    def writec(self, c: int):
        self.output.write(bytes([c]))
        if c == 10:
            self.output.flush()

    def print_list(self, lst: Expr) -> int:
        while True:
            c = self.cn_to_int(lst.apply(LazyK.CTRUE))
            if c >= 256:
                return (c - 256)
            self.writec(c)
            lst = lst.apply(LazyK.CFALSE)

    def __init__(
        self, input: BinaryIO, output: BinaryIO, textmode: bool = False
    ) -> None:
        self.input = input
        self.output = output
        self.textmode = textmode

    def run(self, src: str) -> int:
        prog = Parser.parse(src)
        return self.print_list(prog.apply(Expr(EType.Read)))

    def run_file(self, srcfile: str) -> int:
        with open(srcfile) as fp:
            return self.run(fp.read())


def __showhelp():
    print("Usage: python lazyk.py [-t] srcfile")
    print("Option")
    print("   -t : text mode (read crlf as lf)")
    sys.exit(1)


def __main(args):
    textmode = False
    srcfile = ""
    for a in args:
        if a == "-t":
            textmode = True
        else:
            srcfile = a
            break
    if srcfile == "":
        __showhelp()
    m = LazyK(sys.stdin.buffer, sys.stdout.buffer, textmode)
    try:
        if srcfile == "-":
            result = m.run(sys.stdin.read(-1))
        else:
            result = m.run_file(srcfile)
        return result
    except Exception as e:
        print(f"ERROR: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(__main(sys.argv[1:]))
