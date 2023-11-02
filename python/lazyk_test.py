import unittest
from io import BytesIO
from functools import reduce
from lazyk import EType, Expr, Parser, LazyK


class Util:
    NK = LazyK(None, None)

    @staticmethod
    def cn2i(e: Expr) -> int:
        return Util.NK.cn_to_int(e)


class ParserTestCase(unittest.TestCase):
    def test_single_token(self):
        self.assertEqual(Parser.parse("s"), LazyK.S)
        self.assertEqual(Parser.parse("k"), LazyK.K)
        self.assertEqual(Parser.parse("i"), LazyK.I)

    def test_applications(self):
        self.assertEqual(str(Parser.parse("SKK")), "((S K) K)")
        self.assertEqual(str(Parser.parse("S(KK)")), "(S (K K))")
        self.assertEqual(str(Parser.parse("``skk")), "((S K) K)")
        self.assertEqual(str(Parser.parse("`s`kk")), "(S (K K))")
        self.assertEqual(str(Parser.parse("*i**iii")), "(Iota ((Iota Iota) Iota))")
        self.assertEqual(str(Parser.parse("`i*iIi")), "((I (Iota I)) I)")
        self.assertEqual(str(Parser.parse("01")), "(S (K ((I S) K)))")
        self.assertEqual(
            str(Parser.parse("`s(SK*01ii)")), "(S (((S K) ((S (K ((I S) K))) Iota)) I))"
        )

    def test_spaces_and_comments(self):
        self.assertEqual(
            str(Parser.parse(" S # l1\n  (K # l2\n\n K # l4\n ) # l5")), "(S (K K))"
        )

    def test_blank(self):
        self.assertEqual(Parser.parse(""), LazyK.I)
        self.assertEqual(str(Parser.parse("K()()")), "((K I) I)")

    def test_syntax_error(self):
        self.assertRaises(Exception, lambda: Parser.parse("SNK"))
        self.assertRaises(Exception, lambda: Parser.parse("(SK"))
        self.assertRaises(Exception, lambda: Parser.parse("``sk"))
        self.assertRaises(Exception, lambda: Parser.parse("*i"))


class CombinatorTestCase(unittest.TestCase):
    def combinator_test(self):
        s = LazyK.S
        k = LazyK.K
        i = LazyK.I
        iota = LazyK.IOTA
        f = LazyK.CFALSE
        t = LazyK.CTRUE
        n1 = LazyK.CNUMS[1]
        n2 = LazyK.CNUMS[2]
        self.assertEqual(LazyK.eval_expr(i.apply(n1)), n1)
        self.assertEqual(LazyK.eval_expr(k.apply(n1).apply(n2)), n1)
        self.assertEqual(LazyK.eval_expr(s.apply(n1).apply(k).apply(k).apply(n1)), n1)
        self.assertEqual(LazyK.eval_expr(f.apply(n1).apply(n2)), n2)
        self.assertEqual(LazyK.eval_expr(iota.apply(t)), s)
        self.assertEqual(LazyK.eval_expr(iota.apply(f)), k)


class ChurchNumTestCase(unittest.TestCase):
    def test_primitive_churchnum(self):
        for i in range(256):
            self.assertEqual(Util.cn2i(LazyK.CNUMS[i]), i)
        cn2 = LazyK.CNUMS[2]
        cn3 = LazyK.CNUMS[3]
        self.assertEqual(Util.cn2i(cn3.apply(cn2)), 8)
        self.assertEqual(Util.cn2i(cn2.apply(cn3.apply(cn2))), 64)
        self.assertEqual(Util.cn2i(cn3.apply(cn2).apply(cn2)), 256)

    def test_composed_churchnum(self):
        s = LazyK.S
        k = LazyK.K
        i = LazyK.I
        f = LazyK.CFALSE
        self.assertEqual(Util.cn2i(f), 0)
        self.assertEqual(Util.cn2i(i), 1)

        succ = s.apply(s.apply(k.apply(s)).apply(k))
        cn2 = succ.apply(i)
        cn3 = succ.apply(cn2)
        self.assertEqual(Util.cn2i(cn2), 2)
        self.assertEqual(Util.cn2i(cn3), 3)
        self.assertEqual(Util.cn2i(cn3.apply(cn2)), 8)
        self.assertEqual(Util.cn2i(cn2.apply(cn3.apply(cn2))), 64)
        self.assertEqual(Util.cn2i(cn3.apply(cn2).apply(cn2)), 256)


class IOTestCase(unittest.TestCase):
    def test_input(self):
        input = BytesIO(b"bar\n")
        output = BytesIO()
        m = LazyK(input, output)
        car = lambda e: e.apply(LazyK.CTRUE)
        cdr = lambda e: e.apply(LazyK.CFALSE)
        cn2i = lambda e: m.cn_to_int(e)
        r = Expr(EType.Read)
        self.assertEqual(cn2i(car(r)), 98)
        self.assertEqual(cn2i(car(cdr(r))), 97)
        self.assertEqual(cn2i(car(cdr(cdr(r)))), 114)
        self.assertEqual(cn2i(car(cdr(cdr(cdr(r))))), 10)
        self.assertEqual(cn2i(car(cdr(cdr(cdr(cdr(r)))))), 256)

    def test_output(self):
        cons = Parser.parse("S(S(KS)(S(KK)(S(KS)(S(K(SI))K))))(KK)")
        o = [97, 10, 98, 99, 256]
        list.reverse(o)
        l = reduce(lambda a, i: cons.apply(LazyK.CNUMS[i]).apply(a), o, LazyK.NUM0)
        input = BytesIO()
        output = BytesIO()
        m = LazyK(input, output)
        m.print_list(l)
        self.assertEqual(output.getvalue(), b"a\nbc")


class ProgramTestCases(unittest.TestCase):
    def run_lazyk(self, src, input):
        output = BytesIO()
        m = LazyK(BytesIO(input), output)
        result = m.run(src)
        return (result, output.getvalue())

    def test_program(self):
        self.assertEqual(self.run_lazyk("SKK", b"asdf\nqwer"), (0, b"asdf\nqwer"))
        self.assertEqual(self.run_lazyk("SI`k`ki", b"hjkl"), (0, b"jkl"))
        self.assertEqual(
            self.run_lazyk(
                "11111110001111111000111100111110001111111000111100111111000111111111000001111111000111100111001111111000111100111111000111100111111110001111111000111100111110001110011111110001111111000111100111110001110011111111100000111111100011111110001111001111100011100111111111000001111111000111111111000001111001110011110011110011100",
                b"!",
            ),
            (8, b""),
        )


if __name__ == "__main__":
    unittest.main(verbosity=2)
