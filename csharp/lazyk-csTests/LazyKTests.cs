using Microsoft.VisualStudio.TestTools.UnitTesting;
using lazyk_cs;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Data;
using System.Threading.Tasks;
using System.Reflection.PortableExecutable;

namespace lazyk_cs.Tests
{
    [TestClass()]
    public class LazyKTests
    {
        private LazyK _nlk = new LazyK(new MemoryStream(new byte[0], false), new MemoryStream(0), false);

        private int cn2i(Expr e) { return _nlk.ChurchNumToInt(e);  }

        [TestMethod()]
        public void ParseTest()
        {
            Assert.AreEqual("S", Parser.Parse("s").Dump(-1));
            Assert.AreEqual("K", Parser.Parse("k").Dump(-1));
            Assert.AreEqual("I", Parser.Parse("i").Dump(-1));
            Assert.AreEqual("((S K) K)", Parser.Parse("SKK").Dump(-1));
            Assert.AreEqual("(S (K K))", Parser.Parse("S(KK)").Dump(-1));
            Assert.AreEqual("((S K) K)", Parser.Parse("``skk").Dump(-1));
            Assert.AreEqual("(S (K K))", Parser.Parse("`s`kk").Dump(-1));
            Assert.AreEqual("(Iota ((Iota Iota) Iota))", Parser.Parse("*i**iii").Dump(-1));
            Assert.AreEqual("((I (Iota I)) I)", Parser.Parse("`i*iIi").Dump(-1));
            Assert.AreEqual("(S (K ((I S) K)))", Parser.Parse("01").Dump(-1));
            Assert.AreEqual("(S (((S K) ((S (K ((I S) K))) Iota)) I))", Parser.Parse("`s(SK*01ii)").Dump(-1));
            Assert.AreEqual("(S (K K))", Parser.Parse(" S # l1\n  (K # l2\n\n K # l4\n ) # l5").Dump(-1));
            Assert.AreEqual("I", Parser.Parse("").Dump(-1));
            Assert.AreEqual("((K I) I)", Parser.Parse("K()()").Dump(-1));

            Assert.ThrowsException<SyntaxErrorException>(() => Parser.Parse("SNK"));
            Assert.ThrowsException<SyntaxErrorException>(() => Parser.Parse("(SK"));
            Assert.ThrowsException<SyntaxErrorException>(() => Parser.Parse("``sk"));
            Assert.ThrowsException<SyntaxErrorException>(() => Parser.Parse("*i"));
        }

        [TestMethod()]
        public void CompbinatorTest()
        {
            var s = Expr.S;
            var k = Expr.K;
            var i = Expr.I;
            var f = Expr.False;
            var iota = Expr.Iota;
            var n1 = Expr.Num(1);
            var n2 = Expr.Num(2);
            var n3 = Expr.Num(3);
            Assert.AreEqual(n1, _nlk.Eval(i.Apply(n1)));
            Assert.AreEqual(n1, _nlk.Eval(k.Apply(n1).Apply(n2)));
            Assert.AreEqual(n2, _nlk.Eval(k.Apply(k).Apply(n1).Apply(n2).Apply(n3)));
            Assert.AreEqual(n2, _nlk.Eval(f.Apply(n1).Apply(n2)));
            Assert.AreEqual(n1, _nlk.Eval(s.Apply(k).Apply(k).Apply(n1)));
            Assert.AreEqual(s.Type, _nlk.Eval(iota.Apply(k)).Type);
            Assert.AreEqual(k.Type, _nlk.Eval(iota.Apply(f)).Type);
        }


        [TestMethod()]
        public void PrimitiveCNumTest()
        {
            for(int i = 0; i <= 256; i++)
            {
                Assert.AreEqual(i, cn2i(Expr.CNums[i]));
            }
            Expr cn2 = Expr.CNums[2];
            Expr cn3 = Expr.CNums[3];
            Assert.AreEqual(8, cn2i(cn3.Apply(cn2)));
            Assert.AreEqual(64, cn2i(cn2.Apply(cn3.Apply(cn2))));
            Assert.AreEqual(256, cn2i(cn3.Apply(cn2).Apply(cn2)));
        }

        [TestMethod()]
        public void ComposedCNumTest()
        {
            Assert.AreEqual(0, cn2i(Expr.K.Apply(Expr.I)));
            Assert.AreEqual(1, cn2i(Expr.I));
            Assert.AreEqual(1, cn2i(Expr.S.Apply(Expr.K).Apply(Expr.K)));

            Expr succ = Expr.S.Apply(Expr.S.Apply(Expr.K.Apply(Expr.S)).Apply(Expr.K));
            Expr cn2 = succ.Apply(Expr.I);
            Expr cn3 = succ.Apply(cn2);
            Assert.AreEqual(2, cn2i(cn2));
            Assert.AreEqual(3, cn2i(cn3));
            Assert.AreEqual(8, cn2i(cn3.Apply(cn2)));
            Assert.AreEqual(64, cn2i(cn2.Apply(cn3.Apply(cn2))));
            Assert.AreEqual(256, cn2i(cn3.Apply(cn2).Apply(cn2)));
        }


        [TestMethod()]
        public void InputTest()
        {
            var car = (Expr e) => e.Apply(Expr.K);
            var cdr = (Expr e) => e.Apply(Expr.False);
            var input = new MemoryStream(new byte[] {98, 97, 114, 13, 10});
            var output = new MemoryStream();
            var lk = new LazyK(input, output, true);
            var read = new Expr(Expr.EType.Read);
            Assert.AreEqual(98, lk.ChurchNumToInt(car(read)));
            Assert.AreEqual(97, lk.ChurchNumToInt(car(cdr(read))));
            Assert.AreEqual(114, lk.ChurchNumToInt(car(cdr(cdr(read)))));
            Assert.AreEqual(10, lk.ChurchNumToInt(car(cdr(cdr(cdr(read))))));
            Assert.AreEqual(256, lk.ChurchNumToInt(car(cdr(cdr(cdr(cdr(read)))))));

            input = new MemoryStream(new byte[] { 98, 13, 10 });
            lk = new LazyK(input, output, false);
            read = new Expr(Expr.EType.Read);
            Assert.AreEqual(13, lk.ChurchNumToInt(car(cdr(read))));
            Assert.AreEqual(10, lk.ChurchNumToInt(car(cdr(cdr(read)))));
        }

        [TestMethod()]
        public void OutputTest()
        {
            var input = new MemoryStream();
            var output = new MemoryStream();
            var lk = new LazyK(input, output, true);
            var cons = Parser.Parse("S(S(KS)(S(KK)(S(KS)(S(K(SI))K))))(KK)");
            var lst = new int[] { 256, 67, 66, 10, 65 }.Aggregate(Expr.Num0, (a, i) =>
            {
                return cons.Apply(Expr.CNums[i]).Apply(a);
            });
            Assert.AreEqual(0, lk.PrintList(lst));
            Assert.AreEqual("A\nBC", Encoding.UTF8.GetString(output.GetBuffer(), 0, (int)output.Position));
        }


        private (int, string) runLazyK(string src, string instr)
        {
            var input = new MemoryStream(Encoding.UTF8.GetBytes(instr));
            var output = new MemoryStream();
            var m = new LazyK(input, output, false);
            int result = m.Run(src);
            return (result, Encoding.UTF8.GetString(output.GetBuffer(), 0, (int)output.Position));
        }

        [TestMethod()]
        public void ProgramTest()
        {
            // prog = (lambda (input) input))
            Assert.AreEqual((0, "asdf\nqwer"), runLazyK("SKK", "asdf\nqwer"));

            // prog = (lambda (input) (cdr input))
            Assert.AreEqual((0, "jkl"), runLazyK("SI`k`ki", "hjkl"));

            // prog = (lambda (input) (cons (* 8 (car input)) k)); input = (33)
            Assert.AreEqual((8, ""), runLazyK("11111110001111111000111100111110001111111000111100111111000111111111000001111111000111100111001111111000111100111111000111100111111110001111111000111100111110001110011111110001111111000111100111110001110011111111100000111111100011111110001111001111100011100111111111000001111111000111111111000001111001110011110011110011100", "!"));
        }

    }
}
