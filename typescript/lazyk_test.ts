import { assertEquals, assertThrows } from "https://deno.land/std@0.203.0/assert/mod.ts";
import { EType, Expr, LazyK, Parser } from "./lazyk.ts";

const devnul = {
  readsync: (_: Uint8Array) => 0,
  writesync: (_: Uint8Array) => {
    return;
  },
};
const lkn = new LazyK(devnul);
const cn2i = (e: Expr) => lkn.churchNumToInt(e);

const runLazyK = (src: string, instr: string): [number, string] => {
  let outbuf = "";
  const td = new TextDecoder();
  const io = {
    readsync: strreader(instr),
    writesync: (b: Uint8Array) => {
      outbuf += td.decode(b);
    },
  };
  const result = new LazyK(io).run(src);
  return [result, outbuf];
};

Deno.test(function parseTest() {
  assertEquals(Parser.parse("s").toString(), "S");
  assertEquals(Parser.parse("k").toString(), "K");
  assertEquals(Parser.parse("i").toString(), "I");
  assertEquals(Parser.parse("SKK").toString(), "((S K) K)");
  assertEquals(Parser.parse("S(KK)").toString(), "(S (K K))");
  assertEquals(Parser.parse("``skk").toString(), "((S K) K)");
  assertEquals(Parser.parse("`s`kk").toString(), "(S (K K))");
  assertEquals(Parser.parse("*i**iii").toString(), "(Iota ((Iota Iota) Iota))");
  assertEquals(Parser.parse("`i*iIi").toString(), "((I (Iota I)) I)");
  assertEquals(Parser.parse("01").toString(), "(S (K ((I S) K)))");
  assertEquals(
    Parser.parse("`s(SK*01ii)").toString(),
    "(S (((S K) ((S (K ((I S) K))) Iota)) I))",
  );
  assertEquals(
    Parser.parse(" S # l1\n  (K # l2\n\n K # l4\n ) # l5").toString(),
    "(S (K K))",
  );
  assertEquals(Parser.parse("").toString(), "I");
  assertEquals(Parser.parse("k()()").toString(), "((K I) I)");

  assertThrows(() => Parser.parse("SNK"), Error, "unexpected char: N");
  assertThrows(() => Parser.parse("(SK"), Error, "unexpected EOF");
  assertThrows(() => Parser.parse("``sk"), Error, "unexpected EOF");
  assertThrows(() => Parser.parse("*i"), Error, "unexpected EOF");
});

Deno.test(function combinatorTest() {
  const s = Expr.S;
  const k = Expr.K;
  const i = Expr.I;
  const f = Expr.False;
  const iota = Expr.Iota;
  const n1 = Expr.Num(1);
  const n2 = Expr.Num(2);
  const n3 = Expr.Num(3);
  assertEquals(lkn.eval(i.apply(n1)), n1);
  assertEquals(lkn.eval(k.apply(n1).apply(n2)), n1);
  assertEquals(lkn.eval(k.apply(k).apply(n1).apply(n2).apply(n3)), n2);
  assertEquals(lkn.eval(s.apply(k).apply(k).apply(n1)), n1);
  assertEquals(lkn.eval(f.apply(n1).apply(n2)), n2);
  assertEquals(lkn.eval(iota.apply(k)), s);
  assertEquals(lkn.eval(iota.apply(f)), k);
});

Deno.test(function primitiveCNumTest() {
  for (let i = 0; i <= 256; i++) {
    assertEquals(cn2i(Expr.CNum(i)), i);
  }
  const cn2 = Expr.CNum(2);
  const cn3 = Expr.CNum(3);
  assertEquals(cn2i(cn3.apply(cn2)), 8);
  assertEquals(cn2i(cn2.apply(cn3.apply(cn2))), 64);
  assertEquals(cn2i(cn3.apply(cn2).apply(cn2)), 256);
});

Deno.test(function composedCNumTest() {
  assertEquals(cn2i(Expr.K.apply(Expr.I)), 0);
  assertEquals(cn2i(Expr.I), 1);

  const succ = Parser.parse("S(S(KS)K)");
  const cn2 = succ.apply(Expr.I);
  const cn3 = succ.apply(cn2);
  assertEquals(cn2i(cn2), 2);
  assertEquals(cn2i(cn3), 3);
  assertEquals(cn2i(cn3.apply(cn2)), 8);
  assertEquals(cn2i(cn2.apply(cn3.apply(cn2))), 64);
  assertEquals(cn2i(cn3.apply(cn2).apply(cn2)), 256);
});

const strreader = (s: string) => {
  const inbuf = Array.from(new TextEncoder().encode(s));
  return (b: Uint8Array) => {
    let i = 0;
    while (inbuf.length > 0 && i < b.length) {
      b[i++] = inbuf.shift() || 0;
    }
    return i === 0 ? null : i;
  };
};

Deno.test(function inputTest() {
  const car = (e: Expr) => e.apply(Expr.K);
  const cdr = (e: Expr) => e.apply(Expr.False);
  const m = new LazyK({
    readsync: strreader("bar\n"),
    writesync: devnul.writesync,
  });
  const read = new Expr(EType.Read);
  assertEquals(m.churchNumToInt(car(read)), 98);
  assertEquals(m.churchNumToInt(car(cdr(read))), 97);
  assertEquals(m.churchNumToInt(car(cdr(cdr(read)))), 114);
  assertEquals(m.churchNumToInt(car(cdr(cdr(cdr(read))))), 10);
  assertEquals(m.churchNumToInt(car(cdr(cdr(cdr(cdr(read)))))), 256);
});

Deno.test(function outputTest() {
  let outbuf = "";
  const td = new TextDecoder();
  const io = {
    readsync: devnul.readsync,
    writesync: (b: Uint8Array) => {
      outbuf += td.decode(b);
    },
  };
  const cons = Parser.parse("S(S(KS)(S(KK)(S(KS)(S(K(SI))K))))(KK)");
  const lst = [65, 10, 66, 67, 256].reduceRight(
    (l, n) => cons.apply(Expr.CNum(n)).apply(l),
    Expr.CNum(256),
  );
  new LazyK(io).printList(lst);
  assertEquals(outbuf, "A\nBC");
});

Deno.test(function runProgramTest() {
  assertEquals(runLazyK("SKK", "asdf\nqwer"), [0, "asdf\nqwer"]);
  assertEquals(runLazyK("SI`k`ki", "hjkl"), [0, "jkl"]);
  assertEquals(
    runLazyK(
      "11111110001111111000111100111110001111111000111100111111000111111111000001111111000111100111001111111000111100111111000111100111111110001111111000111100111110001110011111110001111111000111100111110001110011111111100000111111100011111110001111001111100011100111111111000001111111000111111111000001111001110011110011110011100",
      "!",
    ),
    [8, ""],
  );
});
