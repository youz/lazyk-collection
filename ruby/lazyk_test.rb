require "minitest/autorun"
require "stringio"
require_relative "./lazyk.rb"

class TestLazyK < Minitest::Test

  def test_parse
    assert_equal "S", LazyK::Parser.parse("s").to_s
    assert_equal "K", LazyK::Parser.parse("k").to_s
    assert_equal "I", LazyK::Parser.parse("i").to_s
    assert_equal "((S K) K)", LazyK::Parser.parse("SKK").to_s
    assert_equal "(S (K K))", LazyK::Parser.parse("S(KK)").to_s
    assert_equal "((S K) K)", LazyK::Parser.parse("``skk").to_s
    assert_equal "(S (K K))", LazyK::Parser.parse("`s`kk").to_s
    assert_equal "(Iota ((Iota Iota) Iota))", LazyK::Parser.parse("*i**iii").to_s
    assert_equal "((I (Iota I)) I)", LazyK::Parser.parse("`i*iIi").to_s
    assert_equal "(S (K ((I S) K)))", LazyK::Parser.parse("01").to_s
    assert_equal "(S (((S K) ((S (K ((I S) K))) Iota)) I))", LazyK::Parser.parse("`s(SK*01ii)").to_s
    assert_equal "(S (K K))", LazyK::Parser.parse(" S # l1\n  (K # l2\n\n K # l4\n ) # l5").to_s
    assert_equal "I", LazyK::Parser.parse("").to_s
    assert_equal "((K I) I)", LazyK::Parser.parse("K()()").to_s

    assert_raises(SyntaxError) { LazyK::Parser.parse("SNK") }
    assert_raises(SyntaxError) { LazyK::Parser.parse("(SK") }
    assert_raises(SyntaxError) { LazyK::Parser.parse("``sk") }
    assert_raises(SyntaxError) { LazyK::Parser.parse("*i") }
  end

  def test_comb
    s = LazyK::Expr::S
    k = LazyK::Expr::K
    i = LazyK::Expr::I
    iota = LazyK::Expr::IOTA
    f = LazyK::Expr::FALSE
    n1 = LazyK::Expr.new(:num, 1)
    n2 = LazyK::Expr.new(:num, 2)
    assert_equal n1, i.apply(n1).eval
    assert_equal n1, k.apply(n1).apply(n2).eval
    assert_equal n1, s.apply(k).apply(k).apply(n1).eval
    assert_equal n2, f.apply(n1).apply(n2).eval
    assert_equal s, iota.apply(k).eval
    assert_equal k, iota.apply(f).eval
  end

  def test_primitive_cnum
    [0, 1, 2, 128, 256].each{|i|
      assert_equal i, LazyK::Expr::CNUMS[i].to_i
    }
    cn2 = LazyK::Expr::CNUMS[2]
    cn3 = LazyK::Expr::CNUMS[3]
    assert_equal 8, cn3.apply(cn2).to_i
    assert_equal 64, cn2.apply(cn3.apply(cn2)).to_i
    assert_equal 256, cn3.apply(cn2).apply(cn2).to_i
  end

  def test_composed_cnum
    assert_equal 0, LazyK::Expr::FALSE.to_i
    assert_equal 1, LazyK::Expr::I.to_i
    succ = LazyK::Parser.parse("S(S(KS)K)")
    cn2 = succ.apply(LazyK::Expr::I)
    cn3 = succ.apply(cn2)
    assert_equal 2, cn2.to_i
    assert_equal 3, cn3.to_i
    assert_equal 8, cn3.apply(cn2).to_i
    assert_equal 64, cn2.apply(cn3.apply(cn2)).to_i
    assert_equal 256, cn3.apply(cn2).apply(cn2).to_i
  end

  def test_input
    stdin = StringIO.new("bar\n")
    stdout = StringIO.new
    car = ->(e){ e.apply(LazyK::Expr::TRUE) }
    cdr = ->(e){ e.apply(LazyK::Expr::FALSE) }
    m = LazyK.new(stdin, stdout)
    input = LazyK::Expr.new(:read, m.method(:readc))
    assert_equal 98, car[input].to_i
    assert_equal 97, car[cdr[input]].to_i
    assert_equal 114, car[cdr[cdr[input]]].to_i
    assert_equal 10, car[cdr[cdr[cdr[input]]]].to_i
    assert_equal 256, car[cdr[cdr[cdr[cdr[input]]]]].to_i
  end

  def test_output
    cons = LazyK::Parser.parse("S(S(KS)(S(KK)(S(KS)(S(K(SI))K))))(KK)")
    expect = "A\nBC"
    l = (expect.bytes + [256, 0]).reverse.map{|i| LazyK::Expr::CNUMS[i] }.reduce{|a, c| cons.apply(c).apply(a) }
    stdin = StringIO.new("")
    stdout = StringIO.new
    LazyK.new(stdin, stdout).send(:print_list, l)
    assert_equal expect, stdout.string
  end

  def run_lazyk(src, instr = "")
    stdin = StringIO.new(instr)
    stdout = StringIO.new
    result = LazyK.new(stdin, stdout).run(src)
    [result, stdout.string]
  end
  
  def test_program
    # echo
    assert_equal [0, "asdf\nqwer"], run_lazyk("``skk", "asdf\nqwer")

    # cdr
    assert_equal [0, "jkl"], run_lazyk("SI`k`ki", "hjkl")

    # (lambda (input) (cons (* 8 (car input)) k))
    assert_equal [8, ""], run_lazyk("11111110001111111000111100111110001111111000111100111111000111111111000001111111000111100111001111111000111100111111000111100111111110001111111000111100111110001110011111110001111111000111100111110001110011111111100000111111100011111110001111001111100011100111111111000001111111000111111111000001111001110011110011110011100", "!")
  end
end
