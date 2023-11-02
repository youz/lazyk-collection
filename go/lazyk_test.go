package lazyk

import (
	"bufio"
	"bytes"
	"io"
	"strings"
	"testing"
)

var nlk = NewLazyK(bufio.NewReader(strings.NewReader("")), bufio.NewWriter(io.Discard), true)

func TestCombinator(t *testing.T) {
	n1 := numexpr(ET_Num, 1)
	n2 := numexpr(ET_Num, 2)
	var cases = []struct {
		expr   *Expr
		expect *Expr
	}{
		{E_I.Apply(n1), n1},
		{E_K.Apply(n1).Apply(n2), n1},
		{E_S.Apply(E_K).Apply(E_K).Apply(n1), n1},
		{E_False.Apply(n1).Apply(n2), n2},
		{E_Iota.Apply(E_True), E_S},
		{E_Iota.Apply(E_False), E_K},
	}
	for _, p := range cases {
		a := nlk.Eval(p.expr)
		if a != p.expect {
			t.Errorf("expected: %s, got: %s", p.expect.ToString(), a.ToString())
		}
	}
}

func TestPrimitiveCNum(t *testing.T) {
	cn2 := cnum(2)
	cn3 := cnum(3)
	var cases = []struct {
		expr   *Expr
		expect int
	}{
		{cnum(0), 0},
		{cnum(1), 1},
		{cnum(256), 256},
		{cn3.Apply(cn2), 8},
		{cn2.Apply(cn3.Apply(cn2)), 64},
		{cn3.Apply(cn2).Apply(cn2), 256},
	}
	for _, p := range cases {
		a := nlk.cnum2int(p.expr)
		if p.expect != a {
			t.Errorf("expected: %d, got: %d", p.expect, a)
		}
	}
}

func TestComposedCNum(t *testing.T) {
	succ := E_S.Apply(E_S.Apply(E_K.Apply(E_S)).Apply(E_K)) // S(S(KS)K)
	cn2 := succ.Apply(E_I)
	cn3 := succ.Apply(cn2)
	var cases = []struct {
		expr   *Expr
		expect int
	}{
		{E_False, 0},
		{E_I, 1},
		{cn2, 2},
		{cn3, 3},
		{cn3.Apply(cn2), 8},
		{cn2.Apply(cn3.Apply(cn2)), 64},
		{cn3.Apply(cn2).Apply(cn2), 256},
	}
	for _, p := range cases {
		a := nlk.cnum2int(p.expr)
		if p.expect != a {
			t.Errorf("expected: %d, got: %d", p.expect, a)
		}
	}
}

func TestInptut(t *testing.T) {
	input := strings.NewReader("bar\n")
	lk := NewLazyK(input, io.Discard, false)
	car := func(e *Expr) *Expr { return e.Apply(E_True) }
	cdr := func(e *Expr) *Expr { return e.Apply(E_False) }
	r := expr0(ET_Read)
	expect := []int{98, 97, 114, 10, 256}
	for _, e := range expect {
		a := lk.cnum2int(car(r))
		if a != e {
			t.Errorf("expected: %d, got: %d", e, a)
		}
		r = cdr(r)
	}
}

func TestOutput(t *testing.T) {
	b := bytes.NewBuffer(nil)
	lk := NewLazyK(strings.NewReader(""), bufio.NewWriter(b), false)
	cons := ParseString("S(S(KS)(S(KK)(S(KS)(S(K(SI))K))))(KK)")
	lst := cons.Apply(cnum(256)).Apply(E_K)
	for _, i := range []int{67, 66, 10, 65} {
		lst = cons.Apply(cnum(i)).Apply(lst)
	}
	lk.printList(lst)
	expect := "A\nBC"
	output := b.String()
	if output != expect {
		t.Errorf("expected: %#v, got: %#v", expect, output)
	}
}

func runLazyK(src string, input string) (int, string) {
	b := bytes.NewBuffer(nil)
	lk := NewLazyK(strings.NewReader(input), bufio.NewWriter(b), false)
	result := lk.RunString(src)
	return result, b.String()
}

func TestProgram(t *testing.T) {
	cases := []struct {
		src             string
		input           string
		expect_exitcode int
		expect_output   string
	}{
		{"SKK", "asdf\nqwer", 0, "asdf\nqwer"},
		{"SI`k`ki", "hjkl", 0, "jkl"},
		{"11111110001111111000111100111110001111111000111100111111000111111111000001111111000111100111001111111000111100111111000111100111111110001111111000111100111110001110011111110001111111000111100111110001110011111111100000111111100011111110001111001111100011100111111111000001111111000111111111000001111001110011110011110011100",
			"!", 8, ""},
	}
	for _, c := range cases {
		ec, o := runLazyK(c.src, c.input)
		if ec != c.expect_exitcode || o != c.expect_output {
			t.Errorf("expected: (%d, %#v), got: (%d, %#v)", c.expect_exitcode, c.expect_output, ec, o)
		}
	}
}
