package lazyk

import (
	"testing"
)

func TestParse(t *testing.T) {
	var testcases = []struct {
		src    string
		expect string
	}{
		{"s", "S"},
		{"k", "K"},
		{"i", "I"},
		{"SKK", "((S K) K)"},
		{"S(KK)", "(S (K K))"},
		{"``skk", "((S K) K)"},
		{"`s`kk", "(S (K K))"},
		{"*i**iii", "(Iota ((Iota Iota) Iota))"},
		{"`i*iIi", "((I (Iota I)) I)"},
		{"01", "(S (K ((I S) K)))"},
		{"`s(SK*01ii)", "(S (((S K) ((S (K ((I S) K))) Iota)) I))"},
		{" S # l1\n  (K # l2\n\n K # l4\n ) # l5", "(S (K K))"},
		{"", "I"},
		{"K()()", "((K I) I)"},
	}
	for _, p := range testcases {
		a := ParseString(p.src).ToString()
		if a != p.expect {
			t.Errorf("expected: %v, got: %v", p.expect, a)
		}
	}
}

func TestParseError(t *testing.T) {
	var testcases = []struct {
		src       string
		expectmsg string
	}{
		{"SNK", "unexpected char: N"},
		{"(SK", "unexpected EOF"},
		{"``sk", "unexpected EOF"},
		{"*i", "unexpected EOF"},
	}
	for _, p := range testcases {
		func(src string, msg string) {
			defer func() {
				r := recover()
				if r == nil {
					t.Errorf("expected: panic(%#v), got: nil", msg)
				} else if r != msg {
					t.Errorf("expected: panic(%#v), got: panic(%#v)", msg, r)
				}
			}()
			ParseString(src)
		}(p.src, p.expectmsg)
	}
}
