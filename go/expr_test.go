package lazyk

import (
	"testing"
)

func TestExprToString(t *testing.T) {
	var testcases = []struct {
		expr   *Expr
		expect string
	}{
		{E_S.Apply(E_K).Apply(E_I).Apply(E_Iota.Apply(E_False)), "(((S K) I) (Iota False))"},
		{expr1(ET_S1, expr1(ET_K1, expr1(ET_I1, expr0(ET_Read)))), "(S (K (I Read)))"},
		{E_Num0, "Num#0"},
		{cnum(256), "CNum#256"},
		{&Expr{ET_CNum1, E_K, nil, 1}, "(CNum#1 K)"},
	}
	for _, p := range testcases {
		s := p.expr.ToString()
		if s != p.expect {
			t.Errorf("expected: %v, got: %v", p.expect, s)
		}
	}
}

func TestDropI1(t *testing.T) {
	e := E_K
	for i := 0; i < 10; i++ {
		e = &Expr{ET_I1, e, nil, 0}
	}
	if e.dropI1() != E_K {
		t.Errorf("expected: K, got: %v", e.ToString())
	}
}
