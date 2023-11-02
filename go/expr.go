package lazyk

import (
	"fmt"
)

type ExprType int

const (
	ET_App = iota
	ET_S
	ET_S1
	ET_S2
	ET_K
	ET_K1
	ET_I
	ET_I1
	ET_Iota
	ET_False
	ET_CNum
	ET_CNum1
	ET_Inc
	ET_Num
	ET_Read
)

var ExprTypeName = []string{"App", "S", "S1", "S2", "K", "K1", "I", "I1", "Iota", "False", "CNum", "CNum1", "Inc", "Num", "Read"}

type Expr struct {
	etype  ExprType
	arg1   *Expr
	arg2   *Expr
	intval int
}

func expr0(etype ExprType) *Expr {
	return &Expr{etype, nil, nil, 0}
}
func expr1(etype ExprType, arg1 *Expr) *Expr {
	return &Expr{etype, arg1, nil, 0}
}

func expr2(etype ExprType, arg1 *Expr, arg2 *Expr) *Expr {
	return &Expr{etype, arg1, arg2, 0}
}

func numexpr(etype ExprType, intval int) *Expr {
	return &Expr{etype, nil, nil, intval}
}

var (
	E_S     = expr0(ET_S)
	E_K     = expr0(ET_K)
	E_I     = expr0(ET_I)
	E_Iota  = expr0(ET_Iota)
	E_True  = E_K
	E_False = expr0(ET_False)
	E_Inc   = expr0(ET_Inc)
	E_Num0  = numexpr(ET_Num, 0)
	E_CNums = make([]*Expr, 257)
)

func (fun *Expr) Apply(arg *Expr) *Expr {
	return &Expr{ET_App, fun, arg, 0}
}

func cnum(i int) *Expr {
	if i > 256 {
		panic("bug: primitive cnum must <= 256")
	}
	if E_CNums[i] == nil {
		E_CNums[i] = numexpr(ET_CNum, i)
	}
	return E_CNums[i]
}

func (e *Expr) dropI1() *Expr {
	for e.etype == ET_I1 {
		e = e.arg1
	}
	return e
}

func (e *Expr) ToString() string {
	tname := ExprTypeName[e.etype]
	switch e.etype {
	case ET_App:
		if e.arg1 == nil || e.arg2 == nil {
			panic("invalid expr value")
		}
		return fmt.Sprintf("(%s %s)", e.arg1.ToString(), e.arg2.ToString())
	case ET_S2:
		if e.arg1 == nil || e.arg2 == nil {
			panic("invalid expr value")
		}
		return fmt.Sprintf("(S %s %s)", e.arg1.ToString(), e.arg2.ToString())
	case ET_S1, ET_K1, ET_I1:
		if e.arg1 == nil {
			panic("invalid expr value")
		}
		return fmt.Sprintf("(%s %s)", tname[0:1], e.arg1.ToString())
	case ET_CNum, ET_Num:
		return fmt.Sprintf("%s#%d", tname, e.intval)
	case ET_CNum1:
		return fmt.Sprintf("(CNum#%d %s)", e.intval, e.arg1.ToString())
	default:
		return tname
	}
}
