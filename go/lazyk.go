package lazyk

import (
	"bufio"
	"io"
	"os"
)

type LazyK struct {
	reader   *bufio.Reader
	writer   *bufio.Writer
	textmode bool
}

func (lk *LazyK) readc() int {
	lk.writer.Flush()
	b, err := lk.reader.ReadByte()
	if err != nil {
		if err == io.EOF {
			return 256
		}
		panic(err)
	}
	if b == 13 && lk.textmode {
		return lk.readc()
	}
	return int(b)
}

func (lk *LazyK) writec(c int) {
	lk.writer.WriteByte(byte(c))
	if c == 10 {
		lk.writer.Flush()
	}
}

func (lk *LazyK) Eval(e *Expr) *Expr {
	cur := e
	var stack []*Expr
	for {
		cur = cur.dropI1()
		for cur.etype == ET_App {
			stack = append(stack, cur)
			cur = cur.arg1.dropI1()
		}
		if len(stack) == 0 {
			return cur
		}
		a := cur
		cur, stack = stack[len(stack)-1], stack[:len(stack)-1]
		cur.arg1 = a
		lk.evalPrimitive(cur)
	}
}

func (lk *LazyK) evalPrimitive(e *Expr) {
	lhs := e.arg1
	rhs := e.arg2.dropI1()
	e.arg1 = nil
	e.arg2 = nil
	switch lhs.etype {
	case ET_False:
		e.etype = ET_I
		return
	case ET_I:
		e.etype = ET_I1
		e.arg1 = rhs
		return
	case ET_K:
		e.etype = ET_K1
		e.arg1 = rhs
		return
	case ET_K1:
		e.etype = ET_I1
		e.arg1 = lhs.arg1
		return
	case ET_S:
		e.etype = ET_S1
		e.arg1 = rhs
		return
	case ET_S1:
		e.etype = ET_S2
		e.arg1 = lhs.arg1
		e.arg2 = rhs
		return
	case ET_S2:
		e.arg1 = lhs.arg1.Apply(rhs)
		e.arg2 = lhs.arg2.Apply(rhs)
		return
	case ET_Iota:
		e.arg1 = rhs.Apply(E_S)
		e.arg2 = E_K
		return
	case ET_Read:
		cn := cnum(lk.readc())
		readnext := expr0(ET_Read)
		e.arg1 = rhs.Apply(cn)
		e.arg2 = readnext
		lhs.etype = ET_S2
		lhs.arg1 = expr2(ET_S2, E_I, expr1(ET_K1, cn))
		lhs.arg2 = expr1(ET_K1, readnext)
		return
	case ET_CNum:
		e.etype = ET_CNum1
		e.arg1 = rhs
		e.intval = lhs.intval
		return
	case ET_CNum1:
		if lhs.arg1 == E_Inc {
			if rhs.etype != ET_Num {
				rhs = lk.Eval(rhs)
			}
			if rhs.etype == ET_Num {
				e.etype = ET_Num
				e.intval = lhs.intval + rhs.intval
				return
			} else {
				panic("invalid output format")
			}
		} else {
			f := lhs.arg1
			x := rhs
			for i := 0; i < lhs.intval; i++ {
				x = f.Apply(x)
			}
			e.etype = x.etype
			e.arg1 = x.arg1
			e.arg2 = x.arg2
			e.intval = x.intval
			return
		}
	case ET_Inc:
		if rhs.etype != ET_Num {
			rhs = lk.Eval(rhs)
		}
		if rhs.etype != ET_Num {
			panic("invalid output format")
		}
		e.etype = ET_Num
		e.intval = rhs.intval + 1
		return
	case ET_Num:
		panic("invalid output format")
	default:
		panic("bug: unexpected state")
	}
}

func (lk *LazyK) cnum2int(cn *Expr) int {
	n := lk.Eval(cn.Apply(E_Inc).Apply(E_Num0))
	if n.etype != ET_Num {
		panic("invalid output format")
	}
	return n.intval
}

func (lk *LazyK) printList(lst *Expr) int {
	car := func(e *Expr) *Expr { return e.Apply(E_True) }
	cdr := func(e *Expr) *Expr { return e.Apply(E_False) }
	for {
		c := lk.cnum2int(car(lst))
		if c >= 256 {
			lk.writer.Flush()
			return c - 256
		}
		lk.writec(c)
		lst = cdr(lst)
	}
}

func (lk *LazyK) run(prog *Expr) int {
	return lk.printList(prog.Apply(expr0(ET_Read)))
}

func (lk *LazyK) RunFile(srcpath string) int {
	fi, err := os.Open(srcpath)
	if err != nil {
		panic(err)
	}
	defer func() {
		if err := fi.Close(); err != nil {
			panic(err)
		}
	}()
	result := lk.run(Parse(fi))
	return result
}

func (lk *LazyK) RunString(src string) int {
	prog := ParseString(src)
	return lk.run(prog)
}

func NewLazyK(rr io.Reader, wr io.Writer, textmode bool) *LazyK {
	brr := bufio.NewReader(rr)
	bwr := bufio.NewWriter(wr)
	return &LazyK{brr, bwr, textmode}
}
