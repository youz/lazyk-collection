package lazyk

import (
	"bufio"
	"io"
	"strings"
	"unicode"
)

type scanner struct {
	reader  *bufio.Reader
	_unread int
}

func (sc *scanner) unreadRune(c rune) {
	if sc._unread >= 0 {
		panic("bug: don't unread twice")
	}
	sc._unread = int(c)
}

func (sc *scanner) nextRune() (rune, error) {
	if sc._unread >= 0 {
		c := rune(sc._unread)
		sc._unread = -1
		return c, nil
	}
	for {
		c, _, err := sc.reader.ReadRune()
		for c == '#' {
			sc.reader.ReadLine()
			c, _, err = sc.reader.ReadRune()
		}
		if err != nil {
			if err == io.EOF {
				return c, err
			}
			panic(err)
		} else if !unicode.IsSpace(c) {
			return c, nil
		}
	}
}

func readCC(sc *scanner, needcloseparen bool) *Expr {
	c, err := sc.nextRune()
	if (c == ')' && needcloseparen) || (err != nil && !needcloseparen) {
		return E_I
	}
	sc.unreadRune(c)
	e := readOne(sc, false)
	for {
		c, err = sc.nextRune()
		if err != nil {
			if !needcloseparen {
				return e
			}
			panic("unexpected EOF")
		}
		if c == ')' && needcloseparen {
			return e
		}
		sc.unreadRune(c)
		e = e.Apply(readOne(sc, false))
	}
}

func readOne(sc *scanner, i_is_iota bool) *Expr {
	c, err := sc.nextRune()
	if err != nil {
		panic("unexpected EOF")
	}
	switch c {
	case '`', '*':
		return readApp(sc, c)
	case '(':
		return readCC(sc, true)
	case 'S', 's':
		return E_S
	case 'K', 'k':
		return E_K
	case 'I':
		return E_I
	case 'i':
		if i_is_iota {
			return E_Iota
		} else {
			return E_I
		}
	case '0', '1':
		sc.unreadRune(c)
		return readJot(sc)
	default:
		panic("unexpected char: " + string(c))
	}
}

type appframe = struct {
	is_iota bool
	buf     []*Expr
}

func readApp(sc *scanner, c0 rune) *Expr {
	var stack []*appframe
	is_iota := c0 == '*'
	buf := make([]*Expr, 0, 2)
	for {
		c, err := sc.nextRune()
		if err != nil {
			panic("unexpected EOF")
		}
		switch c {
		case '`', '*':
			stack = append(stack, &appframe{is_iota, buf})
			is_iota = c == '*'
			buf = make([]*Expr, 0, 2)
		default:
			sc.unreadRune(c)
			buf = append(buf, readOne(sc, is_iota))
		}
		for len(buf) == 2 {
			e := buf[0].Apply(buf[1])
			if len(stack) == 0 {
				return e
			}
			f := stack[len(stack)-1]
			is_iota, buf = f.is_iota, f.buf
			stack = stack[:len(stack)-1]
			buf = append(buf, e)
		}
	}
}

func readJot(sc *scanner) *Expr {
	e := E_I
	for {
		c, err := sc.nextRune()
		if err != nil {
			return e
		}
		if c == '0' {
			e = e.Apply(E_S).Apply(E_K)
		} else if c == '1' {
			e = E_S.Apply(E_K.Apply(e))
		} else {
			sc.unreadRune(c)
			return e
		}
	}
}

func Parse(r io.Reader) *Expr {
	sc := &scanner{bufio.NewReader(r), -1}
	return readCC(sc, false)
}

func ParseString(src string) *Expr {
	return Parse(strings.NewReader(src))
}
