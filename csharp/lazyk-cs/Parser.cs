using System.Data;

namespace lazyk_cs
{
    public class Parser
    {
        private TextReader _r;
        private Stack<int> _unget;

        private Parser(TextReader r)
        {
            _r = r;
            _unget = new Stack<int>();
        }

        private void ungetChar(int c)
        {
            if (c >= 0) _unget.Push(c);
        }

        private int nextChar()
        {
            int c;
            if (_unget.Count > 0) return _unget.Pop();
            do
            {
                c = _r.Read();
                while (c == '#')
                {
                    _r.ReadLine();
                    c = _r.Read();
                }
            } while (c >= 0 && char.IsWhiteSpace((char)c));
            return c;
        }

        private int peekChar()
        {
            int c = nextChar();
            ungetChar(c);
            return c;
        }

        private Expr readCC(int closingChar)
        {
            int c = nextChar();
            if (c == closingChar)
            {
                return Expr.I;
            }
            ungetChar(c);
            Expr e = readOne(false);
            while (peekChar() != closingChar)
            {
                e = e.Apply(readOne(false));
            }
            if (closingChar >= 0) nextChar();
            return e;
        }

        private Expr readOne(bool i_is_iota)
        {
            int c = nextChar();
            switch (c)
            {
                case '`':
                case '*':
                    return readApp(c);
                case '(':
                    return readCC(')');
                case 'S':
                case 's':
                    return Expr.S;
                case 'K':
                case 'k':
                    return Expr.K;
                case 'I':
                    return Expr.I;
                case 'i':
                    return i_is_iota ? Expr.Iota : Expr.I;
                case '0':
                case '1':
                    return readJot(c);
                case -1:
                    throw new SyntaxErrorException("unexpected EOF");
                default:
                    throw new SyntaxErrorException(String.Format("unexpected char: {0}", c));
            }
        }

        private Expr readApp(int c0)
        {
            var stack = new Stack<(bool, List<Expr>)>();
            bool in_iota = c0 == '*';
            var buf = new List<Expr>();
            while (true)
            {
                int c = nextChar();
                if (c == '`' || c == '*')
                {
                    stack.Push((in_iota, buf));
                    buf = new List<Expr>();
                    in_iota = c == '*';
                }
                else
                {
                    ungetChar(c);
                    buf.Add(readOne(in_iota));
                }
                while (buf.Count == 2)
                {
                    var e = buf[0].Apply(buf[1]);
                    if (stack.Count == 0)
                    {
                        return e;
                    }
                    (in_iota, buf) = stack.Pop();
                    buf.Add(e);
                }
            }
        }

        private Expr readJot(int c0)
        {
            int c = c0;
            Expr e = Expr.I;
            while (c == '0' || c == '1')
            {
                if (c == '0')
                    e = e.Apply(Expr.S).Apply(Expr.K);
                else
                    e = Expr.S.Apply(Expr.K.Apply(e));
                c = nextChar();
            }
            ungetChar(c);
            return e;
        }

        public static Expr Parse(string src)
        {
            var sr = new StringReader(src);
            return new Parser(sr).readCC(-1);
        }
    }
}
