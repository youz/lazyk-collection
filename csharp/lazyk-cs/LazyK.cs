using System;
using System.Buffers;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static lazyk_cs.Expr;

namespace lazyk_cs
{
    public class LazyK
    {
        private Stream input;
        private byte[] input_buf;
        private int input_len;
        private int input_pos;
        private bool input_isEOF;
        private bool textmode;
        private Stream output;
        private byte[] output_buf;
        private int output_pos;

        public LazyK(Stream input, Stream output, bool textmode)
        {
            if (!input.CanRead)
            {
                throw new Exception(String.Format("{0} is not readable", input.ToString()));
            }
            if (!output.CanWrite)
            {
                throw new Exception(String.Format("{0} is not readable", output.ToString()));
            }
            this.input = input;
            this.input_isEOF = false;
            this.input_buf = new byte[1024];
            this.input_len = 0;
            this.input_pos = 0;
            this.output = output;
            this.output_buf = new byte[1024];
            this.output_pos = 0;
            this.textmode = textmode;
        }

        #region IO
        private int readc()
        {
            if (input_pos == input_len && !input_isEOF)
            {
                flush();
                input_pos = 0;
                input_len = input.Read(input_buf, 0, input_buf.Length);
                if (input_len == 0)
                {
                    input_isEOF = true;
                    return 256;
                }
            }
            if (input_pos < input_len)
            {
                int c = input_buf[input_pos++];
                if (c == 13 && textmode)
                {
                    return readc();
                }
                else
                {
                    return c;
                }
            }
            else
            {
                return 256;
            }
        }

        private void flush()
        {
            output.Write(output_buf, 0, output_pos);
            output_pos = 0;
            output.Flush();
        }

        private void writec(byte b)
        {
            output_buf[output_pos++] = b;
            if (b == 10 || output_pos == output_buf.Length)
            {
                flush();
            }
        }

        public int PrintList(Expr lst)
        {
            while (true)
            {
                int c = ChurchNumToInt(lst.Apply(Expr.K));
                if (c < 256)
                {
                    writec((byte)c);
                }
                else
                {
                    flush();
                    return c - 256;
                }
                lst = Eval(lst.Apply(Expr.False));
            }
        }
        #endregion


        #region
        public Expr Eval(Expr e)
        {
            var stack = new Stack<Expr>();
            Expr cur = e;
            while (true)
            {
                cur = cur.DropI1();
                while (cur.Type == EType.App)
                {
                    Debug.Assert(cur.Arg1 != null && cur.Arg2 != null);
                    stack.Push(cur);
                    cur = cur.Arg1.DropI1();
                }
                if (stack.Count == 0)
                {
                    return cur;
                }
                var a = cur;
                cur = stack.Pop();
                cur.Arg1 = a;
                evalPrimitive(cur);
            }
        }

        private void evalPrimitive(Expr e)
        {
            Debug.Assert(e.Type == EType.App && e.Arg1 != null && e.Arg2 != null);
            Expr lhs = e.Arg1;
            Expr rhs = e.Arg2;
            e.Arg1 = e.Arg2 = null;
            Debug.Assert(lhs.Type != EType.I1);
            switch (lhs.Type)
            {
                case EType.False:
                    e.Type = EType.I;
                    break;
                case EType.I:
                    e.Type = EType.I1;
                    e.Arg1 = rhs;
                    break;
                case EType.K:
                    e.Type = EType.K1;
                    e.Arg1 = rhs;
                    break;
                case EType.K1:
                    e.Type = EType.I1;
                    e.Arg1 = lhs.Arg1;
                    break;
                case EType.S:
                    e.Type = EType.S1;
                    e.Arg1 = rhs;
                    break;
                case EType.S1:
                    Debug.Assert(lhs.Arg1 != null);
                    e.Type = EType.S2;
                    e.Arg1 = lhs.Arg1;
                    e.Arg2 = rhs;
                    break;
                case EType.S2:
                    Debug.Assert(lhs.Arg1 != null && lhs.Arg2 != null);
                    e.Arg1 = lhs.Arg1.Apply(rhs);
                    e.Arg2 = lhs.Arg2.Apply(rhs);
                    break;
                case EType.Iota:
                    e.Arg1 = rhs.Apply(S);
                    e.Arg2 = K;
                    break;
                case EType.Read:
                    Expr ch = CNums[readc()];
                    Expr readnext = new Expr(EType.Read);
                    e.Arg1 = rhs.Apply(ch);
                    e.Arg2 = readnext;
                    lhs.Type = EType.S2;
                    lhs.Arg1 = new Expr(EType.S2, Expr.I, new Expr(EType.K1, ch));
                    lhs.Arg2 = new Expr(EType.K1, readnext);
                    lhs.IntVal = -1;
                    break;
                case EType.CNum:
                    Debug.Assert(lhs.IntVal >= 0);
                    e.Type = EType.CNum1;
                    e.Arg1 = rhs;
                    e.IntVal = lhs.IntVal;
                    break;
                case EType.CNum1:
                    Debug.Assert(lhs.IntVal >= 0 && lhs.Arg1 != null);
                    if (lhs.Arg1.Type == EType.Inc)
                    {
                        if (rhs.Type != EType.Num) rhs = Eval(rhs);
                        if (rhs.Type == EType.Num)
                        {
                            e.Type = EType.Num;
                            e.IntVal = lhs.IntVal + rhs.IntVal;
                        }
                        else
                        {
                            throw new Exception(String.Format("invalid output format (attempted to apply inc to a non-number): ({0})", rhs.ToString()));
                        }
                    }
                    else
                    {
                        var f = lhs.Arg1;
                        var x = rhs;
                        for (int i = 0; i < lhs.IntVal; i++)
                        {
                            x = f.Apply(x);
                        }
                        e.Type = x.Type;
                        e.Arg1 = x.Arg1;
                        e.Arg2 = x.Arg2;
                        e.IntVal = x.IntVal;
                    }
                    break;
                case EType.Inc:
                    if (rhs.Type != EType.Num)
                    {
                        rhs = Eval(rhs);
                    }
                    if (rhs.Type == EType.Num)
                    {
                        e.Type = EType.Num;
                        e.IntVal = rhs.IntVal + 1;
                    }
                    else
                    {
                        throw new Exception(String.Format("invalid output format (attempted to apply inc to a non-number): ({0})", rhs.ToString()));
                    }
                    break;
                case EType.Num:
                    throw new Exception(String.Format("invalid output format ({0} is not applicable)", lhs.ToString()));
                default:
                    throw new Exception(String.Format("unexpected state (lhs={0} rhs={1})", lhs.ToString(), rhs.ToString()));
            }
            return;
        }

        public int ChurchNumToInt(Expr e)
        {
            var n = Eval(e.Apply(Expr.Inc).Apply(Expr.Num0));
            if (n.Type == EType.Num)
            {
                return n.IntVal;
            }
            else
            {
                throw new Exception(String.Format("invalid output format (result was not a number): {0}", e.Dump(-1)));
            }
        }
        #endregion
        public int Run(string src)
        {
            try
            {
                var prog = Parser.Parse(src).Apply(new Expr(Expr.EType.Read));
                int result = PrintList(Eval(prog));
                return result;
            }
            catch (Exception e)
            {
                output.Flush();
                Console.Error.WriteLine(e.ToString());
                return 1;
            }

        }
    }
}
