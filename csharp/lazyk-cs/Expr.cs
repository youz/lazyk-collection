using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace lazyk_cs
{
    public class Expr
    {
        public enum EType
        {
            App, S, S1, S2, K, K1, I, I1, Iota, False, CNum, CNum1, Num, Read, Inc
        };

        public EType Type { get; set; }
        public Expr? Arg1 { get; set; }
        public Expr? Arg2 { get; set; }
        public int IntVal { get; set; }

        public Expr(EType type) {
            this.Type = type;
            this.IntVal = -1;
        }

        public Expr(EType type, Expr arg) : this(type)
        {
            this.Arg1 = arg;
        }

        public Expr(EType type, Expr arg1, Expr arg2) : this(type, arg1)
        {
            this.Arg2 = arg2;
        }

        public Expr(EType type, int i)
        {
            Debug.Assert(type == EType.Num || type == EType.CNum);
            this.Type = type;
            this.IntVal = i;
        }

        public static Expr Num(int i) { return new Expr(EType.Num, i); }
        public static Expr CNum(int i) { return new Expr(EType.CNum, i); }

        public static Expr S = new Expr(EType.S);
        public static Expr K = new Expr(EType.K);
        public static Expr I = new Expr(EType.I);
        public static Expr Iota = new Expr(EType.Iota);
        public static Expr False = new Expr(EType.False);
        public static Expr Num0 = Expr.Num(0);
        public static Expr Inc = new Expr(EType.Inc);
        public static Expr[] CNums = Enumerable.Range(0, 257).Select(i => Expr.CNum(i)).ToArray();

        public Expr Apply(Expr arg)
        {
            return new Expr(EType.App, this, arg);
        }

        public Expr DropI1()
        {
            var cur = this;
            while (cur.Type == EType.I1)
            {
                Debug.Assert(cur.Arg1 != null);
                cur = cur.Arg1;
            }
            return cur;
        }

        public string Dump(int level)
        {
            level = Math.Max(level, -1);
            switch (this.Type)
            {
                case EType.App:
                    Debug.Assert(this.Arg1 != null && this.Arg2 != null);
                    if (level == 0)
                        return String.Format("(<{0}> <{1}>)", this.Arg1.Type.ToString(), this.Arg2.Type.ToString());
                    else
                        return String.Format("({0} {1})", this.Arg1.Dump(level - 1), this.Arg2.Dump(level - 1));
                case EType.S1:
                case EType.K1:
                case EType.I1:
                    Debug.Assert(this.Arg1 != null);
                    if (level == 0)
                        return String.Format("({0} <{1}>)", this.Type.ToString()[0], this.Arg1.Type.ToString());
                    else
                        return String.Format("({0} {1})", this.Type.ToString()[0], this.Arg1.Dump(level - 1));
                case EType.S2:
                    Debug.Assert(this.Arg1 != null && this.Arg2 != null);
                    if (level == 0)
                        return String.Format("(S <{1}> <{2}>)", this.Arg1.Type.ToString(), this.Arg2.Type.ToString());
                    else
                        return String.Format("(S {1} {2})", this.Arg1.Dump(level - 1), this.Arg2.Dump(level - 1));
                case EType.CNum:
                case EType.Num:
                    return String.Format("{0}#{1}", this.Type.ToString(), this.IntVal);
                case EType.CNum1:
                    Debug.Assert(this.Arg1 != null);
                    if (level == 0)
                        return String.Format("(CNum#{0} <{1}>)", this.IntVal, this.Arg1.Type.ToString());
                    else
                        return String.Format("(CNum#{0} {1})", this.IntVal, this.Arg1.Dump(level - 1));
                default:
                    return this.Type.ToString();
            }
        }

        public override string ToString()
        {
            return Dump(0);
        }
    }
}
