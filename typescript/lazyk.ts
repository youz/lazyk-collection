export enum EType {
  App = 0,
  S,
  S1,
  S2,
  K,
  K1,
  I,
  I1,
  Iota,
  False,
  CNum,
  CNum1,
  Inc,
  Num,
  Read,
}

export class Expr {
  type: number;
  intval: number;
  arg1?: Expr;
  arg2?: Expr;
  constructor(t: EType, a1?: Expr, a2?: Expr, intval: number = 0) {
    this.type = t;
    this.arg1 = a1;
    this.arg2 = a2;
    this.intval = intval;
  }

  // static members
  static S = new Expr(EType.S);
  static K = new Expr(EType.K);
  static I = new Expr(EType.I);
  static Iota = new Expr(EType.Iota);
  static False = new Expr(EType.False);
  static Inc = new Expr(EType.Inc);
  private static _cnums = new Array(257);
  private static _succ = Expr.S.apply(Expr.S.apply(Expr.K.apply(Expr.S)).apply(Expr.K))
  static CNum(n: number): Expr {
    return Expr._cnums[n] ||= new Expr(EType.CNum, undefined, undefined, n);
  }
  static Num(n: number): Expr {
    return new Expr(EType.Num, undefined, undefined, n);
  }
  static Num0 = Expr.Num(0);

  // methods
  apply(a: Expr): Expr {
    return new Expr(EType.App, this, a);
  }

  toString(): string {
    switch (this.type) {
      case EType.S1, EType.K1, EType.I1:
        return `(${EType[this.type][0]} ${this.arg1?.toString()})`;
      case EType.App:
        return `(${this.arg1?.toString()} ${this.arg2?.toString()})`;
      case EType.S2:
        return `(S ${this.arg1?.toString()} ${this.arg2?.toString()})`;
      case EType.CNum, EType.Num:
        return EType[this.type] + `#${this.intval}`;
      case EType.CNum1:
        return `(CNum#${this.intval} ${this.arg1?.toString()})`;
      default:
        return EType[this.type];
    }
  }

  dropI1(): Expr {
    let cur: Expr = this;
    while (cur.type === EType.I1) {
      cur = cur.arg1 as Expr;
    }
    return cur;
  }
}

export class Parser {
  private _src: string;
  private _p: number;
  private _unget: string[];
  constructor(src: string) {
    this._src = src;
    this._p = 0;
    this._unget = [];
  }

  private nextc(): string {
    if (this._unget.length > 0) return this._unget.pop() as string;
    let c: string;
    do {
      if (this._p === this._src.length) return "";
      c = this._src[this._p++];
      if (c === "#") {
        do {
          if (this._p === this._src.length) return "";
          c = this._src[this._p++];
        } while (c !== "\n");
      }
    } while (c.match(/\s/));
    return c;
  }

  private ungetc(c: string): void {
    this._unget.push(c);
  }

  readCC(closingchar: string): Expr {
    let c = this.nextc();
    if (c === closingchar) return Expr.I;
    this.ungetc(c);
    let e: Expr = this.readOne(false);
    while ((c = this.nextc()) !== closingchar) {
      this.ungetc(c);
      e = e.apply(this.readOne(false));
    }
    return e;
  }

  readOne(i_is_iota: boolean): Expr {
    const c = this.nextc();
    switch (c) {
      case "`":
      case "*":
        return this.readApp(c);
      case "(":
        return this.readCC(")");
      case "S":
      case "s":
        return Expr.S;
      case "K":
      case "k":
        return Expr.K;
      case "I":
        return Expr.I;
      case "i":
        return (i_is_iota ? Expr.Iota : Expr.I);
      case "0":
      case "1":
        return this.readJot(c);
      case "":
        throw new Error("unexpected EOF");
      default:
        throw new Error(`unexpected char: ${c}`);
    }
  }

  readApp(c0: string): Expr {
    const stack: { in_iota: boolean; buf: Expr[] }[] = [];
    let buf: Expr[] = [];
    let in_iota = c0 == "*";
    while (true) {
      const c = this.nextc();
      if (c === "`" || c === "*")
      {
        stack.push({ in_iota, buf });
        in_iota = c === "*";
        buf = [];
      } else {
        this.ungetc(c);
        buf.push(this.readOne(in_iota));
      }
      while (buf.length === 2) {
        const e = buf[0].apply(buf[1]);
        if (stack.length === 0) return e;
        const s = stack.pop();
        if (s) {
          in_iota = s.in_iota;
          buf = s.buf;
          buf.push(e);
        }
      }
    }
  }

  readJot(c0: string): Expr {
    let e = Expr.I;
    let c = c0;
    while (c === "0" || c === "1") {
      if (c === "0") {
        e = e.apply(Expr.S).apply(Expr.K);
      } else {
        e = Expr.S.apply(Expr.K.apply(e));
      }
      c = this.nextc();
    }
    this.ungetc(c);
    return e;
  }

  static parse(src: string): Expr {
    return new Parser(src).readCC("");
  }
}

export interface IObase {
  readsync(buf: Uint8Array): number | null;
  writesync(buf: Uint8Array): void;
}

export class LazyK {
  private readonly _io: IObase;
  private _inbuf: number[];
  private _iniseof: boolean;
  private _outbuf: number[];

  constructor(io: IObase) {
    this._io = io;
    this._inbuf = [];
    this._iniseof = false;
    this._outbuf = [];
  }

  private read = (): number => {
    if (!this._iniseof && this._inbuf.length === 0) {
      this.flush();
      const buf = new Uint8Array(1024);
      const len = this._io.readsync(buf);
      if (len === null) {
        this._iniseof = true;
      } else {
        this._inbuf = Array.from(buf.slice(0, len));
      }
    }
    if (this._iniseof) {
      return 256;
    } else {
      return this._inbuf.shift() as number;
    }
  };

  private flush(): void {
    const a = new Uint8Array(this._outbuf);
    this._outbuf = [];
    this._io.writesync(a);
  }

  private write(c: number): void {
    this._outbuf.push(c);
    if (c === 10) this.flush();
  }

  churchNumToInt(cn: Expr): number {
    const n = this.eval(cn.apply(Expr.Inc).apply(Expr.Num0));
    if (n.type === EType.Num) {
      return n.intval;
    } else {
      throw `invalid output format: ${this.toString()}`;
    }
  }

  eval(e: Expr): Expr {
    const stack: Expr[] = [];
    let cur: Expr = e;
    while (true) {
      cur = cur.dropI1();
      while (cur.type === EType.App) {
        stack.push(cur);
        cur = (cur.arg1 as Expr).dropI1();
      }
      if (stack.length === 0) return cur;
      const a = cur;
      cur = stack.pop() as Expr;
      cur.arg1 = a;
      this.evalPrimitive(cur);
    }
  }

  private evalPrimitive(e: Expr): void {
    let lhs: Expr = e.arg1 as Expr;
    let rhs: Expr = e.arg2 as Expr;
    let ch: Expr;
    let readnext: Expr;
    e.arg1 = e.arg2 = undefined;
    switch (lhs.type) {
      case EType.False:
        e.type = EType.I;
        break;
      case EType.I:
        e.type = EType.I1;
        e.arg1 = rhs;
        break;
      case EType.K:
        e.type = EType.K1;
        e.arg1 = rhs;
        break;
      case EType.K1:
        e.type = EType.I1;
        e.arg1 = lhs.arg1;
        break;
      case EType.S:
        e.type = EType.S1;
        e.arg1 = rhs;
        break;
      case EType.S1:
        e.type = EType.S2;
        e.arg1 = lhs.arg1;
        e.arg2 = rhs;
        break;
      case EType.S2:
        e.arg1 = (lhs.arg1 as Expr).apply(rhs);
        e.arg2 = (lhs.arg2 as Expr).apply(rhs);
        break;
      case EType.Iota:
        e.arg1 = rhs.apply(Expr.S);
        e.arg2 = Expr.K;
        break;
      case EType.Read:
        ch = Expr.CNum(this.read());
        readnext = new Expr(EType.Read);
        e.arg1 = rhs.apply(ch);
        e.arg2 = readnext;
        lhs.type = EType.S2;
        lhs.arg1 = new Expr(EType.S2, Expr.I, new Expr(EType.K1, ch));
        lhs.arg2 = new Expr(EType.K1, readnext);
        lhs.intval = 0;
        break;
      case EType.CNum:
        e.type = EType.CNum1;
        e.arg1 = rhs;
        e.intval = lhs.intval;
        break;
      case EType.CNum1:
        if ((lhs.arg1 as Expr).type === EType.Inc && rhs.type === EType.Num) {
          e.type = EType.Num;
          e.intval = lhs.intval + rhs.intval;
        } else {
          const f = lhs.arg1 as Expr;
          let x = rhs;
          for (let i = 0; i < lhs.intval; i++) {
            x = f.apply(x);
          }
          e.type = x.type;
          e.arg1 = x.arg1;
          e.arg2 = x.arg2;
          e.intval = x.intval;
        }
        break;
      case EType.Inc:
        if (rhs.type !== EType.Num) {
          rhs = this.eval(rhs);
        }
        if (rhs.type === EType.Num) {
          e.type = EType.Num;
          e.intval = rhs.intval + 1;
        } else {
          throw new Error(
            `invalid output format (attempted to apply inc to a non-number): (${rhs.toString()})`,
          );
        }
        break;
      case EType.Num:
        throw new Error(
          `invalid output format (${lhs.toString()} is not applicable)`,
        );
      default:
        throw new Error(
          `unexpected state (lhs=${lhs.toString()} rhs=${rhs.toString()})`,
        );
    }
  }

  printList(lst: Expr): number {
    const carch = (e: Expr) => this.churchNumToInt(e.apply(Expr.K));
    const cdr = (e: Expr) => this.eval(e.apply(Expr.False));
    let c: number;
    while ((c = carch(lst)) < 256) {
      this.write(c);
      lst = cdr(lst);
    }
    this.flush();
    return c - 256;
  }

  run(src: string): number {
    const parsed = Parser.parse(src);
    try {
      return this.printList(parsed.apply(new Expr(EType.Read)));
    } catch (e) {
      this.flush();
      console.log(e);
      return 1;
    }
  }
}

function main() {
  if (Deno.args.length === 0) {
    console.log("usage: deno run --allow-read lazyk.ts <source.lazy>");
    Deno.exit(1);
  }
  const src = Deno.readTextFileSync(Deno.args[0]);
  const io = {
    readsync: (buf: Uint8Array) => {
      return Deno.stdin.readSync(buf);
    },
    writesync: (buf: Uint8Array) => {
      Deno.stdout.writeSync(buf);
    },
  };
  Deno.exit(new LazyK(io).run(src));
}

if (import.meta.main) {
  main();
}
