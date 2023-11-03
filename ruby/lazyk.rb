#!/usr/bin/env ruby

require "stringio"

class LazyK

  class Expr
    attr_accessor :type, :arg1, :arg2

    def initialize(t, a1 = nil, a2 = nil)
      @type = t
      @arg1 = a1
      @arg2 = a2
    end

    S = Expr.new(:s)
    K = Expr.new(:k)
    I = Expr.new(:i)
    IOTA = Expr.new(:iota)
    TRUE = K
    FALSE = Expr.new(:false)
    INC = Expr.new(:inc)
    NUM0 = Expr.new(:num, 0)
    CNUMS = (0..256).map{|i| Expr.new(:cn, i) }

    def drop_i1
      cur = self
      while cur.type == :i1
        cur = cur.arg1
      end
      cur
    end

    def apply(arg)
      Expr.new(:app, self, arg)
    end

    def eval()
      cur = self
      stack = []
      while true
        cur = cur.drop_i1
        while cur.type == :app
          stack << cur
          cur = cur.arg1.drop_i1
        end
        if stack.empty?
          return cur
        end
        a = cur
        cur = stack.pop
        cur.arg1 = a
        cur.eval_primitive
      end
    end

    def eval_primitive()
      lhs = @arg1
      rhs = @arg2.drop_i1
      @arg1 = @arg2 = nil
      case lhs.type
      when :false
        @type = :i
      when :i
        @type = :i1
        @arg1 = rhs
      when :k
        @type = :k1
        @arg1 = rhs
      when :k1
        @type = :i1
        @arg1 = lhs.arg1
      when :s
        @type = :s1
        @arg1 = rhs
      when :s1
        @type = :s2
        @arg1 = lhs.arg1
        @arg2 = rhs
      when :s2
        @arg1 = lhs.arg1.apply(rhs)
        @arg2 = lhs.arg2.apply(rhs)
      when :iota
        @arg1 = rhs.apply(Expr::S)
        @arg2 = Expr::K
      when :read
        reader = lhs.arg1
        cn = CNUMS[reader[]]
        readnext = Expr.new(:read, reader)
        @arg1 = rhs.apply(cn)
        @arg2 = readnext
        # S(SI(K c))(K r) = \b. b c r = cons c r
        lhs.type = :s2
        lhs.arg1 = Expr.new(:s2, Expr::I, Expr.new(:k1, cn))
        lhs.arg2 = Expr.new(:k1, readnext)
      when :cn
        @type = :cn1
        @arg1 = lhs.arg1
        @arg2 = rhs
      when :cn1
        if lhs.arg2.type == :inc
          rhs = rhs.eval if rhs.type != :num
          if rhs.type == :num
            @type = :num
            @arg1 = lhs.arg1 + rhs.arg1
          else
            raise "invalid output format (attempted to apply inc to a non-number): (#{rhs})"
          end
        else
          f = lhs.arg2
          x = rhs
          lhs.arg1.times{ x = f.apply(x) }
          @type = x.type
          @arg1 = x.arg1
          @arg2 = x.arg2
        end
      when :inc
        rhs = rhs.eval if rhs.type != :num
        if rhs.type == :num
          @type = :num
          @arg1 = rhs.arg1 + 1
        else
          raise "invalid output format (attempted to apply inc to a non-number): (#{rhs})"
        end
      when :num
        raise "invalid output format (attempted to apply a number): #{lhs} #{rhs}"
      else
        raise ScriptError.new("unexpected state: lhs=#{lhs} rhs=#{rhs}")
      end
    end

    def to_i
      e = self.apply(Expr::INC).apply(Expr::NUM0).eval
      if e.type == :num
        e.arg1
      else
        raise "invalid output format (result was not a number): #{e}"
      end
    end

    def to_s
      case @type
      when :app then "(#{@arg1.to_s} #{@arg2.to_s})"
      when :i1, :s1, :k1 then "(#{@type} #{@arg1.to_s})"
      when :s2 then "(S #{@arg1.to_s} #{@arg2.to_s})"
      when :cn then "CN#{@arg1}"
      when :cn1 then "(CN#{@arg1} #{@arg2.to_s})"
      when :num then @arg1.to_s
      when :iota then "Iota"
      else @type.to_s.upcase
      end
    end
  end

  class Parser
    def initialize(src)
      @input = StringIO.new(src)
    end

    def nextchar()
      begin
        c = @input.readchar
        while c == "#"
          @input.readline
          c = @input.readchar
        end
      rescue EOFError
        return nil
      end while c.match(/\s/)
      c
    end

    def read_cc(closingchar = nil)
      c = nextchar
      if (c == closingchar)
        return Expr::I
      end
      @input.ungetc(c)
      e = read_one(false)
      while (c = nextchar) != closingchar
        @input.ungetc(c)
        e = e.apply(read_one(false))
      end
      e
    end

    def read_one(i_is_iota)
      c = nextchar
      case c
      when "`", "*"
        read_app(c)
      when "("
        read_cc(")")
      when "S", "s"
        Expr::S
      when "K", "k"
        Expr::K
      when "I"
        Expr::I
      when "i"
        i_is_iota ? Expr::IOTA : Expr::I
      when "0", "1"
        read_jot(c)
      when nil
        raise SyntaxError.new("unexpected EOF")
      else
        raise SyntaxError.new("unexpected char: '${c}'")
      end
    end

    def read_app(c0)
      stack = []
      buf = []
      in_iota = c0 == "*"
      while true
        c = nextchar
        if c == "`" || c == "*"
          stack << [in_iota, buf]
          buf = []
          in_iota = c == "*"
        else
          @input.ungetc(c)
          buf << read_one(in_iota)
        end
        while buf.length == 2
          e = buf[0].apply(buf[1])
          if stack.empty?
            return e
          end
          in_iota, buf = stack.pop
          buf << e
        end
      end
    end

    def read_jot(c0)
      c = c0
      e = Expr::I
      while true
        if c == "0"
          e = e.apply(Expr::S).apply(Expr::K)
        elsif c == "1"
          e = Expr::S.apply(Expr::K.apply(e))
        else
          break
        end
        c = nextchar
      end
      @input.ungetc(c)
      e
    end

    def self.parse(src)
      Parser.new(src).read_cc
    end
  end

  def initialize(stdin = $stdin, stdout = $stdout, textmode = false)
    @stdout = stdout
    @stdout.binmode
    @stdin = stdin
    @textmode = textmode
  end

  def readc()
    begin
      @stdout.flush
      b = @stdin.readbyte
      if b == 13 && @textmode
        return readc
      else
        return b
      end
    rescue EOFError
      return 256
    end
  end

  def writec(c)
    @stdout.write([c].pack("C"))
    @stdout.flush if c == 10
  end

  def print_list(lst)
    car = ->(e) { e.apply(Expr::K).eval }
    cdr = ->(e) { e.apply(Expr::FALSE).eval }
    while true
      c = car[lst].to_i
      if c >= 256
        @stdout.flush
        return c-256
      else
        writec(c)
      end
      lst = cdr[lst]
    end
  end

  def run(src)
    prog = Parser.parse(src)
    input = Expr.new(:read, method(:readc))
    print_list(prog.apply(input))
  end

  def run_file(filepath)
    File.open(filepath, "r"){|f| run(f.read) }
  end
  public :run, :run_file
end

def show_help_and_exit
  puts "Usage: ruby [-t] lazyk.rb prog.rb"
  puts "Options"
  puts "  -t : text mode (read crlf as lf)"
  exit(1)
end

if __FILE__ == $0
  if ARGV.length == 0
    textmode = false
    if $stdin.tty?
      show_help_and_exit
    else
      exit LazyK.new(StringIO.new(""), textmode).run($stdin.read)
    end
  else
    if ARGV[0] == "-h" || ARGV[0] == "--help"
      show_help_and_exit
    else
      if ARGV[0] == "-t"
        textmode = true
        ARGV.shift
      end
      exit LazyK.new($stdin, $stdout, textmode).run_file(ARGV[0])
    end
  end
end
