using System.Reflection.Metadata;
using System.Diagnostics;
using System.ComponentModel.Design;
using System.Runtime.CompilerServices;
using System.Text;
using System.Linq.Expressions;
using System;
using System.IO;
using System.ComponentModel.DataAnnotations;

namespace lazyk_cs
{
    internal class Program
    {

        static void showUsage()
        {
            Console.WriteLine("Usage: lazyk-cs.exe [options] [programfile]");
            Console.WriteLine("Options:");
            Console.WriteLine("  -t   text input mode (read CRLF as LF)");
            Console.WriteLine("  -h   show this help");
            Console.WriteLine("  If no program file is specified, the program is read from the standard input.");
            Environment.Exit(1);
        }

        static void Main(string[] args)
        {
            string srcfile = "";
            bool textmode = false;

            foreach (string arg in args)
            {
                switch (arg)
                {
                    case "-t":
                        textmode = true;
                        break;
                    case "-h":
                        showUsage();
                        break;
                    default:
                        srcfile = arg;
                        break;
                }
            }

            string src;
            if (srcfile == "" && !Console.IsInputRedirected)
            {
                showUsage();
                return;
            }
            else if (srcfile == "-" || srcfile == "")
            {
                src = Console.In.ReadToEnd();
            }
            else
            {
                if (File.Exists(srcfile))
                {
                    using (StreamReader sr = new StreamReader(srcfile, Encoding.UTF8))
                    {
                        src = sr.ReadToEnd();
                    }
                }
                else
                {
                    Console.WriteLine("can't open file '{0}'", srcfile);
                    Environment.Exit(2);
                    return;
                }
            }
            var lk = new LazyK(Console.OpenStandardInput(), Console.OpenStandardOutput(), textmode);
            Environment.Exit(lk.Run(src));
        }
    }
}