// To compile this module as a DLL:
//
//                  dmcs /t:library bcuplib.cs
//
// To link this DLL to a program written in C#:
//
//                  dmcs /r:bcuplib.dll someprogram.cs

using System.Collections;

namespace Basic {
    
    using System;

    public class Utils {

        public static void Print(string i) {
            Console.Write(i + "\t");
        }
        
        public static void Print(double d) {
            Console.Write(d + "\t");
        }
        
        public static void PrintNewLine() {
            Console.WriteLine();
        }
    }
}

