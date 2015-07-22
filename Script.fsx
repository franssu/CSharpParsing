#r @"..\packages\FParsec.1.0.1\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.1\lib\net40-client\FParsec.dll"
#r @"System.Core.dll"
#r @"System.dll"
#r @"System.Numerics.dll"

#load @"Library1.fs"

open FParsec
open CSharpParsing.Library1
    
let program = """
#region toto
using System; 
namespace CompanyName { 
  namespace ProjectName.FolderName {
  /* My class */
  class MyClass : IMarker {
    const float y = 42;
    // My foo
    bool MyMethod(string arg) {
      string s = (string) "hello"; // assign string lit
      for(int i = 1; i<=100; i++) {
        if (i % 15 == 0) Console.WriteLine("FizzBuzz");
      }
      switch(y) { case 0: break; case 1: y = 1; default: }
      bool x;   
      x = true;  
      while(x) { 
        if (true) { x = false; } else if (false) { goto end; }
      }
end:
      return true;
    }
    bool MyProp {
      get { }
      set { y = value; }
    }
  } 
  struct MyStruct { }
  interface IMarker { }
  enum MyEnum { One, Two, Three }
  } 
}
"""
run pnsscope program

let test = System.IO.File.ReadAllText(@"C:\Projets\SOFTPC\AllSpark\AnalogWay.ARCORX.Boot\Bootstrapper.cs")
run pnsscope test