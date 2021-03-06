﻿#r @"..\packages\FParsec.1.0.1\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.1\lib\net40-client\FParsec.dll"
#r @"System.Core.dll"
#r @"System.dll"
#r @"System.Numerics.dll"

#load @"Types.fs"
#load @"Parsing.fs"

open FParsec
open CSharpParsing.Types
open CSharpParsing.Parsing
    
let program = """
#region toto
using System; 
namespace CompanyName { 
  namespace ProjectName.FolderName {
  /* My class */
  class MyClass : IMarker {
    private const string CatalogUri = @"\n/AnalogWay.ARCORX.Boot;component/ModulesCatalog.xaml";
    private const string CatalogUri = "\n/AnalogWay.ARCORX.Boot;component/ModulesCatalog.xaml";
    float y = 42;
    // My foo

    protected override IModuleCatalog CreateModuleCatalog()
    {
        toto(32);
        return Microsoft.Practices.Prism.Modularity.ModuleCatalog.CreateFromXaml(new Uri(CatalogUri, UriKind.RelativeOrAbsolute));
    }

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