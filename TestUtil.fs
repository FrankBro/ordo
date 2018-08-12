module TestUtil

open System
open System.IO

open Xunit
open Xunit.Abstractions

type MakeConsoleWork (output: ITestOutputHelper) =

    let _output = output
    let _originalOut = Console.Out
    let _textWriter = new StringWriter()
    do Console.SetOut(_textWriter)

    interface IDisposable with
        member x.Dispose () =
            _output.WriteLine(_textWriter.ToString())
            Console.SetOut(_originalOut)
