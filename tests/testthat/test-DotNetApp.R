hello_world_cs <- "
using System;

class Program
{
  static void Main(string[] args) {
    Console.WriteLine(\"Hello World!\");
  }
}
"

test_that("C#", {
  csharp_app <- dotnet:::DotNetApp$new("HelloWorld", file.path(tempdir(), "CSharp"))
  csharp_app$add_source(hello_world_cs)
  output <- csharp_app$run()
  expect_equal(output, "Hello World!")
})

hello_world_fs <- "
open System

[<EntryPoint>]
let main argv =
    printfn \"Hello World from F#!\"
    0 // return an integer exit code
"

test_that("F#", {
  fsharp_app <- dotnet:::DotNetApp$new("HelloWorld", file.path(tempdir(), "FSharp"), language = "F#")
  fsharp_app$add_source(hello_world_fs)
  output <- fsharp_app$run()
  expect_equal(output, "Hello World from F#!")
})

simple_example_cs <- "
using System;
using Microsoft.ML.Probabilistic.Models;

class Program
{
  static void Main(string[] args)
  {
    Variable<bool> firstCoin = Variable.Bernoulli(0.5);
    Variable<bool> secondCoin = Variable.Bernoulli(0.5);
    Variable<bool> bothHeads = firstCoin & secondCoin;
    InferenceEngine engine = new InferenceEngine();
    Console.WriteLine(\"Probability both coins are heads: \" + engine.Infer(bothHeads));
  }
}
"

test_that("Infer.NET", {
  infer_app <- dotnet:::DotNetApp$new("SimpleExample", file.path(tempdir(), "InferNET"))
  infer_app$add_package("Microsoft.ML.Probabilistic.Compiler")
  infer_app$add_source(simple_example_cs)
  output <- infer_app$run()
  expect_equal(output[1], "Compiling model...done.")
  expect_equal(output[2], "Probability both coins are heads: Bernoulli(0.25)")
})
