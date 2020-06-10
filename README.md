
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dotnet

<!-- badges: start -->

<!-- badges: end -->

The primary goal of {dotnet} is to provide a
[{knitr}](https://yihui.org/knitr/)
[engine](https://yihui.org/knitr/demo/engines/) for the
[C\#](https://docs.microsoft.com/en-us/dotnet/csharp/) and
[F\#](https://docs.microsoft.com/en-us/dotnet/fsharp/) languages and the
[.NET Framework](https://dotnet.microsoft.com/). Internally, it
accomplishes via an R6-based interface for the `dotnet` command line
interface for building .NET applications. See `?DotNetApp` for details.

*Not to be confused* with:

  - [{rDotNet}](https://cran.r-project.org/package=rDotNet) R package
    which is a “low-level interface to ‘.NET’ virtual machine” and which
    “can create ‘.NET’ objects, call methods, get or set properties,
    call static functions, etc.”
  - [R.NET](https://rdotnet.github.io/rdotnet/) NuGet package which
    “enables the .NET Framework to interoperate with the R statistical
    language in the same process”

**Notice**: this package is released as-is. I don’t know that I’ll be
able to maintain it all that much. Like, the original motivation for
this package was wanting to write a blog post about Bayesian inference
and probabilistic programming with
[Infer.NET](https://dotnet.github.io/infer/) in [R
Markdown](https://rmarkdown.rstudio.com/) for my
[{blogdown}](https://bookdown.org/yihui/blogdown/)-based site. Which is
to say it was a fun, goofy exercise in learning how to make a custom
knitr engine. Also, “C\#/F\# in R Markdown in blogdown-based websites”
is a VERY obscure use-case. Let me know on Twitter if you use this
package. (I’m [bearloga](https://twitter.com/bearloga) on there.)

It is licensed under [BSD 2-Clause “Simplified” License](LICENSE.md).

## Installation

To start rendering R Markdown documents with `dotnet` engine chunks, you
need to be able to build .NET apps on your system. Download and install
the [.NET SDK](https://dotnet.microsoft.com/download). If you can run
`dotnet` in Terminal without problems, you’re set.

``` r
remotes::install_github("bearloga/dotnet")
```

## Usage

First, the `dotnet` engine must be registered with knitr:

``` r
dotnet::register_engine()
```

Once registered with knitr, we use the `dotnet` engine to run programs
written in C\#:

    ```{dotnet}
    using System;
    ```

The following examples are `dotnet`-using chunks in this README’s source
Rmd document.

``` csharp
using System;

class Program {
  static void Main(string[] args) {
    Console.WriteLine("Hello from C# and R Markdown!");
  }
}
#> Hello from C# and R Markdown!
```

…and programs written in F\#, by specifying `engine.opts = list(language
= 'F#')` in the chunk options:

``` fsharp
open System

[<EntryPoint>]
let main argv =
    printfn "Hello from F# and R Markdown!"
    0 // return an integer exit code
#> Hello from F# and R Markdown!
```

Programs with source code split across multiple chunks are supported.
Refer to the [multi-chunk programs
vignette](https://github.com/bearloga/dotnet/blob/master/vignettes/multi-chunk-program.Rmd)
for reference.

Programs with dependencies on NuGet packages can add those via
`engine.opts`. For example:

    ```{dotnet, engine.opts = list(add_packages = c('Microsoft.ML.Probabilistic.Compiler'))}
    using System;
    using Microsoft.ML.Probabilistic.Models;
    ```

This is demonstrated in the [“Inference on two coins with Infer.NET”
vignette](https://github.com/bearloga/dotnet/blob/master/vignettes/two-coins.Rmd).

Refer to `?register_engine` for more information about the `dotnet`
knitr engine..
