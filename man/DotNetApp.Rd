% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dotnet-app.R
\name{DotNetApp}
\alias{DotNetApp}
\title{.NET application}
\description{
Documentation for dotnet's internal 'DotNetApp'
\code{\link[R6:R6Class]{R6}} class.
}
\section{Methods}{

\describe{
\item{\code{new(app_name, app_dir, language, packages)}}{Creates a new .NET \strong{console} application in \code{app_dir}}
\item{\code{add_package(package_name)}}{Adds NuGet package to the app}
\item{\code{add_source(source_code, file_name = "Program")}}{Add source code to the app; \strong{note}: the extension will be appended automatically based on the app's \code{language}}
\item{\code{build(output_dir)}}{Builds app and outputs artifacts to \code{output_dir}}
\item{\code{run(capture_output = TRUE)}}{Runs app and returns character output}
}
}

\section{Properties}{

\describe{
\item{\code{language}}{The app's language (C# or F#, specified during creation)}
\item{\code{path}}{Full path to the app's files}
\item{\code{packages}}{NuGet package names, added with \verb{$add_package()} method}
\item{\code{source_files}}{Source file names, added with \verb{$add_source()} method}
\item{\code{source_code}}{Named \code{list} of source files' contents}
}
}

\section{Languages}{

When creating a new \code{DotNetApp}, it defaults to the programming language set
in \code{dotnet.language} option, which is C# by default. This can be overridden
by specifying \code{language = "F#"} when creating a new instance or by changing
the default via \code{options(dotnet.language = "F#")}, to avoid having to specify
it every time.
}

\section{Packages}{

Packages can be added to the app after it was created by using \code{add_package}
method. Packages can be added at create time by specifying them with the
\code{packages} argument when calling \verb{$new()}.

To avoid having to add packages manually every time when creating a new app
or if you have multiple chunks using the \code{dotnet} knitr engine and they all
depend on the same set of packages, you can set a character vector of package
names in the \code{dotnet.packages} option. Those packages will be added
automatically to all newly created apps.
}

\section{Debug mode}{

To facilitate development, the \code{debug_mode_on} flag in \verb{$new()} can be used
to suppress any \code{dotnet} commands from actually running. Instead, the command
that \emph{would have} run is printed to the console for the user to run manually
in the shell of their choice.\preformatted{debug_csharp_app <- dotnet:::DotNetApp$new(debug_mode_on = TRUE)
debug_csharp_app$run()
}
}

