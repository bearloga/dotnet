#' @title Register .NET knitr engine
#' @description Registers `dotnet` knitr engine for use in R Markdown chunks.
#' @section Engine options:
#' Options (and their defaults) that can be set in `engine.opts`:
#' \describe{
#'   \item{`name`}{application name ("myApp" by default)}
#'   \item{`dir`}{path to the application source, can be existing application}
#'   \item{`file`}{source file name ("Program" by default)}
#'   \item{`run_app`}{`TRUE` by default; set to `FALSE` to not run application}
#'   \item{`language`}{"C#" or "F#"; if omitted uses `getOption("dotnet.language")` which is C# by default}
#'   \item{`add_packages`}{NuGet packages the code depends on, in addition to those listed in `getOption("dotnet.language")` (if any)}
#' }
#' @section Caching:
#' If `cache=TRUE` in chunk options, `engine.opts$dir` and `engine.opts$file`
#' will be preset and cannot be overridden. Caching only works for single-chunk
#' programs.
#' @section Multi-chunk programs:
#' In cases where you want to split your program across multiple chunks, there
#' are a few things to keep in mind. Most importantly is that because this is
#' accomplished by specifying `engine.opts(name = "appName", dir = "/path/to/app")`,
#' you will need to manually manage that directory. A new application will be
#' created there the first time the document is knit, but will not be deleted
#' after the fact.
#'
#' If an application is not ready to be run, set `engine.opts(run_app = FALSE)`.
#' @inheritSection DotNetApp Languages
#' @inheritSection DotNetApp Packages
#' @references
#' [Registering a custom language engine](https://bookdown.org/yihui/rmarkdown-cookbook/custom-engine.html)
#' @export
register_engine <- function() {
  knitr::knit_engines$set(
    dotnet = function(options) {
      opts <- options[["engine.opts"]]

      if ("language" %in% names(opts)) {
        language <- opts[["language"]]
      } else {
        language <- getOption("dotnet.language")
      }
      highlights <- c(
        "C#" = "csharp",
        "F#" = "fsharp"
      )
      langs <- names(highlights)
      if (!language %in% langs) {
        stop(language, " is not one of: ", paste0(langs, collapse = ", "))
      }
      code <- options$code

      if (options$eval) {

        if ("name" %in% names(opts)) {
          app_name <- opts[["name"]]
        } else {
          app_name <- "myApp"
        }

        cache <- options$cache
        if (cache) {
          cache_path <- knitr:::valid_path(options[["cache.path"]], options$label)
          app_dir <- file.path(cache_path, app_name)
        } else {
          if ("dir" %in% names(opts)) {
            app_dir <- opts[["dir"]]
          } else {
            app_dir <- tempdir()
          }
        }

        app <- DotNetApp$new(app_name, app_dir, language)

        if ("add_packages" %in% names(opts)) {
          for (pkg in opts[["add_packages"]]) {
            app$add_package(pkg)
          }
        }

        if ("file" %in% names(opts) && !cache) {
          file_name <- opts[["file"]]
        } else {
          file_name <- "Program"
        }
        app$add_source(code, file_name)

        if ("run_app" %in% names(opts)) {
          run_app <- opts[["run_app"]]
        } else {
          run_app <- TRUE
        }

        if (run_app) {
          out <- app$run(capture_output = TRUE)
        } else {
          out <- ""
        }

      } else {
        out <- ""
      }

      options$engine <- highlights[language]
      knitr::engine_output(options, code, out)

    })
}
