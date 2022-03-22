#' @title .NET application
#' @description Documentation for dotnet's internal 'DotNetApp'
#'   [`R6`][R6::R6Class] class.
#' @section Methods:
#' \describe{
#'   \item{`new(app_name, app_dir, language, packages)`}{Creates a new .NET **console** application in `app_dir`}
#'   \item{`add_package(package_name)`}{Adds NuGet package to the app}
#'   \item{`add_source(source_code, file_name = "Program")`}{Add source code to the app; **note**: the extension will be appended automatically based on the app's `language`}
#'   \item{`build(output_dir)`}{Builds app and outputs artifacts to `output_dir`}
#'   \item{`run(capture_output = TRUE)`}{Runs app and returns character output}
#' }
#' @section Properties:
#' \describe{
#'   \item{`language`}{The app's language (C# or F#, specified during creation)}
#'   \item{`path`}{Full path to the app's files}
#'   \item{`packages`}{NuGet package names, added with `$add_package()` method}
#'   \item{`source_files`}{Source file names, added with `$add_source()` method}
#'   \item{`source_code`}{Named `list` of source files' contents}
#' }
#' @section Languages:
#' When creating a new `DotNetApp`, it defaults to the programming language set
#' in `dotnet.language` option, which is C# by default. This can be overridden
#' by specifying `language = "F#"` when creating a new instance or by changing
#' the default via `options(dotnet.language = "F#")`, to avoid having to specify
#' it every time.
#' @section Packages:
#' Packages can be added to the app after it was created by using `add_package`
#' method. Packages can be added at create time by specifying them with the
#' `packages` argument when calling `$new()`.
#'
#' To avoid having to add packages manually every time when creating a new app
#' or if you have multiple chunks using the `dotnet` knitr engine and they all
#' depend on the same set of packages, you can set a character vector of package
#' names in the `dotnet.packages` option. Those packages will be added
#' automatically to all newly created apps.
#' @section Debug mode:
#' To facilitate development, the `debug_mode_on` flag in `$new()` can be used
#' to suppress any `dotnet` commands from actually running. Instead, the command
#' that *would have* run is printed to the console for the user to run manually
#' in the shell of their choice.
#' ```
#' debug_csharp_app <- dotnet:::DotNetApp$new(debug_mode_on = TRUE)
#' debug_csharp_app$run()
#' ```
#' @name DotNetApp
NULL

DotNetApp <- R6::R6Class(
  "DotNetApp",
  lock_class = TRUE,
  cloneable = FALSE,
  private = list(
    lang = "",
    app_name = "",
    app_dir = "",
    pkgs = NULL,
    files = NULL,
    debug_mode_on = NULL
  ),
  public = list(
    initialize = function(
      app_name = "myApp",
      app_dir = file.path(tempdir(), app_name),
      language = getOption("dotnet.language"),
      packages = getOption("dotnet.packages"),
      debug_mode_on = FALSE)
    {
      if (!language %in% c("C#", "F#")) {
        stop("language must be one of: 'C#' or 'F#'")
      } else {
        private$lang <- language
      }
      proj_name <- paste0(app_name, ifelse(language == "C#", ".csproj", ".fsproj"))
      proj_path <- file.path(app_dir, proj_name)
      # Keep internal track:
      private$app_name <- app_name
      private$app_dir <- app_dir
      private$debug_mode_on <- debug_mode_on
      if (file.exists(proj_path)) {
        message(proj_path, " already exists")
      } else {
        sys_args <- c(
          "new", "console",
          "--language", language,
          "--name", app_name,
          "--output", app_dir
        )
        if (private$debug_mode_on) {
          cat("dotnet", sys_args)
          return(invisible(NULL))
        }
        system2("dotnet", args = sys_args)
      }
      # Add packages, if any:
      if (!is.null(packages)) {
        for (pkg in packages) {
          self$add_package(pkg)
        }
      }
    },
    add_package = function(package_name) {
      # Usage: dotnet [options] add [<PROJECT>] [command]
      if (package_name %in% private$packages) {
        message("Package '", package_name, "' has already been added")
        return(invisible(NULL))
      }
      # Keep internal track of added package:
      private$pkgs <- c(private$pkgs, package_name)
      sys_args <- c("add", private$app_dir, "package", package_name)
      if (private$debug_mode_on) {
        cat("dotnet", sys_args)
        return(invisible(NULL))
      }
      system2("dotnet", args = sys_args)
    },
    add_source = function(source_code, file_name = "Program") {
      file_ext <- ifelse(private$lang == "C#", ".cs", ".fs")
      file_name <- paste0(file_name, file_ext)
      file_path <- file.path(private$app_dir, file_name)
      if (length(source_code) > 1) {
        source_code <- paste0(source_code, collapse = "\n")
      }
      writeLines(source_code, file_path)
      # Keep internal track of added file:
      private$files <- union(private$files, file_name)
    },
    build = function(output_dir) {
      # Usage: dotnet [options] build [<PROJECT | SOLUTION>...]
      sys_args <- c("build", "--output", output_dir, private$app_dir)
      if (private$debug_mode_on) {
        cat("dotnet", sys_args)
        return(invisible(NULL))
      }
      system2("dotnet", args = sys_args)
    },
    run = function(capture_output = TRUE) {
      # Usage: dotnet [options] run [[--] <additional arguments>...]]
      sys_args <- c("run", "--project", private$app_dir)
      if (private$debug_mode_on) {
        cat("dotnet", sys_args)
        return(invisible(NULL))
      }
      out <- system2("dotnet", args = sys_args, stdout = capture_output)
      if (capture_output) {
        return(out)
      } else {
        return(invisible(NULL))
      }
    }
  ),
  active = list(
    language = function() {
      return(private$lang)
    },
    path = function() {
      return(private$app_dir)
    },
    packages = function() {
      return(private$pkgs)
    },
    source_files = function() {
      return(private$files)
    },
    source_code = function() {
      source_code <- lapply(private$files, readLines)
      names(source_code) <- private$files
      return(source_code)
    }
  )
)
