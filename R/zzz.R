.onLoad <- function(libname, pkgname) {
  options(dotnet.language = getOption("dotnet.language", default = "C#"))
  return(invisible(NULL))
}
