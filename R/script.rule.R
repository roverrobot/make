#' checks if file is contained in files
#' @param files a character vector specifying file names
#' @param file a character specifying the file to be searched
containsFile <- function (files, file) {
  file = normalizePath(file, mustWork = FALSE)
  files = normalizePath(files, mustWork = FALSE)
  file %in% files
}

scriptRule <- setRefClass("scriptRule",
  contains = c("makeRule"),
  methods = list(
    initialize = function(..., .script = NULL, interpreter="") {
      callSuper(..., recipe = NULL)
      if (!is.null(.script)) {
        if (!containsFile(depend, .script))
          depend <<- c(depend, script)
      } else if (length(depend) > 0)
        .script = depend[[1]]
      recipe <<- scriptRecipe(script=.script, interpreter=interpreter)
    }
  )
)

scriptRecipe <- setRefClass(
  "scriptRecipe",
  contains = c("Recipe"),
  fields = c(
    script = "character",
    interpreter = "character"
  ),
  methods = list(
    run = function(target, depend) {
      if (!file.exists(script))
        stop("The recipe script ", script, " does not exist.")
      # if the interpreter is not specified, see if we can read fromthe script
      if (nchar(interpreter) == 0) {
        first.line = readLines(script, n=1)
        match <- regexpr("^#!\\s*(?'handler'.*?)\\s*(\\n|$)", first.line[[1]], perl=TRUE)
        if (match == 1) {
          start <- attr(match, "capture.start")["handler"]
          length <- attr(match, "capture.length")["handler"]
          interpreter <<- substr(script[[1]], start, start + length - 1)
        }
      }
      # if still no interpreter, see if an option make:interpreter is set
      if (nchar(interpreter) == 0) {
        opt = getOption("make:interpreter")
        if (is.character(opt)) interpreter <<- opt
      }
      # the default interpreter is /bin/sh
      if (nchar(interpreter) == 0) {
        interpreter <<- "/bin/sh --"
      }
      system(paste(c(interpreter, script, target, depend), collapse = " ")) == 0
    }
  )
)
