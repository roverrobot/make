# Recipe represents the recipe for a rule
Recipe <- setClass("Recipe",
  slots  = c(
    name = "character",
    script = "character",
    interpreter = "character"
  ),
  prototype = list(
    name = "",
    script = "",
    interpreter = "/bin/sh"
  )
)

setGeneric(name="run",
  def = function(.Object, ...) {
   standardGeneric("run")
  }
)

setMethod("run",
  signature = c("Recipe"),
  definition = function(.Object, ..., target, depends) {
    exec <- function(interpreter) {
      f = pipe(interpreter, open="w")
      cat(.Object@script, sep = "\n", file=f)
      result = close(f)
      is.na(result) || result == 0
    }
    match <- regexpr("^#!\\s*(?'handler'.*?)\\s*(\\n|$)", .Object@script[[1]], perl=TRUE)
    if (match == 1) {
      start <- attr(match, "capture.start")["handler"]
      length <- attr(match, "capture.length")["handler"]
      interpreter <- substr(.Object@script[[1]], start, start + length - 1)
    } else interpreter <- .Object@interpreter
    result <- tryCatch(exec(interpreter))
    result
  }
)
