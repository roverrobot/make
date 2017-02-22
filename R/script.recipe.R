#' this is a Recipe that makes a target file by running a script,
#' interpreted by an interpreter.
scriptRecipe <- setRefClass(
  "scriptRecipe",
  contains = c("Recipe"),
  fields = c(
    #' the interpreter used to run the script.
    interpreter = "character"
  ),
  methods = list(
    #' the method for making the target from a vector of dependences
    #' @param target the target file
    #' @param depend the vector of dependences, the first file in depend is the script name
    run = function(target, depend) {
      # the script is the first dependent file
      if (length(depend) == 0)
        stop("A script must be specified as the first dependent file.")
      script <- depend[[1]]
      if (!file.exists(script))
        stop("The recipe script ", script, " does not exist.")
      # check for interpreter
      run <- interpreter
      # if the interpreter is not specified, check if it is specified in the script first
      if (nchar(run) == 0) {
        first.line = readLines(script, n=1)
        match <- regexpr("^#!\\s*(?'handler'.*?)\\s*(\\n|$)", first.line[[1]], perl=TRUE)
        if (match > 0) {
          start <- attr(match, "capture.start")["handler"]
          length <- attr(match, "capture.length")["handler"]
          run <- substr(script[[1]], start, start + length - 1)
        }
      }
      # if still not specified, check for thelist of known interpreters
      if (nchar(run) == 0)
        run <- interpreters$get(script)
      # if still not specified, check for a global option
      if (nchar(run) == 0) {
        opt = getOption("make:interpreter")
        if (is.character(opt)) run <- opt
      }
      # the default interpreter is /bin/sh
      if (nchar(run) == 0) {
        "/bin/sh --"
      }
      system(paste(c(run, script, target, depend), collapse = " ")) == 0
    }
    ,
    #' pretty print a scriptRecipe object
    show = function() {
      cat("scriptRecipe")
      if (nchar(interpreter) > 0) cat(" interpretered by:", interpreter, "\n")
      cat("\n")
    }
  )
)

Interpreters <- setRefClass(
  "Interpreters",
  fields = c(interpreters = "list"),
  methods = list(
    get = function(script) {
      ext = tail(strsplit(script, "\\.")[[1]], n=1)
      interpreters[[tolower(ext)]]
    }
    ,
    set = function(ext, value) {
      interpreters[[tolower(ext)]] <<- value
    }
    ,
    initialize = function() {
      interpreters <<- list(
        "r" = "Rscript --vanilla",
        "sh" = "sh --",
        "py" = "python",
        "pl" = "perl",
        "m" = "matlab"
      )
    }
  )
)

interpreters = Interpreters()

#' find an interpreter for a script
#' @param script the script
getInterpreter <- function(script) {
  interpreters$get(script)
}

#' set an interpreter for an extension
#' @param ext the file extension
setInterpreter <- function(ext, value) {
  interpreters$set(ext, value)
}
