#' this is a Recipe that makes a target file by running a script,
#' interpreted by an interpreter.
scriptRecipe <- setRefClass(
  "scriptRecipe",
  contains = c("Recipe"),
  fields = c(
    #' the script file name
    script = "character",
    #' the interpreter used to run the script.
    interpreter = "character"
  ),
  methods = list(
    #' initializer
    #' @param script the script name
    #' @param interpreter the interpreter to run the script
    initialize = function(script, interpreter = "") {
      script <<- script
      if (nchar(interpreter) == 0) {
        first.line = readLines(script, n=1)
        match <- regexpr("^#!\\s*(?'handler'.*?)\\s*(\\n|$)", first.line[[1]], perl=TRUE)
        if (match > 0) {
          start <- attr(match, "capture.start")["handler"]
          length <- attr(match, "capture.length")["handler"]
          interpreter <<- substr(script[[1]], start, start + length - 1)
        } else {
          interpreter <<- getInterpreter(script)
        }
      }
    }
    ,
    #' the method for making the target from a vector of dependences
    #' @param target the target file
    #' @param depend the vector of dependences
    run = function(target, depend) {
      if (!file.exists(script))
        stop("The recipe script ", script, " does not exist.")
      system(paste(c(get.interpreter(), script, target, depend), collapse = " ")) == 0
    }
    ,
    #' find the proper interpreter to run the script
    get.interpreter = function() {
      run <- interpreter
      if (nchar(run) == 0) {
        opt = getOption("make:interpreter")
        if (is.character(opt)) run <- opt
      }
      # the default interpreter is /bin/sh
      if (nchar(run) == 0) {
        "/bin/sh --"
      } else run
    }
    ,
    #' pretty print a scriptRecipe object
    show = function() {
      cat(get.interpreter(), script, "\n")
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
