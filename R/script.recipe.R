#' this class defines an interpreter to run a type of script (distinguished by extensions)
Interpreter <- R6::R6Class(
  "Interpreter",
  inherit = FileHandler,
  public = list(
    #' the command to runa script
    command = "",
    #' the method for making the target from a vector of dependences
    #' @param script the script to run
    #' @param target the target file
    #' @param depend the vector of dependences, the first file in depend is the script name
    run = function(script, target, depend) {
      # if the interpreter is not specified, check if it is specified in the script first
      com <- self$command
      if (nchar(com) == 0) {
        opt <- getOption("make:interpreter")
        if (is.character(opt)) com <- opt
      }
      # the default interpreter is /bin/sh
      if (nchar(com) == 0) {
        com <- "/bin/sh --"
      }
      com <- paste(c(com, script, target, depend), collapse = " ")
      if (system(com) != 0)
        stop("Failed to run: ", com, call.=FALSE)
    },
    #' initializer
    #' @param pattern a list or a vector of patterns that this interpreter can run
    #' @param command the command to run a given script
    #' @param register whether toautomatically add to the interpreter manager
    initialize = function(pattern, command = "", register=TRUE) {
      self$pattern <- pattern
      self$command <- command
      if (register) interpreters$add(self)
    }
  )
)

interpreters <- Manager$new(class="Interpreter")
Interpreter$new("%", "") # the default one
Interpreter$new("%.py", "python")
Interpreter$new("%.pl", "perl")
if (system("which matlab") == 0) {
  Interpreter$new("%.m", "matlab")
} else if (system("which octave") == 0) {
  Interpreter$new("%.m", "octave")
}

#' this is a Recipe that makes a target file by running a script,
#' interpreted by an interpreter.
#' @include make.rule.R
scriptRecipe <- R6::R6Class(
  "scriptRecipe",
  inherit = Recipe,
  public = list(
    #' the interpreter used to run the script.
    interpreter = NULL,
    #' the method for making the target from a vector of dependences
    #' @param target the target file
    #' @param depend the vector of dependences, the first file in depend is the script name
    run = function(target, depend) {
      # the script is the first dependent file
      if (length(depend) == 0)
        stop("In making ", 
             target, 
             ": a script must be specified as the first dependent file.", 
             call.=FALSE)
      script <- depend[[1]]
      if (!file.exists(script))
        stop("The recipe script ", script, " does not exist.", call.=FALSE)
      # check for interpreter
      run <- self$interpreter
      # if the interpreter is not specified, check if it is specified in the script first
      if (is.null(run)) {
        first.line = readLines(script, n=1)
        match <- regexpr("^#!\\s*(?'handler'.*?)\\s*(\\n|$)", first.line[[1]], perl=TRUE)
        if (match > 0) {
          start <- attr(match, "capture.start")["handler"]
          length <- attr(match, "capture.length")["handler"]
          run <- Interpreter("", substr(script[[1]], start, start + length - 1), FALSE)
        }
      }
      # if still not specified, check for the list of known interpreters
      if (is.null(run))
        run <- interpreters$get(script)
      if (is.null(run))
        stop("Do not know how to interpret ", script, ".", call.=FALSE)
      run$run(script, target, depend)
    }
    ,
    #' pretty print a scriptRecipe object
    show = function() {
      cat("scriptRecipe")
      if (!is.null(self$interpreter)) {
        cat(" interpretered by: ")
        show(interpreter)
      }
      cat("\n")
    }
    ,
    #' initializer
    initialize = function(interpreter = NULL) {
      self$interpreter <- interpreter
    }
  )
)

#' the interpreter for R scripts
RInterpreter <- R6::R6Class(
  "RInterpreter",
  inherit = Interpreter,
  public = list(
    #' initializer
    #' @param pattern a list or a vector of patterns that this interpreter can run
    #' @param register whether toautomatically add to the interpreter manager
    initialize = function(pattern = c("%.R", "%.r"), register = TRUE) {
      super$initialize(pattern = pattern, command = "", register = register)
    },
    #' the method for making the target from a vector of dependences
    #' @param script the script to run
    #' @param target the target file
    #' @param depend the vector of dependences, the first file in depend is the script name
    run = function(script, target, depend) {
      commandArgs <- function(trailingOnly = FALSE) {
        v <- c(target, depend)
        args = sapply(v, identity)
        if (trailingOnly) args else c(script, args)
      }
      `<<-` <- function(name, x) {
        var <- as.character(substitute(name))
        env <- parent.frame()
        env[[var]] <- NULL
        do.call(base::`<<-`, list(var, x))
      }
      con <- connection.base$hooks[["base:file"]]$saved(script, "r")
      tracker$push()
      tryCatch(source(con, local=TRUE), finally=close(con))
      deps <- tracker$pop()
      if (length(deps) > 0) {
        rule <- maker$explicit.rules[[script]]
        if (is.null(rule)) {
          rule <- MakeRule(script, recipe=NULL)
        }
        rule$addDependences(deps)
      }
    }
  )
)

RInterpreter$new()
