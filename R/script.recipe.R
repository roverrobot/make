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
    #' the method for making the target from a vector of dependences
    #' @param target the target file
    #' @param depend the vector of dependences
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
      system(paste(c(getInterpreter(), script, target, depend), collapse = " ")) == 0
    }
    ,
    #' find the proper interpreter to run the script
    getInterpreter = function() {
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
      cat(getInterpreter(), script, "\n")
    }
  )
)
