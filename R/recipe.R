# Recipe represents the recipe for a rule
Recipe <- setRefClass(
  "Recipe",
  fields  = c(
    script = "character",
    interpreter = "character"
  ),
  methods = list(
    initialize = function(script, interpreter = "") {
      .self$script <<- script
      .self$interpreter <<- interpreter
      match <- regexpr("^#!\\s*(?'handler'.*?)\\s*(\\n|$)", script[[1]], perl=TRUE)
      if (match == 1) {
        start <- attr(match, "capture.start")["handler"]
        length <- attr(match, "capture.length")["handler"]
        if (nchar(interpreter) == 0)
          .self$interpreter <<- substr(script[[1]], start, start + length - 1)
      }
      if (nchar(.self$interpreter) == 0)
        .self$interpreter <<- "/bin/sh"
    }
    ,
    run = function(target, depend) {
      exec = function() {
        f <- pipe(interpreter, open="w")
        cat(script, sep = "\n", file=f)
        result <- close(f)
        is.na(result) || result == 0
      }
      # check if a handler is specified in the first line of the script
      try(exec())
    }
  )
)
