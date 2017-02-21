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
    initialize = function(..., script, interpreter="/bin/sh") {
      callSuper(..., recipe= scriptRecipe(script, interpreter))
      if (!containsFile(depend, script))
        depend <<- c(depend, script)
    }
  )
)

scriptRecipe <- setRefClass(
  "scriptRecipe",
  contains = c("Recipe"),
  fields = c(
    script.file = "character"
  ),
  methods = list(
    initialize = function(script.file, interpreter="/bin/sh") {
      .self$script.file <<- script.file
      callSuper(script="", interpreter=interpreter)
    }
    ,
    run = function(target, depend) {
      if (file.exists(script.file)) {
        script <<- readLines(script.file)
      } else stop("The script file ", script, " does not exist")
      result = callSuper(target, depend)
      script <<- ""
      result
    }
  )
)
