# makeRule implements a rule that is similar to a Makefile rule

setClassUnion("RecipeOrNULL", members=c("Recipe", "NULL"))

makeRule <- setClass("makeRule",
  contains = c("Rule"),
  slots = c(
    target = "character",
    depend = "list",
    recipe = "RecipeOrNULL"
  ),
  prototype = list(
    target = "",
    depend = list(),
    recipe = NULL
  )
)

#' match file to a pattern, which can contain a stem, e.g., dir/%.c
#' @param pattern the pattern to match to
#' @param file the file name
#' @return a string for the stem, if matched, or NULL is not matched
match.stem = function(pattern, file) {
  parts = strsplit(pattern, "%")[[1]]
  if (length(parts) > 2)
    warning("Rule ", .Object@rule, " has an invalid pattern in the target.",
            "Only a single % is allowed.")
  if (parts[[1]] != "") {
    n1 = nchar(parts[[1]])
    match = substr(file, 1, n1) == parts[[1]]
  } else {
    n1 = 0
    match = TRUE
  }
  if (match) {
    if (length(parts) > 1) {
      n2 = nchar(file) + 1 - nchar(parts[[2]])
      match = substring(file, n2) == parts[[2]]
    } else n2 = NULL
  }
  if (!match) {
    file.abs = normalizePath(file, mustWork = FALSE)
    if (file.abs != file) {
      stem = match.stem(pattern, file.abs)
    } else stem = NULL
  } else {
    if (is.null(n2)) {
      stem = substring(file, n1+1)
    } else stem = substr(file, n1 + 1, n2 - 1)
  }
  stem
}

setMethod(initialize,
    signature <- c("makeRule"),
    definition <- function(.Object, ..., target="", depend="", recipe=NULL) {
      .Object@target <- target
      .Object@depend <- depend
      .Object@recipe <- recipe
      callNextMethod(.Object, ...)
    }
)

setMethod(make,
  signature = c("makeRule"),
  definition = function(.Object, file, force = FALSE) {
    # match file to targets, which can contain a stem, e.g., dir/%.c
    patterns = strsplit(.Object@target, "\\s")[[1]]
    stem = NULL
    for (pattern in patterns) {
      stem = match.stem(pattern, file)
      if (!is.null(stem)) break
    }
    if (is.null(stem)) return(NULL)
    depends = sub("%", stem, .Object@depend)
    if (length(depends) == 0) {
      old = TRUE
    } else {
      old = FALSE
      for (dep in depends) {
        maker$make(dep)
        old = old || stale(file, dep)
      }
    }
    if (!old) return(TRUE)
    run(.Object@recipe, file, depends)
  }
)
