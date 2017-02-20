# makeRule implements a rule that is similar to a Makefile rule

setClassUnion("RecipeOrNULL", members=c("Recipe", "NULL"))

makeRule <- setRefClass("makeRule",
  contains = c("Rule"),
  fields = c(
    target = "character",
    depend = "list",
    recipe = "RecipeOrNULL"
  ),
  methods = list(
    initialize = function(target="", depend="", recipe=NULL, ...) {
      callSuper(...)
      .self$target <<- target
      .self$depend <<- depend
      .self$recipe <<- recipe
    }
    ,
    make = function(file, force = FALSE) {
      # match file to targets, which can contain a stem, e.g., dir/%.c
      patterns = strsplit(target, "\\s")[[1]]
      stem = NULL
      for (pattern in patterns) {
        stem = match.stem(pattern, file)
        if (!is.null(stem)) break
      }
      if (is.null(stem)) return(NULL)
      deps = sub("%", stem, depend)
      if (stem != "") {
        rule <- makeRule(file, deps, recipe)
        return(rule$make(file, force))
      }
      old = FALSE
      # if a file depends on nothing, it does not need to be rebuilt
      if (length(deps) > 0) {
        for (dep in deps) {
          maker$make(dep)
          old = old || stale(file, dep)
        }
      }
      # check if target does not exist, always build
      target.info = file.info(file)
      if (length(which(!is.na(target.info$size))) == 0)
        old = TRUE
      # if force, always build.
      if (force) old = TRUE
      if (!old) return(TRUE)
      run(recipe, file, depends)
    }
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
