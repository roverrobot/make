#' Recipe is the root class for all recipes for making a file
Recipe <- R6::R6Class(
  "Recipe",
  public = list(
    #' run is the main function of a recipe for making a target file from a list of dependences
    #' @param target the file to make
    #' @param depend the list of dependences
    #' @return this function has no return value. It should throw an error and stop if it fails to make the target.
    run = function(target, depend) {
    }
  )
)

#' parse a target
#' @param target an expression
#' @return a target and its dependences
parseTarget <- function(target, env) {
  # vlist evaluates a list of variables in v, 
  vlist <- function(v) {
    sapply(v, function(var) {eval(as.name(var), envir=env)})
  }
  l <- as.list(target)
  switch (
    class(target),
    call = {
      first <- parseTarget(l[[2]], env)
      if (length(l) > 2) second <- parseTarget(l[[3]], env)
      switch (
        as.character(l[[1]]),
        "~" = return(list(target=first, depend=second)),
        "+" = return(c(first, second)),
        "/" = return(paste(c(first, second), collapse = .Platform$file.sep)),
        ":" = return(paste(first, second, sep = ":"))
      )
    },
    name = return(if (l[[1]]==".") c() else as.character(l[[1]])),
    character = return(as.character(l[[1]])),
    "(" = return(vlist(parseTarget(l[[2]], env)))
  )
}

#' the base class for make rules
#' It is the root class for both a node and an implicit rule
#' It has the notion of a dependence list, a recipeID, and a timestamp
MakeRule <- R6::R6Class(
  "MakeRule",
  private = list(
    #' a vector of dependent files
    depend = c(),
    #' the recipeID for the recipe to make the target
    recipeID = NULL
  ),
  active = list(
    #' the dependences
    dependences = function() { private$depend },
    #' the recipeID
    recipe = function() { private$recipeID }
  ),
  public = list(
    #' the time stamp for last scan, NULL if not scanned
    timestamp = NULL,
    #' initializer,
    #' @param depend the dependences, 
    #' @param recipeID a unique recipe ID.
    initialize = function(depend, recipeID) {
      private$depend <- unique(depend)
      if (!is.null(recipeID) && !is.character(recipeID))
        stop("Invalid recipeID ", recipeID, call. = FALSE)
      # for each target, set up a rule
      private$recipeID <- recipeID
      self$timestamp <- NULL
    },
    #' pretty print a makeRule object
    print = function() {
      deps <- self$dependences
      if (length(deps) > 0) {
        cat(deps[[1]])
        for (dep in deps[-1]) cat(" +", dep)
      }
      cat("\n")
    }
  )
)

#' a node in the dependence graph
Node <- R6::R6Class(
  "Node",
  inherit = MakeRule,
  active = list(
    #' the dependences
    dependences = function() { unique(c(private$depend, self$auto)) }
  ),
  public = list(
    #' a vector of automatic dependences
    auto = c()
  )
)

#' Represents an imlicit rule
ImplicitRule <- R6::R6Class(
  "ImplicitRule",
  inherit = MakeRule,
  private = list(
    # the pattern for handlable target files
    pattern = NULL
  ),
  public = list(
    #' initializer
    #' @param pattern the pattern for the target files
    #' @param depend the patterns for dependences
    #' @param recipeID the recipe ID to generate a target
    initialize = function(pattern, depend, recipeID) {
      super$initialize(depend, recipeID)
      private$pattern <- pattern
    },
    #' match to a file
    #' @param file the file for which to create the rule
    #' @return a Node object or NULL if cannot handle the file
    match = function(file) {
      matched <- match.stem(private$pattern, file)
      if (!matched) return(NULL)
      stem <- attr(matched, "stem")
      deps <- sub("%", stem, private$depend)
      Node$new(deps, private$recipeID)
    },
    #' pretty print a makeRule object
    print = function() {
      cat(private$pattern, "~ ")
      super$print()
    }
  )
)
#' create a make rule
#' @param target a formula specifying target ~ dependences, the dependences are separated by +, or a target name (string), in which case there is no dependence.
#' @param depend the dependences, if target is formula, then depend is appended to the end of the dependences specified in the formula
#' @param recipe a recipe to make the target, either an R function(target, depend), or a Recipe object, or NULL (the rule makes nothing and always success, after successfully checked dependences).
#' @param interpreter f using the first dependent file as a script, this is the interpreter to run the script.
#' @return a MakeRule R6 object
#' @export
makeRule <- function(target,
                     depend = c(),
                     recipe=scriptRecipe$new(interpreter),
                     interpreter = NULL,
                     env = parent.frame()) {
  # parse the target
  parsed <- parseTarget(substitute(target), env)
  if (is.list(parsed)) {
    target <- parsed$target
    depend <- c(parsed$depend, depend)
  } else {
    target <- parsed
  }
  pkg.env$maker$addRule(target, depend, recipe)
}

#' match file to patterns, which can contain a stem, e.g., dir/\%.c
#' @param pattern the pattern (or a list of vector of them) to match to
#' @param file the file name
#' @return logical indicating if file matches pattern, optionally with an attribute named "stem", returning the part of file matching the \%, if the matching pattern contains a \%.
match.stem = function(pattern, file) {
  # if pattern is a vector (or a list), match to each
  if (length(pattern) > 1) {
    for (pat in pattern) {
      result = match.stem(pat, file)
      if (result) return(result)
    }
    return (FALSE)
  }
  # if we reach here, pattern is a single pattern.
  # if the pattern is not an implicit rule, i.e., does not contain %, then
  # we simply compare the canonical paths of pattern and file
  if (!grepl("%", pattern)) {
    pattern <- normalizePath(pattern, mustWork = FALSE)
    file <- normalizePath(file, mustWork = FALSE)
    return (pattern == file)
  }
  # if we read here, then the pattern contains %, and is thus an implicit rule
  parts = strsplit(pattern, "%")[[1]]
  if (length(parts) > 2)
    warning("pattern ", pattern, " has an invalid pattern in the target.",
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
    # if file does not match, check if the canonical path of file matches.
    file.abs = normalizePath(file)
    if (file.abs != file)
      return(match.stem(pattern, file.abs))
    return (FALSE)
  }
  if (is.null(n2)) {
    stem = substring(file, n1+1)
  } else stem = substr(file, n1 + 1, n2 - 1)
  attr(match, "stem") = stem
  match
}
