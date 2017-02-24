# Recipe represents the recipe for a rule
Recipe <- setRefClass(
  "Recipe",
  methods = list(
    run = function(target, depend) {
      FALSE
    }
  )
)

setClassUnion("RecipeField", members=c("Recipe", "function", "logical", "NULL"))

# makeRule implements a rule that is similar to a Makefile rule
makeRule <- setRefClass(
  "makeRule",
  contains = c("FileHandler"),
  fields = c(
    #' a vector of dependent files
    depend = "character",
    #' the recipe to make the target
    recipe = "RecipeField"
  ),
  methods = list(
    #' initializer,
    #' @param target a formula specifying target ~ dependences, the dependences are separated by +, or a target name (string), in which case the dependences are specified by depend.
    #' @param depend the dependences, if target is formula, then depend is appended to the end of the dependences specified in the formula
    #' @param recipe a recipe to make the target, either an R function(target, depend), or a Recipe object, or NULL (the match to target will fail), or TRUE (the rule always success), or FALSE (the rule always fail). Note that NULL/TRUE/FALSE are returned after successfully checked dependences.
    #' @param interpreter f using the first dependent file as a script, this is the interpreter to run the script.
    #' @param replace If TRUE, it replaces the rule to make the same target. If FALSE, and a rule to make the same target exists, it complains and fail.
    initialize = function(target,
                          recipe=scriptRecipe(interpreter=interpreter),
                          depend=c(),
                          interpreter = NULL,
                          replace=FALSE) {
      if (is(target, "formula")) {
        # split by +
        t <- strsplit(as.character(terms(target,
                                         allowDotAsName = TRUE,
                                         keep.order = TRUE)),
                      split="\\+(?=(?:[^`]*`[^`]*`)*[^`]*$)",
                      perl=TRUE)
        t <- sapply(t, function(s) {trimws(gsub("`", "", s))})
        target <- t[[2]][which(t[[2]] != ".")]
        depend <<- c(t[[3]][which(t[[3]] != ".")], depend)
      } else if (!is.character(target)) {
        stop("The target must be either a formula or a string.")
      } else if (length(target) == 0) {
        stop("a target must be specified.")
      } else depend <<- depend

      callSuper(pattern = target[[1]])
      recipe <<- recipe
      maker$add.rule(.self, replace)

      # set up a rule for each target specified
      for (targ in target[-1]) {
        makeRule(target=targ, recipe=recipe, depend=depend, interpreter = interpreter,
                 replace)
      }
    }
    ,
    #' canHandle checks if a rule can handle a specific file.
    #' @param file the file to check
    #' @return a logical indicating whether the file can be handled. In the case that it can be handled, it contains an attributed named "rule". If the rule is implicit, the returned rule is an explicit rule that can handle the file. If the rule is explicit, the returned rule is itself.
    canHandle = function(file) {
      result = callSuper(file)
      if (result) {
        stem <- attr(result, "stem")
        if (is.null(stem)) {
          attr(result, "rule") <- .self
        } else {
          deps <- if (is.null(stem)) depend else sub("%", stem, depend)
          attr(result, "rule") <- makeRule(target = file,
                                           recipe=recipe,
                                           depend = deps,
                                           interpreter = interpreter)
        }
      }
      result
    }
    ,
    #' make a file
    #' @param file the file to make
    #' @param force force to build the file regardless if it is stale or not.
    #' @return TRUE if successful, FALSE is failed, and NULL if do not know how to make it.
    make = function(file, force = FALSE) {
      # implicit rules cannot make a file
      if (isImplicit())
        stop("Needs an explicit rule to make a file")
      # match file to targets, which can contain a stem, e.g., dir/%.c
      target.info = file.info(file)
      target.exists = !is.na(target.info$mtime)
      # if force or file does not exist, always build.
      old = !target.exists || force
      for (dep in depend) {
        result = maker$make(dep, silent = TRUE)
        # if dep does not exist and no rule matches to make it, then it is the wrong rule.
        if (is.null(result) && !file.exists(dep)) {
          return(NULL)
        }
        depend.info = file.info(dep)
        if (target.exists)
          old = old || (depend.info$mtime > target.info$mtime)
      }
      if (!old) {
        TRUE
      } else if (is.null(recipe)) {
        NULL
      } else if (is.logical(recipe)) {
        recipe
      } else if (is.function(recipe)) {
        recipe(file, depend)
      } else {
        recipe$run(file, depend)
      }
    }
    ,
    #' whether the rule is implicit or not
    isImplicit = function() {
      grepl("%", pattern)
    }
    ,
    #' pretty print a makeRule object
    show = function() {
      cat(pattern, "~", depend, "\n")
      cat("recipe = ")
      if (is.null(recipe)) {
        cat("NULL\n")
      } else methods::show(recipe)
    }
  )
)

#' match file to patterns, which can contain a stem, e.g., dir/%.c
#' @param pattern the pattern (or a list of vector of them) to match to
#' @param file the file name
#' @return logical indicating if file matches pattern, optionally with an attribute named "stem", returning the part of file matching the %, if the matching pattern contains a %.
match.stem = function(pattern, file) {
  # if pattern is a vector (or a list), match to each
  if (length(pattern) > 1) {
    for (pat in pattern) {
      result = (match.stem(pat, file))
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
    file.abs = normalizePath(file, mustWork = FALSE)
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
