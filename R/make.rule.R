# Recipe represents the recipe for a rule
Recipe <- setRefClass(
  "Recipe",
  methods = list(
    run = function(target, depend) {
    }
  )
)

setClassUnion("RecipeField", members=c("Recipe", "function", "logical"))
setClassUnion("characterOrNULL", members=c("character", "NULL"))

#' parse a target
#' @param target an expression
#' @return a target and its dependences
parseTarget <- function(target, env) {
  vlist <- function(v) {
    res = c()
    for (i in v)
      res <- c(res, eval(as.name(i), envir=env))
    res
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

# makeRule implements a rule that is similar to a Makefile rule
makeRule <- setRefClass(
  "makeRule",
  contains = c("FileHandler"),
  fields = c(
    #' a vector of dependent files
    depend = "list",
    #' the recipe to make the target
    recipe = "RecipeField",
    #' the time stamp for last scan, -Inf if not scanned
    timestamp = "numeric"
  ),
  methods = list(
    #' initializer,
    #' @param target a formula specifying target ~ dependences, the dependences are separated by +, or a target name (string), in which case the dependences are specified by depend.
    #' @param depend the dependences, if target is formula, then depend is appended to the end of the dependences specified in the formula
    #' @param recipe a recipe to make the target, either an R function(target, depend), or a Recipe object, or TRUE (the rule always success), or FALSE (do not know how to make the target). Note that TRUE/FALSE are returned after successfully checked dependences.
    #' @param interpreter f using the first dependent file as a script, this is the interpreter to run the script.
    #' @param replace If TRUE, it replaces the rule to make the same target. If FALSE, and a rule to make the same target exists, it complains and fail.
    initialize = function(target,
                          recipe=scriptRecipe(interpreter=interpreter),
                          depend=list(),
                          interpreter = NULL,
                          replace=FALSE,
                          env = environment()) {
      parsed <- parseTarget(substitute(target), env)
      if (is.list(parsed)) {
        target <- parsed[[1]]
        depend <<- as.list(c(parsed[[2]], depend))
      } else {
        target <- parsed
        depend <<- as.list(depend)
      }
      if (length(target) == 0) {
        stop("Target ", 
             as.character(substitute(target)), 
             ": a target must be specified.", call.=FALSE)
      }
      
      callSuper(pattern = target[[1]])
      recipe <<- recipe
      timestamp <<- -Inf
      maker$add.rule(.self, replace)

      # set up a rule for each target specified
      for (targ in target[-1]) {
        makeRule(target=(targ), recipe=recipe, depend=depend, 
                 interpreter = interpreter,
                 replace, env=environment())
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
          attr(result, "rule") <- makeRule(target = (file),
                                           recipe=recipe,
                                           depend = deps,
                                           interpreter = interpreter,
                                           env = environment())
        }
      }
      result
    }
    ,
    #' make a file
    #' @param file the file to make
    #' @param force force to build the file regardless if it is stale or not.
    #' @return logical. TRUE if successful, and FALSE if do not know how to make it. If the make fails, the function stops with an error.
    make = function(file, force = FALSE) {
      # implicit rules cannot make a file
      if (isImplicit())
        stop("Needs an explicit rule to make a file")
      # get the timestamp of file
      mtime <- file.mtime(file)
      mtime <- if (is.na(mtime)) Inf else as.numeric(mtime)
      if (timestamp < mtime) {
        timestamp <<- if (is.infinite(mtime)) -Inf else mtime
        scan()
      }
      # if force or file does not exist, always build.
      mtime <- -Inf
      making <- attr(file, "making")
      if (is.null(making)) making <- c(file)
      # skip staled automatic dependence
      for (dep in depend) {
        attr(dep, "making") <- making
        result <- maker$make(dep, silent = TRUE)
        dep.mtime <- attr(result, "timestamp")
        # if dep does not exist and no rule matches to make it, then it is the wrong rule.
        if (!result && is.null(dep.mtime)) 
          return(FALSE)
        # if dep.time is NA but make(dep) succeeded, ignore it
        mtime = max(mtime, dep.mtime, na.rm = TRUE)
      }
      if (!is.infinite(timestamp) && timestamp >= mtime) {
        result <- TRUE
      } else if (is.null(recipe) || is.logical(recipe)) {
        result <- if (is.null(recipe)) FALSE else recipe
        timestamp <<- mtime
      } else {
        if (is.function(recipe)) {
          recipe(file, depend)
        } else recipe$run(file, depend)
        result <- TRUE
        timestamp <<- as.numeric(Sys.time())
      }
      attr(result, "timestamp") <- timestamp
      result
    }
    ,
    #' scan for dependences
    scan = function() {
      if (isImplicit())
        stop("Cannot scan an implicit target for depenences")
      # remove the stale dependences
      deps <- list()
      for (dep in depend) {
        dep.time <- attr(dep, "timestamp")
        if (is.null(dep.time)) deps <- c(deps, dep)
      }
      depend <<- deps
      # if file does exists, do not scan
      if (is.infinite(timestamp)) return()
      # scan
      scanner <- scanners$get(pattern)
      if (is.null(scanner)) return()
      addDependences(scanner$scan(pattern))
    }
    ,
    #' add dependences
    #' @param deps the dependences
    #' @param timestamp the timestamp to be added to the deps
    addDependences = function(deps) {
      for (dep in deps)
        if (!(dep %in% depend)) {
          depend <<- c(depend, dep)
          attr(depend[[length(depend)]], "timestamp") <<- timestamp
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
      cat(pattern, "~")
      for (dep in depend) 
        cat("", dep)
      cat("\n")
      cat("recipe = ")
      if (is.logical(recipe)) {
        cat(recipe, "\n")
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
