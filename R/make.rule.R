# Recipe represents the recipe for a rule
Recipe <- setRefClass(
  "Recipe",
  methods = list(
    run = function(target, depend) {
      FALSE
    }
  )
)

setClassUnion("RecipeField", members=c("Recipe", "function", "NULL"))

# makeRule implements a rule that is similar to a Makefile rule
makeRule <- setRefClass("makeRule",
  contains = c("Rule"),
  fields = c(
    #' the target to make
    target = "character",
    #' a vector of dependent files
    depend = "character",
    #' the recipe to make the target
    recipe = "RecipeField"
  ),
  methods = list(
    #' initializer,
    #' @param relation a formula specifying target ~ dependences, the dependences are separated by +
    #' @param recipe a recipe to make the target, either an R function(target, depend), or a Recipe object, or NULL (use the first dependent file as a script to make the target)
    #' @param interpreter f using the first dependent file as a script, this is the interpreter to run the script.
    #' @param ... parameters to pass to Rule, like first.rule or replace.
    initialize = function(relation=NULL, recipe=NULL,
                          .target=NULL, .depend=c(),
                          interpreter = "", ...) {
      if (is.null(relation) && is.null(target)) {
        stop("Either relation or target must be specified")
      }
      if (is.null(.target)) {
        if (!inherits(relation, "formula"))
          stop("relation must be a formula")
        # split by +
        t <- strsplit(as.character(terms(relation,
                                         allowDotAsName = TRUE,
                                         keep.order = TRUE)),
                      split="\\+(?=(?:[^`]*`[^`]*`)*[^`]*$)",
                      perl=TRUE)
        t <- sapply(t, function(s) {trimws(gsub("`", "", s))})
        .target <- t[[2]][which(t[[2]] != ".")]
        .depend <- c(t[[3]][which(t[[3]] != ".")], .depend)
      }
      if (length(.target) == 0)
        stop("a target must be specified.")

      target <<- .target[[1]]
      depend <<- .depend
      recipe <<- if (is.null(recipe) && length(depend) > 0) {
        scriptRecipe(script = depend[[1]], interpreter = interpreter)
      } else recipe
      callSuper(...)

      # set up a rule for each target specified
      for (targ in .target[-1]) {
        makeRule(recipe=recipe, .target=targ, .depend=.depend, interpreter = interpreter, ...)
      }
    }
    ,
    #' returns the target, overrides the method in the parent (Rule) class.
    getTarget = function() {
      target
    }
    ,
    #' make a file
    #' @param file the file to make
    #' @param force force to build the file regardless if it is stale or not.
    #' @return TRUE if successful, FALSE is failed, and NULL if do not know how to make it.
    make = function(file, force = FALSE) {
      # match file to targets, which can contain a stem, e.g., dir/%.c
      stem <- NULL
      matched <- FALSE
      for (pattern in target) {
        match <- match.stem(pattern, file)
        stem <- match$stem
        matched <- match$match
        if (matched) break
      }
      if (!matched) return(NULL)
      # if stem is not NULL, then this pattern is an implict rule
      # we create a specific rule for this file.
      if (!is.null(stem)) {
        deps <- sub("%", stem, depend)
        rule <- makeRule(recipe=recipe, .target=file, .depend=deps,
                         replace=TRUE, first.rule=TRUE)
        return(rule$make(file, force))
      }
      target.info = file.info(file)
      target.exists = length(which(!is.na(target.info))) > 0
      # if force or file does not exist, always build.
      old = !target.exists || force
      for (dep in depend) {
        maker$make(dep)
        depend.info = file.info(dep)
        if (target.exists)
          old = old || (depend.info$mtime > target.info$mtime)
      }
      if (!old) {
        TRUE
      } else if (is.null(recipe)) {
        FALSE
      } else if (is.function(recipe)) {
        recipe(file, depend)
      } else {
        recipe$run(file, depend)
      }
    }
    ,
    #' pretty print a makeRule object
    show = function() {
      cat("~", depend, "\n")
      cat("recipe = ")
      if (is.null(recipe)) {
        cat("NULL\n")
      } else methods::show(recipe)
    }
  )
)

#' match file to a pattern, which can contain a stem, e.g., dir/%.c
#' @param pattern the pattern to match to
#' @param file the file name
#' @return a list, $match denoting if file matches pattern, and $stem is NULL if pattern does not contain %, or a string for the stem (matched part of %).
match.stem = function(pattern, file) {
  # if the pattern is not an implicit rule, i.e., does not contain %, then
  # we simply compare the canonical paths of pattern and file
  if (!grepl("%", pattern)) {
    pattern <- normalizePath(pattern, mustWork = FALSE)
    file <- normalizePath(file, mustWork = FALSE)
    return(list(match=pattern == file, stem = NULL))
  }
  # if we read here, then the pattern contains %, and is thus an implicit rule
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
    # if file does not match, check if the canonical path of file matches.
    file.abs = normalizePath(file, mustWork = FALSE)
    if (file.abs != file)
      return(match.stem(pattern, file.abs))
    stem <- NULL
  } else {
    if (is.null(n2)) {
      stem = substring(file, n1+1)
    } else stem = substr(file, n1 + 1, n2 - 1)
  }
  list(match=match, stem=stem)
}
