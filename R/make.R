#' the Maker class is responsible for making a file.
Maker <- setRefClass(
  "Maker",
  fields = c(
    #' the list of explicit make rules (i.e., the pattern of a rule does not contain %)
    explicit.rules = "list",
    #' the list of explicit make rules (i.e., the pattern of a rule does not contain %)
    implicit.rules = "list",
    #' the directory that this maker is responsible for, i.e., the current working directory when the maker is created.
    dir = "character"
  ),
  methods = list(
    #' make a file
    #' @param file the file to make
    #' @param force force to build the file regardless if it is stale or not.
    #' @param silent In the case that no rule matches, complain and stop if TRUE, or silently return if FALSE. Still complains and stop if a rule matches but failed to make the file.
    #' @return logical. TRUE if successful, and FALSE if do not know how to make it. If the make fails, the function stops with an error.
    make = function(file, force=FALSE, silent = FALSE) {
      # if asked to make a list of files, make them one by one
      if (length(file) > 1) {
        for (f in file)
          result <- make(f, foruce, silent)
        return(result)
      }
      # only make files in the dir
      abs <- normalizePath(file, mustWork = FALSE)
      if (isAbsolutePath(abs) && substr(abs, 1, nchar(dir)) != dir)
        return (FALSE)
      # check for circular dependence
      making <- attr(file, "making")
      if (is.null(making)) making <- c()
      if (file %in% making) {
        stop("circular dependences: ", making, " ", file, call.=FALSE)
      }
      attr(file, "making") <- c(making, file)
      # search for an explicit rule for file
      rule <- explicit.rules[[file]]
      # if not found, search for an implicit rule
      if (is.null(rule)) {
        for (r in implicit.rules) {
          result = r$canHandle(file)
          if (result) {
            rule <- attr(result, "rule")
            break
          }
        }
      }
      # see if we can find a scanner for file
      if (is.null(rule)) {
        scanner <- scanners$get(file)
        if (!is.null(scanner)) {
          rule <- makeRule(target=(file), recipe=FALSE, env=environment())
        }
      }
      # make
      if (!is.null(rule)) {
        result = NULL
        tryCatch(result <- rule$make(file, force),
                 finally = if (is.null(result) && file.exists(file)) file.remove(file))
        if (is.null(result)) stop("failed to make ", file, call.=FALSE)
      } else {
        result <- FALSE
        mtime <- file.mtime(file)
        if (!is.na(mtime)) attr(result, "timestamp") <- as.numeric(mtime)
      }
      if (!result) {
        if (!is.null(attr(result, "timestamp")) || silent) 
          return (result)
        stop("do not know how to make file: ", file, call. = FALSE)
      }
      result
    }
    ,
    #' add a rule to the list of rules
    #' @param rule the rule to add
    #' @param replace If TRUE, it replaces the rule to make the same target. If FALSE, and a rule to make the same target exists, it complains and fail.
    add.rule = function(rule, replace=FALSE) {
      if (!is(rule, "makeRule"))
        stop("A rule must be an object of the class Rule.", call.=FALSE)
      name <- rule$pattern
      if (is.null(name) || length(name) == 0)
        stop("A rule must have a target", call.=FALSE)
      if (rule$isImplicit()) {
        implicit.rules <<- c(implicit.rules, rule)
      } else if (is.null(explicit.rules[[name]]) || replace) {
        explicit.rules[[name]] <<- rule
      } else stop("A rule for a target ", name, " already exits.", call.=FALSE)
    }
    , 
    #' clear the rules
    clear = function() {
      explicit.rules <<- list()
      implicit.rules <<- list()
    }
  )
)

maker = Maker()

#' return the list of rules
getRules <- function () {
  list(explicit=maker$explicit.rules, implicit=maker$implicit.rules)
}

#' clear the list of rules and load from Makefile.R
resetRules <- function() {
  maker$dir <- file.path(getwd(), "")
  maker$clear()
  if (file.exists("Makefile.R"))
    try(source("Makefile.R"))
}

#' tracks the files being automatically opened.
MakeTracker <- setRefClass(
  "MakeTracker",
  fields = c(
    #' a stack of trackers
    listeners = "list"),
  methods = list(
    #' adding the current file to the top tracker
    #' @param file the tracked file
    track = function(file) {
      if (length(listeners) != 0)
        listeners[[1]] <<- c(listeners[[1]], file)
    },
    #' starts a new tracker
    push = function() {
      listeners <<- c(list(c()), listeners)
    },
    #' finish tracking and pop off the stack
    #' @return the files being used 
    pop = function() {
      l <- listeners[[1]]
      listeners <<- listeners[-1]
      l
    }
  )
)

tracker <- MakeTracker()

#' make a file
#' @param file the file to make
#' @param force force to build the file regardless if it is stale or not.
#' @return TRUE if successful, FALSE is failed, and NULL if do not know how to make it.
make <- function(file, force=FALSE) {
  if (length(maker$dir) == 0) return (FALSE)
  tracker$track(file)
  result <- maker$make(file)
  attr(result, "timestamp") <- NULL
  result
}
