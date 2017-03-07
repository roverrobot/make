#' the Maker class is responsible for making a file.
#' @include file.handler.R
Maker <- R6::R6Class(
  "Maker",
  inherit = FileHandler, 
  private = list(
    #' the list of explicit make rules (i.e., the pattern of a rule does not contain %)
    explicit.rules = list(),
    #' the list of explicit make rules (i.e., the pattern of a rule does not contain %)
    implicit.rules = list()
  ),
  public = list(
    #' returns a rule for handling a file
    #' @param file the file to make
    #' @return a MakeRule object, or NULL if do not know how to make it.
    ruleForFile = function(file) {
      # search for an explicit rule for file
      rule <- private$explicit.rules[[file]]
      # if not found, search for an implicit rule
      if (is.null(rule)) {
        for (r in private$implicit.rules) {
          result = r$canHandle(file)
          if (result) {
            rule <- attr(result, "rule")
            break
          }
        }
      }
      # see if we can find a scanner for file
      if (is.null(rule) && file.exists(file)) {
        scanner <- scanners$get(file)
        if (!is.null(scanner)) {
          rule <- MakeRule$new(target=file, recipe=NULL)
        }
      }
      rule
    }
    ,
    #' check if the file can be handled by this maker
    #' 
    #' it checks if the file is in the dir (self$pattern)
    #' @param file the file to check
    canHandle = function(file) {
      # only make files in the dir
      abs <- normalizePath(file, mustWork = FALSE)
      dir <- self$pattern
      return (!isAbsolutePath(abs) || substr(abs, 1, nchar(dir)) == dir)
    }
    ,
    #' make a file
    #' @param file the file to make
    #' @param force force to build the file regardless if it is stale or not.
    #' @param silent In the case that no rule matches, complain and stop if TRUE, or silently return if FALSE. Still complains and stop if a rule matches but failed to make the file.
    #' @return logical. TRUE if successful, and FALSE if do not know how to make it. If the make fails, the function stops with an error.
    make = function(file, force=FALSE, silent = FALSE) {
      # if asked to make a list of files, make them one by one
      if (length(file) > 1) {
        for (f in file) {
          result <- make(f, foruce, silent)
          if (!result) break
        }
        return(result)
      }
      # check for circular dependence
      making <- attr(file, "making")
      if (is.null(making)) making <- c()
      if (file %in% making) {
        stop("circular dependences: ", making, " ", file, call.=FALSE)
      }
      attr(file, "making") <- c(making, file)
      # only make files in the dir
      rule <- if (self$canHandle(file)) self$ruleForFile(file) else NULL
      # make
      if (!is.null(rule)) {
        result = NULL
        result <- rule$make(file, force)
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
      if (!is(rule, "MakeRule"))
        stop("A rule must be an object of the class MakeRule.", call.=FALSE)
      name <- rule$pattern
      if (is.null(name) || length(name) == 0)
        stop("A rule must have a target", call.=FALSE)
      if (rule$isImplicit()) {
        private$implicit.rules <- c(private$implicit.rules, rule)
      } else if (is.null(private$explicit.rules[[name]]) || replace) {
        private$explicit.rules[[name]] <- rule
      } else stop("A rule for a target ", name, " already exits.", call.=FALSE)
    }
    , 
    #' clear the rules
    clear = function() {
      private$explicit.rules <- list()
      private$implicit.rules <- list()
    }
    ,
    #' initializer
    #' @param dir the directory that this maker managers
    initialize = function(dir) {
      if (!file.exists(dir))
        stop("The directory ", dir, " does not exist.", call.=FALSE)
      self$pattern <- dir
      self$clear()
      makefile <- file.path(dir, "Makefile.R")
      if (file.exists(makefile))
        try(source(makefile))
    }
    ,
    #' print the maker
    print = function() {
      cat("dir =", self$dir, "\n")
      cat("explicit rules:\n")
      for (r in private$explicit.rules)
        print(r)
      cat("implicit rules:\n")
      for (r in private$implicit.rules)
        print(r)
    }
  )
)

maker = Maker$new(getwd())

#' clear the list of rules and load from Makefile.R
#' @export
resetRules <- function() {
  maker$initialize(getwd())
}

#' tracks the files being automatically opened.
MakeTracker <- R6::R6Class(
  "MakeTracker",
  private = list(
    #' a stack of trackers
    listeners = list()
  ),
  public = list(
    #' adding the current file to the top tracker
    #' @param file the tracked file
    track = function(file) {
      if (length(private$listeners) != 0)
        private$listeners[[1]] <- c(private$listeners[[1]], file)
    },
    #' starts a new tracker
    push = function() {
      private$listeners <- c(list(c()), private$listeners)
    },
    #' finish tracking and pop off the stack
    #' @return the files being used 
    pop = function() {
      l <- private$listeners[[1]]
      private$listeners <- private$listeners[-1]
      l
    }
  )
)

tracker <- MakeTracker$new()

#' make a file
#' @param file the file to make
#' @param force force to build the file regardless if it is stale or not.
#' @return TRUE if successful, FALSE is failed, and NULL if do not know how to make it.
#' @export
make <- function(file="all", force=FALSE) {
  if (length(maker$pattern) == 0) return (FALSE)
  tracker$track(file)
  result <- maker$make(file)
  attr(result, "timestamp") <- NULL
  result
}
