#' split a path into a vector of components
#' @param path the file path to split
#' @return a vector of path components
splitPath <- function(path) {
  base <- basename(path)
  if (base == "") {
    "" 
  } else if (base == path) {
    path
  } else c(splitPath(dirname(path)), base)
}

#' normalize a path
#' @param path the path to normalize
#' @return a vector of normalized path components
normalizePath <- function(path) {
  if (!isAbsolutePath(path)) path <- file.path(getwd(), path)
  v <- splitPath(path)
  n <- c()
  for (p in v) {
    if (p == ".") next
    n <- if (p == "..") n[-length(n)] else c(n, p)
  }
  paste(n, collapse=.Platform$file.sep)
}

#' the Maker class is responsible for making a file.
#' @include file.handler.R
Maker <- R6::R6Class(
  "Maker",
  private = list(
    #' the dir that this maker manages
    dir = NULL,
    #' the list of file nodes
    nodes = list(),
    #' the list of implicit make rules (i.e., the pattern of a rule does not contain %)
    implicit = list(),
    #' recipes, named by their recipe ID.
    recipes = list(),
    #' traverse the dependence graph
    #' @param file the file to start from
    #' @patam path the path that has traversed
    #' @param makelist the list of files that needs to be made before file
    #' @return a new makelist including file and all its dependences
    traverse = function(file, path, makelist) {
      if (file %in% path) 
        stop("circular dependences: ", 
             paste(c(path, file), collapse = " -> "), call.=FALSE)
      parent <- tail(path, 1)
      # is file already requested for make?
      entry <- makelist[[file]]
      if (!is.null(entry)) {
        # mark an extra parent and 
        if (length(parent) > 0) {
          entry$parents = c(entry$parents, parent)
          makelist[[file]] <- entry
        }
        return(makelist)
      }
      # traverse its dependences
      path <- c(path, file)
      nodes <- private$nodesForFile(file)
      for (node in nodes) {
        result <- makelist
        private$scan(file, node)
        deps <- node$dependences
        for (dep in deps) {
          result <- private$traverse(dep, path, result)
          if (is.null(result)) {
            if (dep %in% node$auto)
              stop("File ", file, " cannot make dependence ", dep, call.=FALSE)
            break
          }
        }
        # add file to the makelist
        if (!is.null(result)) {
          result[[file]] <- list(parent=parent, depend=deps)
          private$nodes[[file]] <-node
          return(result)
        }
      }
      NULL
    },
    #' scans for automatic dependences for a file
    #' @param file the file to scan
    #' @param node the dependence node of the file
    scan = function(file, node) {
      node$timestamp <- file.mtime(file)
      # scan only if file exists
      node$auto <- if (!is.null(node$timestamp)) {
        scanner <- scanners$get(file)
        if (!is.null(scanner)) scanner$scan(file) else c()
      } else c()
    },
    #' returns a list of nodes that can handle a file
    #' @param file the file to make
    #' @return a list of Node object, or NULL if file does not exist and there is no rule corresponding to the file.
    nodesForFile = function(file) {
      # search for a previously created node for the file
      node <- private$nodes[[file]]
      # if not found, search for an implicit rule
      if (!is.null(node)) return(list(node))
      nodes <- list()
      for (r in private$implicit) {
        node <- r$match(file)
        if (!is.null(node)) nodes <- c(nodes, node)
      }
      if (length(nodes) == 0 && file.exists(file)) {
        list(Node$new(c(), NULL))
      } else nodes
    },
    #' build a file
    #' @param file the file to build
    #' @param timestamp the timestamp of the dependences
    #' @return the timestamp of the file
    build = function(file, timestamp) {
      node <- private$nodes[[file]]
      mtime <- as.numeric(file.mtime(file))
      if (is.null(node)) {
        if (!is.na(mtime)) return(mtime)
        stop("Internal error in making ", file, call.=FALSE)
      }
      if (is.null(node$timestamp) || is.na(node$timestamp) ||
          is.na(mtime) || node$timestamp < mtime)
        node$timestamp <- mtime
      if (is.na(node$timestamp)) {
        build <- TRUE
      } else if (is.null(timestamp)) {
        build <- FALSE
      } else build <- (node$timestamp < timestamp)
      if (build) {
        cat("building ", file, "\n", sep="", file=stderr())
        recipeID <- node$recipe
        recipe <- if (is.null(recipeID)) NULL else private$recipes[[recipeID]]
        if (is.null(recipe)) {
          if (is.na(node$timestamp))
            stop("Do not know how to make ", file, call.=FALSE)
        } else {
          tryCatch({
            ok <- FALSE
            if (is.function(recipe)) {
              recipe(file, node$dependences)
            } else if (is(recipe, "Recipe")) {
              recipe$run(file, node$dependences)
            }
            ok <- TRUE
          }, finally = if (!ok) file.remove(file))
        }
        node$timestamp <- as.numeric(Sys.time())
      }
      node$timestamp
    }
  ),
  public = list(
    #' check if the file can be handled by this maker
    #' it checks if the file is in the dir (self$pattern)
    #' @param file the file to check
    #' @return 
    canHandle = function(file) {
      return (substr(file, 1, nchar(private$dir)) == private$dir)
    },
    #' make a file
    #' @param file the file to make
    #' @param force force to build the file regardless if it is stale or not.
    #' @param silent In the case that no rule matches, complain and stop if TRUE, or silently return if FALSE.
    make = function(file, force=FALSE, silent = FALSE) {
      if (isAbsolutePath(file)) {
        file <- substring(file, nchar(private$dir)+2)
      }
      # if asked to make a list of files, make them one by one
      if (length(file) > 1) {
        for (f in file)
          self$make(f, foruce, silent)
        return()
      }
      # traverse the tree, while issuing make commands
      makelist <- private$traverse(file, path=c(), makelist=list())
      if (is.null(makelist))
        stop("Do not know how to make ", file, call.=FALSE)
      timestamp <- NULL
      while (length(makelist) > 0) {
        # make all the iles with no dependences
        pick <- sapply(makelist, function(file) { length(file$depend) == 0 })
        if (length(which(pick)) == 0) {
          stop("No leaf in the dependence graph!", call.=FALSE)
        }
        leaves <- makelist[pick]
        for (leaf in names(leaves)) {
          timestamp <- private$build(leaf, timestamp)
          # remove leaf from each parent's dependences
          # and update the parent's timestamp
          for (parent in leaves[[leaf]]$parent) {
            p <- makelist[[parent]]
            deps <- p$depend
            p$depend <- deps[which(deps != leaf)]
            if (is.null(p$timestamp) || p$timestamp < timestamp)
              p$timestamp <- timestamp
            makelist[[parent]] <- p
          }
        }
        makelist <- makelist[!pick]
      }
    },
    #' add a rule to the list of rules
    #' @param target of the rule
    #' @param depend the list of dependences of the target
    #' @param recipe the recipe to make the target
    #' @return the dependence node or the implicit rule
    addRule = function(target, depend=c(), recipe=NULL) {
      if (length(target) > 1) {
        for (targ in target) self$addRule(targ, depend, recipe)
      } else {
        if (length(target) == 0 || target == "")
          stop("A rule must have a target", call.=FALSE)
        # compute the recipeID
        recipeID <- paste(target, paste(depend, collapse="+"), sep="~")
        if (!is.null(private$recipes[[recipeID]])) {
          stop("The rule for ", recipeID, " already exists. Skipping it.")
        }
        private$recipes[[recipeID]] <- recipe
        # check if this is an implicit rule.
        if (grepl("%", target)) {
          node <- ImplicitRule$new(target, depend, recipeID)
          private$implicit[[recipeID]] <- node
        } else {
          node <- Node$new(depend, recipeID)
          private$nodes[[target]] <- node
        }
        node
      }
    }
    , 
    #' clear the rules
    clear = function() {
      private$nodes <- list()
      private$implicit <- list()
      private$recipes <- list()
    }
    ,
    #' initializer
    #' @param dir the directory that this maker managers
    initialize = function(dir) {
      private$dir <- normalizePath(dir)
      if (!file.exists(private$dir))
        stop("The directory ", dir, " does not exist.", call.=FALSE)
      self$clear()
    },
    #' load the rules from Makefile.R
    load = function() {
      makefile <- file.path(private$dir, "Makefile.R")
      if (file.exists(makefile)) {
        source(makefile, local=TRUE)
      }
    },
    #' print the maker
    print = function() {
      cat("dir =", private$dir, "\n")
      cat("explicit rules:\n")
      for (r in names(private$nodes)) {
        cat(r, "~ ")
        print(private$nodes[[r]])
      }
      cat("\nimplicit rules:\n")
      for (r in private$implicit) {
        print(r)
      }
      cat("\nrecipes:\n")
      for (r in names(private$recipes)) {
        cat(r, "= ")
        print(private$recipes[[r]])
      }
    }
  )
)

pkg.env <- new.env()

#' clear the list of rules and load from Makefile.R
#' @export
resetRules <- function() {
  pkg.env$maker <- Maker$new(getwd())
  pkg.env$maker$load()
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
  maker <- pkg.env$maker
  if (is.null(maker)) return()
  abs <- normalizePath(file)
  if (maker$canHandle(abs)) {
    tracker$track(file)
    maker$make(file)
  } else if (!file.exists(file)) 
    stop("do not know how to make ", file)
}
