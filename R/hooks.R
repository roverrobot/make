# hooks.R contains all the hooks to intercept the file opening for read

#' Hook class intercept one call in base
#' subclasses must implement a hook method
Hook <- R6::R6Class(
  "Hook",
  private = list(
    #' the package to hook into
    package = NULL,
    #' the function that replaces
    fun = NULL,
    #' the saved version of the function
    saved = NULL
  ),
  active = list(
    #' returns the function that this hook intercepts
    f = function() {paste(private$package, private$fun, sep="::")},
    #' check if the hook is set
    isSet = function() {
      !is.null(private$package) && !is.null(private$fun) 
    }
  ),
  public = list(
    #' set the function hook
    set = function() {
      if (!self$isSet)
        stop("the hook is not initialized.", call.=FALSE)
      if (!is.null(private$saved))
        stop("the function ", private$fun, " in package ", self$package, 
             "has been saved.", call.=FALSE)
      pkg <- getNamespace(private$package)
      private$saved <- pkg[[private$fun]]
      unlockBinding(private$fun, pkg)
      pkg[[private$fun]] <- self$hook
      lockBinding(private$fun, pkg)
    }
    ,
    #' restores the functions intercepted by the hooks.
    restore = function() {
      if (is.null(private$saved)) return()
      pkg <- getNamespace(private$package)
      unlockBinding(private$fun, pkg)
      pkg[[private$fun]] <- private$saved
      lockBinding(private$fun, pkg)
      private$saved <- NULL
    }
    ,
    #' the initializaer
    #' @param fun the function to intercept
    #' @param package the package were the function is defined.
    initialize = function(fun, package="base") {
      private$package <- package
      private$fun <- fun
      private$saved <- NULL
    }
  )
)

#' defines a hook for base::file like connection creation functions
fileHook <- R6::R6Class(
  "fileHook",
  inherit = Hook,
  public = list(
    hook=function(description="", open="", ...) {
      con = private$saved(description, "", ...)
      if (open != "") open(con, open)
      con
    }
  )
)

#' defines a hook for base::open
openHook <- R6::R6Class(
  "FileHook",
  inherit = Hook,
  public = list(
    hook=function(con, open = "", ...) {
      info = summary.connection(con)
      if (is.file(info) && grepl("r", open))
        make(info$description)
      private$saved(con, open, ...)
    }
    ,
    initialize= function() {
      super$initialize("open")
    }
  )
)

#' defines a hook for base::readChar
readCharHook <- R6::R6Class(
  "readCharHook",
  inherit = Hook,
  public = list(
    #' the hook for base::readChar
    hook = function(con, ...) {
      if (!is.character(con)) {
        info = summary.connection(con)
        file = info$description
      } else file = con
      make(file)
      private$saved(con, ...)
    }
    ,
    #' initializer
    initialize = function() {
      super$initialize("readChar")
    }
  )
)

#' if the connection is a local file
#' @param con summary of a connection, as returned by summary.connection
#' @return logical
is.file = function(con) {
  con$class %in% c("file", "gzfile", "bzfile", "xzfile", "unz")
}

#' connections from the base package
BaseConnection <- R6::R6Class(
  "BaseConnection",
  private = list(
    #' the list of hooks that is related to connections
    hooks = list()
  ),
  active = list(
    #' the names of the hooks
    hookNames = function() { names(private$hooks) }
  ),
  public = list(
    #' set all the hooks
    #' @param name seet a specific hook given by name, or all the hooks if name=="".
    set = function(name = "") {
      if (name == "") {
        for (hook in private$hooks)
          hook$set()
      } else {
        hook = private$hooks[[name]]
        if (is.null(hook)) {
          warning("The hook ", name, "does not exist. Skip setting it.")
        } else hook$set()
      }
    }
    ,
    #' restore all the hooks
    #' @param name the specific hook with name, or all the hooks if name==""
    restore = function(name = "") {
      if (name == "") {
        for (hook in private$hooks)
          hook$restore()
      } else {
        hook = private$hooks[[name]]
        if (is.null(hook)) {
          warning("The hook ", name, "does not exist. Skip setting it.")
        } else hook$restore()
      }
    }
    ,
    #' add a hook
    add = function(hook) {
      if (!hook$isSet)
        stop("the hook is uninitialized.", call.=FALSE)
      private$hooks[[hook$f]] <- hook
    }
    ,
    #' the initializer
    initialize = function() {
      self$add(fileHook$new(fun="file"))
      self$add(openHook$new())
      self$add(fileHook$new(fun="gzfile"))
      self$add(fileHook$new(fun="bzfile"))
      self$add(fileHook$new(fun="xzfile"))
      self$add(fileHook$new(fun="unz"))
      self$add(readCharHook$new())
    }
  )
)

#' the hooks related to connections from base 
connection.base <- BaseConnection$new()

# the load hook
.onLoad <- function(libname, pkgname) {
  connection.base$set()
  options(`make:interpreter` = "/bin/sh --")
}

# the unload hook
.onUnload <- function(libpath) {
  connection.base$restore()
}

# the detach hook
.onDetach <- function(libpath) {
}

# the attach hook
.onAttach <- function(libpath, pkgname) {
  resetRules()
}
