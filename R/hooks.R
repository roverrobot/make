# hooks.R contains all the hooks to intercept the file opening for read

#' Hook class intercept one call in base
#' subclasses must implement a hook method
Hook <- R6::R6Class(
  "Hook",
  public = list(
    #' the package to hook into
    package = NULL,
    #' the function that replaces
    fun = NULL,
    #' the saved version of the function
    saved = NULL,
    #' set the function hook
    set = function() {
      if (is.null(self$fun) || is.null(self$hook))
        stop("the hook is not initialized.", call.=FALSE)
      if (!is.null(self$saved))
        stop("the function ", self$fun, " in package ", self$package, 
             "has been saved.", call.=FALSE)
      pkg <- getNamespace(self$package)
      self$saved <- pkg[[self$fun]]
      unlockBinding(self$fun, pkg)
      pkg[[self$fun]] <- self$hook
      lockBinding(self$fun, pkg)
    }
    ,
    #' restores the functions intercepted by the hooks.
    restore = function() {
      if (is.null(self$saved)) return()
      pkg <- getNamespace(self$package)
      unlockBinding(self$fun, pkg)
      pkg[[self$fun]] <- self$saved
      lockBinding(self$fun, pkg)
      self$saved <- NULL
    }
    ,
    #' the initializaer
    #' @param fun the function to intercept
    #' @param package the package were the function is defined.
    initialize = function(fun, package="base") {
      self$package <- package
      self$fun <- fun
      self$saved <- NULL
    }
  )
)

#' defines a hook for base::file like connection creation functions
fileHook <- R6::R6Class(
  "fileHook",
  inherit = Hook,
  public = list(
    hook=function(description="", open="", ...) {
      con = self$saved(description, "", ...)
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
      self$saved(con, open, ...)
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
      self$saved(con, ...)
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
  public = list(
    #' the list of hooks that is related to connections
    hooks = list(),
    #' set all the hooks
    #' @param name seet a specific hook given by name, or all the hooks if name=="".
    set = function(name = "") {
      if (name == "") {
        for (hook in self$hooks)
          hook$set()
      } else {
        hook = self$hooks[[name]]
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
        for (hook in self$hooks)
          hook$restore()
      } else {
        hook = self$hooks[[name]]
        if (is.null(hook)) {
          warning("The hook ", name, "does not exist. Skip setting it.")
        } else hook$restore()
      }
    }
    ,
    #' add a hook
    add = function(hook) {
      if (hook$package == "" || hook$fun == "" || is.null(hook$hook))
        stop("the hook is uninitialized.", call.=FALSE)
      name = paste(hook$package, hook$fun, sep=":")
      self$hooks[[name]] <<- hook
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
