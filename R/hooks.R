# hooks.R contains all the hooks to intercept the file opening for read

# Hook class intercept one call in base
# base classes must implement a hook method
setClassUnion("functionOrNULL",members=c("function", "NULL"))
Hook <- setRefClass("Hook",
  fields = c(
    package = "character",
    fun = "character",
    saved = "functionOrNULL"
  )
  ,
  methods = list(
    set = function(overwrite = FALSE) {
      if (is.null(fun) || is.null(hook)) {
        warning("the hook is not initialized. The hook is skipped.")
        return()
      }
      if (!is.null(saved) && !overwrite) {
        warning("the function ", fun, " in package ", package, "has been saved. The hook is skipped.")
        return()
      }
      pkg <- getNamespace(package)
      saved <<- pkg[[fun]]
      unlockBinding(fun, pkg)
      pkg[[fun]] <- .self$hook
      lockBinding(fun, pkg)
    }
    ,
    restore = function() {
      if (is.null(saved)) {
        warning("the hook is not initialized. Restoration is skipped.")
        return()
      }
      pkg <- getNamespace(package)
      unlockBinding(fun, pkg)
      pkg[[fun]] <- saved
      lockBinding(fun, pkg)
      saved <<- NULL
    }
  )
)

fileHook <- setRefClass("fileHook",
  contains = c("Hook"),
  methods = c(
    hook=function(description="", open="", ...) {
      con = saved(description, "", ...)
      info = summary.connection(con)
      if (is.read(info)) read(info$description)
      if (open != "") open(con, open)
      con
    }
    ,
    initialize = function(fun) {
      initFields(package="base", fun=fun, saved=NULL)
    }
  )
)

openHook <- setRefClass("FileHook",
  contains = c("Hook"),
  methods = c(
    hook=function(con, open = "", ...) {
      info = summary.connection(con)
      if (info$class == "file" && grepl("r", open))
        read(info$description)
      saved(con, open, ...)
    }
    ,
    initialize= function() {
      package <<- "base"
      fun <<- "open"
    }
  )
)

#' if the connection is being opened for read
#' @param con summary of a connection, as returned by summary.connection
#' @return logical
is.read = function(con) {
  con$class %in% c("file", "gzfile", "bzfile", "xzfile", "unz") &&
    con$opened == "opened" && grepl("r", con$mode)
}

BaseConnection <- setRefClass("BaseConnection",
  fields = c(hooks="list"),
  methods = list(
    set = function(name = "") {
      if (name == "") {
        for (hook in hooks)
          hook$set()
      } else {
        hook = hooks[[name]]
        if (is.null(hook)) {
          warning("The hook ", name, "does not exist. Skip setting it.")
        } else hook$set()
      }
    }
    ,
    restore = function(name = "") {
      if (name == "") {
        for (hook in hooks)
          hook$restore()
      } else {
        hook = hooks[[name]]
        if (is.null(hook)) {
          warning("The hook ", name, "does not exist. Skip setting it.")
        } else hook$restore()
      }
    }
    ,
    add = function(hook) {
      if (hook$package == "" || hook$fun == "" || is.null(hook$hook)) {
        warning("the hook is uninitialized. Skip it.")
        return()
      }
      name = paste(hook$package, hook$fun, sep=":")
      hooks[[name]] <<- hook
    }
    ,
    initialize = function() {
      add(fileHook(fun="file"))
      add(openHook())
      add(fileHook(fun="gzfile"))
      add(fileHook(fun="bzfile"))
      add(fileHook(fun="xzfile"))
      add(fileHook(fun="unz"))
    }
  )
)

connection.base <- BaseConnection()

#' the load hook
.onLoad <- function(libname, pkgname) {
  connection.base$set()
}

#' the unload hook
.onUnload <- function(libpath) {
  connection.base$restore()
}

#' the detach hook
.onDetach <- function(libpath) {
  clearRules()
}

#' the attach hook
.onAttach <- function(libpath, pkgname) {
  if (file.exists("Makefile.R"))
    tryCatch(source("Makefile.R"))
}
