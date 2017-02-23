#' check if a file name has an extension
#' @param file the filename to check
#' @param ext the extension to check
has.ext <- function(file, ext) {
  substring(tolower(file), nchar(file)-nchar(ext)) == paste(".", tolower(ext), sep="")
}

#' this class defines a general interface for handling a file
FileHandler <- setRefClass(
  "FileHandler",
  fields = c(
    # the list of file extensions that this handler can handle
    ext = "list"),
  methods = list(
    #' check if this interpreter can run a script
    #' @param script the script to run
    #' @return logical, TRUE if it can run, FALSE if it cannot.
    canHandle = function(script) {
      # can run all scripts (i.e., the default) if no ext is specified
      if (length(ext) == 0) return(TRUE)
      file <- tolower(script)
      for (e in ext)
        if (has.ext(file, e)) return(TRUE)
      FALSE
    }
    ,
    #' initializer
    #' @param ext a list or a vector of extensions that this interpreter can run
    initialize = function(ext = list()) {
      if (is.character(ext)) {
        e <- as.list(ext[which(ext != "")])
      } else if (is.list(ext)) {
        e <- ext
      } else stop("ext must be a list or a vector of extensions")
      ext <<- e
    }
  )
)

#' defines a file handler manager
Manager <- setRefClass(
  "Manager",
  fields = c(
    #' the list of file handlers to manage
    handlers = "list",
    #' the class of handlers that it managers
    class = "character"),
  methods = list(
    #' get a handler that can handle a file
    #' @param file, the name of a file to search for a handler
    #' @return a handler
    get = function(file) {
      for (i in handlers)
        if (i$canHandle(file)) return(i)
      return (NULL)
    }
    ,
    #' add  a handler to the first of the list of handlers
    #' @param handler a file handler
    add = function(handler) {
      if (!is(handler, class))
        stop("The interpreter must be an object of ", class)
      handlers <<- c(list(handler), handlers)
    }
  )
)

#' scans for dependences of a file
Scanner <- setRefClass(
  "Scanner",
  contains = c("FileHandler"),
  methods = list(
    #' scan a file for dependences
    #' @param file the file to scan
    #' @return a list of dependences, or NULL if none
    scan = function(file) {
      NULL
    },
    #' initializer
    initialize = function(ext = list()) {
      callSuper(ext=ext)
      scanners$add(.self)
    }
  )
)

scanners <- Manager(class="Scanner")
