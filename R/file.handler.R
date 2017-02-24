#' this class defines a general interface for handling a file
FileHandler <- setRefClass(
  "FileHandler",
  fields = c(
    # the list of file extensions that this handler can handle
    pattern = "character"),
  methods = list(
    #' check if handler can handle the given file
    #' @param file the file to run
    #' @return logical, TRUE if it can run, FALSE if it cannot.
    canHandle = function(file) {
      return(match.stem(pattern, file))
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
    initialize = function(pattern) {
      callSuper(pattern=pattern)
      scanners$add(.self)
    }
  )
)

scanners <- Manager(class="Scanner")
