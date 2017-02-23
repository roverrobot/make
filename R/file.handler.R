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
    canRun = function(script) {
      # can run all scripts (i.e., the default) if no ext is specified
      if (length(ext) == 0) return(TRUE)
      has.ext <- function(file, ext) {
        substring(file, length(file)-length(ext)) == ext
      }
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
