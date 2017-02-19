## the most important method that it has is make
Rule <- setClass("Rule",
  slots = c(name="character"),
  prototype = list(
    name = ""
  )
)

# create a method to get the value of active
setMethod(initialize,
  signature = c("Rule"),
  definition = function(.Object, name="", replace=FALSE) {
    .Object@name <- name
    maker$add.rule(.Object, replace)
    invisible(.Object)
  }
)

# create a method to get the value of active
setGeneric(name="make",
  def=function(.Object, file, force=FALSE) {
    "returning NULL means do not know how to make the file. TRUE/FALSE means success/failure."
    standardGeneric("make")
  }
)

setMethod(make,
  signature = c("Rule"),
  definition = function(.Object, file, force=FALSE) {
    NULL
  }
)

#' check if the target file is older than the depend file
#' @param target the target file name
#' @param depend the depend file name, which must exist
#' @return logical
stale <- function(target, depend) {
  target.info = file.info(target)
  # check if target does not exist
  if (length(which(!is.na(target.info$size))) == 0)
    return (FALSE)
  depend.info = file.info(depend)
  if (length(which(!is.na(target.info$size))) == 0)
    return (TRUE)
  return (depend.info$mtime > target.info$mtime)
}
