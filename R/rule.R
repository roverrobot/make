## the most important method that it has is make
Rule <- setRefClass(
  "Rule",
  fields = c(name="character"),
  methods = list(
    initialize = function(name="", replace=FALSE) {
      .self$name <<- name
      maker$add.rule(.self, replace)
    }
    ,
    # make will fail for any file
    make = function(file, force=FALSE) {
      NULL
    }
  )
)

#' check if the target file is older than the depend file
#' @param target the target file name, which must exist
#' @param depend the depend file name
#' @return logical
stale <- function(target, depend) {
  depend.info = file.info(depend)
  if (length(which(!is.na(target.info$size))) == 0)
    return (TRUE)
  return (depend.info$mtime > target.info$mtime)
}
