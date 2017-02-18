# the Maker class is responsible for making a file.
Maker <- setRefClass(
  "Make",
  fields = c(
    rules = "list"
  ),
  methods = list(
    make = function(file) {
      "Make the file whih is a file path"
      file = normalizePath(file, mustWork = FALSE)
      rule = NULL
      for (handler in rules)
        if (handler$can.handle(file)) {
          rule = handler$handle(file)
          rules[[file]] <<- rule
          break
        }
      if (is.null(rule)) {
        if (file.exists(file)) return(invisible())
        stop("do not know how to make ", file)
      }
      warning("has a rule for ", file, ", but do not know how to use the rule yet.")
    }
  )
)

maker = Maker()

ls.make <- function () {
  print(list(maker=maker, connection.base=connection.base))
}

#' a load file is to be read
#' @param file the path of the local file to be read
read <- function(file) {
  maker$make(file)
}
