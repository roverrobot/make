# the Maker class is responsible for making a file.
Maker <- setRefClass(
  "Maker",
  fields = c(
    rules = "list"
  ),
  methods = list(
    make = function(file) {
      result = NULL
      for (rule in rules) {
        result = rule$make(file=file)
        if (!is.null(result)) break
      }
      if (is.null(result)) {
        if (file.exists(file)) return(invisible(NULL))
        stop("do not know how to make file: ", file, call. = FALSE)
      } else if (!result)
        stop("failed to make file: ", file, call. = FALSE)
      TRUE
    }
    ,
    add.rule = function(rule, replace=FALSE, first.rule=FALSE) {
      if (!is(rule, "Rule"))
        stop("A rule must be an object of the class Rule.")
      name <- rule$getTarget()
      if (is.null(name) || length(name) == 0)
        stop("A rule must have a target")
      if (is.null(rules[[name]])) {
        add <- list()
        add[[name]] <- rule
        rules <<- if (first.rule) c(add, rules) else c(rules, add)
      } else if (replace) {
        rules[[name]] <<- rule
      } else stop("A rule for a target ", name, "already exits.")
    }
  )
)

maker = Maker()

getRules <- function () {
  maker$rules
}

clearRules <- function() {
  maker$rules = list()
}

#' a load file is to be read
#' @param file the path of the local file to be read
read <- function(file) {
  maker$make(file)
}
