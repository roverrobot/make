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
        result = make::make(rule, file=file)
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
    add.rule = function(rule, replace=FALSE) {
      if (!is(rule, "Rule"))
        warning("Skipped adding the rule ", rule, ", which is not an object of the class Rule.")
      name = rule@name
      if (name == "") {
        rules <<- c(rules, list(rule))
      } else if (is.null(rules[[name]]) || replace) {
        rules[[name]] <<- rule
      } else warning("A rule named ", name, "already exits. Skip adding the new rule.")
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
