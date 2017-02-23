#' the Maker class is responsible for making a file.
Maker <- setRefClass(
  "Maker",
  fields = c(
    #' the list of make rules
    rules = "list",
    #' the list of files currently being made
    making = "list"
  ),
  methods = list(
    #' make a file
    #' @param file the file to make
    #' @param force force to build the file regardless if it is stale or not.
    #' @param silent In the case that no rule matches, complain and stop if TRUE, or silently return if FALSE. Still complains and stop if a rule matches but failed to make the file.
    #' @return TRUE if successful, and NULL if do not know how to make it.
    make = function(file, force=FALSE, silent = FALSE) {
      if (file %in% making) {
        making <<- list()
        stop("circular dependences: ", making, " ", file)
      }
      making <<- c(making, file)
      result = NULL
      for (rule in rules) {
        result = tryCatch(rule$make(file, force),
                          error = function(e) {
                            making <<- list()
                            stop(geterrmessage(), call.=FALSE)})
        if (!is.null(result)) break
      }
      making <<- making[-length(making)]
      if (is.null(result)) {
        if (file.exists(file) || silent) return(NULL)
        making <<- list()
        stop("do not know how to make file: ", file, call. = FALSE)
      } else if (!result) {
        if (file.exists(file)) file.remove(file)
        making <<- list()
        stop("failed to make file: ", file, call. = FALSE)
      }
      TRUE
    }
    ,
    #' add a rule to the list of rules
    #' @param rule the rule to add
    #' @param replace If TRUE, it replaces the rule to make the same target. If FALSE, and a rule to make the same target exists, it complains and fail.
    #' @param first.rule If TRUE, add to the top of the list. If FALSE, add to the bottom of the list. Note that the rules are searched from top to bottom until the first one which target matches the file to be made if found.
    add.rule = function(rule, replace=FALSE, first.rule=FALSE) {
      if (!is(rule, "makeRule"))
        stop("A rule must be an object of the class Rule.")
      name <- rule$target
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

#' return the list of rules
getRules <- function () {
  maker$rules
}

#' clear the list of rules and load from Makefile.R
resetRules <- function() {
  maker$rules <- list()
  maker$making <- list()
  if (file.exists("Makefile.R"))
    try(source("Makefile.R"))
}

#' make a file
#' @param file the file to make
#' @param force force to build the file regardless if it is stale or not.
#' @return TRUE if successful, FALSE is failed, and NULL if do not know how to make it.
make <- function(file, force=FALSE) {
  maker$make(file)
}
