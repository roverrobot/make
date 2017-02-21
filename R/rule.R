## the most important method that it has is make
Rule <- setRefClass(
  "Rule",
  methods = list(
    initialize = function(replace=FALSE, first.rule = FALSE) {
      maker$add.rule(.self, replace, first.rule)
    }
    ,
    getTarget = function() {
      NULL
    }
    ,
    # make will fail for any file
    make = function(file, force=FALSE) {
      NULL
    }
  )
)
