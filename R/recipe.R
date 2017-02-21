# Recipe represents the recipe for a rule
Recipe <- setRefClass(
  "Recipe",
  methods = list(
    run = function(target, depend) {
      FALSE
    }
  )
)
