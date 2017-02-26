# make
This is an R package to provide a make-like utility for R, in R.

## Usage
- Write an R script named Makefile.R in your working directory.
- load the make library, e.g., library(make)
- either directly open any file, which will be automatically made according to the rules defined in Makefile.R, or use the make function in the form of `make("filename")`.

## Makefile.R
- all rules are defined by the "makeRule" function:
  - The first parameter is target, which is either a file name, or a formula in the form of target ~ dependences
    - file names that can confuse R should be quoted by ``.
    - dependences are separated by +
    - a file which name should be taken from a local variable, say var, should be put in a pair of parenthasis, like `(var)`.
  - A rule can be either explicit, i.e., specifying the rule for a specific file, or an implicit rule, which can make files which names match a specific pattern. The wildcard character is %, as in Makefiles.
    - Example for an explicit rule: `test.RData ~ test.csv`
    - Example for an implicit rule: `%.RData ~ %.csv`
  - The second parameter is a recipe, which can be one of
    - TRUE/FALSE: TRUE means alway success, and FALSE means do not know how to make it.
    - an R function, in the form of `function(target, depend)`, here target is the target to make, and depend is the list of dependences to make the target.
    - by default, it uses the first dependent file as a script. 
      - The script is interpreted by an interpreter.
      - The currently supported interpreters include: pdftex or pdflatex (%.tex), sh (default), perl (%.pl), python (%.py), matlab or octave (%.m), R (%.R).
      - New interpreters can be defined in Makefile.R using `Interpreter(pattern, command)`, for example, the sh interpreter is defined as `Interpreter("%", "sh --")`.

Here is an example Makefile.R
<code>
# implicit rule for *.tex -> *.pdf
# note the use of `` for files that are illegal in a formula.
makeRule(`%.pdf` ~ `%.tex`)
# this rule will generate an error on circular dependences
makeRule(at.txt ~ at.txt)
# an implicit rule for at*.txt, but will fail because of an error in test.sh
makeRule(`at%.txt` ~ test.sh)

makeRule(r ~ test.R)

exclude <- function(a, b) {
  a[!(a %in% b)]
}

# as long as a rule does not create a file, and there is no corresponding file
# with the same name, then it is a phony rule.
makeRule(clean, recipe = function(target, depend) {
  files.txt <- Sys.glob("at*.txt")
  files.data <- exclude(Sys.glob("test.*"), c("test.sh", "test.R"))
  files <- c(files.tex, files.txt, files.data)
  if (length(files) > 0) file.remove(files)
})

# create an RData file out of a csv file
makeRule(test.RData ~ test.csv, recipe=function(target, depend) {
  csv = depend[[1]]
  data = read.csv(csv)
  save(file=target, data)
})

# create a csv file by hand
makeRule(test.csv ~ ., recipe=function(target, depend) {
  data = data.frame(a=c(1,2), b=c("yes", "no"))
  write.csv(file=target, row.names = FALSE, data)
})
</code>
