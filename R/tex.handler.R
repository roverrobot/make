#' this class processes a TeX/LaTeX file
texHandler <- R6::R6Class(
  "texHandler",
  private = list(
    #' the tex file to process
    script = NULL,
    #' saves the content of the file
    content = NULL
  ),
  public = list(
    #' matches a command in the form \\command[]\{\}
    #' @param command the tex command to search for
    #' @param first.command whether this command should be the first command
    #' @param to.space whether the parameter is up to the first space (in the absence of \{\})
    matchCommand = function(command,
                            first.command = FALSE,
                            to.space=FALSE) {
      pattern <- paste(if (first.command) "^\\s*" else "",
                       "\\\\", command,
                       "(?:(?:\\s+",
                       if (to.space) {
                         "(?'match1'.*?)[\\s$]" 
                       } else "(?'match1'.?)",
                       ")|(?:\\s*",
                       "(?:\\[[^]]*\\])?{(?'match2'[^}]*)}))", 
                       sep="")
      x <- gregexpr(pattern, private$content, perl=TRUE)[[1]]
      start <- rowSums(attr(x, "capture.start"))
      len <- rowSums(attr(x, "capture.length"))
      end = start + len - 1
      if (x[[1]] > 0) {
        mapply(function(start, end) {substr(private$content, start, end)}, start, end)
      } else c()
    },
    #' check latex or plain tex
    #' @param file the tex file to check
    isLatex = function() {
      !is.null(self$matchCommand("documentclass", first.command = TRUE))
    },
    # initializer
    initialize = function(script) {
      private$script <- script
      if (!file.exists(script)) return(NULL)
      con <- connection.base$original("base::file", description=script, open="r")
      lines <- readLines(con)
      close(con)
      strip <- sapply(lines, function(line) {
        s <- strsplit(line, "%")[[1]]
        if (length(s) == 0) "" else s[1]
      })
      private$content <- paste(strip, sep="", collapse="\n")
    }
  )
)

tex.ext = c("%.tex", "%.ltx")

#' this Interpreter subclass compiles a tex file
#' @include script.recipe.R
texCompiler <- R6::R6Class(
  "texCompiler",
  inherit = Interpreter,
  private = list(
    #' the latex compiler
    latex = NULL,
    #' the tex compiler
    tex = NULL,
    #' the bibtex processor
    bibtex = NULL
  ),
  active = list(
    latexCommand = function() { private$latex },
    texCommand = function() { private$tex },
    bibexCommand = function() { private$bibtex }
  ),
  public = list(
    #' initializer
    #' @param ext a list or a vector of extensions that this interpreter can run
    #' @param register whether toautomatically add to the interpreter manager
    #' @param latex the latex compiler command
    #' @param tex the plain tex compiler command
    #' @param bibtex the bibtex processor command
    initialize = function(pattern = tex.ext, register=TRUE,
                          latex = "pdflatex -interaction=nonstopmode",
                          tex="pdftex -interaction=nonstopmode",
                          bibtex="bibtex") {
      super$initialize(pattern=pattern, register=register)
      private$latex <- latex
      private$tex <- tex
      private$bibtex <- bibtex
    },
    #' the method for making the target from a vector of dependences
    #' @param script the script to run
    #' @param target the target file
    #' @param depend the vector of dependences, the first file in depend is the script name
    run = function(script, target, depend) {
      exec <- function(run) {
        setwd(wd.tex)
        tryCatch(run$run(base, "", list()),
                 error=function(e) { stop(geterrmessage(), call. = FALSE)},
                 finally = setwd(wd.save))
      }
      h <- texHandler$new(script)
      wd.save <- getwd()
      script <- normalizePath(script)
      wd.tex = dirname(script)
      base <- basename(script)
      parts <- strsplit(base, "\\.")[[1]]
      base <- paste(parts[1:(length(parts)-1)], collapse=".")
      run <- if (h$isLatex()) private$latex else private$tex
      run.tex <- Interpreter$new(pattern=basename(script), command=run)
      exec(run.tex)
      aux <- texHandler$new(file.path(wd.tex, paste(base, "aux", sep=".")))
      bibs = aux$matchCommand("bibdata")
      cite = aux$matchCommand("citation")
      if (length(bibs) > 0 && length(cite) > 0) {
        for (bib in bibs) {
          if (!match.stem("%.bib", bib)) 
            bib=file.path(wd.tex, paste(bib, "bib", sep="."))
          if (!file.exists(bib))
            stop("Bibtex database ", bib, " does not exit.")
          run.bib <- Interpreter$new(pattern=bib, command=private$bibtex)
          exec(run.bib)
        }
        exec(run.tex)
        exec(run.tex)
      }
    }
  )
)

texCompiler$new()

#' checks if a path is an absolute path
#' @param path the path to check
#' @return logical
isAbsolutePath <- function(path) {
  pattern <- if (.Platform$file.sep == "/") "^(/|~)" else "^([A-Za-z]:)?\\\\"
  grepl(pattern, path)
}

#' this class defines a TeX/LaTeX file scanner for automatic dependences
texScanner <- R6::R6Class(
  "texScanner",
  inherit = Scanner,
  public = list(
    #' scan a file for dependences
    #' @param file the file to scan
    #' @return a vector of dependences, or c() if none
    scan = function(file) {
      add.suffix <- function(strs, suffix) {
        sapply(strs, function(str) {
          parts <- strsplit(basename(str),"\\.")[[1]]
          if (length(parts) > 1) str else paste(str, suffix, sep=".")
        })
      }
      tex <- texHandler$new(file)
      figures <- tex$matchCommand("includegraphics")
      figures <- add.suffix(figures, "pdf")
      bibs <- tex$matchCommand("bibliography")
      if (length(bibs) > 0) {
        bibs <- trimws(strsplit(bibs, ","))[[1]]
        bibs <- add.suffix(bibs, "bib")
      }
      inputs <- tex$matchCommand("input", to.space = TRUE)
      inputs <- add.suffix(inputs, "tex")
      deps <- c(figures, bibs, inputs)
      sapply(deps, function(dep) {
        if (isAbsolutePath(dep) || dirname(file) == ".") dep else file.path(dirname(file), dep)}
      )
    },
    #' initializer
    initialize = function() {
      super$initialize(pattern=tex.ext)
    }
  )
)

texScanner$new()
