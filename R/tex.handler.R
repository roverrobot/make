#' this class processes a tex file
texHandler <- setRefClass(
  "texHandler",
  fields = c(
    #' the tex file to process
    script = "character",
    #' saves the content of the file
    content = "character"
  ),
  methods = list(
    #' matches a command in the form \command[]{}
    #' @param command the tex command to search for
    #' @param first.command whether this command should be the first command
    #' @param to.space whether the parameter is up to the first space (in the absence of {})
    matchCommand = function(command,
                            first.command = FALSE,
                            to.space=FALSE) {
      pattern <- paste(if (first.command) "^" else "",
                       "\\\\", command,
                       "(?:\\s+(?'match1'",
                       if (to.space) "[^\\s]*(?:\\s|$)" else ".?",
                       ")|\\s*",
                       "(?:\\[[^]]*\\])?{(?'match2'[^}]*)})", sep="")
      x <- gregexpr(pattern, content, perl=TRUE)[[1]]
      start <- rowSums(attr(x, "capture.start"))
      len <- rowSums(attr(x, "capture.length"))
      end = start + len - 1
      if (x[[1]] > 0) {
        mapply(function(start, end) {substr(content, start, end)}, start, end)
      } else c()
    },
    #' check latex or plain tex
    #' @param file the tex file to check
    isLatex = function() {
      !is.null(matchCommand("documentclass", first.command = TRUE))
    },
    # initializer
    initialize = function(script) {
      file <- connection.base$hooks[["base:file"]]$saved
      script <<- script
      if (!file.exists(script)) return(NULL)
      con <- file(script, "r")
      lines <- readLines(con)
      close(con)
      strip <- sapply(lines, function(line) {
        s <- strsplit(line, "%")[[1]]
        if (length(s) == 0) "" else s[1]
      })
      content <<- paste(strip, sep="", collapse="\n")
    }
  )
)

tex.ext = c("%.tex", "%.ltx")

#' this Interpreter subclass compiles a tex file
texCompiler <- setRefClass(
  "texInterpreter",
  contains = c("Interpreter"),
  fields = c(
    #' the latex compiler
    latex = "character",
    #' the tex compiler
    tex = "character",
    #' the bibtex processor
    bibtex = "character"
  ),
  methods = list(
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
      callSuper(pattern=pattern, register=register)
      latex <<- latex
      tex <<- tex
      bibtex <<- bibtex
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
      h <- texHandler(script)
      wd.save <- getwd()
      script <- normalizePath(script)
      wd.tex = dirname(script)
      base <- basename(script)
      parts <- strsplit(base, "\\.")[[1]]
      base <- paste(parts[1:(length(parts)-1)], collapse=".")
      run <- if (h$isLatex()) latex else tex
      run.tex <- Interpreter(pattern=basename(script), command=run)
      exec(run.tex)
      aux <- texHandler(file.path(wd.tex, paste(base, "aux", sep=".")))
      bibs = aux$matchCommand("bibdata")
      cite = aux$matchCommand("citation")
      if (length(bibs) > 0 && length(cite) > 0) {
        for (bib in bibs) {
          if (!match.stem("%.bib", bib)) 
            bib=file.path(wd.tex, paste(bib, "bib", sep="."))
          if (!file.exists(bib))
            stop("Bibtex database ", bib, " does not exit.")
          run.bib <- Interpreter(pattern=bib, command=bibtex)
          exec(run.bib)
        }
        exec(run.tex)
        exec(run.tex)
      }
    }
  )
)

texCompiler()

#' checks if a path is an absolute path
#' @param path the path to check
#' @return logical
isAbsolutePath <- function(path) {
  pattern <- if (.Platform$file.sep == "/") "^(/|~)" else "^([A-Za-z]:)?\\\\"
  grepl(pattern, path)
}

texScanner <- setRefClass(
  "texScanner",
  contains = c("Scanner"),
  methods = list(
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

      tex <- texHandler(file)
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
        if (isAbsolutePath(dep)) dep else file.path(dirname(file), dep)}
      )
    },
    #' initializer
    initialize = function() {
      callSuper(pattern=tex.ext)
    }
  )
)

texScanner()
