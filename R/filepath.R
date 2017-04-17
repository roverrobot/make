#' split a path into a vector of components
#' @param path the file path to split
#' @return a vector of path components
splitPath <- function(path) {
  base <- basename(path)
  if (base == "") {
    "" 
  } else if (base == path) {
    path
  } else c(splitPath(dirname(path)), base)
}

#' normalize a path
#' @param path the path to normalize
#' @return a vector of normalized path components
normalizePath <- function(path) {
  if (!isAbsolutePath(path)) path <- file.path(getwd(), path)
  v <- splitPath(path)
  n <- c()
  for (p in v) {
    if (p == ".") next
    n <- if (p == "..") n[-length(n)] else c(n, p)
  }
  paste(n, collapse=.Platform$file.sep)
}

#' checks if a path is an absolute path
#' @param path the path to check
#' @return logical
isAbsolutePath <- function(path) {
  pattern <- if (.Platform$file.sep == "/") "^(/|~)" else "^([A-Za-z]:)?\\\\"
  grepl(pattern, path)
}

#' match file to patterns, which can contain a stem, e.g., dir/\%.c
#' @param pattern the pattern (or a list of vector of them) to match to
#' @param file the file name
#' @return logical indicating if file matches pattern, optionally with an attribute named "stem", returning the part of file matching the \%, if the matching pattern contains a \%.
match.stem = function(pattern, file) {
  # if pattern is a vector (or a list), match to each
  if (length(pattern) > 1) {
    for (pat in pattern) {
      result = match.stem(pat, file)
      if (result) return(result)
    }
    return (FALSE)
  }
  # if we reach here, pattern is a single pattern.
  # if the pattern is not an implicit rule, i.e., does not contain %, then
  # we simply compare the canonical paths of pattern and file
  if (!grepl("%", pattern)) {
    pattern <- normalizePath(pattern)
    file <- normalizePath(file)
    return (pattern == file)
  }
  # if we read here, then the pattern contains %, and is thus an implicit rule
  parts = strsplit(pattern, "%")[[1]]
  if (length(parts) > 2)
    warning("pattern ", pattern, " has an invalid pattern in the target.",
            "Only a single % is allowed.")
  if (parts[[1]] != "") {
    n1 = nchar(parts[[1]])
    match = substr(file, 1, n1) == parts[[1]]
  } else {
    n1 = 0
    match = TRUE
  }
  if (match) {
    if (length(parts) > 1) {
      n2 = nchar(file) + 1 - nchar(parts[[2]])
      match = substring(file, n2) == parts[[2]]
    } else n2 = NULL
  }
  if (!match) {
    # if file does not match, check if the canonical path of file matches.
    file.abs = normalizePath(file)
    if (file.abs != file)
      return(match.stem(pattern, file.abs))
    return (FALSE)
  }
  if (is.null(n2)) {
    stem = substring(file, n1+1)
  } else stem = substr(file, n1 + 1, n2 - 1)
  attr(match, "stem") = stem
  match
}
