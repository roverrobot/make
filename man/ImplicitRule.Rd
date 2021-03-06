% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.rule.R
\docType{data}
\name{ImplicitRule}
\alias{ImplicitRule}
\title{Represents an imlicit rule
initializer}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
ImplicitRule
}
\arguments{
\item{pattern}{the pattern for the target files}

\item{depend}{the patterns for dependences}

\item{recipeID}{the recipe ID to generate a target
match to a file}

\item{file}{the file for which to create the rule}
}
\value{
a Node object or NULL if cannot handle the file
pretty print a makeRule object
}
\description{
Represents an imlicit rule
initializer
}
\keyword{datasets}
