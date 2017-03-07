% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.rule.R
\docType{data}
\name{MakeRule}
\alias{MakeRule}
\title{makeRule implements a rule that is similar to a Makefile rule}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
MakeRule
}
\arguments{
\item{target}{a pattern or a file name}

\item{depend}{the dependences,}

\item{recipe}{a recipe to make the target, either an R function(target, depend), or a Recipe object, or NULL (the rule makes nothing and always success, after successfully checked dependences).}

\item{interpreter}{f using the first dependent file as a script, this is the interpreter to run the script.}

\item{replace}{If TRUE, it replaces the rule to make the same target. If FALSE, and a rule to make the same target exists, it complains and fail.
canHandle checks if a rule can handle a specific file.}

\item{file}{the file to check}

\item{file}{the file to make}

\item{force}{force to build the file regardless if it is stale or not.}

\item{deps}{the dependences}

\item{timestamp}{the timestamp to be added to the deps
whether the rule is implicit or not
pretty print a makeRule object}
}
\value{
a logical indicating whether the file can be handled. In the case that it can be handled, it contains an attributed named "rule". If the rule is implicit, the returned rule is an explicit rule that can handle the file. If the rule is explicit, the returned rule is itself.
make a file

logical. TRUE if successful, and FALSE if do not know how to make it. If the make fails, the function stops with an error.
scan for dependences
add dependences
}
\description{
makeRule implements a rule that is similar to a Makefile rule
}
\keyword{datasets}