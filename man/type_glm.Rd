% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/type_glm.R
\name{type_glm}
\alias{type_glm}
\title{Generalized linear model plot type}
\usage{
type_glm(family = "gaussian", se = TRUE, level = 0.95, type = "response")
}
\arguments{
\item{family}{a description of the error distribution and link
    function to be used in the model.  For \code{glm} this can be a
    character string naming a family function, a family function or the
    result of a call to a family function.  For \code{glm.fit} only the
    third option is supported.  (See \code{\link[stats]{family}} for details of
    family functions.)}

\item{se}{logical. If TRUE, confidence intervals are drawn.}

\item{level}{the confidence level required.}

\item{type}{character, partial matching allowed.  Type of weights to
    extract from the fitted model object.  Can be abbreviated.}
}
\description{
Type function for plotting a generalized model fit.
Arguments are passed to \code{\link[stats]{glm}}.
}
\examples{
# "glm" type convenience string
tinyplot(am ~ mpg, data = mtcars, type = "glm")

# Use `type_glm()` to pass extra arguments for customization
tinyplot(am ~ mpg, data = mtcars, type = type_glm(family = "binomial"))
}
