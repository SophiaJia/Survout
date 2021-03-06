% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor2ind.R
\name{factor2ind}
\alias{factor2ind}
\title{competing risk helper function, crr}
\usage{
factor2ind(x, baseline)
}
\arguments{
\item{x}{a Variable}

\item{baseline}{Reference level}
}
\value{
x variable with some NA Values
}
\description{
Given a factor variable x, create an indicator matrix of dimension
length(x) x (nlevels(x)-1) dropping the column corresponding to the
}
\examples{

factor2ind(Sex,"M")
factor2ind(Sex)


x = gl(4, 2, labels = LETTERS[1:4])
factor2ind(x)
factor2ind(x, "C")
}
