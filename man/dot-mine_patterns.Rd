% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mine_patterns.R
\name{.mine_patterns}
\alias{.mine_patterns}
\title{Auxiliary function minePatterns (using FP-growth algorithm)}
\usage{
.mine_patterns(data, support = 0.05, max_length = 3)
}
\arguments{
\item{data}{a data frame containing the variables in the model}

\item{support}{the minimum support of the pattern}

\item{max_length}{the maximum length of the output patterns}
}
\value{
a list of all patterns in the given data, indexed by lengths
}
\description{
The function uses fp-growth algorithm from rCBA package to mine all frequent
patterns from the given data. The user can provide minimum pattern support -
the proportion of the data that have such pattern.
The function returns the patterns in the form of a list
indexed by the pattern lengths.
}
\keyword{internal}
