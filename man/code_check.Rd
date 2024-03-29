% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_edit.R
\name{code_check}
\alias{code_check}
\title{Check R code for issues}
\usage{
code_check(
  code,
  additional_instructions = NULL,
  explain = TRUE,
  simplify = TRUE,
  ...
)
}
\arguments{
\item{code}{R code to be edited}

\item{explain}{Logical, indicating whether to include explanation messages}

\item{...}{Additional arguments to pass to \code{\link{create_chat_completion}}}
}
\value{
A \code{\link{TextEditing}} object
}
\description{
This function checks R code for any issues present in the code.
}
