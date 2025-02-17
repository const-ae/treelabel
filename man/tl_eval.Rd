% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tl_eval.R
\name{tl_eval}
\alias{tl_eval}
\title{Evaluate an expression in the tree label environment}
\usage{
tl_eval(x, expr, check_bounds = TRUE)
}
\arguments{
\item{x}{\code{treelabel} vector}

\item{expr}{an unquoted expression that is evaluated in the treelabel
environment. Can, for example, be used to check if a particular
score is higher than a threshold}

\item{check_bounds}{flag that indicates if}

\item{...}{multiple \code{expr}.

The \code{expr} can refer to two additional values:
\itemize{
\item \code{.tl} a data pronoun that can be used to refer to nodes in the
tree (e.g., \code{tl_eval(x, .tl$Bird)} instead of \code{tl_eval(x, Bird)})
\item \code{.scores} a reference to the \code{tl_score_matrix}.
}

\strong{Note}: Do not perform any stateful calculation in \code{expr} as it
is evaluated multiple times if \code{check_bounds == TRUE}.}
}
\value{
\code{tl_eval} returns the result of evaluating \code{expr}.
\code{tl_eval_multi} returns a  \code{tibble} where each
\code{expr} is one column.
}
\description{
Evaluate an expression in the tree label environment
}
