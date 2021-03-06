% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unclean_column_names.R
\name{clean_column_names}
\alias{clean_column_names}
\title{A version of \code{\link[janitor:clean_names]{janitor::clean_names()}} that also cleans dplyr groups}
\usage{
clean_column_names(data)
}
\arguments{
\item{data}{A data frame}
}
\value{
A data frame
}
\description{
With dplyr >= 0.8.99.9002 \code{janitor::clean_names()} seems to also clean groups
(\href{https://gist.github.com/maurolepore/6558292c5003a15e246a88091c307b65}{gist}).
}
\examples{
library(dplyr)

clean_column_names(group_by(tibble(x.x = 1), x.x))
}
\seealso{
\code{\link[janitor:clean_names]{janitor::clean_names()}}, \code{\link[=unclean_column_names]{unclean_column_names()}}.

Other functions to deal with unclean names: 
\code{\link{clean_quo}()},
\code{\link{quo_chr}()},
\code{\link{unclean_column_names}()}
}
\concept{functions to deal with unclean names}
