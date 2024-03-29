% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rclust.R
\name{rhclust}
\alias{rhclust}
\title{Route Clustering}
\usage{
rhclust(pdist, ptime, weight, data)
}
\arguments{
\item{pdist}{A name.}

\item{ptime}{"continous" or "discrete".}

\item{weight}{density function.}

\item{data}{support.}
}
\value{
An hclust class.
}
\description{
Route hclust function, see \href{https://tinyurl.com/rclustering-essay}{this essay} for more details.

OOP.
}
\examples{
system.time(temp <- rhclust(pdis, ptim, 0, data=rdata))
cutree(temp,5)
plot(temp)
rect.hclust(temp, k=5, border=2:6)

test_path <- c('p2','p1','d2','d1')
test_weight = 0.5
cost(test_path, pdis, ptim, test_weight, data=rdata)
}
