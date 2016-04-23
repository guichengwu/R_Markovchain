\name{stn-methods}
\docType{methods}
\alias{stn-methods}
\alias{stn,mc-method}
\alias{stn,k-method}
\title{Methods for Function \code{stn}}
\description{
Methods for claculting the stationary distribution, considering both finite state and infinite state
}
\section{Methods}{
\describe{
\item{\code{signature(z = "mc")}}{
 x parameter means the object of class "mc"
}
\item{\code{signature(e = "ANY")}}{
 e parameter means the increase rate of k
}
\item{\code{signature(dif = "ANY")}}{
 dif parameter means difference parameter
}
\item{\code{signature(sum_pi = "ANY")}}{
 sum_pi parameter means sum of pi, it should be close to 1
}
}}
\keyword{stationary distribution}
