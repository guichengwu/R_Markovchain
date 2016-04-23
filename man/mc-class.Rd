\name{mc-class}
\Rdversion{1.1}
\docType{class}
\alias{mc-class}

\title{Class \code{"mc"}}
\description{
The mc class is a markov chain class, considering both discrete time and continous time.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("mc")}.
mymc <- new("mc") 
}
\section{Slots}{
  \describe{
    \item{\code{pijdef}:}{Object of class \code{"matrix"}}
    \item{\code{qidef}:}{Object of class \code{"vector"}}
  }
}
\section{Methods}{
  \describe{
    \item{callsleavingqueue}{\code{signature(z = "mc")}: ... }
    \item{exphit}{\code{signature(z = "mc")}: ... }
    \item{finddeclined}{\code{signature(z = "mc")}: ... }
    \item{findpi}{\code{signature(z = "mc")}: ... }
    \item{findpicontin}{\code{signature(z = "mc")}: ... }
    \item{findq}{\code{signature(z = "mc")}: ... }
    \item{rowsb}{\code{signature(z = "mc")}: ... }
    \item{stn}{\code{signature(z = "mc")}: ... }
	 }
}
\references{
Matloff, Norm. "From Algorithm to Z-Scores: Probabilistic and Statistical Modeling in Computer Science." (2009).
}
\author{
  Guicheng Wu, Eric Sturzinger, Rafael Braz Reboucas Lourenco
}
\examples{
showClass("mc")
}
\keyword{classes}
