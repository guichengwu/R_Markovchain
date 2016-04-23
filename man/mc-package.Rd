\name{mc-package}
\alias{mc-package}
\docType{package}
\title{
\packageTitle{mc}
}
\description{
\packageDescription{mc}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{mc}
\packageIndices{mc}
First, we should new a object of the class. e.g. mymc <- new("mc").
Then the user can define its transition matrix m, giving m as parameter to the object, e.g. mymc@pijdef <- m.
Finally, we can call the function. e.g. stn(mymc).
}
\author{
\packageAuthor{mc}

Maintainer: \packageMaintainer{mc}
}
\keyword{ package }
\examples{
#Test case 1:finite state space with trasition matrix
mymc <- new("mc")
m <- matrix(rep(0, 9) , nrow=3)
m[1, 1] <- 0.5
m[1, 2] <- 0.5
m[2, 3] <- 0.5
m[2, 1] <- 0.5
m[3, 1] <- 1
mymc@pijdef <- m
# call the stationary distribution method
stn(mymc)
# call the expected hitting time method
exphit(mymc, 1, 3)
}
