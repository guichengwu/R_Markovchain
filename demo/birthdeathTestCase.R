# birth death model
birthdeathmodel <- function(states, birth=0.1, death=0.2) {
  result <- matrix(rep(0, states^2), nrow = states)
  for (i in 1:states) {
    for (j in 1:states) {
      if (i == 1) {
        d = 0
      } else {
        d = death
      }
      if (j == i+1) {
        result[i, j] = birth
      } else if (j == i-1) {
        result[i, j] = d
      } else if (j == i) {
        result[i, j] <- 1-birth-d
      } else {
        result[i, j] <- 0
      }
    }
  }
  return(result)
}

#infinite state space Test case : use the birth death model as test case
mymc <- new("mc")
mymc@pijdef <- birthdeathmodel(1000)
#call the stationary distribution method
stn(mymc, 1.2, 0.001, 0.99)
#call the expected hitting time method
exphit(mymc, 4, 10)