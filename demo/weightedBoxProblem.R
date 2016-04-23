weightedBoxProblem <- function(wmax) {
  d = 2 / (wmax + 1);
  p = matrix(rep(0,wmax ^ 2),nrow = wmax)
  for (i in 1:wmax) {
    for (j in 1:wmax) {
      if (j <= i && i + j >= wmax + 1) {
        p[i,j] = d * j / wmax
      }
      if (i + j <= wmax && j > i) {
        p[i,j] = d * (j - i) / wmax
      }
      if (i < j && i + j >= wmax + 1) {
        p[i,j] = d * (2 * j - i) / wmax
      }
    }
  }
  return(p)
}
#Test case
mymc <- new("mc")
mymc@pijdef <- weightedBoxProblem(10)
#call the stationary distribution method
stn(mymc)
#call the expected hitting time method
exphit(mymc, 9, 10)
