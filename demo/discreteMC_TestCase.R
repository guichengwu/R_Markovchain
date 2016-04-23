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
stn(mymc, 1.2, 0.01, 0.99)
# call the expected hitting time method, the method can get the expected hitting time from state 1 to state 3
exphit(mymc, 3, 2)
