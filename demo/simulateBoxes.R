# Call x <- simulateBoxes(1000) to get the Pi Vector with 1k iterations.
# Call x <- simulateBoxes(1000, 1) to get the average weight per closed box with 1k iterations.
# Call x <- simulateBoxes(1000, 2) to get the average number of items per box 1k iterations.
# Call x <- simulateBoxes(1000, 3) to get the average weight of the opened box with 1k iterations.

simulateBoxes <- function(iterations, ret = 0) {
  maxWeight <- 10
  
  # Generate weights: runif() generate random numbers between 0 and 1.
  weights <- (runif(iterations, 0, 1))
  
  # Initiating variables and Pi Vector
  currWeight <- 0
  totalWeight <- 0
  numOfBoxes <- 1
  pi <- rep(0, 10)
  probs <- rep(0, 10)
  
  # Iterate through all boxes
  for (w in weights) {
    
    if (w < 2/110) w = 1
    else if (w < (3*2/110) ) w = 2
    else if (w < (6*2/110) ) w = 3
    else if (w < (10*2/110) ) w = 4
    else if (w < (15*2/110) ) w = 5
    else if (w < (21*2/110) ) w = 6
    else if (w < (28*2/110) ) w = 7
    else if (w < (36*2/110) ) w = 8
    else if (w < (45*2/110) ) w = 9
    else w = 10
    
    probs[w] <- probs[w] + 1
    
    # If current weight of box plus new item is heavier than maxWeight, get new box
    if  ( (w + currWeight) > maxWeight ) {
      numOfBoxes <- numOfBoxes + 1
      currWeight <- w
      pi[currWeight] <- pi[currWeight] + 1
    } else if ( (w + currWeight) == maxWeight ) {
      currWeight <- currWeight + w
      numOfBoxes <- numOfBoxes + 1
      pi[currWeight] <- pi[currWeight] + 1
      currWeight <- 0
    } else {
      currWeight <- currWeight + w
      pi[currWeight] <- pi[currWeight] + 1
    }
    
    totalWeight <- totalWeight + w
    
  }
  
  #print(probs/iterations)
  
  # Returns
  if (ret == 0) return(pi/iterations) 
  
  if (ret == 1) {
    averageWeightClosedBoxes <- totalWeight / numOfBoxes
    return(averageWeightClosedBoxes)
  }
  
  if (ret == 2) {
    averageItems <- iterations / numOfBoxes
    return(averageItems)
  }
  
  if (ret == 3) {
    averageWeightOpenBox <- 0
    i <- 0
    piVect <- pi/iterations
    for (p in piVect) {
      i <- i + 1
      averageWeightOpenBox <- averageWeightOpenBox + p*i
    }
    
    return(averageWeightOpenBox)
  }
  
}