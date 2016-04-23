#define the class mc with parameter pijdef as transition matrix; type = 0 means discrete;
#type 1 means continue
setClass("mc", representation(pijdef = "matrix", qidef = "vector"))

#set method function to calculate pi
setGeneric("findpi", function(z, k)
  attributes(z, k))
setMethod("findpi", "mc", function(z, k) {
  truncatedM <- z@pijdef[1:k, 1:k]
  imp <- diag(k) - t(truncatedM)
  imp[k,] <- rep(1, k)
  rhs <- c(rep(0, k - 1), 1)
  solve(imp, rhs)
})

#set method function to calculate the stationary Distribution
setGeneric("stn", function(z, e = 2, dif = 0.00001, sum_pi = 0.9999)
  attributes(z, e, dif, sum_pi))
setMethod("stn", "mc", function(z, e = 2, dif = 0.00001, sum_pi = 0.9999) {
  n <- nrow(z@pijdef)
  #finite state space
  if (n <= 100) {
    return(findpi(z, n))
  }
  # infinite state space
  else {
    k <- 10
    resultA <- findpi(z, k)
    k <- e * k
    resultB <- findpi(z, k)
    
    # calcuate difference
    differ <- 0
    for (i in 1:length(resultA)) {
      differ <- max(differ,(abs(resultA[i] - resultB[i])) / (resultA[i]))
    }
    # verify the sum of pi
    sumpi <- sum(resultB[1:length(resultA)])
    while (differ > dif || sumpi < sum_pi) {
      resultA <- resultB
      k <- e * k
      if (k >= n) {
        break
      }
      
      resultB <- findpi(z, k)
      differ <- 0
      for (i in 1:length(resultA)) {
        differ <- max(differ,(abs(resultA[i] - resultB[i])) / (resultA[i]))
      }
      sumpi <- sum(resultB[1:length(resultA)])
    }
    
    return(resultB)
  }
})

#set method to calculate the expected hitting time
setGeneric("exphit", function(z, i = 1, j = 1)
  attributes(z, i, j))
setMethod("exphit", "mc", function(z, i = 1, j = 1) {
  if (i == j) {
    return(0)
  }
  
  n <- nrow(z@pijdef)
  q <- diag(n) - z@pijdef
  
  if (i > n) {
    return("i out of bound")
  }
  
  if (i == 1) {
    q <- q[(2:n), (2:n)]
  } else if (i != n) {
    q <- q[c((1:(i - 1)),(i + 1):n), c((1:(i - 1)),(i + 1):n)]
  } else {
    q <- q[1:(n - 1), 1:(n - 1)]
  }
  ones <- rep(1, n - 1)
  t <- solve(q, ones)
  if (j > length(t) + 1) {
    return("j out of bound")
  }
  if (i < j)
    return(t[j - 1])
  else
    return(t[j])
})


#rowsb function calculates the number of rows and colums for the Q matrix based on all combinations of i+j<= b
setGeneric("rowsb", function(z, b)
  attributes(z, b))
setMethod("rowsb", "mc", function(z, b) {
  for (g in 1:(b + 1)) {
    r = r + g
  }
  return(r)
})

# calculate Q matrix
setGeneric("findq", function(z)
  attributes(z))
setMethod("findq", "mc", function(z) {
    rownumbers <- rowsb(z, b)
    Q = matrix(data = 0, nrow = rownumbers, ncol = rownumbers) #initializes the infinitesimal generator matrix Qij
    for (x in 1:rownumbers) {
      for (y in 1:rownumbers) {
        if (x == y) {
          Q[x, y] = (-1) * (z@qidef[x])
        } else {
          Q[x, y] = (z@qidef[y]) * (z@pijdef[y, x])
        }
      }
    }
    return(Q)
})

#function to find pi vector
setGeneric("findpicontin", function(z)
  attributes(z))
setMethod("findpicontin", "mc", function(z) {
  q <- findq(z)
  n <- nrow(q)
  q[n,] = rep(1, n)
  rhs <- c(rep(0, n - 1), 1)
  pivec <- solve(q, rhs)
  return(pivec)
})

#function to find declined calls
setGeneric("finddeclined", function(z, b)
  attributes(z, b))
setMethod("finddeclined", "mc", function(z, b) {
  q <- findpicontin(z)
  c = 0
  d = 0
  k = rep(0, rowsb(z, b))
  for (z in 1:rowsb(z, b)) {
    if (c + d < b) {
      d = d + 1
    } else if (c + d == b) {
      #only use pi values when the system is full (i+j=b)
      k[z] = q[z]
      c = c + 1
      d = 0
    }
  }
  return(sum(k))
})

#function to calculate proportion of calls resulting in customer hanging up
setGeneric("callsleavingqueue", function(z, w, u, b)
  attributes(z, w, u, b))
setMethod("callsleavingqueue", "mc", function(z, w, u, b) {
  q <- findpicontin(z)
  c = 0
  d = 0
  k = rep(0,rowsb(z, b))
  g = rep(0,rowsb(z, b))
  h = rep(0,rowsb(z, b))
  for (z in 1:rowsb(z, b)) {
    if (c + d < b) {
      if (c > s &
          c <= b) {
        #use pi values when i > s, meaning at least 1 call is in the queue
        h[z] = q[z]
        h[z] = (c - u) * h[z]
      }
      d = d + 1
    }  else if (c + d == b) {
      if (c > u &
          c <= b) {
        #use pi values when i > s, meaning at least 1 call is in the queue
        h[z] = q[z]
        h[z] = (c - u) * h[z]
      }
      c = c + 1
      d = 0
    }
  }
  total = (w / alpha) * sum(h)#all states must be multiplied by the individual rate of leaving the queue = omega
  return(total)
})
