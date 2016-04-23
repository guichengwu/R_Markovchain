#the user will supply a function qidef() that similarly specifies the holding-time rates
qidef <- function(z,q,sigma,alpha,mu,omega,s,b) {
  Q <- rep(0, rowsb(z, b), nrow = 1)
  ix = 0
  jx = 0
  iy = 0
  jy = 0
  x = 1
  y = 1
  for (x in 1:rowsb(z, b)) {
    #generate diagonal matrix values = -(lambda_i)
    if (ix == 0 && jx == 0) {
      Q[x] = alpha
    }
    if (jx == 0 && ix <= s && ix >= 0) {
      Q[x] = alpha + ix * sigma
    }
    if (ix == 0 && jx <= b && jx >= 1) {
      Q[x] = alpha + mu 
    }
    if (ix >= 1 && ix <= s & jx >= 1 & jx < b - ix) {
      Q[x] = alpha + ix * sigma + mu 
    }
    if (ix > s && ix < b && jx == 0) {
      Q[x] = alpha + s * sigma + (ix - s) * omega
    }
    if (jx == 1 && ix < b - 1 && ix > s) {
      Q[x] = alpha + s * sigma + (ix - s) * omega + mu
    }
    if (jx >= 1 && ix > s && ix <= b - jx) {
      Q[x] = alpha + s * sigma + (ix - s) * omega + mu
    }
    if (jx == 0 && ix == b) {
      Q[x] = s * sigma + (ix - s) * omega
    }
    if (jx == 1 && ix == b - 1) {
      Q[x] = s * sigma + (ix - s) * omega + mu
    }
    if (ix >= s + 1 && ix <= b - 1 && jx == b - ix) {
      Q[x] = s * sigma + (ix - s) * omega + mu
    }
    if (ix >= 1 && ix <= s && jx + ix == b) {
      Q[x] = ix * sigma + mu
    }
    if (ix == 0 && jx == b) {
      Q[x] = mu
    }
    if (ix + jx < b) {
      #map matrix row indices to i,j
      jx = jx + 1
    } else if (ix + jx == b) {
      ix = ix + 1
      jx = 0
    }
  }
  return(Q)
}
#the user provide the pijdef function
pijdef <- function(z,b) {
  #claculate rate of holding time
  #qide <- qiDefinition(mycontinuemc, q,sigma,alpha,mu,omega,s,b)
  
  P = matrix(data = 0, nrow = rowsb(z, b), ncol = rowsb(z, b)) #initializes the infinitesimal generator matrix Qij
  ix = 0
  jx = 0
  iy = 0
  jy = 0
  x = 1
  y = 1
  
  for (x in 1:rowsb(z, b)) {
    for (y in 1:rowsb(z, b)) {
      if (x != y) {
        if (jx >= 1 &&
            jx < b - ix &&
            jy == jx - 1 && ix >= 0 && ix < s && iy == ix) {
          P[y, x] = (alpha * q) / z@qidef[y]
        }
        if (ix >= 1 &&
            ix <= s &&
            iy == ix - 1 && jx >= 0 && jx <= b - ix && jx == jy) {
          P[y, x] = (alpha * (1 - q)) /z@qidef[y]
        }
        if (ix == iy &&
            iy >= 0 &&
            iy <= b - jy && jx >= 0 && jx <= b && jy == jx + 1) {
          P[y, x] = mu /z@qidef[y]
        }
        if (iy >= s &&
            iy < b &&
            ix == iy + 1 && jx >= 0 && jx <= b - ix && jy == jx) {
          P[y, x] = alpha /z@qidef[y]
        }
        if (ix >= s &&
            ix <= b - 1 &&
            iy == ix + 1 && jy >= 0 && jy <= b - iy && jx == jy) {
          P[y, x] = (s * sigma * (1 - q) + (iy - s) * omega)/z@qidef[y]
        }
        if (ix >= 0 &&
            ix <= s - 1 &&
            ix + 1 == iy && jy >= 0 && jy <= b - iy && jy == jx) {
          P[y, x] = (sigma * iy)/z@qidef[y]
        }
        if (iy > s &&
            iy <= b &&
            jy >= 0 &&
            jy <= b - iy &&
            ix >= s && ix <= iy - 2 && jx == jy + iy - ix - 1) {
          P[y, x] = (s * sigma * (1 - q) * (q ^ (jx - jy))) /z@qidef[y]
        }
        if (iy > s &&
            iy <= b &&
            jy >= 0 &&
            jy <= b - iy && ix == s - 1 && jx == iy - s + jy) {
          P[y, x] = (s * sigma * (q ^ (iy - s))) / z@qidef[y]
        }
      }
      if (iy + jy < b) {
        #map matrix column indices to i, j
        jy = jy + 1
      } else if (iy + jy == b) {
        iy = iy + 1
        jy = 0
      }
    }
    iy = 0
    jy = 0
    if (ix + jx < b) {
      #map matrix row indices to i,j
      jx = jx + 1
    } else if (ix + jx == b) {
      ix = ix + 1
      jx = 0
    }
  }
  return(P)
}

q=.1  #proportion of calls that are complex
sigma=2 #service rate of ordinary calls
alpha = 4 #total arrival rate of all calls
mu=.2 # service rate for complex calls
omega=.1 # rate of callers in the queue hanging up
s=5; # number of servers for ordinary calls
b=8;#maximum calls in the system (bufffer)
r=0

q2=.1
sigma2=2
alpha2=4
mu2=.2
omega2=.5
s2=4
b2=6
mycontinuemc <- new("mc")
mycontinuemc@qidef <- qidef(mycontinuemc,q,sigma,alpha,mu,omega,s,b)
mycontinuemc@pijdef <- pijdef(mycontinuemc,b)
qMatrix <- findq(mycontinuemc)
#find pi vector
findpicontin(mycontinuemc)

print(paste0("Proportion of Calls Declined = ", finddeclined(mycontinuemc,b)))
#show answer for decliend calls for specific parameters
print(paste0("Proportion of Calls Resulting Customers Hanging Up = ", callsleavingqueue(mycontinuemc,omega,s,b)))