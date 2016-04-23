
# Call Center Simulation
# alpha: Arrival rate
# sigma: Ordinary calls service rate
# mu: Complex calls service rate 
# omega: Waiting patience "rate"

# Example use:
# s = 5 servers
# b = 8 available in spaces in the system
# p = 0.1 proportion of complex calls
# alpha = 4 arrivals per hour
# sigma = 2 ordinary clients served per hour
# mu = 0.2 complex call solved per hour
# meanWait = 10 (omega = 0.1) average amount of hours clients are likely to wait
# All distributions exponential (if not, add optional paramenters), no debugging, and 50000 time limit:
# simCallCenter(50000, 8, 5, 0.1, 4, 2, 0.2, 10)

simCallCenter <- function(timelim, b, s, p, alpha, sigma, mu, meanWait, arrDist = 1, ordCallDist = 1, compCallDist = 1, waitTimeDist = 1, dbg=FALSE, dbgHandler = FALSE) {
   omega <- 1/meanWait
   # set up structures
   simCC <- newsim(dbg)
   simCC$dbgEvnt <- dbgHandler
   simCC$blockedMsg = "Try again later"
   simCC$reactevent <- callCenterReact  
   simCC$arrvrate <- alpha
   simCC$ordsrvrate <- sigma
   simCC$compsrvrate <- mu
   simCC$waitrate <- omega
   simCC$bufferSize <- b
   simCC$currBuff <- 0
   simCC$servedClients <- 0
   simCC$offeredClients <- 0
   simCC$blockedClients <- 0
   simCC$tiredCustomers <- 0
   simCC$busyMgr <- FALSE
   simCC$queue <- newqueue(5)
   simCC$managerQueue <- newqueue(5)
   simCC$numOfSrvs <- s
   simCC$busySrvs <- 0
   simCC$complexProportion <- p
   #simCC$totWaitServedClients <- 0
   #simCC$totWaitServedComplexClients <- 0
   #simCC$totWaitTiredClients <- 0
   
   # defining job numbers is good practice, always invaluable during
   # debugging
   simCC$jobnum <- 0
   
   # event type codes: 1 for arrival, 2 for service completion, 3 for customer leaving
   simCC$arrvevnt <- 1
   simCC$srvevnt <- 2
   simCC$srvevntMgr <- 3
   
   simCC$arrRand <- switch(arrDist,
                           rexp, # Exponential Distribution
                           rweibull, # Weibull Distribution
                           rlnorm, # Lognormal Distribution
                           runif, # Uniform Distribution
                           rgamma # Gamma Distribution
   )
   
   simCC$ordSrvcRand <- switch(ordCallDist,
                               rexp, # Exponential Distribution
                               rweibull, # Weibull Distribution
                               rlnorm, # Lognormal Distribution
                               runif, # Uniform Distribution
                               rgamma # Gamma Distribution
   )
   
   simCC$compSrvcRand <- switch(compCallDist,
                                rexp, # Exponential Distribution
                                rweibull, # Weibull Distribution
                                rlnorm, # Lognormal Distribution
                                runif, # Uniform Distribution
                                rgamma # Gamma Distribution
   )
   
   simCC$waitTimeRand <- switch(waitTimeDist,
                                rexp, # Exponential Distribution
                                rweibull, # Weibull Distribution
                                rlnorm, # Lognormal Distribution
                                runif, # Uniform Distribution
                                rgamma # Gamma Distribution
   )
   
   timeto1starrival <- simCC$arrRand(1,simCC$arrvrate)
   jobnum <- incremjobnum(simCC)
   
   schedevnt(timeto1starrival,simCC$arrvevnt,simCC,
             c(timeto1starrival,jobnum,timeto1starrival+simCC$waitTimeRand(1,simCC$waitrate)))
   
   # start sim
   mainloop(simCC,timelim)

   # sim done, subtract the residual buffer from the total treated clients
   simCC$offeredClients <- simCC$offeredClients - simCC$currBuff
   
   cat("Results (out of ",simCC$offeredClients,"):\n")
   cat("Served clients: ", simCC$servedClients,"(",simCC$servedClients/simCC$offeredClients*100,"%)\n")
   cat("Blocked clients: ", simCC$blockedClients,"(",simCC$blockedClients/simCC$offeredClients*100,"%)\n")
   cat("Tired clients: ", simCC$tiredCustomers,"(",simCC$tiredCustomers/simCC$offeredClients*100,"%)\n")
   
}

incremjobnum <- function(simlist) {
  jobnum <- simlist$jobnum + 1
  simlist$jobnum <- jobnum
  jobnum
}

# what new events are triggered by the occurrence of an old one?
callCenterReact <- function(evnt,simCC) {
   etype <- evnt[2]
   if (simCC$dbgEvnt) print(evnt)
   if (etype == simCC$arrvevnt) {  # client arrival
     simCC$offeredClients <- simCC$offeredClients + 1
     # schedule next arrival
     timeofnextarrival <- simCC$currtime + simCC$arrRand(1, simCC$arrvrate)
     waitTime <- simCC$waitTimeRand(1,simCC$waitrate)
     jobnum <- incremjobnum(simCC)
     schedevnt(timeofnextarrival,simCC$arrvevnt,simCC,
               c(timeofnextarrival,jobnum,timeofnextarrival+waitTime))
     
     if ((simCC$currBuff + 1) <= simCC$bufferSize) {
        simCC$currBuff <- simCC$currBuff + 1
        if (simCC$dbgEvnt) cat("Next job: ",jobnum," time of next arrival: ",timeofnextarrival," wait time: ",timeofnextarrival+waitTime,"\n")
        
        # start newly-arrived job or queue it in the arrivals queue
        if (simCC$busySrvs < simCC$numOfSrvs) {  # server free, start job service
          # if the server is free, determine whether the call is complex or not
          coinToss <- runif(1)
          if (coinToss > simCC$complexProportion) { # 1-p percent of calls are ordinary
            simCC$busySrvs <- simCC$busySrvs + 1 # increment number of busy servers
            srvduration <- simCC$ordSrvcRand(1, simCC$ordsrvrate) # ordinary call
            schedevnt(simCC$currtime+srvduration,simCC$srvevnt,simCC,evnt[3:5])  # copy over previous data for this job
            if (simCC$dbgEvnt) print("client in server")
            if (simCC$dbgEvnt) cat("busy servers: ",simCC$busySrvs)
          } else { # send job to manager
            if (!simCC$busyMgr) { # manager is free
              simCC$busyMgr <- TRUE
              srvduration <- simCC$compSrvcRand(1, simCC$compsrvrate) # complex call
              schedevnt(simCC$currtime+srvduration,simCC$srvevntMgr,simCC,evnt[3:5])  # copy over previous data for this job
              if (simCC$dbgEvnt) print("client in mgr")
            } else { #server is free but manager is not, queue client
              if (simCC$dbgEvnt) print("client added to manager queue")
              appendfcfs(simCC$managerQueue,evnt)
            }
          }
        } else {  # server busy, add job to queue (whether it is complex or not)
          if (simCC$dbgEvnt) print("client added to queue")
          appendfcfs(simCC$queue,evnt)
        }
      } else {
        # no place on system, block client
        if (simCC$dbgEvnt) cat("Number of clients in system: ", simCC$currBuff," max size of buffer: ",simCC$bufferSize,"\n")
        print(simCC$blockedMsg)
        simCC$blockedClients <- simCC$blockedClients + 1
      }
   } else if (etype == simCC$srvevnt) {  # ordinary job completion
      # a server becomes free, one more completed service
      simCC$servedClients <- simCC$servedClients + 1
      simCC$busySrvs <- simCC$busySrvs - 1
      simCC$currBuff <- simCC$currBuff - 1
      pushFromQ <- TRUE
      
      # check queue for waiting jobs
      while ((nrow(simCC$queue$m) > 0) && (pushFromQ)) {  # nonempty queue
       # while loop because if the next job is complex, then send it to the manager/managerQueue and receive the following
       # in addition, if next job got tired, get the following
       qhead <- delfcfs(simCC$queue)
       if (simCC$dbgEvnt) print("head of the queue:")
       if (simCC$dbgEvnt) print(qhead)
       if (qhead[5] > simCC$currtime) {
         # start job service
         coinToss <- runif(1)
         if (coinToss > simCC$complexProportion) { # 1-p percent of calls are ordinary
           simCC$busySrvs <- simCC$busySrvs + 1 # increment number of busy servers
           srvduration <- simCC$ordSrvcRand(1, simCC$ordsrvrate) # ordinary call
           schedevnt(simCC$currtime+srvduration,simCC$srvevnt,simCC,qhead[3:5])  # copy over previous data for this job
           #break
           if (simCC$dbgEvnt) print("client in server")
           if (simCC$dbgEvnt) cat("busy servers: ",simCC$busySrvs)
           pushFromQ <- FALSE
         } else { # send job to manager (complex job)
           if (!simCC$busyMgr) { # manager is free
             simCC$busyMgr <- TRUE
             srvduration <- simCC$compSrvcRand(1, simCC$compsrvrate) # complex call
             schedevnt(simCC$currtime+srvduration,simCC$srvevntMgr,simCC,qhead[3:5])  # copy over previous data for this job
             if (simCC$dbgEvnt) print("client in mgr")
           } else { # a server is free but manager is not, queue client
             if (simCC$dbgEvnt) print("client added to manager queue")
             appendfcfs(simCC$managerQueue,qhead)
           }
         }
       } else {
         #customer got tired of waiting in the arrivals queue (whether it was complex or not)
         simCC$tiredCustomers <- simCC$tiredCustomers + 1
         simCC$currBuff <- simCC$currBuff - 1
         if (simCC$dbgEvnt) cat("Customers left: ",simCC$tiredCustomers," current buffer: ",simCC$currBuff,"\n")
       }
      }
    } else if (etype == simCC$srvevntMgr) {  # manager job completion
      # clients in the manager queue dont get tired
      # the manager becomes free, one more completed service, and one less client in the system
      simCC$servedClients <- simCC$servedClients + 1
      simCC$busyMgr <- FALSE
      simCC$currBuff <- simCC$currBuff - 1
      #print(simCC$currBuff)
      
      if (nrow(simCC$managerQueue$m) > 0) { #non-empty manager queue
        simCC$busyMgr <- TRUE
        qhead <- delfcfs(simCC$managerQueue)
        if (simCC$dbgEvnt) print("mgr queue sent to mgr")
        srvduration <- simCC$compSrvcRand(1, simCC$compsrvrate) # complex call
        schedevnt(simCC$currtime+srvduration,simCC$srvevntMgr,simCC,qhead[3:5])  # copy over previous data for this job
      }
    }
   if (simCC$dbgEvnt) cat("Current buffer:", simCC$currBuff,"\n")
}
