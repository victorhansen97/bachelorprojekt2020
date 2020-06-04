rm(list=ls())

library(ggplot2)
library(zoo)
library(timeDate)
library(reshape2)

## Functions used
d1 <- function(S,K,r,q,sigma,tau){
  (log(S/K)+(r-q)*tau + 1/2*sigma^2*tau)/(sigma*sqrt(tau))
}

d2 <- function(S,K,r,q,sigma,tau){
  d1(S,K,r,q,sigma,tau) - sigma*sqrt(tau)
}


# Black-Scholes Price function
C <- function(S,K,r,q,sigma,tau,option="Call"){
  d1 <- d1(S,K,r,q,sigma,tau)
  d2 <- d2(S,K,r,q,sigma,tau)
  
  if (option=="Call"){
    result <- exp(-q*tau)*S*pnorm(d1) - K*exp(-r*tau)*pnorm(d2)
  }
  
  if (option=="Put"){
    result <-  exp(-q*tau)*S*pnorm(d1) - K*exp(-r*tau)*pnorm(d2) -exp(-q*tau)*S + K*exp(-r*tau)
  }
  result
}
C <- Vectorize(C)

# Black-Scholes Delta
BS.Delta <- function(S,K,r,q,sigma,tau,option="Call"){
  
  if (option == "Call"){
    result <-  exp(-q*tau)*pnorm(d1(S,K,r,q,sigma,tau))
  }
  
  if (option == "Put"){
    result <-  exp(-q*tau)*(pnorm(d1(S,K,r,q,sigma,tau))-1)
  }
  
  result
}

BS.Delta <- Vectorize(BS.Delta)

gamma <- function(S,K,r,q,sigma,tau) {
  dnorm(d1(S,K,r,q,sigma,tau))/(S*sigma*sqrt(tau))
}

gamma <- Vectorize(gamma)


## delta hedging strategy

delta_hedge_error <- function(S,K,r,q,sigma, tau, call_price, strategy=-1,option="Call"){
  
  delta   <- BS.Delta(S=S,K=K,r=r,q=q, sigma=sigma,tau=tau, option=option)
  gamma <- gamma(S=S,K=K,r=r,q=q, sigma=sigma,tau=tau)
  
  n            <- length(S[-length(S)])
  
  V            <- numeric(n+1); V[1] <- call_price[1] # Value process 
  Pi           <- numeric(n+1);Pi[1]  <- 0  ### Path of PL/Hedging Error
  dPi          <- numeric(n+1);dPi[1] <- 0  ### Hedging error over time interval
  
  if (length(r)==1){r<-rep(r,n)}
  if (length(q)==1){q<-rep(q,n)}
  
  for (i in 1:n){
    dt      <- tau[i] - tau[i+1] 
    B       <- Pi[i] + strategy*delta[i]*S[i] - strategy*call_price[i] # Rebalance with money account
    V[i+1] <- exp(r[i]*dt)*B - strategy*delta[i]*exp(q[i]*dt)*S[i+1] # Mark-to-market for portfolio
    Pi[i+1] <- V[i+1] + strategy*call_price[i+1] # Mark-to-market for adjusted portfolio/cumulated hedging error
    dPi[i+1] <- Pi[i+1] - Pi[i] # 
  }
  output=list(pf_value= V, error_cum = Pi, error_dt = dPi, delta=delta, gamma=gamma) # Save output
}

############### reading data

calldata <- read.table("calldata.txt")
head(calldata)

# for (i in 1:37){
#   i <- length(calldata$S[calldata$Option == i])
#   print(i)
# }

#VIX
library(quantmod)

env_vix <- new.env()

getSymbols("^VIX", env = env_vix , src = "yahoo", from = as.Date("2004-04-01"), to = as.Date("2013-07-04"), warnings = F)

vix <- env_vix$VIX

vix <- data.frame(Date = as.Date(index(vix)), vix = as.vector(vix$VIX.Close/100))
# dim(vix)

#merging vix with data
calldata$Date <- as.Date(calldata$Date)
vixmerge <- merge(x=calldata, y=vix, by='Date', all.x = TRUE)

#sum(is.na(vixmerge$vix))
# vixmerge$Date[which(is.na(vixmerge$vix) == TRUE)]

vixmerge$vix <- na.approx(vixmerge$vix) #zoo

calldata <- vixmerge

# calculating no-dividend prices
calldata$Cnod <- C(calldata$S,calldata$K,calldata$r,q=0,calldata$sigma,calldata$tau,option="Call")
# adding mean price coloumn (mean of initial price for all options)
calldata$meanpricenod <- mean(aggregate(calldata$Cnod, by=list(calldata$Option), FUN=first)$x)
# adding mean of ALL rates
calldata$meanrate <- mean(calldata$r)
