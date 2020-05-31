rm(list=ls())

library(ggplot2)
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

## delta hedging strategy

delta_hedge_error <- function(S,K,r,q,sigma, tau, call_price, strategy=-1,option="Call"){
  
  delta   <- BS.Delta(S=S,K=K,r=r,q=q, sigma=sigma,tau=tau, option=option)
  
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
  output=list(pf_value= V, error_cum = Pi, error_dt = dPi, delta=delta) # Save output
}



# Model parameters
mu <- 0.07 # drift
sigma <- 0.2 # true vol

# Market parameters
q <- 0 # no dividends
r <- 0.02 # short rate
S0 <- 100 # start spot
K <- 100 # Initially at-the-money
sigmai <- 0.3 # implied vol

# Time grid
T <- 1/4
N <- 2500
t <- seq(0,T,length=N+1) # N intervals
tau <- T -t # time to maturity
dt <- T/N   # Equidistant time step for each simulated path


# hedging with true vol

df_time <- data.frame(time = t)

df <- df_time
# df_delta <- df_time
# qua <- vector() # for quadrativ variation

set.seed(0506)
for (i in 1:10){
  Z <- rnorm(N, mean=0, sd=1) 
  X <- cumsum((mu-1/2*sigma^2)*dt + sigma*sqrt(dt)*Z)
  S <- c(S0,S0*exp(X)) # simulated stock path
  
  vol <- sigma #true vol
  short <- -1; long <- 1
  strategy <- short
  
  call_price <- C(S=S,K=K,r=r,q=q,sigma=sigmai,tau=tau,option="Call") #market prices
  
  res <- delta_hedge_error(S=S, K=K, r=r,q=q, sigma=vol, tau=tau, call_price=call_price, strategy=strategy, option="Call")
  df <- cbind(df, res$error_cum)
  colnames(df)[i+1] <- paste("error_cum_", i, sep="")
  
  # df_delta <- cbind(df_delta, res$delta)
  # colnames(df_delta)[i+1] <- paste("delta_", i, sep="")
  # 
  # qua <- c(qua, mean(res$delta^2)) # quadratic variation
}

df_melt_true <- melt(df, id.vars = c('time'))
df_melt_true$panel <- "(a) Hedge med sand volatilitet"


## hedging with implied vol

df <- df_time
# df_delta <- df_time
# qua <- vector() # for quadrativ variation

set.seed(0506)
for (i in 1:10){
  Z <- rnorm(N, mean=0, sd=1) 
  X <- cumsum((mu-1/2*sigma^2)*dt + sigma*sqrt(dt)*Z)
  S <- c(S0,S0*exp(X)) # simulated stock paths
  
  vol <- sigmai #implied vol
  short <- -1; long <- 1
  strategy <- short
  
  call_price <- C(S=S,K=K,r=r,q=q,sigma=sigmai,tau=tau,option="Call") #market prices
  
  res <- delta_hedge_error(S=S, K=K, r=r,q=q, sigma=vol, tau=tau, call_price=call_price, strategy=strategy, option="Call")
  df <- cbind(df, res$error_cum)
  colnames(df)[i+1] <- paste("error_cum_", i, sep="")
  
  # df_delta <- cbind(df_delta, res$delta)
  # colnames(df_delta)[i+1] <- paste("delta_", i, sep="")
  # 
  # qua <- c(qua, mean(res$delta^2)) # quadratic variation
}

df_melt_implied <- melt(df, id.vars = c('time'))
df_melt_implied$panel <- "(b) Hedge med implied volatilitet"


# plot

p_wilmott <- ggplot(data=df_melt_true, aes(x=time, y=value, group=variable)) + 
  geom_line() +
  geom_line(data=df_melt_implied) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Tid (år)") +
  ylab("Kumuleret hedging fejl (P&L)") +
  ggtitle("") + 
  facet_wrap(~panel, ncol=2) +
  theme(plot.title = element_text(lineheight=0.8,face="bold")
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))


# expected profit-and-loss for hedge with true vol

exp(-r*T)*(C(S=S[1],K=K,r=r,q=q,sigma=sigmai,tau=tau[1],option="Call")
           -C(S=S[1],K=K,r=r,q=q,sigma=sigma,tau=tau[1],option="Call")
)

C(S=S[1],K=K,r=r,q=q,sigma=sigmai,tau=tau[1],option="Call")
C(S=S[1],K=K,r=r,q=q,sigma=sigma,tau=tau[1],option="Call")

# export

p_wilmott
pdf('/Users/VictorHansen/Google Drev/UNI/År3/Blok34/Bachelor/Rplots/p_wilmott.pdf', width = 10, height = 5)
p_wilmott
dev.off()