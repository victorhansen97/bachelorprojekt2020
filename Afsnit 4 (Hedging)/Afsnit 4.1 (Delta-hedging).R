rm(list=ls())

library(ggplot2)
library(reshape2)

# functions used
d1 <- function(S,K,r,q,sigma,tau){
  (log(S/K)+(r-q)*tau + 1/2*sigma^2*tau)/(sigma*sqrt(tau))
}

d2 <- function(S,K,r,q,sigma,tau){
  d1(S,K,r,q,sigma,tau) - sigma*sqrt(tau)
}

delta <- function(S,K,r,q,sigma,tau) {
  d1 <- d1(S,K,r,q,sigma,tau)
  d2 <- d2(S,K,r,q,sigma,tau)
  pnorm(d1)
}

gamma <- function(S,K,r,q,sigma,tau) {
  d1 <- d1(S,K,r,q,sigma,tau)
  d2 <- d2(S,K,r,q,sigma,tau)
  dnorm(d1)/(S*sigma*sqrt(tau))
}

vega <- function(S,K,r,q,sigma,tau) {
  d1 <- d1(S,K,r,q,sigma,tau)
  d2 <- d2(S,K,r,q,sigma,tau)
  S*dnorm(d1)*sqrt(tau)
}

# parameters

K <- 100
r <- 0.02
q <- 0
sigma <- 0.2

T <- 1
N <- 1000
t <- seq(0,T,length=N)
tau <- T - t

S <- seq(K-100,K+100,length=N)

# plots of greeks as function of S

df <- data.frame(S=S, delta = delta(S=S,K=K,r=r,q=q,sigma=sigma,tau=tau), gamma = gamma(S=S,K=K,r=r,q=q,sigma=sigma,tau=tau), vega = vega(S=S,K=K,r=r,q=q,sigma=sigma,tau=tau))

df_delta <- melt(df, id.vars = "S", measure.vars = "delta")
df_delta$panel <- "Delta"

df_gamma <- melt(df, id.vars = "S", measure.vars = "gamma")
df_gamma$panel <- "Gamma"

df_vega <- melt(df, id.vars = "S", measure.vars = "vega")
df_vega$panel <- "Vega"

p_greeks <- ggplot(data=df_delta, aes(x=S, y=value)) + 
  geom_line() +
  geom_line(data=df_gamma) +
  geom_line(data=df_vega) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("S") +
  ylab("Værdi") +
  ggtitle("") + 
  facet_wrap(panel~., scales="free_y") +
  theme(plot.title = element_text(lineheight=0.8,face="bold")
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))

p_greeks
pdf('/Users/VictorHansen/Google Drev/UNI/År3/Blok34/Bachelor/Rplots/p_greeks.pdf', width = 10, height = 5)
p_greeks
dev.off()