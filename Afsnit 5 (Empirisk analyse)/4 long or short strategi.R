
o <- 18

# rv5 mix

df <- df_time
df_delta <- df_time
df_gamma <- df_time
qua <- vector()
qua_delta <- vector()

s <- rep(NA, 37)
for (i in 1:37){
  loop_calldata <- subset(calldata, Option == i)
  q <- 0
  #q <- loop_calldata$q
  sigma <- loop_calldata$rv5
  short <- -1; long <- 1
  
  if (loop_calldata$sigma[1] > loop_calldata$rv5[1]) {
    strategy <- short
    s[i] <- "short"
  } else {
    strategy <- long
    s[i] <- "long"
  }
  
  res <- delta_hedge_error(S=loop_calldata$S,K=loop_calldata$K,r=loop_calldata$r,q=q,sigma=sigma, tau=loop_calldata$tau, call_price=loop_calldata$Cnod, strategy=strategy, option="Call")
  
  df <- cbind(df, res$error_cum)
  colnames(df)[i+1] <- paste("error_cum_", i, sep="")
  qua <- c(qua, mean(res$error_dt^2))
  
  df_delta <- cbind(df_delta, res$delta)
  colnames(df_delta)[i+1] <- paste("error_cum_", i, sep="")
  qua_delta <- c(qua, mean(res$delta^2))
  
  df_gamma <- cbind(df_gamma, res$gamma)
  colnames(df_gamma)[i+1] <- paste("error_cum_", i, sep="")
}

df_melt <- melt(df, id.vars = c('time'))
dim(df_melt)
for (i in 1:37){
  for (j in 1:dim(df_melt)[1]){
    if (df_melt$variable[j] == paste("error_cum_", i, sep="")){
      df_melt$strategy[j] <- s[i]
    }
  }
}

p_rv5_mix <- ggplot(data=df_melt, aes(x=time, y=value, group = variable)) + 
  geom_line(aes(linetype=strategy,  color=strategy)) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_colour_manual(values=c("red", "black")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("Mark-to-market (P&L)") +
  ggtitle("RV5 vol") +
  theme(plot.title = element_text(lineheight=0.5,vjust=1)        
        ,axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


mean_rv5_mix <- rowMeans(df[63,-1])
sd_rv5_mix <- sd(df[63,-1])
mean_qua_rv5_mix <- mean(qua)
sd_qua_rv5_mix <- sd(qua)

rv5_mix <- df[63,-1]
qua_rv5_mix <- qua

sharpe_rv5_mix <- (4* mean_rv5_mix/calldata$meanpricenod[1] - calldata$meanrate[1])/(sqrt(4)* sd_rv5_mix/calldata$meanpricenod[1])


#zoom

subcall <- subset(calldata, Option==o)
subcall$delta <- df_delta[,o+1]
subcall$gamma <- df_gamma[,o+1]
subcall$pl <- df[,o+1]

colnames(subcall)[10] <- "implied"
colnames(subcall)[19] <- "naiv"
colnames(subcall)[4] <- "aktie"
colnames(subcall)[5] <- "strike"
colnames(subcall)[26] <- "P&L"

vol_melt <- melt(subcall, id.vars = "Date", measure.vars = c("rv5", "vix", "naiv")) 
imp_melt <- melt(subcall, id.vars = "Date", measure.vars = c("implied"))
delta_melt <- melt(subcall, id.vars = "Date", measure.vars = c("delta"))
gamma_melt <- melt(subcall, id.vars = "Date", measure.vars = c("gamma"))
pl_melt <- melt(subcall, id.vars = "Date", measure.vars = c("P&L"))
sp500_melt <- melt(subcall, id.vars = "Date", measure.vars = c("aktie"))
strike_melt <- melt(subcall, id.vars = "Date", measure.vars = c("strike"))

vol_melt$panel <- "(b) Volatilitet"
imp_melt$panel <- "(b) Volatilitet"
delta_melt$panel <- "(d) Delta"
gamma_melt$panel <- "(e) Gamma"
pl_melt$panel <- "(a) P&L"
sp500_melt$panel <- "(c) S&P500"
strike_melt$panel <- "(c) S&P500"

p_zoomrv5mix <- ggplot(data=sp500_melt, aes(x=Date, y=value, group=variable)) + 
  geom_line() +
  geom_line(data=vol_melt, aes(x=Date, y=value, color=variable)) +
  geom_line(data = delta_melt, aes(x=Date, y=value)) +
  geom_line(data = gamma_melt, aes(x=Date, y=value)) +
  geom_line(data = pl_melt, aes(x=Date, y=value)) +
  geom_line(data = strike_melt, aes(x=Date, y=value, linetype=variable)) +
  geom_point(data = imp_melt, aes(x=Date, y=value, color=variable)) +
  scale_color_manual(values =c("gray10","green","orangered","cyan")) +
  scale_linetype_manual(values =c("dotted")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("RV5 vol") + 
  facet_grid(panel~., scales="free_y") +
  theme(plot.title = element_text(lineheight=0.8)
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1),
        legend.position = "none"
  ) +
  guides(color=guide_legend("")) +
  guides(linetype=guide_legend(""))


# naive mix

df <- df_time
df_delta <- df_time
df_gamma <- df_time
qua <- vector()
qua_delta <- vector()

s <- rep(NA, 37)
for (i in 1:37){
  loop_calldata <- subset(calldata, Option == i)
  q <- 0
  #q <- loop_calldata$q
  sigma <- loop_calldata$naive
  short <- -1; long <- 1
  
  if (loop_calldata$sigma[1] > loop_calldata$naive[1]) {
    strategy <- short
    s[i] <- "short"
  } else {
    strategy <- long
    s[i] <- "long"
  }
  
  res <- delta_hedge_error(S=loop_calldata$S,K=loop_calldata$K,r=loop_calldata$r,q=q,sigma=sigma, tau=loop_calldata$tau, call_price=loop_calldata$Cnod, strategy=strategy, option="Call")
  df <- cbind(df, res$error_cum)
  colnames(df)[i+1] <- paste("error_cum_", i, sep="")
  qua <- c(qua, mean(res$error_dt^2))
  
  df_delta <- cbind(df_delta, res$delta)
  colnames(df_delta)[i+1] <- paste("error_cum_", i, sep="")
  qua_delta <- c(qua, mean(res$delta^2))
  
  df_gamma <- cbind(df_gamma, res$gamma)
  colnames(df_gamma)[i+1] <- paste("error_cum_", i, sep="")
}

df_melt <- melt(df, id.vars = c('time'))

for (i in 1:37){
  for (j in 1:dim(df_melt)[1]){
    if (df_melt$variable[j] == paste("error_cum_", i, sep="")){
      df_melt$strategy[j] <- s[i]
    }
  }
}

p_naive_mix <- ggplot(data=df_melt, aes(x=time, y=value, group = variable)) + 
  geom_line(aes(linetype=strategy)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Naiv vol") +
  theme(plot.title = element_text(lineheight=0.5,vjust=1)        
        ,axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


mean_naive_mix <- rowMeans(df[63,-1])
sd_naive_mix <- sd(df[63,-1])
mean_qua_naive_mix <- mean(qua)
sd_qua_naive_mix <- sd(qua)

naive_mix <- df[63,-1]
qua_naive_mix <- qua

sharpe_naive_mix <- (4* mean_naive_mix/calldata$meanpricenod[1] - calldata$meanrate[1])/(sqrt(4)* sd_naive_mix/calldata$meanpricenod[1])


#zoom

subcall <- subset(calldata, Option==o)
subcall$delta <- df_delta[,o+1]
subcall$gamma <- df_gamma[,o+1]
subcall$pl <- df[,o+1]

colnames(subcall)[10] <- "implied"
colnames(subcall)[19] <- "naiv"
colnames(subcall)[4] <- "aktie"
colnames(subcall)[5] <- "strike"
colnames(subcall)[26] <- "P&L"

vol_melt <- melt(subcall, id.vars = "Date", measure.vars = c("rv5", "vix", "naiv")) 
imp_melt <- melt(subcall, id.vars = "Date", measure.vars = c("implied"))
delta_melt <- melt(subcall, id.vars = "Date", measure.vars = c("delta"))
gamma_melt <- melt(subcall, id.vars = "Date", measure.vars = c("gamma"))
pl_melt <- melt(subcall, id.vars = "Date", measure.vars = c("P&L"))
sp500_melt <- melt(subcall, id.vars = "Date", measure.vars = c("aktie"))
strike_melt <- melt(subcall, id.vars = "Date", measure.vars = c("strike"))

vol_melt$panel <- "(b) Volatilitet"
imp_melt$panel <- "(b) Volatilitet"
delta_melt$panel <- "(d) Delta"
gamma_melt$panel <- "(e) Gamma"
pl_melt$panel <- "(a) P&L"
sp500_melt$panel <- "(c) S&P500"
strike_melt$panel <- "(c) S&P500"

p_zoomnaivemix <- ggplot(data=sp500_melt, aes(x=Date, y=value, group=variable)) + 
  geom_line() +
  geom_line(data=vol_melt, aes(x=Date, y=value, color=variable)) +
  geom_line(data = delta_melt, aes(x=Date, y=value)) +
  geom_line(data = gamma_melt, aes(x=Date, y=value)) +
  geom_line(data = pl_melt, aes(x=Date, y=value)) +
  geom_line(data = strike_melt, aes(x=Date, y=value, linetype=variable)) +
  geom_point(data = imp_melt, aes(x=Date, y=value, color=variable)) +
  scale_color_manual(values =c("gray10","green","orangered","cyan")) +
  scale_linetype_manual(values =c("dotted")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Naiv vol") + 
  facet_grid(panel~., scales="free_y") +
  theme(plot.title = element_text(lineheight=0.8)
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1),
        legend.position = "none"
  ) +
  guides(color=guide_legend("")) +
  guides(linetype=guide_legend(""))



# implied/rv5 mix

df <- df_time
df_delta <- df_time
df_gamma <- df_time
qua <- vector()
qua_delta <- vector()

s <- rep(NA, 37)
for (i in 1:37){
  loop_calldata <- subset(calldata, Option == i)
  q <- 0
  #q <- loop_calldata$q
  sigma <- loop_calldata$sigma
  short <- -1; long <- 1
  
  if (loop_calldata$sigma[1] > loop_calldata$rv5[1]) {
    strategy <- short
    s[i] <- "short"
  } else {
    strategy <- long
    s[i] <- "long"
  }
  
  res <- delta_hedge_error(S=loop_calldata$S,K=loop_calldata$K,r=loop_calldata$r,q=q,sigma=sigma, tau=loop_calldata$tau, call_price=loop_calldata$Cnod, strategy=strategy, option="Call")
  df <- cbind(df, res$error_cum)
  colnames(df)[i+1] <- paste("error_cum_", i, sep="")
  qua <- c(qua, mean(res$error_dt^2))
  
  df_delta <- cbind(df_delta, res$delta)
  colnames(df_delta)[i+1] <- paste("error_cum_", i, sep="")
  qua_delta <- c(qua, mean(res$delta^2))
  
  df_gamma <- cbind(df_gamma, res$gamma)
  colnames(df_gamma)[i+1] <- paste("error_cum_", i, sep="")
}

df_melt <- melt(df, id.vars = c('time'))

for (i in 1:37){
  for (j in 1:dim(df_melt)[1]){
    if (df_melt$variable[j] == paste("error_cum_", i, sep="")){
      df_melt$strategy[j] <- s[i]
    }
  }
}

p_imprv5_mix <- ggplot(data=df_melt, aes(x=time, y=value, group = variable)) + 
  geom_line(aes(linetype=strategy,  color=strategy)) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_colour_manual(values=c("red", "black")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Tid (år)") +
  ylab("Mark-to-market (P&L)") +
  ggtitle("Implied vol") +
  theme(plot.title = element_text(lineheight=0.5,vjust=1)        
        ,axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


mean_imprv5_mix <- rowMeans(df[63,-1])
sd_imprv5_mix <- sd(df[63,-1])
mean_qua_imprv5_mix <- mean(qua)
sd_qua_imprv5_mix <- sd(qua)

imprv5_mix <- df[63,-1]
qua_imprv5_mix <- qua

sharpe_imprv5_mix <- (4* mean_imprv5_mix/calldata$meanpricenod[1] - calldata$meanrate[1])/(sqrt(4)* sd_imprv5_mix/calldata$meanpricenod[1])


#zoom

subcall <- subset(calldata, Option==o)
subcall$delta <- df_delta[,o+1]
subcall$gamma <- df_gamma[,o+1]
subcall$pl <- df[,o+1]

colnames(subcall)[10] <- "implied"
colnames(subcall)[19] <- "naiv"
colnames(subcall)[4] <- "aktie"
colnames(subcall)[5] <- "strike"
colnames(subcall)[26] <- "P&L"

vol_melt <- melt(subcall, id.vars = "Date", measure.vars = c("rv5", "vix", "naiv")) 
imp_melt <- melt(subcall, id.vars = "Date", measure.vars = c("implied"))
delta_melt <- melt(subcall, id.vars = "Date", measure.vars = c("delta"))
gamma_melt <- melt(subcall, id.vars = "Date", measure.vars = c("gamma"))
pl_melt <- melt(subcall, id.vars = "Date", measure.vars = c("P&L"))
sp500_melt <- melt(subcall, id.vars = "Date", measure.vars = c("aktie"))
strike_melt <- melt(subcall, id.vars = "Date", measure.vars = c("strike"))

vol_melt$panel <- "(b) Volatilitet"
imp_melt$panel <- "(b) Volatilitet"
delta_melt$panel <- "(d) Delta"
gamma_melt$panel <- "(e) Gamma"
pl_melt$panel <- "(a) P&L"
sp500_melt$panel <- "(c) S&P500"
strike_melt$panel <- "(c) S&P500"

p_zoomimpmix <- ggplot(data=sp500_melt, aes(x=Date, y=value, group=variable)) + 
  geom_line() +
  geom_line(data=vol_melt, aes(x=Date, y=value, color=variable)) +
  geom_line(data = delta_melt, aes(x=Date, y=value)) +
  geom_line(data = gamma_melt, aes(x=Date, y=value)) +
  geom_line(data = pl_melt, aes(x=Date, y=value)) +
  geom_line(data = strike_melt, aes(x=Date, y=value, linetype=variable)) +
  geom_point(data = imp_melt, aes(x=Date, y=value, color=variable)) +
  scale_color_manual(values =c("gray10","green","orangered","cyan")) +
  scale_linetype_manual(values =c("dotted")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Implied vol") + 
  facet_grid(panel~., scales="free_y") +
  theme(plot.title = element_text(lineheight=0.8)
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1),
        legend.position = "bottom"
  ) +
  guides(color=guide_legend("")) +
  guides(linetype=guide_legend(""))



# implied/naive mix

df <- df_time
df_delta <- df_time
df_gamma <- df_time
qua <- vector()
qua_delta <- vector()

s <- rep(NA, 37)
for (i in 1:37){
  loop_calldata <- subset(calldata, Option == i)
  q <- 0
  #q <- loop_calldata$q
  sigma <- loop_calldata$sigma
  short <- -1; long <- 1
  
  if (loop_calldata$sigma[1] > loop_calldata$naive[1]) {
    strategy <- short
    s[i] <- "short"
  } else {
    strategy <- long
    s[i] <- "long"
  }
  
  res <- delta_hedge_error(S=loop_calldata$S,K=loop_calldata$K,r=loop_calldata$r,q=q,sigma=sigma, tau=loop_calldata$tau, call_price=loop_calldata$Cnod, strategy=strategy, option="Call")
  df <- cbind(df, res$error_cum)
  colnames(df)[i+1] <- paste("error_cum_", i, sep="")
  qua <- c(qua, mean(res$error_dt^2))
  
  df_delta <- cbind(df_delta, res$delta)
  colnames(df_delta)[i+1] <- paste("error_cum_", i, sep="")
  qua_delta <- c(qua, mean(res$delta^2))
  
  df_gamma <- cbind(df_gamma, res$gamma)
  colnames(df_gamma)[i+1] <- paste("error_cum_", i, sep="")
}

df_melt <- melt(df, id.vars = c('time'))

for (i in 1:37){
  for (j in 1:dim(df_melt)[1]){
    if (df_melt$variable[j] == paste("error_cum_", i, sep="")){
      df_melt$strategy[j] <- s[i]
    }
  }
}

p_impnaive_mix <- ggplot(data=df_melt, aes(x=time, y=value, group = variable)) + 
  geom_line(aes(linetype=strategy)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Tid (år)") +
  ylab("Mark-to-market (P&L)") +
  ggtitle("Hedging med implied volatilitet (long/short by naiv)") +
  theme(plot.title = element_text(lineheight=0.8, face="bold",vjust=1)        
        ,axis.text.x = element_text(angle = 45, hjust = 1))


mean_impnaive_mix <- rowMeans(df[63,-1])
sd_impnaive_mix <- sd(df[63,-1])
mean_qua_impnaive_mix <- mean(qua)
sd_qua_impnaive_mix <- sd(qua)

impnaive_mix <- df[63,-1]
qua_impnaive_mix <- qua

sharpe_impnaive_mix <- (4* mean_impnaive_mix/calldata$meanpricenod[1] - calldata$meanrate[1])/(sqrt(4)* sd_impnaive_mix/calldata$meanpricenod[1])


# vix mix

df <- df_time
df_delta <- df_time
df_gamma <- df_time
qua <- vector()
qua_delta <- vector()

for (i in 1:37){
  loop_calldata <- subset(calldata, Option == i)
  q <- 0
  #q <- loop_calldata$q
  sigma <- loop_calldata$vix
  short <- -1; long <- 1
  
  if (loop_calldata$sigma[1] > loop_calldata$rv5[1]) {
    strategy <- short
    s[i] <- "short"
  } else {
    strategy <- long
    s[i] <- "long"
  }
  
  res <- delta_hedge_error(S=loop_calldata$S,K=loop_calldata$K,r=loop_calldata$r,q=q,sigma=sigma, tau=loop_calldata$tau, call_price=loop_calldata$Cnod, strategy=strategy, option="Call")
  df <- cbind(df, res$error_cum)
  colnames(df)[i+1] <- paste("error_cum_", i, sep="")
  qua <- c(qua, mean(res$error_dt^2))
  
  df_delta <- cbind(df_delta, res$delta)
  colnames(df_delta)[i+1] <- paste("error_cum_", i, sep="")
  qua_delta <- c(qua, mean(res$delta^2))
  
  df_gamma <- cbind(df_gamma, res$gamma)
  colnames(df_gamma)[i+1] <- paste("error_cum_", i, sep="")
}

df_melt <- melt(df, id.vars = c('time'))

for (i in 1:37){
  for (j in 1:dim(df_melt)[1]){
    if (df_melt$variable[j] == paste("error_cum_", i, sep="")){
      df_melt$strategy[j] <- s[i]
    }
  }
}

p_vix_mix <- ggplot(data=df_melt, aes(x=time, y=value, group = variable)) + 
  geom_line(aes(linetype=strategy,  color=strategy)) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_colour_manual(values=c("red", "black")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Tid (år)") +
  ylab("") +
  ggtitle("VIX vol") +
  theme(plot.title = element_text(lineheight=0.5,vjust=1)        
        ,axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


mean_vix_mix <- rowMeans(df[63,-1])
sd_vix_mix <- sd(df[63,-1])
mean_qua_vix_mix <- mean(qua)
sd_qua_vix_mix <- sd(qua)

vix_mix <- df[63,-1]
qua_vix_mix <- qua

sharpe_vix_mix <- (4* mean_vix_mix/calldata$meanpricenod[1] - calldata$meanrate[1])/(sqrt(4)* sd_vix_mix/calldata$meanpricenod[1])


#zoom

subcall <- subset(calldata, Option==o)
subcall$delta <- df_delta[,o+1]
subcall$gamma <- df_gamma[,o+1]
subcall$pl <- df[,o+1]

colnames(subcall)[10] <- "implied"
colnames(subcall)[19] <- "naiv"
colnames(subcall)[4] <- "aktie"
colnames(subcall)[5] <- "strike"
colnames(subcall)[26] <- "P&L"

vol_melt <- melt(subcall, id.vars = "Date", measure.vars = c("rv5", "vix", "naiv")) 
imp_melt <- melt(subcall, id.vars = "Date", measure.vars = c("implied"))
delta_melt <- melt(subcall, id.vars = "Date", measure.vars = c("delta"))
gamma_melt <- melt(subcall, id.vars = "Date", measure.vars = c("gamma"))
pl_melt <- melt(subcall, id.vars = "Date", measure.vars = c("P&L"))
sp500_melt <- melt(subcall, id.vars = "Date", measure.vars = c("aktie"))
strike_melt <- melt(subcall, id.vars = "Date", measure.vars = c("strike"))

vol_melt$panel <- "(b) Volatilitet"
imp_melt$panel <- "(b) Volatilitet"
delta_melt$panel <- "(d) Delta"
gamma_melt$panel <- "(e) Gamma"
pl_melt$panel <- "(a) P&L"
sp500_melt$panel <- "(c) S&P500"
strike_melt$panel <- "(c) S&P500"

p_zoomvixmix <- ggplot(data=sp500_melt, aes(x=Date, y=value, group=variable)) + 
  geom_line() +
  geom_line(data=vol_melt, aes(x=Date, y=value, color=variable)) +
  geom_line(data = delta_melt, aes(x=Date, y=value)) +
  geom_line(data = gamma_melt, aes(x=Date, y=value)) +
  geom_line(data = pl_melt, aes(x=Date, y=value)) +
  geom_line(data = strike_melt, aes(x=Date, y=value, linetype=variable)) +
  geom_point(data = imp_melt, aes(x=Date, y=value, color=variable)) +
  scale_color_manual(values =c("gray10","green","orangered","cyan")) +
  scale_linetype_manual(values =c("dotted")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("VIX vol") + 
  facet_grid(panel~., scales="free_y") +
  theme(plot.title = element_text(lineheight=0.8)
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1),
        legend.position = "none"
  ) +
  guides(color=guide_legend("")) +
  guides(linetype=guide_legend(""))



mylegend <- g_legend(p_zoomimpmix)

p_mixzooms <- grid.arrange(arrangeGrob(p_zoomrv5mix, p_zoomnaivemix, p_zoomimpmix+theme(legend.position="none"), p_zoomvixmix, ncol=4), mylegend, nrow=2, heights=c(10, 1),
                           top = grid::textGrob("2. jul 2008 - 30. sep 2008", x = 0.4, hjust = 0,
                                                gp=gpar(fontface="bold")))

pdf('/Users/VictorHansen/Google Drev/UNI/År3/Blok34/Bachelor/Rplots/p_mixzooms.pdf', width = 10, height = 9)
grid.arrange(arrangeGrob(p_zoomrv5mix, p_zoomnaivemix, p_zoomimpmix+theme(legend.position="none"), p_zoomvixmix, ncol=4), mylegend, nrow=2, heights=c(10, 1),
             top = grid::textGrob("2. jul 2008 - 30. sep 2008", x = 0.4, hjust = 0,
                                  gp=gpar(fontface="bold")))
dev.off()



mylegend <- g_legend(p_vix_mix)

p_mixs <- grid.arrange(arrangeGrob(p_rv5_mix, p_naive_mix, p_imprv5_mix ,p_vix_mix+theme(legend.position="none"), ncol=2, nrow=2), mylegend, nrow=2, heights=c(10, 1),
                       top = grid::textGrob("Strategi: long/short", x = 0.4, hjust = 0,
                                            gp=gpar(fontface="bold")))

pdf('/Users/VictorHansen/Google Drev/UNI/År3/Blok34/Bachelor/Rplots/p_mixs.pdf', width = 10, height = 7)
grid.arrange(arrangeGrob(p_rv5_mix, p_naive_mix, p_imprv5_mix ,p_vix_mix+theme(legend.position="none"), ncol=2, nrow=2), mylegend, nrow=2, heights=c(10, 1),
             top = grid::textGrob("Strategi: long/short", x = 0.44, hjust = 0,
                                  gp=gpar(fontface="bold")))
dev.off()




rv5mix <- c(
  mean_rv5_mix
  ,sd_rv5_mix
  ,mean_qua_rv5_mix
  ,sd_qua_rv5_mix 
)

naivemix <- c(
  mean_naive_mix
  ,sd_naive_mix
  ,mean_qua_naive_mix
  ,sd_qua_naive_mix
)

imprv5mix <- c(
  mean_imprv5_mix
  ,sd_imprv5_mix
  ,mean_qua_imprv5_mix
  ,sd_qua_imprv5_mix
)

impnaivemix <- c(
  mean_impnaive_mix
  ,sd_impnaive_mix
  ,mean_qua_impnaive_mix
  ,sd_qua_impnaive_mix
)

vixmix <- c(
  mean_vix_mix
  ,sd_vix_mix
  ,mean_qua_vix_mix
  ,sd_qua_vix_mix
)

sharpe_mix <- c("sharpe",sharpe_rv5_mix, sharpe_naive_mix, sharpe_imprv5_mix, sharpe_impnaive_mix, sharpe_vix_mix)

mixs <- data.frame(mål = c("mean","sd_mean","qua","sd_qua"))
mixs <- cbind(mixs, rv5mix, naivemix, imprv5mix, impnaivemix, vixmix)
mixs$mål <- as.character(mixs$mål)
mixs <- rbind(mixs, sharpe_mix)
mixs

t.test(rv5_mix)
t.test(naive_mix)
t.test(imprv5_mix)
t.test(vix_mix)