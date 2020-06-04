
o <- 19 # option of interest

colnames(calldata)[10] <- "sigma"
colnames(calldata)[19] <- "naive"
colnames(calldata)[4] <- "S"

library("plyr")
first <- ddply(calldata, .(Option), function(x) x[c(1), ])

# laver tidssøjle

time_df <- subset(calldata, Option == 1)
df_time <- data.frame(time = time_df$t)

# rv5 short only

df <- df_time
df_delta <- df_time
df_gamma <- df_time
qua <- vector()
qua_delta <- vector()

for (i in 1:37){
  loop_calldata <- subset(calldata, Option == i)
  q <- 0
  #q <- loop_calldata$q
  sigma <- loop_calldata$rv5
  short <- -1; long <- 1
  strategy <- short
  
  res <- delta_hedge_error(S=loop_calldata$S,K=loop_calldata$K,r=loop_calldata$r,q=q,sigma=sigma, tau=loop_calldata$tau, call_price=loop_calldata$Cnod, strategy=strategy, option="Call")
  df <- cbind(df, res$error_cum)
  colnames(df)[i+1] <- paste(as.Date(first[i,1]))
  qua <- c(qua, mean(res$error_dt^2))
  
  df_delta <- cbind(df_delta, res$delta)
  colnames(df_delta)[i+1] <- paste(as.Date(first[i,1]))
  qua_delta <- c(qua, mean(res$delta^2))
  
  df_gamma <- cbind(df_gamma, res$gamma)
  colnames(df_gamma)[i+1] <- paste(as.Date(first[i,1]))
}

df_melt <- melt(df, id.vars = c('time'))

p_rv5_short <- ggplot(data=df_melt, aes(x=time, y=value, group=variable)) + 
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("Mark-to-market (P&L)") +
  ggtitle("RV5 vol") + 
  theme(plot.title = element_text(lineheight=0.5)
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))


mean_rv5_short <- rowMeans(df[63,-1])
sd_rv5_short <- sd(df[63,-1])
mean_qua_rv5_short <- mean(qua)
sd_qua_rv5_short <- sd(qua)

rv5_short <- df[63,-1]
qua_rv5_short <- qua

sharpe_rv5_short <- (4* mean_rv5_short/calldata$meanpricenod[1] - calldata$meanrate[1])/(sqrt(4)* sd_rv5_short/calldata$meanpricenod[1])

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

p_zoom <- ggplot(data=sp500_melt, aes(x=Date, y=value, group=variable)) + 
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
  )



# naive short only

df <- df_time
df_delta <- df_time
df_gamma <- df_time
qua <- vector()
qua_delta <- vector()

for (i in 1:37){
  loop_calldata <- subset(calldata, Option == i)
  q <- 0
  #q <- loop_calldata$q
  sigma <- loop_calldata$naive
  short <- -1; long <- 1
  strategy <- short
  
  res <- delta_hedge_error(S=loop_calldata$S,K=loop_calldata$K,r=loop_calldata$r,q=q,sigma=sigma, tau=loop_calldata$tau, call_price=loop_calldata$Cnod, strategy=strategy, option="Call")
  df <- cbind(df, res$error_cum)
  colnames(df)[i+1] <- paste("error_cum_", i, sep="")
  qua <- c(qua, mean(res$error_dt^2))
  
  df_delta <- cbind(df_delta, res$delta)
  colnames(df_delta)[i+1] <- paste(as.Date(first[i,1]))
  qua_delta <- c(qua, mean(res$delta^2))
  
  df_gamma <- cbind(df_gamma, res$gamma)
  colnames(df_gamma)[i+1] <- paste(as.Date(first[i,1]))
}

df_melt <- melt(df, id.vars = c('time'))

p_naive_short <- ggplot(data=df_melt, aes(x=time, y=value, group=variable)) + 
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Naiv vol") + 
  theme(plot.title = element_text(lineheight=0.5)
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))


mean_naive_short <- rowMeans(df[63,-1])
sd_naive_short <- sd(df[63,-1])
mean_qua_naive_short <- mean(qua)
sd_qua_naive_short <- sd(qua)

naive_short <- df[63,-1]
qua_naive_short <- qua

sharpe_naive_short <- (4* mean_naive_short/calldata$meanpricenod[1] - calldata$meanrate[1])/(sqrt(4)* sd_naive_short/calldata$meanpricenod[1])


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

p_zoomnaiv <- ggplot(data=sp500_melt, aes(x=Date, y=value, group=variable)) + 
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
  )




# implied short only

df <- df_time
df_delta <- df_time
df_gamma <- df_time
qua <- vector()
qua_delta <- vector()

for (i in 1:37){
  loop_calldata <- subset(calldata, Option == i)
  q <- 0
  #q <- loop_calldata$q
  sigma <- loop_calldata$sigma
  short <- -1; long <- 1
  strategy <- short
  
  res <- delta_hedge_error(S=loop_calldata$S,K=loop_calldata$K,r=loop_calldata$r,q=q,sigma=sigma, tau=loop_calldata$tau, call_price=loop_calldata$Cnod, strategy=strategy, option="Call")
  
  df <- cbind(df, res$error_cum)
  colnames(df)[i+1] <- paste(as.Date(first[i,1]))
  qua <- c(qua, mean(res$error_dt^2))
  
  df_delta <- cbind(df_delta, res$delta)
  colnames(df_delta)[i+1] <- paste(as.Date(first[i,1]))
  qua_delta <- c(qua, mean(res$delta^2))
  
  df_gamma <- cbind(df_gamma, res$gamma)
  colnames(df_gamma)[i+1] <- paste(as.Date(first[i,1]))
}

df_melt <- melt(df, id.vars = c('time'))

p_imp_short <- ggplot(data=df_melt, aes(x=time, y=value, group=variable)) + 
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Tid (år)") +
  ylab("Mark-to-market (P&L)") +
  ggtitle("Implied vol") + 
  theme(plot.title = element_text(lineheight=0.5)
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))


mean_imp_short <- rowMeans(df[63,-1])
sd_imp_short <- sd(df[63,-1])
mean_qua_imp_short <- mean(qua)
sd_qua_imp_short <- sd(qua)

imp_short <- df[63,-1]
qua_imp_short <- qua

sharpe_imp_short <- (4* mean_imp_short/calldata$meanpricenod[1] - calldata$meanrate[1])/(sqrt(4)* sd_imp_short/calldata$meanpricenod[1])


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

p_zoomimp <- ggplot(data=sp500_melt, aes(x=Date, y=value, group=variable)) + 
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


# vix short only

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
  strategy <- short
  
  res <- delta_hedge_error(S=loop_calldata$S,K=loop_calldata$K,r=loop_calldata$r,q=q,sigma=sigma, tau=loop_calldata$tau, call_price=loop_calldata$Cnod, strategy=strategy, option="Call")
  
  df <- cbind(df, res$error_cum)
  colnames(df)[i+1] <- paste(as.Date(first[i,1]))
  qua <- c(qua, mean(res$error_dt^2))
  
  df_delta <- cbind(df_delta, res$delta)
  colnames(df_delta)[i+1] <- paste(as.Date(first[i,1]))
  qua_delta <- c(qua, mean(res$delta^2))
  
  df_gamma <- cbind(df_gamma, res$gamma)
  colnames(df_gamma)[i+1] <- paste(as.Date(first[i,1]))
}

df_melt <- melt(df, id.vars = c('time'))

p_vix_short <- ggplot(data=df_melt, aes(x=time, y=value, group=variable)) + 
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Tid (år)") +
  ylab("") +
  ggtitle("VIX vol") + 
  theme(plot.title = element_text(lineheight=0.5)
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))


mean_vix_short <- rowMeans(df[63,-1])
sd_vix_short <- sd(df[63,-1])
mean_qua_vix_short <- mean(qua)
sd_qua_vix_short <- sd(qua)

vix_short <- df[63,-1]
qua_vix_short <- qua

sharpe_vix_short <- (4* mean_vix_short/calldata$meanpricenod[1] - calldata$meanrate[1])/(sqrt(4)* sd_vix_short/calldata$meanpricenod[1])


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

p_zoomvix <- ggplot(data=sp500_melt, aes(x=Date, y=value, group=variable)) + 
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




library(gridExtra)
library(grid)

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend <- g_legend(p_zoomimp)

p_zooms <- grid.arrange(arrangeGrob(p_zoom, p_zoomnaiv, p_zoomimp+theme(legend.position="none"), p_zoomvix, ncol=4), mylegend, nrow=2, heights=c(10, 1),
                        top = grid::textGrob("1. okt 2008 - 30. dec 2008", x = 0.4, hjust = 0,
                                             gp=gpar(fontface="bold")))

pdf('/Users/VictorHansen/Google Drev/UNI/År3/Blok34/Bachelor/Rplots/p_zooms.pdf', width = 10, height = 9)
grid.arrange(arrangeGrob(p_zoom, p_zoomnaiv, p_zoomimp+theme(legend.position="none"), p_zoomvix, ncol=4), mylegend, nrow=2, heights=c(10, 1),
             top = grid::textGrob("1. okt 2008 - 30. dec 2008", x = 0.4, hjust = 0,
                                  gp=gpar(fontface="bold")))
dev.off()


p_shorts <- grid.arrange(p_rv5_short, p_naive_short, p_imp_short, p_vix_short, ncol=2, nrow=2, 
                         top = grid::textGrob("Strategi: short", x = 0.442, hjust = 0,
                                              gp=gpar(fontface="bold")))
pdf('/Users/VictorHansen/Google Drev/UNI/År3/Blok34/Bachelor/Rplots/p_shorts.pdf', width = 10, height = 7)
grid.arrange(p_rv5_short, p_naive_short, p_imp_short, p_vix_short, ncol=2, nrow=2, 
             top = grid::textGrob("Strategi: short", x = 0.462, hjust = 0,
                                  gp=gpar(fontface="bold")))
dev.off()

rv5short <- c(
  mean_rv5_short
  ,sd_rv5_short
  ,mean_qua_rv5_short
  ,sd_qua_rv5_short
)

naiveshort <- c(
  mean_naive_short
  ,sd_naive_short
  ,mean_qua_naive_short
  ,sd_qua_naive_short
)

impshort <- c(
  mean_imp_short
  ,sd_imp_short
  ,mean_qua_imp_short
  ,sd_qua_imp_short
)

vixshort <- c(
  mean_vix_short
  ,sd_vix_short
  ,mean_qua_vix_short
  ,sd_qua_vix_short
)

sharpe_short <- c("sharpe", sharpe_rv5_short, sharpe_naive_short, sharpe_imp_short, sharpe_vix_short)

shorts <- data.frame(mål = c("mean","sd_mean","qua","sd_qua"))
shorts <- cbind(shorts, rv5short, naiveshort, impshort, vixshort)
shorts$mål <- as.character(shorts$mål)
shorts <- rbind(shorts, sharpe_short)
shorts