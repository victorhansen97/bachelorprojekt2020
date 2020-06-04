rm(list=ls())

library(ggplot2)
library(reshape2)

K <- 100
S <- seq(0,200,length=1000)

payoff_call <- pmax(S-K, 0)
payoff_put <- pmax(K-S, 0)

dfb <- data.frame(Aktie=S, Call=payoff_call, Put=payoff_put, Bank = -K)
dfb_melt = melt(dfb, id = "Aktie", measure.vars = c("Call", "Put", "Aktie", "Bank"))

p_parity <- ggplot(data=dfb_melt, aes(x=Aktie, y=value)) + 
  geom_line(aes(linetype = variable, color = variable, size = variable)) +
  scale_color_manual("", values = c("green", "black", "black", "black")) +
  scale_size_manual("", values = c(2, 1, 1, 1)) +
  scale_linetype_manual("", values = c("solid", "longdash", "dotdash", "dotted")) +
  xlab("S(T)") +
  ylab("Payoff ved udløb T") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("")+
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 0, hjust = 1,vjust=1))


p_parity
pdf('/Users/VictorHansen/Google Drev/UNI/År3/Blok34/Bachelor/Rplots/p_parity.pdf', width = 10, height = 5)
p_parity
dev.off()