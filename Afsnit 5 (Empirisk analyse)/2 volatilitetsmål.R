head(calldata)

colnames(calldata)[10] <- "implied"
colnames(calldata)[19] <- "naiv"
colnames(calldata)[4] <- "aktie"

vol_melt <- melt(calldata, id.vars = "Date", measure.vars = c("rv5", "vix", "naiv")) 
imp_melt <- melt(calldata, id.vars = "Date", measure.vars = c("implied"))
sp500_melt <- melt(calldata, id.vars = "Date", measure.vars = c("aktie"))

vol_melt$panel <- "Volatilitetsmål"
imp_melt$panel <- "Volatilitetsmål"
sp500_melt$panel <- "S&P500 lukkekurser"

p_volmål <- ggplot(data=vol_melt, aes(x=Date, y=value, color=variable, size=variable)) + 
  geom_line() +
  geom_line(data = sp500_melt, aes(x=Date, y=value)) +
  geom_point(data = imp_melt, aes(x=Date, y=value)) +
  scale_size_manual(values =c(0.8,0.7,1.1,0.5,0.5)) +
  scale_color_manual(values =c("black","gray10","green","orangered","cyan")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Dato (år)") +
  ylab("Værdi") +
  ggtitle("") + 
  facet_grid(panel~., scales="free_y") +
  theme(plot.title = element_text(lineheight=0.8,face="bold")
        ,axis.text.x = element_text(angle = 45, hjust = 1,vjust=1)
  ) +
  guides(color=guide_legend("")) +
  guides(size=guide_legend(""))

p_volmål
pdf('/Users/VictorHansen/Google Drev/UNI/År3/Blok34/Bachelor/Rplots/p_volmål.pdf', width = 10, height = 5)
p_volmål
dev.off()