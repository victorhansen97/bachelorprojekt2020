rm(list=ls())

library("readxl")

surface <- read_xls("01-apr-2004.xls") #rawdata

spot <- as.numeric(surface[[1,2]]) #spotprice

test <- surface[-2, 3:16] #data
test[1,] <- as.numeric(test[1,])/spot #moneyness

premelt1 <- test[-1,] #no strike head row

colnames(premelt1)[1] <- c("time") #names to keep track when melting
for (i in 2:14){
  colnames(premelt1)[i] <- paste("strike_", (i-1), sep="")
}

library(reshape2)
library(ggplot2)

melt1 <- melt(premelt1, id.vars = "time") #melting on time

#term structures
term_structures <- ggplot(data=melt1, aes(x=as.numeric(time), y=value, color=variable)) + 
  geom_line(size=1) +
  xlab("time") +
  ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Term Structure")+
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 0, hjust = 1,vjust=1)) 


#finding skews and surface

strikes <- test[1,-1] #strikes
transstrikes <- t(strikes)
strikenames <- as.vector(colnames(premelt1)[-1])
transnames <- t(strikenames)
premerge <- cbind(strikenames,transstrikes)
#str(premerge)
colnames(premerge) <- c("variable", "value")

merge <- merge(melt1, premerge, by="variable")

sort.df <- with(merge,  merge[order(variable) , ])

colnames(sort.df) <- c("", "tid", "vol", "strike")

#skews
volatility_skews <- ggplot(data=sort.df, aes(x=as.numeric(as.character(strike)), y=vol, color=tid)) +
  geom_line(size=1) +
  xlab("Moneyness") +
  ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Volatility Smile")+
  theme(plot.title = element_text(lineheight=0.8) # face="bold"
        ,axis.text.x = element_text(angle = 0, hjust = 1,vjust=1)) 

#volatility surface
df_surface <- sort.df[, c("tid", "strike", "vol")]
df_surface <- data.frame(tid = as.numeric(df_surface$tid), 
                         strike = as.numeric(as.character(df_surface$strike)), 
                         vol = as.numeric(df_surface$vol))

#str(df_surface)

x <- df_surface$tid
y <- df_surface$strike
z <- df_surface$vol

# discrete surfaceplot
# library(plot3D)
# scatter3D(x,y,z, theta = 125, phi = 25)

library(plotly)

#axis labels
axx <- list(
  title = "Tid til udløb (år)"
)
axy <- list(
  title = "Moneyness (K/S)"
)
axz <- list(
  title = "Implied volatility"
)

# volatility surface
surfaceplot <- plot_ly(x=x,y=y,z=z, 
                       type="mesh3d",
                       intensity = ~z,
                       colors = colorRamp(
                         c("purple","blue","lightblue","green","yellow","orange","red")
                       )
) %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz, aspectratio = list(x=1, y=1, z=0.6))) %>% colorbar(title = "Implied vol") %>% layout(
  scene= list(
    camera= list(
      eye= list(x= 0.8, y= 1.6, z= 0.6)
    )
  )
)

surfaceplot

#size/axis specifications
m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)

#contourplot
contourplot <- plot_ly(x=x,y=y, z=z,
                       type="contour",
                       intensity = ~z,
                       colors = colorRamp(
                         c("purple","blue","lightblue","green","yellow","orange","red")
                       ),
                       contours = list(coloring = 'heatmap')
) %>% layout(xaxis = axx, yaxis = axy, autosize = F, width = 600, height = 450, margin = m) %>% colorbar(title = "Implied vol") 

contourplot

#export
library(processx)
orca(contourplot, "contourplot.pdf") 
orca(surfaceplot, "surfaceplot.pdf")