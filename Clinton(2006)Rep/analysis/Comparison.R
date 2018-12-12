
library(plotly)
plot_ly(x=data$C_pctsp, y=data$C_pcti, z=data$x106kmean, type="scatter3d", mode="markers", color="partyname", colors=color)%>%
  layout(scene = list(xaxis = list(title = 'Percent Same Party'),
                      yaxis = list(title = 'Percent Independent'),
                      zaxis = list(title = 'Legislator Ideology')))

p <- plot_ly(
  x = data$splc, y = data$C_pctsp,
  z = data$x106kmean, type = "heatmap",colorscale = "Greys"
)

p <- plot_ly(z = volcano, colorscale = "Greys", type = "heatmap")

#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
# Option 1 for showing how vote changes with ideogogy
ggplot(data, aes(x=splc, y=C_pctsp, color=x106mean)) +
  geom_point() + geom_rug()

# Option 2 for showing how vote changes with ideogogy 2/ density circles
ggplot(data, aes(x=splc, y=C_pctsp, color=-(x106mean - x106kmean))) +
  geom_point() + 
  geom_rug()+
  #geom_density_2d()+
  scale_colour_gradient2(low = color[1], mid = "white", high = color[2])
#scale_colour_gradient2(low = "blue", mid = "white", high = "red")
#scale_colour_gradient(low = "blue", mid = "white", high = "red", midpoint = 0) #https://ggplot2.tidyverse.org/reference/scale_gradient.html
#scale_colour_gradient(low = color[1], high = color[2])


ggplot(data, aes(x=splc, y=nsplc, color=x106kmean)) +
  geom_point() + 
  geom_rug()+
  #geom_density_2d()+
  scale_colour_gradient2(low = color[1], mid = "white", high = color[2])
#scale_colour_gradient2(low = "blue", mid = "white", high = "red")
#scale_colour_gradient(low = "blue", mid = "white", high = "red", midpoint = 0) #https://ggplot2.tidyverse.org/reference/scale_gradient.html
#scale_colour_gradient(low = color[1], high = color[2])




# Option 3 for showing how vote changes with ideogogy 2/ density circles
ggplot(data, aes(x=splc, y=C_pctsp, color=x106mean)) +
  geom_point() + 
  geom_rug()+
  stat_density_2d(aes(fill = ..level..), geom="polygon")#+
scale_fill_gradient(low="blue", high="red")

# Option 3 for showing how vote changes with ideogogy 2/ density circle



#http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
# Option 4

ggplot(data, aes(x=splc, y=C_pctsp, color=x106kmean)) +
  #geom_point() +
  geom_bin2d()+
  geom_rug()+
  #geom_density_2d()+
  scale_colour_gradient2(low = color[1], mid = "white", high = color[2])

#https://ggplot2.tidyverse.org/reference/stat_summary_2d.html
ggplot(data, aes(x=splc, y=ilc, z=-(x106mean - x106kmean))) +
  #geom_point() +
  #geom_bin2d()+
  #stat_summary_2d(data,aes(z = splc), bins = 30, fun = mean)+
  geom_rug()+
  #geom_density_2d()+
  #stat_summary_2d(fun= max)+
  stat_summary_hex(bins= 30)+
  scale_fill_gradient2(low = color[1], mid = "white", high = color[2])

ggplot(data, aes(x=splc, y=ilc, z=x106mean)) +
  #geom_point() +
  #geom_bin2d()+
  #stat_summary_2d(data,aes(z = splc), bins = 30, fun = mean)+
  geom_rug()+
  #geom_density_2d()+
  #stat_summary_2d(fun= max)+
  stat_summary_hex(bins= 30)+
  scale_fill_gradient2(low = color[1], mid = "white", high = color[2])

# Same Party vs Independent ************** This is the one
ggplot(data = subset(data), aes(x=C_pctsp, y=C_pcti, z=(x106mean-mean(x106mean)))) +
  #geom_point() +
  #geom_bin2d()+
  #stat_summary_2d(data,aes(z = splc), bins = 30, fun = mean)+
  geom_rug(aes(colour=x106kmean))+
  #geom_density_2d()+
  #stat_summary_2d(fun= max)+
  stat_summary_hex(bins= 30, fun = mean)+
  scale_fill_gradient2(low = color[1], mid = "white", high = color[2])+
  scale_colour_gradient2(low = color[1], mid = "white", high = color[2])

ggplot(data = subset(data), aes(x=C_pctsp, y=C_pcti, z=R)) +
  #geom_point() +
  #geom_bin2d()+
  #stat_summary_2d(data,aes(z = splc), bins = 30, fun = mean)+
  geom_rug()+
  #geom_density_2d()+
  #stat_summary_2d(fun= max)+
  stat_summary_hex(bins= 30, fun = mean)+
  scale_fill_gradient(low = color[1], high = color[2])#+
scale_colour_gradient2(low = color[1], mid = "white", high = color[2])




# Non Party vs Independent
ggplot(data, aes(x=C_pctop, y=C_pcti, z=x106mean)) +
  #geom_point() +
  #geom_bin2d()+
  #stat_summary_2d(data,aes(z = splc), bins = 30, fun = mean)+
  geom_rug()+
  #geom_density_2d()+
  #stat_summary_2d(fun= max)+
  stat_summary_hex(bins= 40, fun = mean)+
  scale_fill_gradient2(low = color[1], mid = "white", high = color[2])

# Key observation
ggplot(data, aes(x=C_pctsp*splc+C_pcti*ilc,y=x106mean, colour=x106mean))+
  geom_point()+ #geom_smooth(method = "lm")  +
  #geom_smooth(aes(x = C_pctsp*splc+C_pcti*ilc, y=x106mean),method = "lm",col = "black",inherit.aes = FALSE)+
  scale_color_gradient2(low = color[1], mid = "white", high = color[2])

ggplot(data, aes(x=C_pctsp*splc+C_pcti*ilc,y=x106mean, colour=partyname))+
  geom_point()#+
#scale_color_gradient2(low = color[1], mid = "white", high = color[2])

ggplot(data, aes(x=lc,y=x106mean, color=partyname))+
  geom_point()#+
#scale_color_gradient2(low = color[1], mid = "white", high = color[2])

# Theory Plot
# Same Party vs Independent
testplot <- ggplot(data = subset(data,R==1), aes(x=C_pctsp, y=C_pcti/2,colour=x106mean)) + #colour=(x106mean-mean(x106mean))/sd(x106mean))
  geom_point(shape=1,size=2,colour="black") +
  geom_point()+#aes(colour=x106kmean)) +
  #geom_bin2d()+
  #stat_summary_2d(data,aes(z = splc), bins = 30, fun = mean)+
  geom_rug()+
  #geom_density_2d()+
  #stat_summary_2d(fun= max)+
  #stat_summary_hex(bins= 40, fun = mean)+
  #scale_color_gradient(low = color[1], high = color[2])
  scale_color_gradient2(low = color[1], mid = "white", high = color[2])+
  guides(col=FALSE, shape = FALSE) + theme(legend.position="bottom")  + guides(shape = FALSE)


testplot + expand_limits(x=0, y=0)+
  geom_abline(intercept = .5, slope = -1)+
  geom_vline(aes(xintercept=.5))+
  geom_hline(aes(yintercept=.5))


library(ggplot2)
ggplot(data, aes(X, Y, z= Z)) + geom_tile(aes(fill = Z)) + theme_bw()

# To change the color of the gradation :

## Beautiful
data2 <- data[order(-data$R,data$C_pctsp),] #Order data DEM to GOP by percent same party
switch <- data2[which(data2$R==1),] # select GOP districts
switch <- switch[order(-switch$C_pctsp),] #order GOP from smalles percent same party to larger
data2[which(data2$R==1),] <-switch   #Replace the order of GOP
trial <- as.data.frame(matrix(c(rep(0,432*3*9)),nrow=432*3,ncol=9))
trial[1:432,2] <- c(rep("SameParty",432))
trial[1:432,1] <- data2[,c("cdcoden")]
trial[1:432,3:8] <- data2[,c("splc","C_pctsp","partyname","lc","x106mean","x106kmean")]
trial[1:432,9]  <- c(1:432)
trial[433:864,2] <- c(rep("Independent",432))
trial[433:864,1] <- data2[,c("cdcoden")]
trial[433:864,3:8] <- data2[,c("ilc","C_pcti","partyname","lc","x106mean","x106kmean")]
trial[433:864,9]  <- c(1:432)
trial[865:1296,2] <- c(rep("OppositeParty",432))
trial[865:1296,1] <- data2[,c("cdcoden")]
trial[865:1296,3:8] <- data2[,c("oplc","C_pctop","partyname","lc","x106mean","x106kmean")]
trial[865:1296,9]  <- c(1:432)


ggplot(data = trial) + geom_bar(aes(y = V4, x = V1, fill = V2),
                    stat="identity")

 
q <-ggplot(data= subset(trial), aes(x = factor(V9), y = V4, fill = V3)) +
  geom_bar(position = position_stack(), stat = "identity", width =.8) +
  #geom_text(aes(label = V2), position = position_stack(vjust = 0.5), size = 2) +
  coord_flip()+
  scale_fill_gradient2(low = color[1], mid = "white", high = color[2])+
  scale_colour_gradient2(low = color[1], mid = "white", high = color[2])

q <-ggplot(data= subset(trial), aes(x = factor(V9), y = V4, fill = V3)) +
  geom_bar(position = position_stack(), stat = "identity", width =.8) +
  #geom_text(aes(label = V2), position = position_stack(vjust = 0.5), size = 2) +
  coord_flip()+
  scale_fill_gradient2(low = color[1], mid = "white", high = color[2])+
  scale_colour_gradient2(low = color[1], mid = "white", high = color[2])

q+ geom_point(data = trial,aes(x =factor(V9), y = (trial$V7+max(trial$V7))/(2*max(trial$V7)), size=1.0)) + 
  geom_point(data = trial,aes(x =factor(V9), y = (trial$V7+max(trial$V7))/(2*max(trial$V7)), colour=trial$V7))


q+ geom_point(data = trial,aes(x =factor(V9), y = (trial$V8+max(trial$V8))/(2*max(trial$V8)), size=1.0)) + 
  geom_point(data = trial,aes(x =factor(V9), y = (trial$V8+max(trial$V8))/(2*max(trial$V8)), colour=trial$V8))


summary(lm((x106kmean-x106mean)~lc+R,data=subset(data)))
summary(lm((x106kmean-x106mean)~C_pctsp*splc+C_pcti*ilc,data=subset(data)))
summary(lm((x106kmean-x106mean)~C_pctsp*splc+C_pcti*ilc+C_pctop*oplc,data=subset(data)))

summary(lm((x106kmean-x106mean)~C_pctsp*splc,data=subset(data)))
summary(lm((x106kmean-x106mean)~C_pctsp*splc+C_pcti*ilc,data=subset(data)))
summary(lm((x106kmean-x106mean)~C_pctsp*splc+C_pcti*ilc+C_pctop*oplc,data=subset(data)))

summary(lm((x106kmean-x106mean)~C_pctsp*splc,data=subset(data,R==1)))
summary(lm((x106kmean-x106mean)~C_pctsp*splc+C_pcti*ilc,data=subset(data,R==1)))
summary(lm((x106kmean-x106mean)~C_pctsp*splc+C_pcti*ilc+C_pctop*oplc,data=subset(data,R==1)))

summary(lm((x106kmean-x106mean)~C_pctsp*splc,data=subset(data,R==0)))
summary(lm((x106kmean-x106mean)~C_pctsp*splc+C_pcti*ilc,data=subset(data,R==0)))
summary(lm((x106kmean-x106mean)~C_pctsp*splc+C_pcti*ilc+C_pctop*oplc,data=subset(data,R==0)))


# Note do a regression on the change in blah.
#

ggplot(data= subset(trial), aes(x=pct_2013, xend=pct_2014, y=Area)) + 
  #create a thick line between x and xend instead of using defaut 
  #provided by geom_dubbell
  geom_segment(aes(x=pct_2013, 
                   xend=pct_2014, 
                   y=Area, 
                   yend=Area), 
               color="#b2b2b2", size=1.5)+
  geom_dumbbell(color="light blue", 
                size_x=3.5, 
                size_xend = 3.5,
                #Note: there is no US:'color' for UK:'colour' 
                # in geom_dumbbel unlike standard geoms in ggplot()
                colour_x="#edae52", 
                colour_xend = "#9fb059")+
  labs(x=NULL, y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Pct Change: 2013 vs 2014")+
  geom_text(color="black", size=2, hjust=-0.5,
            aes(x=pct_2013, label=pct_2013))+
  geom_text(aes(x=pct_2014, label=pct_2014), 
            color="black", size=2, hjust=1.5)









q + aes(x = fct_inorder(V4[which(trial$V2=="SameParty")]))

mean(data$x106kmean[which(data$C_pctsp>.5 & data$R==1)])
mean(data$x106kmean[which(data$C_pctsp<.5 & data$R==1)])
mean(data$x106kmean[which(data$C_pcti<.5 & data$R==1)])

mean(data$x106kmean[which(data$C_pctsp>.5)])
mean(data$x106kmean[which(data$C_pctsp<.5)])
mean(data$x106kmean[which(data$C_pcti<.5)])


# 3D
plot_ly(x=data$C_pctsp[data$R==1], y=data$C_pctop[data$R==1], z=data$C_pcti[data$R==1], type="scatter3d", mode="markers", color=data$x106kmean[data$R==1],colors=color)%>%
  layout(scene = list(xaxis = list(title = 'Percent Same Party'),
                      yaxis = list(title = 'Percent Opposite Party'),
                      zaxis = list(title = 'Percent Independent')))






ggplot(data = subset(data), aes(x=C_pctsp, y=C_pcti, colour=partyname)) +
  geom_point() +
  #geom_bin2d()+
  #stat_summary_2d(data,aes(z = splc), bins = 30, fun = mean)+
  geom_rug()#+
#geom_density_2d()+
#stat_summary_2d(fun= max)+
#stat_summary_hex(bins= 40, fun = mean)+
#scale_color_gradient2(low = color[1], mid = "white", high = color[2])


# Trying something big!

data$uni <- runif(nrow(data), min = 0, max = 1)
data$test <- 1
data$test[which(data$uni<.5)] <- 0
data$MRS <- data$ilc+5/data$splc+5

ggplot(data = subset(data), aes(x=MRS, y=C_pcti-C_pctsp, color=partyname)) +
  geom_point() 

summary(lm(x106kmean ~ 0 + C_pctsp*splc, data=data))
summary(lm(x106kmean ~ 0 + C_pctsp*splc+C_pcti*ilc, data=data))
summary(lm(x106mean ~ 0 + C_pctsp*splc+C_pcti*ilc+C_pctop*oplc, data=data))


summary(lm(x106kmean ~ 0 + C_pctsp*splc + R, data=data))
summary(lm(x106kmean ~ 0 + C_pctsp*splc+C_pcti*ilc + R, data=data))
summary(lm(x106kmean ~ 0 + C_pctsp*splc+C_pcti*ilc+C_pctop*oplc + R, data=data))



summary(lm(x106kmean ~ 0+ C_pctsp*splc+C_pcti*ilc+C_pctop*oplc, data=subset(data,R==0)))
summary(lm(x106mean ~ C_pctsp*splc+C_pcti*ilc, data=subset(data,test==0)))


summary(lm(x106mean ~ C_pctsp*splc+C_pcti*ilc + C_pctnsp*nsplc, data=data))


E = data$C_pctsp+data$S_pcti
test = lm(E~0+data$C_pctsp+data$S_pcti + data$R)



####### Bar
data2 -> 





