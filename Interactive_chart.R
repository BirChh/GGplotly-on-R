rm(list = ls())
#Packages used#####
library(dplyr)
library(ggplot2)
library(eeptools)
library(gridExtra)
library(pastecs)
library(reshape2)
library(plotly)
library(devtools)
##Reading data#######
setwd("C:/Users/Admin/Desktop/PP_Task")
returns=read.csv("Return.csv", stringsAsFactors = FALSE)
apply(is.na(returns),2,sum)
str(returns) #dataframe structure
returns$Date <- as.Date(returns$Date,format="%d/%m/%Y") 
des_stats <- round(stat.desc(returns[2:10]),4) 
des_stats <- des_stats[-c(1,2,3,7),]

##Time Series plot
meltdf <- melt(returns,id="Date")

p <- ggplot(meltdf,aes(x=Date, y=value, colour=variable, group=variable,
                        text = paste("<br>Date: ", as.Date(Date), "\n",
                                     "Value: ", value,
                                     "<br>Variable: ", variable))) + 
  geom_line()+
  scale_x_date(date_breaks = "1 year", date_labels = "%y")+
  theme(axis.text.x=element_text(angle=25,hjust=1),
        panel.background = element_rect(colour = "grey"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Monthly returns on different asset classes(2000-2018)",
       y = "Return(%)",
       x = "Time on Monthly Scale",
       colour="Asset Class")

p <- ggplotly(p, tooltip = c("text"))
p

#Barchart
des_stats1 <- data.frame(t(des_stats[]))
str(des_stats1)
des_stats2 <- setNames(cbind(rownames(des_stats1), des_stats1, row.names = NULL), 
         c("Asset_Class","Min", "Max", "Range", "Median", "Mean",
           "SE.Mean", "CI.Mean.0.95", "Var", "St.Dev", "Coeff.Var"))

#Table 
png("test.png",height = 350, width = 700)
p1<-tableGrob(des_stats2)
grid.arrange(p1)
dev.off()

#Descending
des_stats2$Asset_Class <- factor(des_stats2$Asset_Class, levels = des_stats2$Asset_Class[order((des_stats2$Mean))])

p2 <- ggplot(data = des_stats2, aes(x=Asset_Class, y=Mean))+
  geom_bar(stat = "identity", position = "dodge", fill="#00cc44")+
  labs(x="Return")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(colour = "grey"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=Mean))+
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)

p2

des_stats2$Asset_Class <- factor(des_stats2$Asset_Class, levels = des_stats2$Asset_Class[order((des_stats2$St.Dev))])

p3 <- ggplot(data = des_stats2, aes(x=Asset_Class, y=St.Dev))+
  geom_bar(stat = "identity", position = "dodge", fill="#ff704d")+
  labs(x="SD")+
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(colour = "grey"),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=St.Dev))+
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)
p3
#combine p2 and p3
library(gridExtra)
grid.arrange(p2, p3, nrow = 2, top="Average Return vs Risk across Asset Classes")
