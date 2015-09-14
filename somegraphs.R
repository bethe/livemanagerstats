####
# Some player analysis for playlivemanager.com
####

## Load packages
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("plyr"))
library(sqldf)
library("ggplot2")

## 1 Import Data
load("dataprep.RData")
head(data)
head(marketvalues)
head(master)

## Add Points Per Million Column
data$PPM <- data$EARNINGS / data$INIT_VALUE
head(data[order(-data$PPM),1:7], 10) #Top 10 PPM
head(data[order(-data$EARNINGS),1:7], 10) #Top 10 Earnings
 
## Scatter plot of value vs earnings
plot(data$INIT_VALUE, data$EARNINGS)

png("~/Downloads/plots/plot1.png")
ggplot(data, aes(x = INIT_VALUE, y = EARNINGS, fill = POS)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 0, colour = "grey40") +
  geom_smooth(method = "loess", se=FALSE,  colour = "blue", size = 1)
dev.off()


## Regression, smoothing, etc.

## Sample Code
 +
   geom_abline(intercept = 0, slope = 0, colour = "grey40") +
   geom_smooth(method = "loess", se=TRUE,  colour = "#A0A0A0", size = 4) +
   geom_point(shape = 21, size = 3, alpha = 0.7) +
   scale_fill_gradient2(low = "#0000FF", mid = "#FF9933", high ="#FF0000",
                        midpoint = median(data$Glob)/100, space = "rgb", 
                        guide = "none", name = "Temperature Deviation (C)") +
   ggtitle(expression(atop("Rise in temperatures since 1880",
                      atop(italic("Global temperature deviations from the 1951-1980 average: Yearly values and Trend"), "")))) +
   ylab("Deviation (Degrees Celsius)") +   
   theme(axis.text.x = element_text(angle=90, face="bold", colour="black"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(colour = "white"),
         panel.border = element_rect(fill = "NA"))
