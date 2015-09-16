####
# Some player analysis for playlivemanager.com
####

## Load packages
library("sqldf")
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
png("~/Downloads/plots/plot1.png")
ggplot(data, aes(x = INIT_VALUE, y = EARNINGS, color = POS)) +
  geom_point(size = 3, alpha = 0.7) +
  #geom_abline(intercept = 0, slope = 0, colour = "grey40") +
  geom_smooth(method = "loess", se=FALSE, size = 1)
dev.off()

png("~/Downloads/plots/plot2.png")
ggplot(data, aes(x=INIT_VALUE, y=EARNINGS, color=POS)) +
    geom_point(shape=1) +
    scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    geom_smooth(method=lm,   # Add linear regression lines
    se=FALSE)    # Don't add shaded confidence region
dev.off()


## Calculate scatter plot only for players who scored points
players <- sqldf('SELECT * FROM data WHERE EARNINGS <> 0')

png("~/Downloads/plots/plotplayers2.png")
ggplot(players, aes(x=INIT_VALUE, y=EARNINGS, color=POS)) +
    geom_point(shape=1) +
    scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    geom_smooth(method=lm,   # Add linear regression lines
    se=FALSE)    # Don't add shaded confidence region
dev.off()

png("~/Downloads/plots/plotplayers1.png")
ggplot(players, aes(x = INIT_VALUE, y = EARNINGS, color = POS)) +
  geom_point(size = 3, alpha = 0.7) +
  #geom_abline(intercept = 0, slope = 0, colour = "grey40") +
  geom_smooth(method = "loess", se=FALSE, size = 1)
dev.off()


## Scatter PPM vs VALUE
png("~/Downloads/plots/plotppm1.png")
ggplot(data, aes(x = INIT_VALUE, y = PPM, color = POS)) +
  geom_point(size = 3, alpha = 0.7) +
  #geom_abline(intercept = 0, slope = 0, colour = "grey40") +
  geom_smooth(method = "loess", se=FALSE, size = 1)
dev.off()

png("~/Downloads/plots/plotppm2.png")
ggplot(data, aes(x=INIT_VALUE, y=PPM, color=POS)) +
    geom_point(shape=1) +
    scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    geom_smooth(method=lm,   # Add linear regression lines
    se=FALSE)    # Don't add shaded confidence region
dev.off()


## Boxplots Value and PPM by POS
POSbyMedian <- with(data, reorder(POS, PPM, median)) # get positions in order of PPM median
png("~/Downloads/plots/boxplotppmpos.png")
ggplot(data, aes(x=POSbyMedian, y=PPM, fill=POS)) +
    geom_boxplot() +
    xlab("Position") +   
    guides(fill=FALSE) +
    scale_colour_hue(l=50) 
dev.off()

POSbyMedian <- with(players, reorder(POS, PPM, median)) # get positions in order of PPM median
png("~/Downloads/plots/boxplotppmposplayersonly.png")
ggplot(players, aes(x=POSbyMedian, y=PPM, fill=POS)) +
    geom_boxplot() +
    xlab("Position") +   
    guides(fill=FALSE) +
    scale_colour_hue(l=50) 
dev.off()


## Boxplots PPM by Club
CLUBbyMedian <- with(data, reorder(CLUB, PPM, median)) # get positions in order of CLUB median
png("~/Downloads/plots/boxplotppmclub.png")
ggplot(data, aes(x=CLUBbyMedian, y=PPM, fill=CLUB)) +
    geom_boxplot() +
    xlab("Club") +   
    guides(fill=FALSE) +
    scale_colour_hue(l=50) 
dev.off()

CLUBbyMedian <- with(players, reorder(CLUB, PPM, median)) # get positions in order of CLUB median
png("~/Downloads/plots/boxplotppmclubplayersonly.png")
ggplot(players, aes(x=CLUBbyMedian, y=PPM, fill=CLUB)) +
    geom_boxplot() +
    xlab("Club") +   
    guides(fill=FALSE) +
    scale_colour_hue(l=50) 
dev.off()


## Boxplots Earnings by POS
POSbyMedian <- with(data, reorder(POS, EARNINGS, median)) # get positions in order of PPM median
png("~/Downloads/plots/boxplotpointspos.png")
ggplot(data, aes(x=POSbyMedian, y=EARNINGS, fill=POS)) +
    geom_boxplot() +
    xlab("Position") +   
    guides(fill=FALSE) +
    scale_colour_hue(l=50) 
dev.off()

POSbyMedian <- with(players, reorder(POS, EARNINGS, median)) # get positions in order of PPM median
png("~/Downloads/plots/boxplotpointsposplayersonly.png")
ggplot(players, aes(x=POSbyMedian, y=EARNINGS, fill=POS)) +
    geom_boxplot() +
    xlab("Position") +   
    guides(fill=FALSE) +
    scale_colour_hue(l=50) 
dev.off()


## Boxplots EARNINGS by Club
CLUBbyMedian <- with(data, reorder(CLUB, EARNINGS, median)) # get positions in order of CLUB median
png("~/Downloads/plots/boxplotpointsclub.png")
ggplot(data, aes(x=CLUBbyMedian, y=EARNINGS, fill=CLUB)) +
    geom_boxplot() +
    xlab("Club") +   
    guides(fill=FALSE) +
    scale_colour_hue(l=50) 
dev.off()

CLUBbyMedian <- with(players, reorder(CLUB, EARNINGS, median)) # get positions in order of CLUB median
png("~/Downloads/plots/boxplotpointsclubplayersonly.png")
ggplot(players, aes(x=CLUBbyMedian, y=EARNINGS, fill=CLUB)) +
    geom_boxplot() +
    xlab("Club") +   
    guides(fill=FALSE) +
    scale_colour_hue(l=50) 
dev.off()


# Density plots for players and PPM / Earnings
ggplot(dat, aes(x=rating, colour=cond)) + geom <- density()

png("~/Downloads/plots/densePPMPOS.png")
ggplot(players, aes(x=PPM, fill = POS)) +
    #geom_histogram(alpha = .7) +
    geom_density(alpha = 0.4) +
    #guides(fill=FALSE) +
    scale_colour_hue(l=50) 
dev.off()

png("~/Downloads/plots/histPPMPOS.png") # histogram doesn't make much sense b/c less GOA&ATT than DEF & MID
ggplot(players, aes(x=PPM, fill = POS)) +
    geom_histogram(binwidth = 5, position = "dodge", alpha = 1) +
    #geom_density(alpha = 0.4) +
    #guides(fill=FALSE) +
    scale_colour_hue(l=50) 
dev.off()
