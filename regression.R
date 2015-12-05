## Requires build_clab_table.R has been run

## Import Data
load("dataprep.RData")

## Load Libraries
library("sqldf")
library("ggplot2")


## Build database with 1 row per player per match
playersmatches <- sqldf('SELECT t1.id AS id, t3.Name, t3.Club, t1.matchday AS matchday, total_earnings, pointdiff, pointdiffL6 
                        FROM bl_raw AS t1
                        JOIN matches AS t2
                        JOIN oneliner AS t3
                        ON t1.match_id = t2.match_id
                        AND t1.id = t3.id')

## Add variable to indicate whether played or not
playersmatches$played[playersmatches$total_earnings == 0] <- 0
playersmatches$played[!(playersmatches$total_earnings == 0)] <- 1

# remove lines with NAs
#playersmatches0 <- playersmatches[!is.na(playersmatches$total_earnings),]

# Add matchdaysquared to give later matchdays more weight
playersmatches$matchdaysqrd <- playersmatches$matchday^2
# test 1-1/x to give later matchdays more weight
playersmatches$matchday1x <- 1 - (1 / playersmatches$matchday)

## Linear Regressions by player
upcoming = data.frame(matrix(nrow=18, ncol=3))
diffs = tail(matches[,9:10],9)
upcoming[,1] <- upcoming_matchday$home_full
upcoming[10:18,1] <- upcoming_matchday$away_full
upcoming[,2:3] <- diffs
upcoming[10:18,2:3] <- -diffs
predictionslm = data.frame(matrix(nrow = nrow(oneliner), ncol = 1))
for (p in 1:nrow(oneliner)) {
  
  oneplayer = playersmatches[((p-1)*rounds+1):(p*rounds),]
  lmfit = lm(total_earnings ~ matchday + matchday1x + pointdiff + pointdiffL6 + played, oneplayer, na.action = na.omit)
  pdiffs = subset(upcoming[,2:3], upcoming[,1] == oneplayer$Club[1])
  temp <- data.frame(t(c(rounds+1, 1, pdiffs[,1], pdiffs[,2], mean(oneplayer$played))))
  colnames(temp) <- c("matchday", "matchday1x", "pointdiff", "pointdiffL6", "played")
  pred = predict(lmfit, temp, interval = "prediction")
  predictionslm[p,] <- pred[1,1]
}
colnames(predictionslm) <- c("predictionslm")

ggplot(predictionslm, aes(x=predictionslm)) + geom_histogram()
summary(predictionslm)  


best11val()


## Loess Regressions by player  
# --> need to check further how to calculate...




## Quick testplay
Chicha <- sqldf('SELECT * FROM playersmatches WHERE Name LIKE "%J. Her%"')

lmfitsqrd <- lm(total_earnings ~ matchday1x + matchday,  Chicha)
summary(lmfitsqrd)

lmfitsqrd <- lm(total_earnings ~ matchday1x + matchday,  Chicha)
summary(lmfitsqrd)

# Lewandowski
Lewa <- sqldf('SELECT * FROM playersmatches WHERE Name LIKE "%Lewandowski"')
coef(lm(total_earnings ~ matchday, data = Lewa))
ggplot(oneplayer, aes(y = total_earnings, x = matchday^2)) +
  geom_point() +
#  stat_smooth(method="loess", se=FALSE)
  stat_smooth(method="lm", se=FALSE)
ggplot(Lewa, aes(x = total_earnings)) + geom_histogram()

# all
coef(lm(total_earnings ~ matchday, data = playersmatches))
ggplot(playersmatches, aes(y = total_earnings, x = matchday)) +
  geom_point() +
  stat_smooth(method="loess", se=FALSE)
  #stat_smooth(method="lm", se=FALSE)
ggplot(playersmatches, aes(x = total_earnings, color = Pos)) + geom_histogram()
  

## scrap
sqldf('SELECT Name, id FROM playersmatches WHERE Name LIKE "%Lewandowski%"')

