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

## Linear Regressions by player - next matchday
upcoming = data.frame(matrix(nrow=18, ncol=3))
diffs = tail(matches[,9:10],9)
upcoming[,1] <- upcoming_matchday$home_full
upcoming[10:18,1] <- upcoming_matchday$away_full
upcoming[,2:3] <- diffs
upcoming[10:18,2:3] <- -diffs
predictionsnext = data.frame(matrix(nrow = nrow(oneliner), ncol = 1))
for (p in 1:nrow(oneliner)) {
  
  oneplayer = playersmatches[((p-1)*rounds+1):(p*rounds),]
  lmfit = lm(total_earnings ~ matchday + matchday1x + pointdiff + pointdiffL6 + played, oneplayer, na.action = na.omit)
  pdiffs = subset(upcoming[,2:3], upcoming[,1] == oneplayer$Club[1])
  temp <- data.frame(t(c(rounds+1, 1, pdiffs[,1], pdiffs[,2], mean(oneplayer$played))))
  colnames(temp) <- c("matchday", "matchday1x", "pointdiff", "pointdiffL6", "played")
  pred = predict(lmfit, temp, interval = "prediction")
  predictionsnext[p,] <- pred[1,1]
}
colnames(predictionsnext) <- c("predictions")

ggplot(predictionslm, aes(x=predictionslm)) + geom_histogram()
summary(predictionslm)  


## Linear Regressions for each player p and matchday i
predictionslm = data.frame(matrix(nrow = nrow(oneliner), ncol = rounds))
for (i in (1:rounds)) {
  diffs = data.frame(matrix(nrow=18, ncol=3))
  upcoming = matches[((i-1)*9+1):((i-1)*9+9),]
  diffs[,1] <- upcoming$home_shortname
  diffs[10:18,1] <- upcoming$away_shortname
  diffs[,2:3] <- upcoming[,8:9]
  diffs[10:18,2:3] <- -upcoming[,8:9]
  cat('Calculating Matchday', i)
  for (p in 1:nrow(oneliner)) {   
    oneplayer = playersmatches[((p-1)*rounds+1):((p-1)*rounds+i),]
#    lmfit = lm(total_earnings ~ matchday + matchday1x + pointdiff + pointdiffL6 + played, oneplayer, na.action = na.omit)
    lmfit = lm(total_earnings ~ matchday + pointdiff, oneplayer, na.action = na.omit)
    pdiffs = subset(diffs[,2:3], diffs[,1] == oneplayer$Club[1])
#    temp <- data.frame(t(c(i+1, 1, pdiffs[,1], pdiffs[,2], mean(oneplayer$played))))
#    colnames(temp) <- c("matchday", "matchday1x", "pointdiff", "pointdiffL6", "played")
    temp <- data.frame(t(c(i+1, pdiffs[,1])))
    colnames(temp) <- c("matchday", "pointdiff")
    pred = predict(lmfit, temp)
    predictionslm[p,i] <- as.integer(pred)
  }
}
colnames(predictionslm) <- colnames(fullhouse[(cols+1):(cols+rounds)])

#ggplot(predictionslm, aes(x=predictionslm)) + geom_histogram()
summary(predictionslm)  


## Exclude max & min values for each player from dataset
playersmatchesss = data.frame(matrix(nrow = nrow(playersmatches)*((rounds-2)/rounds), ncol = ncol(playersmatches)))
for (p in 1:nrow(oneliner)) {   
  oneplayer = playersmatches[((p-1)*rounds+1):(p*rounds),]
  max = max(oneplayer$total_earnings[oneplayer$played == 1])
  min = min(oneplayer$total_earnings[oneplayer$played == 1])
  if (max > min) {
    # collect all mins in case there's more than one, then remove random row
    mins = which(oneplayer$total_earnings == min)
    # take out random row with min value in case there is more than 1
    oneplayer = subset(oneplayer, oneplayer$matchday != sample(mins,1))
    maxs = which(oneplayer$total_earnings == max)
    oneplayer = subset(oneplayer, oneplayer$matchday != sample(maxs,1))
    
    oneplayer = subset(oneplayer, oneplayer$total_earnings != max)
    oneplayer = subset(oneplayer, oneplayer$total_earnings != min)
  }
  if (max == min) {
    oneplayer = subset(oneplayer, oneplayer$total_earnings != max)
    zeros = which(oneplayer$total_earnings == 0)
    oneplayer = subset(oneplayer, oneplayer$matchday != oneplayer[sample(zeros,1),"matchday"])
  }  
  playersmatchesss[((p-1)*rounds+1):(p*rounds-2),] = oneplayer
}  
  
  
  ## Loess Regressions by player  
# --> need to check further how to calculate...

## scrap
sqldf('SELECT Name, id FROM playersmatches WHERE Name LIKE "%Lewandowski%"')

