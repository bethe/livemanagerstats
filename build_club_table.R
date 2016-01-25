## Import Data
load("dataprep.RData")
spielplan = read.csv("~/R/livemanager/matches.csv")


## Load Libraries
library("sqldf")

## Clublist
clubs <- sqldf('SELECT home_shortname AS Club FROM bl_raw GROUP BY home_shortname')
clubs$Club <- factor(clubs$Club)

## Matches
matches <- sqldf('SELECT match_id, matchday, home_shortname, away_shortname, home_score, away_score,
                  CASE WHEN home_score > away_score THEN 3 WHEN home_score = away_score THEN 1 ELSE 0 END AS home_points,
                  CASE WHEN home_score > away_score THEN 0 WHEN home_score = away_score THEN 1 ELSE 3 END AS away_points
                 FROM bl_raw GROUP BY match_id ORDER BY matchday, match_id')
matches$home_shortname <- factor(matches$home_shortname)
matches$away_shortname <- factor(matches$away_shortname)

## Join matches and clubs
matchlist = data.frame(matrix(nrow = 18, ncol = rounds))
for (t in 1:18) {
  team = clubs[t,1]
  homepoints <- subset(matches$home_points, matches$home_shortname == team)
  homematches <- subset(matches$matchday, matches$home_shortname == team)
  awaypoints <- subset(matches$away_points, matches$away_shortname == team)
  awaymatches <- subset(matches$matchday, matches$away_shortname == team)
  matchday <- c(homematches, awaymatches)
  points <- c(homepoints, awaypoints)
  df <- data.frame(matchday, points)
  sort <- sqldf('SELECT points FROM df ORDER BY matchday')
  sort <- as.data.frame(t(sort))
  matchlist[t,] <- sort
}
clubspoints <- cbind(clubs, matchlist)
colnames(clubspoints) <- gsub("X", "R", colnames(clubspoints))


## Add Average per matchday, incl. last years for Round1
averages = data.frame(matrix(nrow = 18, ncol = rounds+1))
# take last years average for Round1 ; add mean 1.42 for FCI and D98
averages[,1] <- c(1.79, 1.94, 1.26, 1.03, 1.35, 1.42, 1.44, 2.32, 1.42, 1.09, 1.03, 1.18, 1.18, 1.41, 1.26, 1.29, 1.06, 2.03)
averages[,2] <- (averages[,1] + clubspoints[,2])/2
for (i in 3:(rounds+1)) {
  for (t in 1:18) {
    points = rowSums(clubspoints[t,2:i]) + averages[t,1]
    average = points / i
    averages[t,i] = average
  }
}
colnames(averages) <- gsub("X", "AVG", colnames(averages))

## Add to clubspoints
clubspoints <- cbind(clubspoints, averages)


## Add averages for last 6 matches
averagesl6 = data.frame(matrix(nrow = 18, ncol = rounds+1))
for (i in 7:(rounds+1)) {
  for (t in 1:18) {
    points = rowSums(clubspoints[t,(i-5):(i)])
    average = points / 6
    averagesl6[t,i] = average
  }
}
# Fill Rounds1-6 with data from AVG
averagesl6[,1:6] <- averages[,1:6]
# Update colnames
colnames(averagesl6) <- gsub("X", "AVGLSIX", colnames(averagesl6))

## Add to clubspoints
clubspoints <- cbind(clubspoints, averagesl6)


#### PART 2: Get differences from matches

## Append upcoming matchday to matches
navector <- data.frame(matrix(nrow=9))
upcoming_matchday <- spielplan[(nrow(matches)+1):(nrow(matches)+9),]
matches_form <- cbind(navector, upcoming_matchday, navector, navector, navector, navector)
colnames(matches_form) <- colnames(matches)
matches <- rbind(matches, matches_form)


## Calculate Point Differences
avgpointdiff = data.frame(matrix(nrow = nrow(matches), ncol = 1))
for (i in 1:(rounds+1)) { 
  for (m in 1:9) {
    homeavg = subset(clubspoints[,rounds+i+1], clubspoints$Club == matches$home_shortname[((i-1)*9)+m])
    awayavg = subset(clubspoints[,rounds+i+1], clubspoints$Club == matches$away_shortname[((i-1)*9)+m])
    diff = homeavg - awayavg
    
    avgpointdiff[(i-1)*9+m,] <- diff
  }
}
colnames(avgpointdiff) <- c("pointdiff")


## Calculate Point Differences Last 6
avgpointdiffL6 = data.frame(matrix(nrow = nrow(matches), ncol = 1))
for (i in 1:(rounds+1)) { 
  for (m in 1:9) {
    homeavg = subset(clubspoints[,2*rounds+i+1], clubspoints$Club == matches$home_shortname[((i-1)*9)+m])
    awayavg = subset(clubspoints[,2*rounds+i+1], clubspoints$Club == matches$away_shortname[((i-1)*9)+m])
    diff = homeavg - awayavg
    
    avgpointdiffL6[(i-1)*9+m,] <- diff
  }
}
colnames(avgpointdiffL6) <- c("pointdiffL6")


## Append to matches dataset
matches <- cbind(matches, avgpointdiff, avgpointdiffL6)



## Save
save(bl_raw, bl_rounds, oneliner, strategies, clubspoints, matches, file = "dataprep.RData")
