## Import Data
load("dataprep.RData")

## Load Libraries
library("sqldf")

## Clublist
clubs <- sqldf('SELECT home_shortname FROM bl_raw GROUP BY home_shortname')

## Matches
matches <- sqldf('SELECT match_id, matchday, home_shortname, away_shortname, home_score, away_score,
                  CASE WHEN home_score > away_score THEN 3 WHEN home_score = away_score THEN 1 ELSE 0 END AS home_points,
                  CASE WHEN home_score > away_score THEN 0 WHEN home_score = away_score THEN 1 ELSE 3 END AS away_points
                 FROM bl_raw GROUP BY match_id ORDER BY matchday, match_id')


## Join matches and clubs
matchlist = data.frame(matrix(nrow = 18, ncol = rounds+1))
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
colnames(clubspoints) <- gsub("home_shortname", "Club", colnames(clubspoints))
clubspoints$home_shortname <- factor(clubspoints$home_shortname)


## Add Average per matchday
averages = data.frame(matrix(nrow = 18, ncol = rounds+1))
for (i in 2:(rounds+1)) {
  for (t in 1:18) {
    points = rowSums(clubspoints[t,2:(i+1)])
    average = points / i
    averages[t,i] = average
  }
}
colnames(averages) <- gsub("X", "AVG", colnames(averages))

## Add to clubspoints
clubspoints <- cbind(clubspoints, averages)


## Add averages for last 4 matches
averagesl4 = data.frame(matrix(nrow = 18, ncol = rounds+1))
for (i in 5:(rounds+1)) {
  for (t in 1:18) {
    points = rowSums(clubspoints[t,(i-3):(i)])
    average = points / 4
    averagesl4[t,i] = average
  }
}
colnames(averagesl4) <- gsub("X", "AVGLFOUR", colnames(averages))

## Add to clubspoints
clubspoints <- cbind(clubspoints, averagesl4)


## Fill in NAs:
# AVG1: take last years averages; add mean 1.42 for FCI and D98
clubspoints[,rounds+2] <- c(1.79, 1.94, 1.26, 1.03, 1.35, 1.42, 1.44, 2.32, 1.42, 1.09, 1.03, 1.18, 1.18, 1.41, 1.26, 1.29, 1.06, 2.03)
# AVGL4[1:4] <- AVG[1:4]
clubspoints[,(2*rounds+2):(2*rounds+2+3)] <- clubspoints[,(rounds+2):(rounds+2+3)]



#### PART 2: Get differences from matches
# Add upcoming matchday...
navector <- data.frame(matrix(nrow=9))
matchdayvector <- data.frame(matrix(rep(rounds+1, 9)))
home_shortname <- c("D98", "H96", "TSG", "BRE", "FCB", "M05", "BVB", "FCA", "B04")
away_shortname <- c("KOE", "FCI", "BMG", "HSV", "BSC", "SGE", "VFB", "WOB", "S04")
matches_form <- cbind(navector, matchdayvector, home_shortname, away_shortname, navector, navector, navector, navector)
colnames(matches_form) <- colnames(matches)
matches <- rbind(matches, matches_form)

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


## Append to matches dataset
matches <- cbind(matches, avgpointdiff)

## Get into format for clubspoints
matchlist = data.frame(matrix(nrow = 18, ncol = rounds+1))
for (t in 1:18) {
  team = clubs[t,1]
  homepoints <- subset(matches$pointdiff, matches$home_shortname == team)
  homematches <- subset(matches$matchday, matches$home_shortname == team)
  awaypoints <- subset(matches$pointdiff, matches$away_shortname == team)*-1
  awaymatches <- subset(matches$matchday, matches$away_shortname == team)
  matchday <- c(homematches, awaymatches)
  points <- c(homepoints, awaypoints)
  df <- data.frame(matchday, points)
  sort <- sqldf('SELECT points FROM df ORDER BY matchday')
  sort <- as.data.frame(t(sort))
  matchlist[t,] <- sort
}
clubspoints <- cbind(clubspoints, matchlist)
colnames(clubspoints) <- gsub("X", "Diff", colnames(clubspoints))


save(bl_raw, bl_rounds, oneliner, strategies, clubspoints, matches, file = "dataprep.RData")
