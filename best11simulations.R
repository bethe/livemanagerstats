####
# Calculating best 11 for playlivemanager.com
####



## Optimize with Linear Programming
 # Following instructions from http://lpsolve.sourceforge.net/5.5/R.htm
#install.packages("lpSolve")
#install.packages("lpSolveAPI")
library("lpSolve")
library("lpSolveAPI")
library("plyr")

# Import Data
load("dataprep.RData")



## Function to calculate best11 with a fixed player
best11fix <- function(playerid, objective, value = 100) {     # where v is max total value, r = data to optimize

	## Set up framework LPS
	formation <- make.lp(0,nrow(fullhouse)) 
	lp.control(formation, sense = "max")
	set.objfn(formation, objective) # obj.function to maformoverallimize

	# Total Value constraint
	add.constraint(formation, fullhouse$init_Value, "<=", value)

	# Fixed player constraint
	add.constraint(formation, (fullhouse$id == playerid)*1, "=", 1)

	# Position / Formation constraints
	add.constraint(formation, (fullhouse$Pos == "GOA")*1, "=", 1) # GOA
	add.constraint(formation, (fullhouse$Pos == "DEF")*1, ">=", 3) # DEF
	add.constraint(formation, (fullhouse$Pos == "DEF")*1, "<=", 5) # DEF
	add.constraint(formation, (fullhouse$Pos == "MID")*1, ">=", 3) # MID
	add.constraint(formation, (fullhouse$Pos == "MID")*1, "<=", 5) # MID
	add.constraint(formation, (fullhouse$Pos == "ATT")*1, ">=", 1) # ATT
	add.constraint(formation, (fullhouse$Pos == "ATT")*1, "<=", 3) # ATT
	add.constraint(formation, rep(1, nrow(fullhouse)), "=", 11) # 11 players

	# Max 4 per club constraints
	add.constraint(formation, (fullhouse$Club == "B04")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "BMG")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "BRE")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "BSC")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "BVB")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "D98")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "FCA")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "FCB")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "FCI")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "H96")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "HSV")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "KOE")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "M05")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "S04")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "SGE")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "TSG")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "VFB")*1, "<=", 4)
	add.constraint(formation, (fullhouse$Club == "WOB")*1, "<=", 4)

	# Set type & limit of LPS
	set.type(formation, c(1:nrow(fullhouse)),"integer")
	set.bounds(formation, lower = rep(0, nrow(fullhouse)), upper = rep(1, nrow(fullhouse)), columns = 1:nrow(fullhouse))
	
	# Solve model
	solve(formation)
	
	# Store output
	temp <- fullhouse[,c(1:4,6:7)]
	temp$Earnings <- objective
	temp$top <- get.variables(formation)
	temp <- subset(temp, temp$top == 1)
	temp <- temp[order(temp$poscode),]
	
	#output
	return(temp[,c("id", "Name", "Club", "Pos", "init_Value", "Earnings")])
}


## 1 get best11 nominations with 1 fixed player
simfix1 <- fullhouse[,1:4]
for (i in 1:nrow(simfix1)) {

	for (j in 1:rounds) {
		round11fix <-  best11fix(simfix1[i,1], fullhouse[,(cols+j)], 100)[,1]
		round11fixdf <- as.data.frame(simfix1$id %in% c(round11fix)*1)
		simfix1 <- transform(simfix1, best11 = round11fixdf)
	}
	# Progress update
  	cat('Processing player', i, 'of', nrow(simfix1), '. ID:', simfix1[i,1],'\n')
}
# Aggregate and add results to simfix1
simfix1$rowSums <- rowSums(simfix1[,(5:length(simfix1))])

#
# quick check
temp <- subset(simfix1[,c("id", "Name", "Club", "Pos", "rowSums")], simfix1$rowSums > 0)
head(temp[order(-temp$rowSums),],20)
#


# store in new simulation DB "sim"
sim <- as.data.frame(simfix1$rowSums)
names(sim) <- "FixOneNoms"

# add variable to weigh nominations by matches played
matches <- round(fullhouse$Earnings / fullhouse$Average)
sim$FixOneNomsWeighted <- sim$FixOneNoms / matches



## 2 Get best11 earnings with 1 fixed player
simfix1Earn <- fullhouse[,1:4]
for (i in 1:nrow(simfix1Earn)) {

	for (j in 1:rounds) {
		round11fix <-  best11fix(simfix1Earn[i,1], fullhouse[,(cols+j)], 100)
		round11fixdf <- as.data.frame(simfix1Earn$id %in% c(round11fix$id)*sum(round11fix$Earnings))
		simfix1Earn <- transform(simfix1Earn, best11 = round11fixdf)
	}
	# Progress update
  	cat('Processing player', i, 'of', nrow(simfix1Earn), '. ID:', simfix1Earn[i,1],'\n')
}
# Aggregate and add results to simfix1
simfix1Earn$rowSums <- rowSums(simfix1Earn[,(5:length(simfix1Earn))])


# store in new simulation DB "sim"
sim$FixOneEarn <- simfix1Earn$rowSums

# add variable to weigh nominations by matches played
sim$FixOneEarnAvg <- sim$FixOneEarn / sim$FixOneNoms



## 3 get last 5 best11 nominations with 1 fixed player
simfixFIVE <- fullhouse[,1:4]
for (i in 1:nrow(simfixFIVE)) {

	for (j in (rounds-5):rounds) {
		round11fix <-  best11fix(simfixFIVE[i,1], fullhouse[,(cols+j)], 100)[,1]
		round11fixdf <- as.data.frame(simfixFIVE$id %in% c(round11fix)*1)
		simfixFIVE <- transform(simfixFIVE, best11 = round11fixdf)
	}
	# Progress update
  	cat('Processing player', i, 'of', nrow(simfixFIVE), '. ID:', simfixFIVE[i,1],'\n')
}
# Aggregate and add results to simfixFIVE
simfixFIVE$rowSums <- rowSums(simfixFIVE[,(5:length(simfixFIVE))])


#
# quick check
temp <- subset(simfixFIVE[,c("id", "Name", "Club", "Pos", "rowSums")], simfixFIVE$rowSums > 0)
head(temp[order(-temp$rowSums),],20)
#


# store in new simulation DB "sim"
sim$FixOneNoms5 <- simfixFIVE$rowSums

# add variable to weigh nominations by matches played
sim$FixOneNoms5W <- sim$FixOneNoms5 / matches



## 6 Create function to get nominations for a variable range of rounds
simfixrounds <- function(x, y) { # x = first round, y = last round in range
	simfixtemp <- fullhouse[,1:4]
	for (i in 1:nrow(simfixtemp)) {
	
		for (j in x:y) {
			round11fix <-  best11fix(simfixtemp[i,1], fullhouse[,(cols+j)], 100)[,1]
			round11fixdf <- as.data.frame(simfixtemp$id %in% c(round11fix)*1)
			simfixtemp <- transform(simfixtemp, best11 = round11fixdf)
		}
		# Progress update
	  	cat('Processing player', i, 'of', nrow(simfixtemp), '. ID:', simfixtemp[i,1],'\n')
	}
	# Aggregate and add results to simfixtemp
	simfixtemp$Nominations <- rowSums(simfixtemp[,(5:length(simfixtemp))])

	# Output
	return(simfixtemp[,c("id", "Name", "Pos", "Nominations")])

}



# PLAY
earningsLastEight <- fullhouse[,(cols2-7):cols2]

### Strategies
  # 0 Optimum
stratOptimum <- fullhouse[,(cols3-7):cols3]
  # 1 Previous week's top 11
#stratPrevWeek <- fullhouse[,(cols3-6):(cols3-1)]
#  # 2 Most nominated all
#x <- as.data.frame(fullhouse$id %in% c(best11(sim$FixOneNoms)[,1])*1)
#stratMostNom <- cbind(x,x,x,x,x,x) # Get into format for 6 weeks
#  # 3 Most nominated weighted
#x <- as.data.frame(fullhouse$id %in% c(best11(sim$FixOneNomsWeighted)[,1])*1)
#stratMostNomW <- cbind(x,x,x,x,x,x) # Get into format for 6 weeks
#  # 4 Most nominated Earnings
#x <- as.data.frame(fullhouse$id %in% c(best11(sim$FixOneEarn)[,1])*1)
#stratMostNomEarn <- cbind(x,x,x,x,x,x) # Get into format for 6 weeks
#  # 5 Average Earnings
#x <- as.data.frame(fullhouse$id %in% c(best11(sim$FixOneEarnAvg)[,1])*1)
#stratAvgNomEarn <- cbind(x,x,x,x,x,x) # Get into format for 6 weeks
#  # 6 Most nominations last 5 weeks (post)
#x <- as.data.frame(fullhouse$id %in% c(best11(sim$FixOneNoms5)[,1])*1)
#stratNoms5 <- cbind(x,x,x,x,x,x) # Get into format for 6 weeks
#  # 7 Most nominations last 5 weeks Weighted (post)
#x <- as.data.frame(fullhouse$id %in% c(best11(sim$FixOneNoms5W)[,1])*1)
#stratNoms5W <- cbind(x,x,x,x,x,x) # Get into format for 6 weeks
#  # 8 Most nominations last 5 weeks moving
#L5R1 <- best11(simfixrounds((cols-10),(cols-6))[,4])
#stratNom5Moving <- as.data.frame(fullhouse$id %in% c(L5R1[,1])*1)
#colnames(stratNom5Moving)[1] <- "Round7"
#L5R2 <- best11(simfixrounds((cols-9),(cols-5))[,4])
#stratNom5Moving$Round8 <- as.data.frame(fullhouse$id %in% c(L5R2[,1])*1)
#colnames(stratNom5Moving)[2] <- "Round8"
#L5R3 <- best11(simfixrounds((cols-8),(cols-4))[,4])
#stratNom5Moving$Round9 <- as.data.frame(fullhouse$id %in% c(L5R3[,1])*1)
#colnames(stratNom5Moving)[3] <- "Round9"
#L5R4 <- best11(simfixrounds((cols-7),(cols-3))[,4])
#stratNom5Moving$Round10 <- as.data.frame(fullhouse$id %in% c(L5R4[,1])*1)
#colnames(stratNom5Moving)[4] <- "Round10"
#L5R5 <- best11(simfixrounds((cols-6),(cols-2))[,4])
#stratNom5Moving$Round11 <- as.data.frame(fullhouse$id %in% c(L5R5[,1])*1)
#colnames(stratNom5Moving)[5] <- "Round11"
#L5R6 <- best11(simfixrounds((cols-5),(cols-1))[,4])
#stratNom5Moving$Round12 <- as.data.frame(fullhouse$id %in% c(L5R6[,1])*1)
#colnames(stratNom5Moving)[6] <- "Round12"
## 9 Most Earnings total
#x <- as.data.frame(fullhouse$id %in% c(best11(fullhouse$Earnings)[,1])*1)
#stratBest11Total <- cbind(x,x,x,x,x,x) # Get into format for 6 weeks
  # 10 Most Earnings aggregating
x1 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols+1):(cols2-8)]))[,1])*1)
x2 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols+1):(cols2-7)]))[,1])*1)
x3 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols+1):(cols2-6)]))[,1])*1)
x4 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols+1):(cols2-5)]))[,1])*1)
x5 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols+1):(cols2-4)]))[,1])*1)
x6 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols+1):(cols2-3)]))[,1])*1)
x7 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols+1):(cols2-2)]))[,1])*1)
x8 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols+1):(cols2-1)]))[,1])*1)
stratBest11Agg <- cbind(x1,x2,x3,x4,x5,x6,x7,x8) # Get into format for 6 weeks
  # 11 Most Earnings Last 6 Weeks
x1 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-13):(cols2-8)]))[,1])*1)
x2 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-12):(cols2-7)]))[,1])*1)
x3 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-11):(cols2-6)]))[,1])*1)
x4 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-10):(cols2-5)]))[,1])*1)
x5 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-9):(cols2-4)]))[,1])*1)
x6 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-8):(cols2-3)]))[,1])*1)
x7 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-7):(cols2-2)]))[,1])*1)
x8 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-6):(cols2-1)]))[,1])*1)
stratBest11MoveL6 <- cbind(x1,x2,x3,x4,x5,x6,x7,x8) # Get into format for 8 weeks
# 12 Most Earnings Last 5 Weeks
x1 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-12):(cols2-8)]))[,1])*1)
x2 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-11):(cols2-7)]))[,1])*1)
x3 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-10):(cols2-6)]))[,1])*1)
x4 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-9):(cols2-5)]))[,1])*1)
x5 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-8):(cols2-4)]))[,1])*1)
x6 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-7):(cols2-3)]))[,1])*1)
x7 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-6):(cols2-2)]))[,1])*1)
x8 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-5):(cols2-1)]))[,1])*1)
stratBest11MoveL5 <- cbind(x1,x2,x3,x4,x5,x6,x7,x8) # Get into format for 8 weeks
# 13 Most Earnings Last 3 Weeks
x1 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-10):(cols2-8)]))[,1])*1)
x2 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-9):(cols2-7)]))[,1])*1)
x3 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-8):(cols2-6)]))[,1])*1)
x4 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-7):(cols2-5)]))[,1])*1)
x5 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-6):(cols2-4)]))[,1])*1)
x6 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-5):(cols2-3)]))[,1])*1)
x7 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-4):(cols2-2)]))[,1])*1)
x8 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-3):(cols2-1)]))[,1])*1)
stratBest11MoveL3 <- cbind(x1,x2,x3,x4,x5,x6,x7,x8) # Get into format for 8 weeks
# 14 Most Earnings Last 8 Weeks
x1 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-15):(cols2-8)]))[,1])*1)
x2 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-14):(cols2-7)]))[,1])*1)
x3 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-13):(cols2-6)]))[,1])*1)
x4 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-12):(cols2-5)]))[,1])*1)
x5 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-11):(cols2-4)]))[,1])*1)
x6 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-10):(cols2-3)]))[,1])*1)
x7 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-9):(cols2-2)]))[,1])*1)
x8 <- as.data.frame(fullhouse$id %in% c(best11(rowSums(fullhouse[(cols2-8):(cols2-1)]))[,1])*1)
stratBest11MoveL8 <- cbind(x1,x2,x3,x4,x5,x6,x7,x8) # Get into format for 8 weeks
# 15 Best 11 based on regression analysis
x1 <- (fullhouse$id %in% c(best11(predictionslm[,(rounds-8)])[,1])*1)
x2 <- (fullhouse$id %in% c(best11(predictionslm[,(rounds-7)])[,1])*1)
x3 <- (fullhouse$id %in% c(best11(predictionslm[,(rounds-6)])[,1])*1)
x4 <- (fullhouse$id %in% c(best11(predictionslm[,(rounds-5)])[,1])*1)
x5 <- (fullhouse$id %in% c(best11(predictionslm[,(rounds-4)])[,1])*1)
x6 <- (fullhouse$id %in% c(best11(predictionslm[,(rounds-3)])[,1])*1)
x7 <- (fullhouse$id %in% c(best11(predictionslm[,(rounds-2)])[,1])*1)
x8 <- (fullhouse$id %in% c(best11(predictionslm[,(rounds-1)])[,1])*1)
stratBest11RegrLM <- cbind(x1,x2,x3,x4,x5,x6,x7,x8) # Get into format for 8 weeks



### strategy scores
strat0 <-colSums(earningsLastEight * stratOptimum)
#strat1 <-colSums(earningsLastEight * stratPrevWeek)
#strat2 <-colSums(earningsLastEight * stratMostNom)
#strat3 <-colSums(earningsLastEight * stratMostNomW)
#strat4 <-colSums(earningsLastEight * stratMostNomEarn)
#strat5 <-colSums(earningsLastEight * stratAvgNomEarn)
#strat6 <-colSums(earningsLastEight * stratNoms5)
#strat7 <-colSums(earningsLastEight * stratNoms5W)
#strat8 <-colSums(earningsLastEight * stratNom5Moving)
#strat9 <-colSums(earningsLastEight * stratBest11Total)
strat10 <-colSums(earningsLastEight * stratBest11Agg)
strat11 <-colSums(earningsLastEight * stratBest11MoveL6) ## Seems best so far...
strat12 <-colSums(earningsLastEight * stratBest11MoveL5)
strat13 <-colSums(earningsLastEight * stratBest11MoveL3)
strat14 <-colSums(earningsLastEight * stratBest11MoveL8)
strat15 <-colSums(earningsLastEight * stratBest11RegrLM) ## Sucks major ass, but Tyton and others look suspicious...

mine <- c(1247000, 1266000, 1633000,984500, 1859500, 1039500, 1208000, 998000)
strategies <- rbind(strat0, strat10, strat11, strat12, strat13, strat14, strat15, mine)
rowSums(strategies[,])

save(bl_raw, bl_rounds, oneliner, strategies, file = "dataprep.RData")
