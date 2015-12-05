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



# Add Code for position for sorting later
oneliner$poscode <- as.integer(revalue(oneliner$Pos, c("GOA" = 1, "DEF" = 2, "MID" = 3, "ATT" = 4)))



# Add points earned by matchday in new dataframe "fullhouse"
fullhouse <- oneliner
fullhouse$Round <- NULL
bl_rounds <- subset(bl_raw, bl_raw$id %in% fullhouse$id)

# get number of rounds played
rounds = max(bl_rounds$matchday)

# get columns in dataset now to rename additional columns later
cols = length(fullhouse)

# loop to get all earnings by round
for (i in 1:rounds) {
	round_only <-  subset(bl_rounds["total_earnings"], bl_rounds$matchday == i )
	fullhouse <- transform(fullhouse, round = round_only)
	# NOTE: works as long as both fullhouse & round are ordered by id
	}

# loop to rename columns to "RoundX" format
for (i in (cols+1):(cols+rounds)) {
	colnames(fullhouse)[i] <- paste0("Round",i-cols)
}



## Function to calculate best11
best11 <- function(objective, value = 100) {     # where v is max total value, r = data to optimize

	## Set up framework LPS
	formation <- make.lp(0,nrow(fullhouse)) 
	lp.control(formation, sense = "max")
	set.objfn(formation, objective) # obj.function to formoverallimize

	# Total Value constraint
	add.constraint(formation, fullhouse$init_Value, "<=", value)

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




# Count columns to facilitate renaming later
cols2 = length(fullhouse)
# Get Best11 for each round via Loop
for (i in 1:rounds) {
	round11 <-  best11(fullhouse[,(cols+i)], 100)[,1]
	round11df <- as.data.frame(fullhouse$id %in% c(round11)*1)
#	colnames(round11df)[1] <- "best11"
	fullhouse <- transform(fullhouse, best11 = round11df)
	# NOTE: only works as long as both fullhouse & longvector are ordered by id
	}

# loop to rename columns to "Best11RoundX" format
for (i in (cols2+1):(cols2+rounds)) {
	colnames(fullhouse)[i] <- paste0("Best11Round",i-2*rounds)
}

#Count columns again. Always handy.
cols3 = length(fullhouse)
# Add column with number of Best11s
fullhouse$Best11s <- rowSums(fullhouse[,((cols2+1):cols3)])


# Check checkedicheck
temp <- subset(fullhouse[,c("Name", "Club", "Pos", "init_Value", "poscode", "Best11s")], fullhouse$Best11s > 1)
temp[order(temp$poscode, -temp$Best11s),]


# Save for later
save(cols, cols2, cols3, rounds, bl_raw, bl_rounds, fullset, oneliner, fullhouse, file = "dataprep.RData")
save.image()
