best11(fullhouse$Round12)
best11cap(fullhouse$Round12)

## stratBest11Agg
opt <- (fullhouse$id %in% c(best11val(rowSums(fullhouse[(cols+1):(cols2)]), oneliner$Value, 120.5)[,1])*1)
temp <- fullhouse[,c(1:4,6:7)]
temp$Earnings <- rowSums(fullhouse[(cols+1):(cols2)])[,1]
temp$top <- opt
temp <- subset(temp, temp$top >= 1)
temp <- temp[order(temp$poscode),]

## Function to calculate best11
best11cap <- function(objective, value = 100) {     # where v is max total value, r = data to optimize
  
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
  add.constraint(formation, rep(1, nrow(fullhouse)), "=", 12) # 10 players, 1 captain
  add.constraint(formation, rep(1, nrow(fullhouse)), "=", 60) # 10 players, 1 captain!
  
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
  set.bounds(formation, lower = rep(0, nrow(fullhouse)), upper = rep(2, nrow(fullhouse)), columns = 1:nrow(fullhouse))
  
  # Solve model
  solve(formation)
  
  # Store output
  temp <- fullhouse[,c(1:4,6:7)]
  temp$Earnings <- objective
  temp$top <- get.variables(formation)
  temp <- subset(temp, temp$top >= 1)
  temp <- temp[order(temp$poscode),]
  
  #output
  return(temp[,c("top","id", "Name", "Club", "Pos", "init_Value", "Earnings")])
}






best11val <- function(objective, val, value = 100) {     # where v is max total value, r = data to optimize

	## Set up framework LPS
	formation <- make.lp(0,nrow(fullhouse)) 
	lp.control(formation, sense = "max")
	set.objfn(formation, objective) # obj.function to maformoverallimize

	# Total Value constraint
	add.constraint(formation, val, "<=", value)

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




best11val(rowSums(fullhouse[21:26]), 119.2)

