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

formoverall <- make.lp(0,nrow(oneliner)) # make linear programming with variables rows of st1
lp.control(formoverall, sense = "max")
set.objfn(formoverall, oneliner$Earnings) # obj.function to maformoverallimize
add.constraint(formoverall, oneliner$init_Value, "<=", 100) # add value constraint
# Position / Formation constraints
add.constraint(formoverall, (oneliner$Pos == "GOA")*1, "=", 1) # GOA
add.constraint(formoverall, (oneliner$Pos == "DEF")*1, ">=", 3) # DEF
add.constraint(formoverall, (oneliner$Pos == "DEF")*1, "<=", 5) # DEF
add.constraint(formoverall, (oneliner$Pos == "MID")*1, ">=", 3) # MID
add.constraint(formoverall, (oneliner$Pos == "MID")*1, "<=", 5) # MID
add.constraint(formoverall, (oneliner$Pos == "ATT")*1, ">=", 1) # ATT
add.constraint(formoverall, (oneliner$Pos == "ATT")*1, "<=", 3) # ATT
add.constraint(formoverall, rep(1, nrow(oneliner)), "=", 11) # 11 players
# Max 4 per club constraints
add.constraint(formoverall, (oneliner$Club == "FCB")*1, "<=", 4) # FCB
add.constraint(formoverall, (oneliner$Club == "BVB")*1, "<=", 4) # BVB

set.type(formoverall, c(1:nrow(oneliner)),"integer")
set.bounds(formoverall, lower = rep(0, nrow(oneliner)), upper = rep(1, nrow(oneliner)), columns = 1:nrow(oneliner))
write.lp(formoverall,'modeloverall.lp',type='lp') #write out frequently to check model

solve(formoverall)
get.objective(formoverall)
tempoverall <- oneliner
tempoverall$topoverall <- get.variables(formoverall)
tempoverall <- subset(tempoverall, tempoverall$topoverall == 1)
tempoverall[order(tempoverall$poscode),c(1:6,11)]
