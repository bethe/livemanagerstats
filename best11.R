####
# Calculating best 11 for playlivemanager.com
####

## Load packages
install.packages("googlesheets")
library("googlesheets")
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("plyr"))

## 1 Import Data from spreadsheet
gs_auth()
trix <- gs_title("playlivemanager players")
bl0 <- gs_read(trix, ws=1)
bl1 <- gs_read(trix, ws=2)

##! TO DO: Write function to import all trixes ##

## Get data in format for analysis
 # Merge pre-matchday values with post-matchday scores
st1_full <- merge(x=bl0, y=bl1, by=c("NAME", "POS"))
st1 <- st1_full[,c("NAME", "POS", "CLUB.x", "VALUE.x", "EARNINGS.y", "VALUE.y")]
st1$POS <- factor(st1$POS)

## Optimize with Linear Programming
 # Following instructions from http://lpsolve.sourceforge.net/5.5/R.htm
install.packages("lpSolve")
install.packages("lpSolveAPI")
library("lpSolve")
library("lpSolveAPI")

x <- make.lp(0,nrow(st1)) # make linear programming with variables rows of st1
lp.control(x, sense = "max")
write.lp(x,'model.lp',type='lp') #write out frequently to check model
set.objfn(x, st1$EARNINGS.y) # obj.function to maximize
add.constraint(x, st1$VALUE, "<=", 100) # add value constraint
add.constraint(x, rep(1, nrow(st1)), "=", 11) # vector of ones for 11player constraint
set.type(x, c(1:509),"integer")
set.bounds(x, lower = rep(0, nrow(st1)), upper = rep(1, nrow(st1)), columns = 1:509)

# Solve
solve(x)
get.objective(x)
get.variables(x)
get.constraints(x)

# Identify players
players <- get.variables(x)
form0 <- st1
form0$top11 <- players
form0 <- subset(st1, st1$top11 == 1)
form0[order(form0$poscode),1:5]

## Add formation constraints
st1$poscode <- as.integer(revalue(st1$POS, c("GOA" = 1, "DEF" = 2, "MID" = 4, "ATT" = 8)))
st1$poscode <- as.numeric(st1$poscode)


# 433
form433 <- x
lp.control(form433, sense = "max")
#add.constraint(form433, st1$poscode, "=", 45) # add 4-3-3 constraint
add.constraint(form433, st1$poscode, "=", 1*1+4*2+3*4+3*8) # add 4-3-3 constraint
write.lp(form433,'model433.lp',type='lp') #write out frequently to check model

solve(form433)
get.objective(form433)
st1$top433 <- get.variables(form433)
form433 <- subset(st1, st1$top433 == 1)
form433[order(form433$poscode),1:5]


# 352
form352 <- x
lp.control(form352, sense = "max")
#add.constraint(form352, st1$poscode, "=", 45) # add 4-3-3 constraint
delete.constraint(form352,3)
add.constraint(form352, st1$poscode, "=", ) # add 3-5-2 constraint
write.lp(form352,'model352.lp',type='lp') #write out frequently to check model

solve(form352)

st1$top352 <- get.variables(form352)
form352 <- subset(st1, st1$top352 == 1)
form352[order(form352$poscode),1:5]


##! TO DO: Check if local minima or global minima
--> Search for local vs global minima
