####
# Datamonging playlivemanager.com
####

###
# TO DO: 
# (1) Clean Data Characters (Euro symbol, K, M) in R instead of spreadsheeds;
#     take care of Thomas Muller and other Earnings being converted from K to M
# (2) Write functions to automatically import/edit all sheets
###


## Load packages
library("googlesheets")
library("sqldf")

## Load source file for functions
source("functions.R")

## 1 Import Data from spreadsheet
# gs_auth() # get oauth for spreadsheets
trix <- gs_title("playlivemanager players")
import.sheets(trix)

## Sanity Checks and Data Cleansing
# Check if Name column is unique
sqldf('SELECT NAME, COUNT(*) AS count FROM bl1 GROUP BY NAME ORDER BY count') # 2 T. Werner
sqldf('SELECT * FROM bl1 WHERE NAME = "T. Werner"') # Players have different positions, so can join on combined

# Check if there is data for all players
nrow(bl1) - nrow(bl4) # 3 more players in bl0
sqldf('SELECT bl1.NAME, bl4.NAME FROM bl1 LEFT JOIN bl4 USING(NAME, POS) ORDER BY bl4.NAME LIMIT 20') # lots of players due to August transfers (i.e. de Bruyne) or changes in spelling (i.e. Kuranyi)

# -> (1) Get rid of international characters

## Trying with loop, not working yet...
#for (i in 1:length(gs_ws_ls(trix)))
#     {
#	assign(paste0("bl",i,"[,1]"), to.plain(paste0("bl",i,"[,1]")))
#	print(assign(paste0("bl",i,"[1,1]")))
#     }

bl1$NAME <- to.plain(bl1$NAME)
bl2$NAME <- to.plain(bl2$NAME)
bl3$NAME <- to.plain(bl3$NAME)
bl4$NAME <- to.plain(bl4$NAME)
sqldf('SELECT * FROM bl4 WHERE NAME LIKE "%K. Kur%" OR NAME LIKE "%bjerg"') #Kuranyi/Hojbjerg test successful

# -> Build master database of values
master <- sqldf('SELECT "1" AS entry, * FROM bl1 UNION ALL
                 SELECT "2", NAME, STA, CLUB, POS, VALUE, EARNINGS, AVERAGE, ROUND, G, A, CS FROM bl2 UNION ALL
                 SELECT "3", NAME, STA, CLUB, POS, VALUE, EARNINGS, AVERAGE, ROUND, G, A, CS FROM bl3 UNION ALL
                 SELECT "4", NAME, STA, CLUB, POS, VALUE, EARNINGS, AVERAGE, ROUND, G, A, CS FROM bl4
		')

# -> Split out initial market values
marketvalues <- sqldf('SELECT NAME, POS, MIN(VALUE) AS INIT_VALUE, MAX(entry) AS ENTRY 
		       FROM master WHERE AVERAGE = 0 GROUP BY NAME, POS, VALUE
		      ')

# Sanity check that no duplicates
nrow(marketvalues)
names <- sqldf('SELECT NAME, COUNT(*) AS count FROM marketvalues GROUP BY NAME')
sqldf('SELECT NAME, count FROM names WHERE count > 1') # only T. Werner...all good


## Get data in format for analysis
data <- sqldf('SELECT NAME, POS, CLUB, EARNINGS, AVERAGE, INIT_VALUE FROM bl3
               JOIN marketvalues USING(NAME, POS)')

## Save for later
save(master, marketvalues, data, file = "dataprep.RData")
save.image()

