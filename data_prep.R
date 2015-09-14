####
# Calculating some stats playlivemanager.com
####

## Load packages
library("googlesheets")
library("sqldf")
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("plyr"))

## Load source file for functions
source("functions.R")

## 1 Import Data from spreadsheet
# gs_auth() # get oauth for spreadsheets
trix <- gs_title("playlivemanager players")
bl0 <- gs_read(trix, ws=1)
bl1 <- gs_read(trix, ws=2)
bl2 <- gs_read(trix, ws=3)
bl3 <- gs_read(trix, ws=4)

##! TO DO: Write function to import all trixes ##

## Sanity Checks and Data Cleansing
# Check if Name column is unique
sqldf('SELECT NAME, COUNT(*) AS count FROM bl0 GROUP BY NAME ORDER BY count') # 2 T. Werner
sqldf('SELECT * FROM bl0 WHERE NAME = "T. Werner"') # Players have different positions, so can join on combined

# Check if there is data for all players
nrow(bl0) - nrow(bl3) # 3 more players in bl0
sqldf('SELECT bl0.NAME, bl3.NAME FROM bl0 LEFT JOIN bl3 USING(NAME, POS) ORDER BY bl3.NAME LIMIT 20') # lots of players due to August transfers (i.e. de Bruyne) or changes in spelling (i.e. Kuranyi)

# -> (1) Get rid of international characters
bl0$NAME <- to.plain(bl0$NAME)
bl1$NAME <- to.plain(bl1$NAME)
bl2$NAME <- to.plain(bl2$NAME)
bl3$NAME <- to.plain(bl3$NAME)
sqldf('SELECT * FROM bl3 WHERE NAME LIKE "%K. Kur%" OR NAME LIKE "%bjerg"') #Kuranyi test successful

# -> Build master database of values
master <- sqldf('SELECT "0" AS entry, * FROM bl0 UNION ALL
                 SELECT "1", NAME, STA, CLUB, POS, VALUE, EARNINGS, AVERAGE, ROUND, G, A, CS FROM bl1 UNION ALL
                 SELECT "2", NAME, STA, CLUB, POS, VALUE, EARNINGS, AVERAGE, ROUND, G, A, CS  FROM bl2 UNION ALL
                 SELECT "3", NAME, STA, CLUB, POS, VALUE, EARNINGS, AVERAGE, ROUND, G, A, CS  FROM bl3
')

# -> Split out initial market values
marketvalues <- sqldf('SELECT NAME, POS, MIN(VALUE) AS INIT_VALUE, MAX(entry) AS ENTRY FROM master WHERE AVERAGE = 0 GROUP BY NAME, POS, VALUE')

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

