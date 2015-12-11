#### Data Update
#### (1) Update data with latest matchday
#### (2) Integrate new players


## load libraries
library("sqldf")

### (1) Add latest matchday data

## Import Data, keep colnames
load(".RData")
raw_colnames = colnames(raw)
raw = read.csv("data/playerdata.csv")
colnames(raw) <- raw_colnames

## subset to BL only players
bl_raw <- sqldf('SELECT * FROM raw WHERE home_squad >= 1562 AND home_squad <=1579')

## Get latest round and matches played
max_matchday <- max(bl_raw$matchday)
round <- subset(bl_raw[c("id", "total_earnings")], bl_raw$matchday == max_matchday)
colnames(round) <- c("id", "Round")
totals <- sqldf('SELECT id, SUM(goal) AS G, SUM(assist) AS A, SUM(clean_sheet) AS CS, SUM(total_earnings) AS Earnings, COUNT(*) AS matches, SUM(total_earnings)/COUNT(*) AS Average
		FROM bl_raw
		WHERE status IN ("starter", "sub_in", "sub_out", "red_card")
		GROUP BY id')
webstyle <- sqldf('SELECT totals.id AS id, Earnings, Average, Round, G, A, CS 
		  FROM round JOIN totals
		  ON round.id = totals.id')

## Update Formats and handle NAs
webstyle$G[is.na(webstyle$G)] <- 0
webstyle$A[is.na(webstyle$A)] <- 0
webstyle$CS[is.na(webstyle$CS)] <- 0
webstyle$G <- as.integer(webstyle$G)
webstyle$A <- as.integer(webstyle$A)


## UPDATE EXISTING DATAFRAMES
oneliner <- merge(oneliner[c("id", "Name", "Club", "Pos", "Value", "init_Value", "poscode")],
              webstyle, by = "id")
oneliner$Value <- oneliner$init_Value + round(floor(oneliner$Earnings / 100000 + 0.5)/10, 1)

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

cols2 = length(fullhouse)

save(cols, cols2, rounds, fullhouse, bl_raw, bl_rounds, oneliner, file = "dataprep.RData")