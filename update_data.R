#### Data Update
#### (1) Update data with latest matchday
#### (2) Integrate new players


### (1) Add latest matchday data

## Import Data
load("dataprep.RData")
raw = read.csv("data/playerdata.csv")

## Data formatting
colnames(raw) <- gsub("\\player_status.", "", colnames(raw))
colnames(raw) <- gsub("\\matches_info.0.", "", colnames(raw))
colnames(raw) <- gsub("\\.", "_", colnames(raw))

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

save(bl_raw, bl_rounds, oneliner, file = "dataprep.RData")

##--> Move to best11.R from here

#fullhouse <- merge(fullhouse[, !names(fullhouse) %in% (c("Value", "Earnings", "Average", "G", "A", "CS"))],
#                   oneliner[, names(oneliner) %in% (c("id", "Value", "Earnings", "Average", "G", "A", "CS", "Round"))],
#                   by = "id")
#names(fullhouse) <- sub("^Round$", sprintf("Round%d", max_matchday), names(fullhouse))
#names(fullhouse) <- sub("^Tobi$", "Round", names(fullhouse))
