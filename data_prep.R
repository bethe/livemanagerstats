####
# Import and format data from playlivemanager.com
####

###
# TO DO: 
#  - import from trix: rewrite so no need to manually update latest matchday in query
#  - write separate "update-data" to only update initial stuff
###


## Load packages
library("sqldf")
library("plyr")

## 1 Import Data from webscrape
raw = read.csv("data/playerdata.csv")




# 2 Data formatting
#  simplify colnames, and use '_' as sqldf doesn't like '.' in variable names
colnames(raw) <- gsub("\\X__", "", colnames(raw))
colnames(raw) <- gsub("\\player_status_", "", colnames(raw))
colnames(raw) <- gsub("\\matches_info__", "", colnames(raw))
colnames(raw) <- gsub("\\.", "_", colnames(raw))


# subset to BL only players
#sqldf('SELECT home_squad, home_shortname FROM raw GROUP BY home_squad ORDER BY home_squad') #match squad ids to teams
bl_raw <- sqldf('SELECT * FROM raw WHERE home_squad >= 1562 AND home_squad <=1579')


# Put data into format of web UI to match player names & positions
## (Name) - (Sta) - Club - (POS) -(Value) - Earnings - Average - Round - G - A - CS

# Match players to club
player_club_temp <- sqldf('SELECT id, home_squad, home_shortname, COUNT(*) AS count FROM bl_raw GROUP BY id, home_shortname ORDER BY id')
player_club <- sqldf('SELECT id, home_squad AS squad, home_shortname AS Club, MAX(count) AS matches FROM player_club_temp GROUP BY id')

# Add position for GOA & ATT
player_pos <- sqldf('SELECT id, SUM(shot_on_target) AS shots, SUM(attempt_saved) AS saves FROM bl_raw GROUP BY id ORDER BY id')
player_pos$Pos[!(is.na(player_pos$shots))] <- "ATT"
player_pos$Pos[!(is.na(player_pos$saves))] <- "GOA"

## Get latest round and matches played
max_matchday <- max(bl_raw$matchday)
round <- sqldf('SELECT id, total_earnings AS Round 
	       FROM bl_raw WHERE matchday = 13')
totals <- sqldf('SELECT id, SUM(goal) AS G, SUM(assist) AS A, SUM(clean_sheet) AS CS, SUM(total_earnings) AS Earnings, COUNT(*) AS matches, SUM(total_earnings)/COUNT(*) AS Average
		FROM bl_raw
		WHERE status IN ("starter", "sub_in", "sub_out", "red_card")
		GROUP BY id')
webstyle <- sqldf('SELECT totals.id AS id, Club, Pos, Earnings, Average, Round, G, A, CS 
		  FROM round JOIN totals JOIN player_club JOIN player_pos
		  ON round.id = totals.id AND totals.id = player_club.id AND player_club.id = player_pos.id')



# Format Numbers into 1.0 for millions and 450 for thousands
# Note: round function is such a cunt; using little trick from http://stackoverflow.com/questions/12688717/round-up-from-5-in-r
webstyle$Earnings[webstyle$Earnings < 1000000] <- floor((webstyle$Earnings[webstyle$Earnings < 1000000])/1000 + 0.5) #thousands
webstyle$Earnings[webstyle$Earnings >= 1000000] <- round((webstyle$Earnings[webstyle$Earnings >= 1000000])/1000000,1) #Millions
webstyle$Average[webstyle$Average < 1000000] <- floor((webstyle$Average[webstyle$Average < 1000000])/1000 + 0.5) #thousands
webstyle$Round[webstyle$Round < 1000000] <- floor((webstyle$Round[webstyle$Round < 1000000])/1000 + 0.5) #thousands

# Adjust format for GACS
webstyle$G[is.na(webstyle$G)] <- 0
webstyle$A[is.na(webstyle$A)] <- 0
webstyle$CS[is.na(webstyle$CS)] <- 0
webstyle$G <- as.integer(webstyle$G)
webstyle$A <- as.integer(webstyle$A)



# 3 Import Web UI Data (via copy on Google Spreadsheets)
library("googlesheets")
trix <- gs_title("playlivemanager players")
bl <- gs_read(trix, ws=11)

# Drop Euro sign, millions M, K for thousands and convert 'NA's to 0 so that it's possible to join columns later
bl$VALUE <- gsub( "€ ", "", bl$VALUE)
bl$VALUE <- gsub( "M", "", bl$VALUE)
bl$EARNINGS <- gsub( "M", "", bl$EARNINGS)
bl$EARNINGS <- gsub( "K", "", bl$EARNINGS)
bl$AVERAGE <- gsub( "K", "", bl$AVERAGE)
bl$ROUND <- gsub( "K", "", bl$ROUND)
bl$G[bl$G == "-"] <- 0
bl$A[bl$A == "-"] <- 0
bl$CS[bl$CS == "-"] <- 0

# Align column formats between bl and webstyle
bl$VALUE <- as.numeric(bl$VALUE)
bl$EARNINGS <- as.numeric(bl$EARNINGS)
bl$AVERAGE <- as.numeric(bl$AVERAGE)
bl$ROUND <- as.numeric(bl$ROUND)
bl$CS <- as.integer(bl$CS)



# 4 Match & merge datasets
## Exclude players with 0 earnings
players <- sqldf('SELECT * FROM webstyle WHERE Earnings <> 0')
bl_players <- sqldf('SELECT * FROM bl WHERE EARNINGS <> 0')
player_ids <- sqldf('SELECT id FROM players')

## Match players with completely matching data (Pos only available for GOA & ATT)
full_match_1 <- sqldf('SELECT id, NAME, a.Club, b.POS, VALUE, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
	       FROM players a LEFT JOIN bl_players b
	       ON a.Earnings = b.EARNINGS
	       AND a.Club = b.CLUB
	       AND a.Pos = b.POS
	       AND a.Average = b.Average
	       AND a.Round = b.Round
	       AND a.G = b.G
	       AND a.A = b.A
	       AND a.CS = b.CS')
match_1 <- subset(full_match_1, !(is.na(full_match_1$VALUE)))
remaining_1 <- subset(players, !(players$id %in% match_1[,1]))
remaining_bl_1 <- sqldf('SELECT a.Name, a.Club, a.Pos, a.Value, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS FROM bl_players a LEFT OUTER JOIN match_1 b
			 ON a.NAME = b.NAME
			 AND a.Club = b.Club
			 WHERE b.NAME IS NULL
			')

## Match players with completely matching data excluding Pos
full_match_2 <- sqldf('SELECT id, NAME, a.Club, b.POS, VALUE, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
	       FROM remaining_1 a LEFT JOIN remaining_bl_1 b
	       ON a.Earnings = b.EARNINGS
	       AND a.Club = b.CLUB
	       AND a.Average = b.Average
	       AND a.Round = b.Round
	       AND a.G = b.G
	       AND a.A = b.A
	       AND a.CS = b.CS')
match_2 <- subset(full_match_2, !(is.na(full_match_2$VALUE)))
remaining_2 <- subset(remaining_1, !(remaining_1$id %in% match_2[,1]))
remaining_bl_2 <- sqldf('SELECT a.Name, a.Club, a.Pos, a.Value, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS FROM remaining_bl_1 a LEFT OUTER JOIN match_2 b
			 ON a.NAME = b.NAME
			 AND a.Club = b.Club
			 WHERE b.NAME IS NULL
			')

## Match players with (unique) matching Club/Round/G/A/S stats
remaining_grouped <- sqldf('SELECT Club, Pos, Round, G, A, CS, count(id) AS count FROM remaining_2 GROUP BY Club, Round, G, A, CS')
unique_combos <- sqldf('SELECT a.id, a.Club, a.Pos, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
			 FROM remaining_2 A JOIN remaining_grouped B
			 ON A.Club = B.Club
			 AND A.Round = B.Round
			 AND A.G = B.G
			 AND A.A = B.A
			 AND A.CS = B.CS
			 WHERE B.count = 1')
full_match_3 <- sqldf('SELECT a.id, NAME, a.Club, b.POS, VALUE, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
	       FROM unique_combos a LEFT OUTER JOIN remaining_bl_2 b
	       ON a.Club = b.Club
	       AND a.Round = b.Round
	       AND a.G = b.G
	       AND a.A = b.A
	       AND a.CS = b.CS')

## check for and remove duplicates from matches
a <- sqldf('SELECT id FROM (
      SELECT id, COUNT(*) count FROM full_match_3 GROUP BY 1)
      WHERE count <> 1')
avector <- a[,1]
match_3 <- subset(full_match_3, !(is.na(full_match_3$VALUE)))
match_3 <- subset(match_3, !(match_3$id %in% avector))

## Get remaining players
remaining_3 <- subset(remaining_2, !(remaining_2$id %in% match_3[,1]))
remaining_bl_3 <- sqldf('SELECT a.Name, a.Club, a.Pos, a.Value, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS FROM remaining_bl_2 a LEFT OUTER JOIN match_3 b
			 ON a.NAME = b.NAME
			 AND a.Club = b.Club
			 WHERE b.NAME IS NULL
			')

			
## Match remaining players
# by Club / Pos
match_4 <- sqldf('SELECT a.id, NAME, a.Club, b.POS, VALUE, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
	       FROM remaining_3 a JOIN remaining_bl_3 b
	       ON a.Club = b.Club
	       AND a.Pos = b.POS')
remaining_4 <- subset(remaining_3, !(remaining_3$id %in% match_4[,1]))
remaining_bl_4 <- sqldf('SELECT a.Name, a.Club, a.Pos, a.Value, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS FROM remaining_bl_3 a LEFT OUTER JOIN match_4 b
			 ON a.NAME = b.NAME
			 AND a.Club = b.Club
			 WHERE b.NAME IS NULL
			')

# by club & last round
match_5 <- sqldf('SELECT a.id, NAME, a.Club, b.POS, VALUE, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
	       FROM remaining_4 a JOIN remaining_bl_4 b
	       ON a.Club = b.Club
	       AND a.Round = b.Round
	       WHERE a.Round <> 0')
remaining_5 <- subset(remaining_4, !(remaining_4$id %in% match_5[,1]))
remaining_bl_5 <- sqldf('SELECT a.Name, a.Club, a.Pos, a.Value, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS FROM remaining_bl_4 a LEFT OUTER JOIN match_5 b
			 ON a.NAME = b.NAME
			 AND a.Club = b.Club
			 WHERE b.NAME IS NULL
			')

# by club and rounded average
remaining_5$EarningsR <- round(remaining_5$Earnings,-2)
remaining_bl_5$EarningsR <- round(remaining_bl_5$EARNINGS,-2)
match_6 <- sqldf('SELECT a.id, NAME, a.Club, b.POS, VALUE, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
	       FROM remaining_5 a JOIN remaining_bl_5 b
	       ON a.Club = b.Club
	       AND a.EarningsR = b.EarningsR')
remaining_6 <- subset(remaining_5, !(remaining_5$id %in% match_6[,1]))
remaining_bl_6 <- sqldf('SELECT a.Name, a.Club, a.Pos, a.Value, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS FROM remaining_bl_5 a LEFT OUTER JOIN match_6 b
			 ON a.NAME = b.NAME
			 AND a.Club = b.Club
			 WHERE b.NAME IS NULL
			')
fullset <- rbind(match_1, match_2, match_3, match_4, match_5, match_6)
#fullset$Earnings[abs(fullset$Earnings) < abs(fullset$Average)] <- fullset$Earnings[abs(fullset$Earnings) < abs(fullset$Average)]*1000
oneliner <- sqldf('SELECT a.id, b.NAME AS Name, b.CLUB AS Club, b.POS AS Pos, b.VALUE AS Value, a.Earnings, a.Average, b.G, b.A, b.CS
		  FROM totals a JOIN fullset b
		  ON a.id = b.id')
oneliner$init_Value <- round((oneliner$Value - oneliner$Earnings/1000000), 0.5)

## Save for later
save(bl_raw, fullset, oneliner, file = "dataprep.RData")
