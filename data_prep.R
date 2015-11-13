####
# Import and format data from playlivemanager.com
####

###
# TO DO: 
###


## Load packages
library("sqldf")

## Load source file for functions
# source("functions.R") ## ?still needed?

## 1 Import Data from spreadsheet
raw = read.csv("data/playerdata.csv")

# 2 Data formatting
#  simplify colnames, and use '_' as sqldf doesn't like '.' in variable names
colnames(raw) <- gsub("\\player_status.", "", colnames(raw))
colnames(raw) <- gsub("\\matches_info.0.", "", colnames(raw))
colnames(raw) <- gsub("\\.", "_", colnames(raw))


# subset to BL only players
#sqldf('SELECT home_squad, home_shortname FROM raw GROUP BY home_squad ORDER BY home_squad') #match squad ids to teams
bl_raw <- sqldf('SELECT * FROM raw WHERE home_squad >= 1562 AND home_squad <=1579')


# Put data into format of web UI to match player names & positions
## (Name) - (Sta) - Club - (POS) -(Value) - Earnings - Average - Round - G - A - CS
# Match players to club
player_club_temp <- sqldf('SELECT id, home_squad, home_shortname, COUNT(*) AS count FROM bl_raw GROUP BY id, home_shortname ORDER BY id')
player_club <- sqldf('SELECT id, home_squad AS squad, home_shortname AS Club, MAX(count) AS matches FROM player_club_temp GROUP BY id')
## Get latest round and matches played
sqldf('SELECT MAX(matchday) FROM bl_raw') # -> 12
round <- sqldf('SELECT id, total_earnings AS Round 
	       FROM bl_raw WHERE matchday = 12')
totals <- sqldf('SELECT id, SUM(goal) AS G, SUM(assist) AS A, SUM(clean_sheet) AS CS, SUM(total_earnings) AS Earnings, COUNT(*) AS matches, SUM(total_earnings)/COUNT(*) AS Average
		FROM bl_raw
		WHERE status IN ("starter", "sub_in", "sub_out", "red_card")
		GROUP BY id')
webstyle <- sqldf('SELECT totals.id AS id, Club, Earnings, Average, Round, G, A, CS FROM round JOIN totals JOIN player_club ON round.id = totals.id AND totals.id = player_club.id')

# Format Numbers into 1.0 for millions and 450 for thousands
# Note: round function is such a cunt; using little trick from http://stackoverflow.com/questions/12688717/round-up-from-5-in-r
webstyle$Earnings[webstyle$Earnings < 1000000] <- floor((webstyle$Earnings[webstyle$Earnings < 1000000])/1000 + 0.5) #thousands
webstyle$Earnings[webstyle$Earnings >= 1000000] <- round((webstyle$Earnings[webstyle$Earnings >= 1000000])/1000000,1) #Millions
webstyle$Average[webstyle$Average < 1000000] <- floor((webstyle$Average[webstyle$Average < 1000000])/1000 + 0.5) #thousands
webstyle$Round[webstyle$Round < 1000000] <- floor((webstyle$Round[webstyle$Round < 1000000])/1000 + 0.5) #thousands

# Import Web Data (via copy on Google Spreadsheets)
library("googlesheets")
trix <- gs_title("playlivemanager players")
bl12 <- gs_read(trix, ws=9)
# Drop Euro sign, millions M, K for thousands and convert 'NA's to 0 so that it's possible to join columns later
bl12$VALUE <- gsub( "€ ", "", bl12$VALUE)
bl12$VALUE <- gsub( "M", "", bl12$VALUE)
bl12$EARNINGS <- gsub( "M", "", bl12$EARNINGS)
bl12$EARNINGS <- gsub( "K", "", bl12$EARNINGS)
bl12$AVERAGE <- gsub( "K", "", bl12$AVERAGE)
bl12$ROUND <- gsub( "K", "", bl12$ROUND)
bl12$G[bl12$G == "-"] <- 0
bl12$A[bl12$A == "-"] <- 0
bl12$CS[bl12$CS == "-"] <- 0
# Align column formats between bl12 and webstyle
bl12$VALUE <- as.numeric(bl12$VALUE)
bl12$EARNINGS <- as.numeric(bl12$EARNINGS)
bl12$AVERAGE <- as.numeric(bl12$AVERAGE)
bl12$ROUND <- as.numeric(bl12$ROUND)
bl12$CS <- as.integer(bl12$CS)
webstyle$G[is.na(webstyle$G)] <- 0
webstyle$A[is.na(webstyle$A)] <- 0
webstyle$CS[is.na(webstyle$CS)] <- 0
webstyle$G <- as.integer(webstyle$G)
webstyle$A <- as.integer(webstyle$A)

# Match & merge datasets
## Exclude players with 0 earnings
players <- sqldf('SELECT * FROM webstyle WHERE Earnings <> 0')
bl12_players <- sqldf('SELECT * FROM bl12 WHERE EARNINGS <> 0')
player_ids <- sqldf('SELECT id FROM players')
## Match players with completely matching data
full_match <- sqldf('SELECT id, NAME, a.Club, POS, VALUE, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
	       FROM players a LEFT OUTER JOIN bl12_players b
	       ON a.Earnings = b.EARNINGS
	       AND a.Average = b.Average
	       AND a.Round = b.Round
	       AND a.G = b.G
	       AND a.A = b.A
	       AND a.CS = b.CS')
match_1 <- subset(full_match, !(is.na(full_match$VALUE)))
remaining_players <- subset(full_match[,c(1,3,6:11)], is.na(full_match$VALUE))
remaining_bl12 <- sqldf('SELECT a.Name, a.Club, a.Pos, a.Value, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS FROM bl12_players a LEFT OUTER JOIN match_1 b
			 ON a.NAME = b.NAME
			 AND a.Club = b.Club
			 AND a.Earnings = b.Earnings
			 WHERE b.NAME IS NULL
			')

## Match players with (unique) matching Club/Round/G/A/S stats
remaining_grouped <- sqldf('SELECT Club, Round, G, A, CS, count(id) AS count FROM unmatched GROUP BY Club, Round, G, A, CS')
unique_combos <- sqldf('SELECT a.id, a.Club, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
			 FROM remaining_players A JOIN remaining_grouped B
			 ON A.Club = B.Club
			 AND A.Round = B.Round
			 AND A.G = B.G
			 AND A.A = B.A
			 AND A.CS = B.CS
			 WHERE B.count = 1')
match_2 <- sqldf('SELECT a.id, NAME, a.Club, POS, VALUE, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
	       FROM unique_combos a LEFT OUTER JOIN remaining_bl12 b
	       ON a.Club = b.Club
	       AND a.Round = b.Round
	       AND a.G = b.G
	       AND a.A = b.A
	       AND a.CS = b.CS')
## check for and remove duplicates
a <- sqldf('SELECT id FROM (
      SELECT id, COUNT(*) count FROM match_2 GROUP BY 1)
      WHERE count <> 1')
avector <- a[,1]
match_2 <- subset(match_2, !(match_2$id %in% avector))
#match_2 <- subset(match_2, !(match_2$id %in% c(64501, 64619)))
## Get remaining players
players_matched_2 <- subset(match_2, !(is.na(match_2$VALUE)))
remaining_players_2 <- subset(match_2[,c(1,3,6:11)], is.na(match_2$VALUE))
remaining_bl12_2 <- sqldf('SELECT a.Name, a.Club, a.Pos, a.Value, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS FROM remaining_bl12 a LEFT OUTER JOIN players_matched_2 b
			 ON a.NAME = b.NAME
			 AND a.Club = b.Club
			 WHERE b.NAME IS NULL
			')

			
## Match remaining players on GACS only
remaining_players_2$GACS <- remaining_players_2$G + remaining_players_2$A + remaining_players_2$CS
remaining_players_GACS <- subset(remaining_players_2, !(remaining_players_2$GACS == 0))
match_3 <- sqldf('SELECT a.id, NAME, a.Club, POS, VALUE, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
	       FROM remaining_players_GACS a LEFT OUTER JOIN remaining_bl12_2 b
	       ON a.Club = b.Club
	       AND a.G = b.G
	       AND a.A = b.A
	       AND a.CS = b.CS')
## Get matched and remaining players
players_matched_3 <- subset(match_3, !(is.na(match_3$VALUE)))
remaining_players_3 <- subset(match_3[,c(1,3,6:11)], is.na(match_3$VALUE))
remaining_bl12_3 <- sqldf('SELECT a.Name, a.Club, a.Pos, a.Value, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS FROM remaining_bl12_2 a LEFT OUTER JOIN players_matched_3 b
			 ON a.NAME = b.NAME
			 AND a.Club = b.Club
			 AND a.Earnings = b.Earnings
			 WHERE b.NAME IS NULL
			')
## manually fix Kim & Schwegler duplicate (both 003 / TSG)
players_matched_3 <- players_matched_3[c(1:13,16),]


## Match remaining players based on club & round
## Note: GACS seems off, i.e. Draxler. Perhaps due to club change.
remaining_players_round <- subset(remaining_players_3, !(remaining_players_3$Round == 0))
match_4 <- sqldf('SELECT a.id, NAME, a.Club, POS, VALUE, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS
	       FROM remaining_players_round a LEFT OUTER JOIN remaining_bl12_3 b
	       ON a.Club = b.Club
	       AND a.Round = b.Round
	       ')
# Last remaining player from WOB only played first 2 matches -> De Bruyne
fullset <- rbind(match_1, match_2, match_3, match_4)
missing_bl12 <- sqldf('SELECT a.Name, a.Club, a.Pos, a.Value, a.Earnings, a.Average, a.Round, a.G, a.A, a.CS FROM remaining_bl12_3 a LEFT OUTER JOIN fullset b
			 ON a.NAME = b.NAME
			 AND a.Club = b.Club
			 WHERE b.NAME IS NULL
			')


## Save for later
save(player_club, webstyle, bl12, fullset, missing_bl12, file = "dataprep.RData")
save.image()

