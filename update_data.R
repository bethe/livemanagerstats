#### Data Update
#### (1) Update data with latest matchday
#### (2) Integrate new players


## load libraries
library("sqldf")
library("plyr")



### 1 Add latest matchday data


# Import Data, keep colnames
setwd("/home/pi/Projects/livemanager")
load("players_match.RData")
raw = read.csv("data/playerdata.csv")
colnames(raw) <- raw_colnames
colnames(raw) <- gsub("\\X__", "", colnames(raw))
colnames(raw) <- gsub("\\player_status_", "", colnames(raw))
colnames(raw) <- gsub("\\matches_info__", "", colnames(raw))
colnames(raw) <- gsub("\\.", "_", colnames(raw))


# subset to BL only players
bl_raw <- sqldf('SELECT * FROM raw WHERE home_squad >= 1562 AND home_squad <=1579')


# join as plm dataset
plm <- merge(x = bl_raw, y = players, by = "id", all.x = TRUE)


# save
save(plm, file = "plm.RData")
