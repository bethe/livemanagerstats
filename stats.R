####
# Calculating some stats playlivemanager.com
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
st1 <- st1_full[,c("NAME", "POS", "CLUB.x", "VALUE.x", "EARNINGS.y")]
st1$POS <- factor(st1$POS)

