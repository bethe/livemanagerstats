#!/bin/bash

cd ~/R/livemanager/data

# curl player data from plm api pages by player ID in [..]. Store as json files by previous player ID (#1)
curl "http://www.playlivemanager.com/api/players/[64400-65700]/stats/round" -o "#1.txt" #Note: Add waiting period to not overload server
curl "http://www.playlivemanager.com/api/players/[66600-66800]/stats/round" -o "#1.txt" #Note: Latest players, i.e. Vidal, Koman, etc.


# merge files, each file a new line
for f in *.txt; do (cat "${f}"; echo) >> playerdata.json; done


# remove first 30 characters of each line in file '{"status": "ok", "response": [' 
# sed -i 's/^.\{30\}//' [filename].json # for each file
sed -i 's/^.\{30\}//g' playerdata.json

# replace last 2 characters in each line ']}' with ','
sed -i 's/.\{2\}$/,/' playerdata.json

# add additional '[' at beginning...
sed -i '1s/^/[/' playerdata.json
# and replace last ',' with ']'
sed -i '$s/,$/]/' playerdata.json

# convert json to csv online at http://www.convertcsv.com/json-to-csv.htm
