# livemanagerstats
playlivemanager.com - optimizing Bundesliga teams for playlivemanager.com

## Approaches
1. Calculate best 11 by optimizing for [earnings] from past [x] matches (x=6 seems optimum at the moment, besides earnings also looking at 'nominations' holding one player constant)
2. Predict individual player scores from past x matches and optimize best 11 based on that (currently w/ linear regression

## ToDos
- best11 function: find a way to include captain multiplier
- scrape data directly from within R
- clubspoints table: add home/away pointdiffs
- regression: try out loess or others...
- add web interface
- ...
