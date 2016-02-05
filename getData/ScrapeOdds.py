
# coding: utf-8

# In[3]:

import os
import re
import requests
from bs4 import BeautifulSoup
from selenium import webdriver
import time
import datetime


# In[4]:

### Functions
### unchanged from http://stackoverflow.com/questions/30745587/why-does-selenium-return-the-source-of-the-previously-loaded-page-in-python 
def read_page(driver, url):
    driver.get(url)
    text = driver.page_source
    return text

def get_league_matches(driver, country, league, league_url):
    print "Country:", country
    print "League:", league
    print "League URL:", league_url
    print

    league_matches = []

    # Read url
    try:
        headers = {'User-agent': 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1)                     Gecko/2008071615 Fedora/3.0.1-1.fc9 Firefox/3.0.1'}
        r = requests.get(league_url, headers=headers)
        text = r.text
    except:
        pass

    if text:
        soup = BeautifulSoup(text)
        seasons_details = []
        for i in soup.findAll("div", {"class": "main-menu2 main-menu-gray"})[0].findAll("li"):
            season_url = "".join([site_url, i.span.a["href"]])
            season = int(i.text.split("/")[-2])
            seasons_details.append([season, season_url])            
        seasons_details = sorted(seasons_details)   

        seasons_details = seasons_details[-1:]
        for each_season in seasons_details:
            season, seasons_url = each_season
            print "Season:", season
            print "Season URL:", seasons_url
            season_matches = get_season_matches(driver, season, country, league, seasons_url)
            if season_matches:
                league_matches.extend(season_matches)
                print
        return league_matches
    else:
        return None


def get_season_matches(driver, season, country, league, seasons_url):
    season_matches = []
    try:
        text = read_page(driver, seasons_url)
    except Exception, e:
            print "Season matches error:", e
            print ">>", seasons_url
    print "Read season url:", len(text)
    if text:
            try:
                page_numbers = list(set(re.findall('href="#/page/([0-9]{1})/"', text)))
            except Exception, e:
                page_numbers = ""
            if page_numbers:
                max_page_num = max(map(int, page_numbers))
                pages_urls = map(lambda x: "".join([seasons_url, "#/page/", str(x), "/"]), range(2, max_page_num+2)) # max_page_num+2))

                for pages_url in pages_urls:
                    page_matches = get_page_matches(driver, season, country, league, pages_url)
                    season_matches.extend(page_matches)
                return season_matches
            else:
                return None
    else:
        return None


def get_page_matches(driver, season, country, league, pages_url):
    page_matches = []
    try:
        text = read_page(driver, pages_url)
    except Exception, e:
        print "Get page matches error:", e
    if text:
        try:
            odds_type = re.findall('id="user-header-oddsformat-expander"><span>(.*?)</span>', text)
            if odds_type:
                odds_type = odds_type[0]
            else:
                odds_type = ""
        except Exception, e:
            odds_type = ""

        soup = BeautifulSoup(text)
        for i in soup.findAll("tr", {"class":re.compile("deactivate")}):
            try:
                timestamp = int(re.findall("[0-9]{10}", str(i.get_text))[0])
                datetime_date = datetime.datetime.utcfromtimestamp(timestamp)
                match_date = datetime_date.strftime('%d-%m-%Y')
                match_time = datetime_date.strftime('%H:%M')
            except Exception, e:
                timestamp, datetime_date, match_date, match_time = "", "", "", ""     

            try:
                ftsc = i.findAll("td", {"class": re.compile("table-score")})[0].text
                fthg, ftag = map(int, ftsc.split(":"))
                ftr_dict = {1: "H", 0: "D", -1: "A"}
                ftr = ftr_dict[cmp(fthg, ftag)]
            except Exception, e:
                ftsc, fthg, ftag, ftr = "", "", "", ""

            try:
                avoh, avod, avoa = re.findall('odds_text">(.*?)</a>', str(i.get_text))
            except Exception, e:
                avoh, avod, avoa = "", "", ""

            try:
                match_url = i.findAll("td", {"class": "name table-participant"})[0]
                hometeam, awayteam = map(lambda x: x.strip(), match_url.text.split("-"))
                match_url = "".join([site_url, match_url.a["href"]])
            except Exception, e:
                match_url, hometeam, awayteam = "", "", ""

            print ">>", match_date, match_time, hometeam, awayteam, ftsc, fthg, ftag, avoh, avod, avoa

            match_details = {"SEASON": season, "COUNTRY": country, "LEAGUE": league, "DATE": match_date, 
                             "TIME": match_time, "HOMETEAM": hometeam, "AWAYTEAM": awayteam, 
                             "FTSC": ftsc, "FTHG": fthg, "FTAG": ftag, "FTR": ftr, "AVOH": avoh, "AVOD": avod, 
                             "AVOA": avoa, "MATCH_URL": match_url, "TIMESTAMP": timestamp, "ODDS_TYPE": odds_type,
                             "PAGE_URL": pages_url}
            page_matches.append(match_details)
        return page_matches
    else:
        return None, None


# In[5]:

# Call
site_url = "http://www.oddsportal.com"
soccer_url = "http://www.oddsportal.com/results/#soccer" 

driver = webdriver.PhantomJS()
country = "Germany"
league = "Bundesliga"
league_url = "http://www.oddsportal.com/soccer/germany/bundesliga/results/"
league_matches = get_league_matches(driver, country, league, league_url)
print len(league_matches)


# In[6]:

# Export as CSV

import csv

path = '../data/match_odds.csv'
keys = league_matches[0].keys()
with open(path, 'wb') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    dict_writer.writerows(league_matches)
print "Stored in", path

