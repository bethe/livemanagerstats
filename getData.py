
# coding: utf-8

# In[421]:

import json
import requests
import pprint
import os
import re
import time
import datetime


# ## Part 1: Downloading data from API and joining in one big JSON file

# In[429]:

start_time = time.time()
print "Starting at " + str(datetime.datetime.fromtimestamp(start_time))
BASE_URL = "http://www.playlivemanager.com/api/players/"
ROUND_URL_EXT = "/stats/round"
PLAYER_IDs = range(64400,66800)


# In[36]:

def query_by_id(playerID):
    print "Processing Player with ID " + playerID
    url = BASE_URL + playerID + ROUND_URL_EXT
    r = requests.get(url)
    print "requesting", r.url
    print r

    if r.status_code == requests.codes.ok:
        return r.json()
        print r.json()
    else:
        r.raise_for_status()


# In[337]:

def download_data():
    for playerID in PLAYER_IDs:
        results = query_by_id(str(playerID))
#        pprint.pprint(results)
        with open("data/{0}.json".format(playerID), mode='w') as f:
            json.dump(results, f)
#            f.write(str(results))
            print "Stored data for player " + str(playerID) + " in " + f.name


# In[392]:

def transform_data():
    print "Deleting old files"
    try:
        os.remove("data/all.json")
    except:
        pass
    print "Begin transforming data"
    for playerID in PLAYER_IDs:
        with open("data/all.json", mode='a') as all:
            f = open("data/{0}.json".format(playerID), 'r')
            print "Processing " + f.name
            file = f.read()
            file = file.strip()
            ## remove root
            file = re.sub('{\"status\": \"ok\", \"response\": \[', "", file)
            ## remove superfluous ']}' at the end and replace with ','
            file = re.sub("}}]}]}", "}}]},", file)
            #aggregate in one big json file
            all.write(file)
            print "Added " + f.name + " to " + all.name

    # wrap content in '[]', replacing last ','
    with open("data/allfinal.json", mode='w') as all:
        print "Wrapping file in json format"
        reader = open("data/all.json", 'r').read()
        reader = '[' + reader
        reader = re.sub(r"\,$", r"]", reader)
        reader = reader.strip()
        all.write("%s\n" % reader)

    print "All data stored in " + all.name


# In[374]:

download_data()


# In[395]:

transform_data()


# ## Part 2: Converting the JSON file to CSV

# In[415]:

## adapted rom https://gist.github.com/tjvc/12e393c48e0025509021

from collections import OrderedDict
import csv
import json
import sys

infile = "data/allfinal.json"
outfile = open("data/playerdata.csv", "w")

writer = csv.writer(outfile, delimiter=",")

data = json.load(open(infile), object_pairs_hook=OrderedDict)

# Recursively flatten JSON
def flatten(structure, key="", path="", flattened=None):
    if flattened is None:
        flattened = OrderedDict()
    if type(structure) not in(OrderedDict, list):
        flattened[((path + "_") if path else "") + key] = structure
    elif isinstance(structure, list):
        for i, item in enumerate(structure):
            flatten(item, "", path + "_" + key, flattened)
    else:
        for new_key, value in structure.items():
            flatten(value, new_key, path + "_" + key, flattened)
    return flattened

# Write fields
fields = []
for result in data:
    flattened = flatten(result)
    for k, v in flattened.iteritems():
        if k not in fields:
            fields.append(k)
writer.writerow(fields)
print "Flattened JSON to the following fields:"
print fields

# Write values
for result in data:
    flattened = flatten(result)
    row = []
    for field in fields:
        if field in flattened.iterkeys():
            row.append(flattened[field])
        else:
            row.append("")
    writer.writerow(row)
print "Stored CSV as " + outfile.name


# In[450]:

print "Started at " + str(datetime.datetime.fromtimestamp(start_time))
print "Finished at " + str(datetime.datetime.fromtimestamp(time.time()))

