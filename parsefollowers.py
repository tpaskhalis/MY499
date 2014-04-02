import tweepy
import csv
import time
from datetime import datetime
from createapi import create_api

def parse_followers_ids(appapi, accountname, outputcsv):
    with open(outputcsv, "w") as f:
        csvwriter = csv.writer(f)
        for page in tweepy.Cursor(appapi.followers_ids, id=accountname, count=5000).pages():
            for follower in page:
                csvwriter.writerow([str(follower)])
            time.sleep(60)

def parse_followers(api, accountname, partyname, outputcsv):
    with open(outputcsv, "w") as f:
        csvwriter = csv.writer(f)
        csvwriter.writerow(['id', 'name', partyname, 'lang', 'timezone', 'location', 'geo', 'tweets', 'following', 'followers', 'protected', 'created'])
        for follower in tweepy.Cursor(api.followers, id=accountname, count=100).items():
            csvwriter.writerow([follower.id] + [follower.name] + ['True'] + [follower.lang] + 
                               [follower.time_zone] + [follower.location] + [follower.geo_enabled] + 
                               [follower.statuses_count] + [follower.friends_count] + [follower.followers_count]
                               + [follower.protected] + [datetime.strftime(follower.created_at, "%d/%m/%Y")])
                               
            time.sleep(3)

api = create_api()
parse_followers_ids(api, 'bnp', './data/bnpids_02042014.csv')
parse_followers_ids(api, 'ukip', './data/ukipids_02042014.csv')
parse_followers_ids(api, 'LibDems', './data/libdemids_02042014.csv')
parse_followers_ids(api, 'Conservatives', './data/consids_02042014.csv')
parse_followers_ids(api, 'UKLabour', './data/labids_02042014.csv')