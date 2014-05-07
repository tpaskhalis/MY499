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

def parse_followers_metadata(appapi, inputcsv, outputcsv):
    with open(inputcsv, "r") as f:
        with open(outputcsv, 'w') as f2:
            csvreader = csv.reader(f)
            csvwriter = csv.writer(f2)
            lines = list(csvreader)
            header = lines[0]
            header.extend(['name', 'protected', 'created', 'lang', 'timezone', 'location', 'geo', 'tweets', 'following', 'followers'])
            csvwriter.writerow(header)
            
            for i in xrange(1,len(lines),100):
                chunk = lines[i:i+100]
                ids = [int(i[1]) for i in chunk]
                users = appapi.lookup_users(user_ids=ids)
                uids = [user.id for user in users]
                diff = [i for i in ids if i not in uids]
                idiff = []
                for i in diff:
                    idiff.extend([ids.index(i)])
                for i in idiff:
                    users.insert(i, None)
                metadata = []
                for user in users:
                    if user:
                        metadata.append([user.name] + [user.protected] + [datetime.strftime(user.created_at, "%d/%m/%Y")] +
                                        [user.lang] + [user.time_zone] + [user.location] + [user.geo_enabled] +
                                        [user.statuses_count] + [user.friends_count] + [user.followers_count])
                    else:
                        metadata.append([None] * 10)
                extchunk = map(list.__add__, chunk, metadata)
                for line in extchunk:
                    csvwriter.writerow(line)
                time.sleep(60)

def parse_followers_friends(appapi, inputcsv, outputcsv):
    with open(inputcsv, "r") as f:
        with open(outputcsv, 'w') as f2:
            csvreader = csv.reader(f)
            csvwriter = csv.writer(f2)
            header = next(csvreader)
            csvwriter.writerow(header)
            for line in csvreader:
                friends = []
                try:
                    for page in tweepy.Cursor(appapi.friends_ids, id=line[0], count=5000).pages():
                        for friend in page:
                            friends.extend([str(friend)])
                        time.sleep(60)
                    line.extend(friends)
                    csvwriter.writerow(line)
                except tweepy.error.TweepError:
                    time.sleep(60)

api = create_api()
#parse_followers_ids(api, 'bnp', './data/17042014/bnpids_17042014.csv')
#parse_followers_ids(api, 'ukip', './data/17042014/ukipids_17042014.csv')
#parse_followers_ids(api, 'LibDems', './data/17042014/libdemids_17042014.csv')
#parse_followers_ids(api, 'Conservatives', './data/17042014/consids_17042014.csv')
#parse_followers_ids(api, 'UKLabour', './data/17042014/labids_17042014.csv')

#parse_followers_metadata(api, './data/followers_ids_02042014.csv', './data/followers_metadata_02042014.csv')

#parse_followers_friends(api, './data/sample.csv', './data/sample_friends.csv')