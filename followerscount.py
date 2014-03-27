import tweepy
import csv
import os
from datetime import datetime
from createapi import create_api

def followers_count(api, outputcsv):
    if os.path.isfile(outputcsv) and os.path.getsize(outputcsv) > 0:
        with open(outputcsv, "a") as f:
            csvwriter = csv.writer(f)
            csvwriter.writerow([datetime.strftime(datetime.utcnow(), "%d/%m/%Y")] + [api.get_user(id='bnp').followers_count] + 
                                [api.get_user(id='ukip').followers_count] + [api.get_user(id='Conservatives').followers_count] + 
                                [api.get_user(id='UKLabour').followers_count] + [api.get_user(id='LibDems').followers_count])
    else:
        with open(outputcsv, "w") as f:
            csvwriter = csv.writer(f)
            csvwriter.writerow(['Date','BNP', 'UKIP', 'Conservative', 'Labour', 'LibDem'])
            csvwriter.writerow([datetime.strftime(datetime.utcnow(), "%d/%m/%Y")] + [api.get_user(id='bnp').followers_count] + 
                                [api.get_user(id='ukip').followers_count] + [api.get_user(id='Conservatives').followers_count] + 
                                [api.get_user(id='UKLabour').followers_count] + [api.get_user(id='LibDems').followers_count])
                                
followers_count(create_api(), './data/followers_count.csv')