import numpy as np
import pandas as pd
import csv

#from pandas import DataFrame
#data1 = DataFrame({'id':[1,2,3,4,5],
#                   'bnp':[True,True,True,True,True]})
#                   
#data2 = DataFrame({'id':[2,3,6,7,8],
#                   'ukip':[True,True,True,True,True]})
#                   
#pd.merge(data1, data2, on='id', how='outer')


#data = DataFrame({'f1': [2,1,2,1],
#                  'f2': [4,3,3,5],
#                  'f3': [6,5,7,6],
#                  'f4': [None,7,None,None]},
#                   index = ['a','b','c','d'])
#
#col = set()                 
#for i in data:
#    col =  col.union(set(data[i].dropna()))
#
#data2 = DataFrame(index = data.index, columns = col)
#for i in data2.index:
#    for c in data2.columns:
#        if c in set(data.ix[i].dropna()):
#            data2.ix[i][c] = True
#        else:
#            data2.ix[i][c] = False

bnp = pd.read_csv('./data/02042014/bnpids_02042014.csv', header=0, names=['id'], encoding = 'utf-8')
bnp['bnp'] = True
ukip = pd.read_csv('./data/02042014/ukipids_02042014.csv', header=0, names=['id'], encoding = 'utf-8')
ukip['ukip'] = True
libdem = pd.read_csv('./data/02042014/libdemids_02042014.csv', header=0, names=['id'], encoding = 'utf-8')
libdem['libdem'] = True
cons = pd.read_csv('./data/02042014/consids_02042014.csv', header=0, names=['id'], encoding = 'utf-8')
cons['cons'] = True
lab = pd.read_csv('./data/02042014/labids_02042014.csv', header=0, names=['id'], encoding = 'utf-8')
lab['lab'] = True

#common = pd.Series(np.intersect1d(temp1['id'], libdem['id']))
#sumlen = len(bnp) + +len(ukip) + len(libdem) + len(cons) + len(lab)
#ratio1 = len(parties)/sumlen
#ratio2 = len(parties)/sumlen

temp1 = pd.merge(bnp, ukip, on='id', how='outer')
temp2 = pd.merge(temp1, libdem, on='id', how='outer')
temp3 = pd.merge(temp2, cons, on='id', how='outer')
parties = pd.merge(temp3, lab, on='id', how='outer')
parties = parties.drop_duplicates(cols=['id'])

parties = parties.replace(np.nan, False)
parties['id'] = parties['id'].astype(np.int64)
parties.to_csv('./data/followers_ids_02042014.csv', index = False)

###############################################################################

followers = pd.read_csv('./data/02042014/followers_metadata_02042014.csv', 
                        dtype={'bnp': bool, 'ukip': bool, 'libdem': bool, 'cons': bool, 'lab': bool})
followers = followers.drop(u'Unnamed: 0', axis = 1)
followers = followers.replace(np.nan, False)
subset = followers[~(pd.isnull(followers['location'])) & (followers['timezone'] == "London") & (followers['protected'] == False) & ((followers['lang'] == "en") | (followers['lang'] == "en-gb") | (followers['lang'] == "en-GB"))]
sample = subset.take(np.random.permutation(len(subset))[:1000])
sample.to_csv('./data/sample.csv', index = False)

###############################################################################

with open('./data/sample_friends.csv', "r") as f:
    csvreader = csv.reader(f)
    unifriends = set()
    header = next(csvreader)
    for line in csvreader:
        friends = [int(i) for i in line[16:]]
        unifriends = unifriends.union(friends)
    frnames = sorted(unifriends)
with open('./data/sample_friends.csv', "r") as f:
    with open('/home/tom/data/sample_full.csv', 'w') as f2:
        csvreader = csv.reader(f)
        csvwriter = csv.writer(f2)
        header = next(csvreader)
        header.extend(frnames)
        csvwriter.writerow(header)
        frvalues = [int(i) for i in frnames]
        for line in csvreader:
            metadata = line[:15]
            friends2 = [int(i) for i in line[16:]]
            frdata = [0 for i in range(len(frvalues))]
            for i in range(len(frvalues)):
                if int(frvalues[i]) in friends2:
                    frdata[i] = 1
            csvwriter.writerow(metadata + frdata)