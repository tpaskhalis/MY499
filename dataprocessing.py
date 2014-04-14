import numpy as np
import pandas as pd

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


bnp = pd.read_csv('./data/bnpids_02042014.csv', header=0, names=['id'], encoding = 'utf-8')
bnp['bnp'] = True
ukip = pd.read_csv('./data/ukipids_02042014.csv', header=0, names=['id'], encoding = 'utf-8')
ukip['ukip'] = True
libdem = pd.read_csv('./data/libdemids_02042014.csv', header=0, names=['id'], encoding = 'utf-8')
libdem['libdem'] = True
cons = pd.read_csv('./data/consids_02042014.csv', header=0, names=['id'], encoding = 'utf-8')
cons['cons'] = True
lab = pd.read_csv('./data/labids_02042014.csv', header=0, names=['id'], encoding = 'utf-8')
lab['lab'] = True

#common = pd.Series(np.intersect1d(temp1['id'], libdem['id']))
#len(temp1) + len(libdem) - len(common)

temp1 = pd.merge(bnp, ukip, on='id', how='outer')
temp2 = pd.merge(temp1, libdem, on='id', how='outer')
temp3 = pd.merge(temp2, cons, on='id', how='outer')
parties = pd.merge(temp3, lab, on='id', how='outer')
parties = parties.drop_duplicates(cols=['id'])

parties.replace(np.nan, False)
parties['id'] = parties['id'].astype(np.int64)
parties.to_csv('./data/followers_ids_02042014.csv')

###################################################################################################

bnp = pd.read_csv('./data/bnpids_10042014.csv', header=0, names=['id'], encoding = 'utf-8')
bnp['bnp'] = True
ukip = pd.read_csv('./data/ukipids_10042014.csv', header=0, names=['id'], encoding = 'utf-8')
ukip['ukip'] = True
libdem = pd.read_csv('./data/libdemids_10042014.csv', header=0, names=['id'], encoding = 'utf-8')
libdem['libdem'] = True
cons = pd.read_csv('./data/consids_10042014.csv', header=0, names=['id'], encoding = 'utf-8')
cons['cons'] = True
lab = pd.read_csv('./data/labids_10042014.csv', header=0, names=['id'], encoding = 'utf-8')
lab['lab'] = True

temp1 = pd.merge(bnp, ukip, on='id', how='outer')
temp2 = pd.merge(temp1, libdem, on='id', how='outer')
temp3 = pd.merge(temp2, cons, on='id', how='outer')
parties = pd.merge(temp3, lab, on='id', how='outer')
parties = parties.drop_duplicates(cols=['id'])

parties.replace(np.nan, False)
parties['id'] = parties['id'].astype(np.int64)
parties.to_csv('./data/followers_ids_10042014.csv')