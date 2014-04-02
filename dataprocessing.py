import numpy as np
import pandas as pd

#from pandas import Series, DataFrame
#data1 = DataFrame({'id':[1,2,3,4,5],
#                   'bnp':[True,True,True,True,True]})
#                   
#data2 = DataFrame({'id':[2,3,6,7,8],
#                   'ukip':[True,True,True,True,True]})
#                   
#pd.merge(data1, data2, on='id', how='outer')

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

parties.replace(np.nan, False)
parties.to_csv('./data/followers_ids.csv')