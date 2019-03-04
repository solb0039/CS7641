import pandas as pd
import sklearn.model_selection as ms
import numpy as np

from sklearn import preprocessing
from helpers import basicResults,makeTimingCurve,iterationLC
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

adult = pd.read_hdf('datasets.hdf','adult')
# Normalize data
adultX = adult.drop('income',1).copy().values
adultX = preprocessing.normalize(adultX)
adultY = adult['income'].copy().values


seg_trgX, seg_tstX, seg_trgY, seg_tstY = ms.train_test_split(adultX, adultY, test_size=0.3, random_state=0,stratify=adultY)
seg_trgY = np.atleast_2d(seg_trgY).T
seg_tstY = np.atleast_2d(seg_tstY).T

seg_trgX, seg_valX, seg_trgY, seg_valY = ms.train_test_split(seg_trgX, seg_trgY, test_size=0.2, random_state=1,stratify=seg_trgY)



tst = pd.DataFrame(np.hstack((seg_tstX,seg_tstY)))
trg = pd.DataFrame(np.hstack((seg_trgX,seg_trgY)))
val = pd.DataFrame(np.hstack((seg_valX,seg_valY)))
tst.to_csv('s_test.csv',index=False,header=False)
trg.to_csv('s_trg.csv',index=False,header=False)
val.to_csv('s_val.csv',index=False,header=False)