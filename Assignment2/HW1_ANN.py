import numpy as np
from sklearn.neural_network import MLPClassifier
import sklearn.model_selection as ms
import pandas as pd
from helpers import basicResults,makeTimingCurve,iterationLC
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import SelectFromModel

adult = pd.read_hdf('datasets.hdf','adult')
adultX = adult.drop('income',1).copy().values
adultY = adult['income'].copy().values

adult_trgX, adult_tstX, adult_trgY, adult_tstY = ms.train_test_split(adultX, adultY, test_size=0.3, random_state=0,stratify=adultY)

pipeA = Pipeline([('Scale',StandardScaler()),
                 ('MLP',MLPClassifier(max_iter=2000,early_stopping=True,random_state=55))])

d = adultX.shape[1]
#hiddens_adult = [(h,)*l for l in [1,2,3] for h in [d/4,d/2,d,int(round(d*1.2,0))]]
hiddens_adult = [56, 56, 56]
#alphas = [10**-x for x in np.arange(-1, 5.01, 0.5)]
alphas = [0.01]

params_adult = {'MLP__activation':['logistic'],'MLP__alpha':alphas,'MLP__hidden_layer_sizes':hiddens_adult}

adult_clf = basicResults(pipeA,adult_trgX,adult_trgY,adult_tstX,adult_tstY,params_adult,'ANN','adult')

#
adult_final_params =adult_clf.best_params_
adult_OF_params =adult_final_params.copy()
adult_OF_params['MLP__alpha'] = 0


#raise

# Make timing curve of final model
#pipeA.set_params(**adult_final_params)
#pipeA.set_params(**{'MLP__early_stopping':False})
#makeTimingCurve(adultX,adultY,pipeA,'ANN','adult')

# Find opt number of iterations; in range 1 to 3000
pipeA.set_params(**adult_final_params)
pipeA.set_params(**{'MLP__early_stopping':False})
iterationLC(pipeA,adult_trgX,adult_trgY,adult_tstX,adult_tstY,{'MLP__max_iter':[2**x for x in range(12)]+[2100,2200,2300,2400,2500,2600,2700,2800,2900]},'ANN','adult')
#iterationLC(pipeA,adult_trgX,adult_trgY,adult_tstX,adult_tstY,{'MLP__max_iter':[3000]},'ANN','adult')


#pipeA.set_params(**adult_OF_params)
#pipeA.set_params(**{'MLP__early_stopping':False})
#iterationLC(pipeA,adult_trgX,adult_trgY,adult_tstX,adult_tstY,{'MLP__max_iter':[2**x for x in range(12)]+[2100,2200,2300,2400,2500,2600,2700,2800,2900,3000]},'ANN_OF','adult')