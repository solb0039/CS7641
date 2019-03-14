import pandas as pd
import numpy as np
from adult_data_analysis import run_adult_analysis, run_adult_PCA, run_NN_base, run_adult_ICA, run_adult_RCA
from credit_data_analysis import run_credit_analysis, run_credit_PCA, run_NN_credit_base, run_credit_ICA, run_credit_RCA

# Read data sets
adult = pd.read_csv('./adult.data.txt', header=None)
adult.columns = ['age', 'employer', 'fnlwt', 'edu', 'edu_num', 'marital', 'occupation', 'relationship', 'race', 'sex',
                 'cap_gain', 'cap_loss', 'hrs', 'country', 'income']
adult['country'] = adult['country'].str.strip()
replacements = {'Cambodia': ' SE-Asia',
                'Canada': ' British-Commonwealth',
                'China': ' China',
                'Columbia': ' South-America',
                'Cuba': ' Other',
                'Dominican-Republic': ' Latin-America',
                'Ecuador': ' South-America',
                'El-Salvador': ' South-America ',
                'England': ' British-Commonwealth',
                'France': ' Euro_1',
                'Germany': ' Euro_1',
                'Greece': ' Euro_2',
                'Guatemala': ' Latin-America',
                'Haiti': ' Latin-America',
                'Holand-Netherlands': ' Euro_1',
                'Honduras': ' Latin-America',
                'Hong': ' China',
                'Hungary': ' Euro_2',
                'India': ' British-Commonwealth',
                'Iran': ' Other',
                'Ireland': ' British-Commonwealth',
                'Italy': ' Euro_1',
                'Jamaica': ' Latin-America',
                'Japan': ' Other',
                'Laos': ' SE-Asia',
                'Mexico': ' Latin-America',
                'Nicaragua': ' Latin-America',
                'Outlying-US(Guam-USVI-etc)': ' Latin-America',
                'Peru': ' South-America',
                'Philippines': ' SE-Asia',
                'Poland': ' Euro_2',
                'Portugal': ' Euro_2',
                'Puerto-Rico': ' Latin-America',
                'Scotland': ' British-Commonwealth',
                'South': ' Euro_2',
                'Taiwan': ' China',
                'Thailand': ' SE-Asia',
                'Trinadad&Tobago': ' Latin-America',
                'United-States': ' United-States',
                'Vietnam': ' SE-Asia',
                'Yugoslavia': ' Euro_2'}

adult = adult.replace(to_replace={'country': replacements,
                                  'employer': {' Without-pay': ' Never-worked'},
                                  'relationship': {' Husband': 'Spouse', ' Wife': 'Spouse'}})
for col in ['employer', 'marital', 'occupation', 'relationship', 'race', 'sex', 'country']:
    adult[col] = adult[col].str.strip()

# Create ordinal vars
adult['relationship'] = adult['relationship'].astype('category').cat.codes
adult['employer'] = adult['employer'].astype('category').cat.codes
adult['edu'] = adult['edu'].astype('category').cat.codes
adult['marital'] = adult['marital'].astype('category').cat.codes
adult['race'] = adult['race'].astype('category').cat.codes
adult['sex'] = adult['sex'].astype('category').cat.codes
adult['country'] = adult['country'].astype('category').cat.codes
adult['income'] = adult['income'].astype('category').cat.codes
adult['occupation'] = adult['occupation'].astype('category').cat.codes


adult = adult.rename(columns=lambda x: x.replace('-', '_'))

adultX = adult.drop('income', 1).copy().values
adultY = adult['income'].copy().values #.reshape(-1,1)

# Run k-means and E-M
#run_adult_analysis(adultX, adultY)   # k-means and E-M

#PCA
#run_adult_PCA(adultX, adultY)  # PCA + NN
#run_NN_base(adultX, adultY)  # Run NN without PCA for comparison

#ICA
#run_adult_ICA(adultX, adultY)

#Random Projections
#run_adult_RCA(adultX, adultY)

#Other





##############################
# REPEAT WITH CREDIT DATASET #
##############################

# Import Credit dataset
credit = pd.read_csv('./default of credit card clients2.csv', header=None)
credit.columns = ['LIMIT_BAL','SEX','EDUCATION','MARRIAGE','AGE','PAY_0','PAY_2','PAY_3','PAY_4','PAY_5','PAY_6',
                  'BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6','PAY_AMT1','PAY_AMT2','PAY_AMT3',
                  'PAY_AMT4','PAY_AMT5','PAY_AMT6','default']

credit['SEX'] = credit['SEX'].astype('category').cat.codes
credit['EDUCATION'] = credit['EDUCATION'].astype('category').cat.codes
credit['MARRIAGE'] = credit['MARRIAGE'].astype('category').cat.codes
credit['PAY_0'] = credit['PAY_0'].astype('category').cat.codes
credit['PAY_2'] = credit['PAY_2'].astype('category').cat.codes
credit['PAY_3'] = credit['PAY_3'].astype('category').cat.codes
credit['PAY_4'] = credit['PAY_4'].astype('category').cat.codes
credit['PAY_5'] = credit['PAY_5'].astype('category').cat.codes
credit['PAY_6'] = credit['PAY_6'].astype('category').cat.codes
credit['default'] = credit['default'].astype('category').cat.codes

creditX = credit.drop('default', 1).copy().values
creditY = credit['default'].copy().values #.reshape(-1,1)


#print(credit.head())
#print(credit.dtypes)
#run_credit_analysis(creditX, creditY)

#run_credit_PCA(creditX, creditY)  # PCA + NN
#run_NN_credit_base(creditX, creditY)  # Run NN without PCA for comparison

#ICA
#run_credit_ICA(creditX, creditY)

#Random Projections
run_credit_RCA(creditX, creditY)
