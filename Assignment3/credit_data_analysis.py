from collections import defaultdict
from sklearn.mixture import GaussianMixture as GMM
from sklearn.cluster import KMeans as kmeans
import pandas as pd
import numpy as np
from time import clock
from sklearn.metrics import homogeneity_score, completeness_score
from sklearn.metrics import adjusted_mutual_info_score as ami
from sklearn.decomposition import PCA
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.metrics import classification_report,confusion_matrix
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import FastICA
from itertools import product
from sklearn.random_projection import SparseRandomProjection
from sklearn.metrics import pairwise_distances
import scipy.sparse as sps
from scipy.linalg import pinv
from sklearn.ensemble import RandomForestClassifier
from sklearn.base import TransformerMixin,BaseEstimator



# Part 1 - Run k-means and EM clustering algorithms
def run_credit_analysis(creditX, creditY):
    np.random.seed(0)
    clusters = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 18, 20, 25, 30]

    print('Part 1 - Running clustering algoirthms on original datasets...credit')
    SSE = defaultdict(dict)
    BIC = defaultdict(dict)
    homo = defaultdict(lambda: defaultdict(dict))
    compl = defaultdict(lambda: defaultdict(dict))
    adjMI = defaultdict(lambda: defaultdict(dict))
    km = kmeans(random_state=5)
    gmm = GMM(random_state=5)

    st = clock()
    for k in clusters:
        km.set_params(n_clusters=k)
        gmm.set_params(n_components=k)
        km.fit(creditX)
        gmm.fit(creditX)
        SSE[k]['Credit SSE'] = km.score(creditX)
        BIC[k]['Credit BIC'] = gmm.bic(creditX)  # BAYESIAN INFORMATION CRITERION
        homo[k]['Credit']['Kmeans'] = homogeneity_score(creditY, km.predict(creditX))  # Agreement of labels
        homo[k]['Credit']['GMM'] = homogeneity_score(creditY, gmm.predict(creditX))
        compl[k]['Credit']['Kmeans'] = completeness_score(creditY, km.predict(
            creditX))  # A clustering result satisfies completeness if all the data points that are members of a given class are elements of the same cluster
        compl[k]['Credit']['GMM'] = completeness_score(creditY, gmm.predict(creditX))
        adjMI[k]['Credit']['Kmeans'] = ami(creditY, km.predict(creditX))  # ADJUSTED MUTUAL INFORMATION
        adjMI[k]['Credit']['GMM'] = ami(creditY, gmm.predict(creditX))

        print(k, clock() - st)

    SSE = (-pd.DataFrame(SSE)).T
    BIC = pd.DataFrame(BIC).T
    homo = pd.Panel(homo)
    compl = pd.Panel(compl)
    adjMI = pd.Panel(adjMI)

    SSE.to_csv('./P1_Clustering_Algorithms_Non_Transformed/Credit_Cluster_Select_Kmeans.csv')
    BIC.to_csv('./P1_Clustering_Algorithms_Non_Transformed/Credit_Cluster_Select_GMM.csv')
    homo.ix[:, :, 'Credit'].to_csv('./P1_Clustering_Algorithms_Non_Transformed/credit_homo.csv')
    compl.ix[:, :, 'Credit'].to_csv('./P1_Clustering_Algorithms_Non_Transformed/credit_compl.csv')
    adjMI.ix[:, :, 'Credit'].to_csv('./P1_Clustering_Algorithms_Non_Transformed/credit_adjMI.csv')



'''PCA of dataset'''
def run_credit_PCA(creditX, creditY):
    # %% Part 2A & 4A - Run Dimensionality Reduction Algorithm PCA, Run NN with reduced dims

    dims_digits = [1, 2, 3,4,5,6,7, 8,9, 10,11, 12,13, 14,15,16,17, 18,19, 20,21, 22, 23]
    print('Part 2A - Starting PCA for digits dataset...credit')
    pca = PCA(random_state=5)
    pca.fit(creditX)
    tmp = pd.Series(data=pca.explained_variance_ratio_, index=range(1, 24))
    tmp.to_csv('./P2_Dimensionality_Reduction/Credit_PCA_explained_variance_ratio.csv')

    # Run Neural Networks
    # Transform X data
    sc = StandardScaler()
    creditX_tr = sc.fit_transform(creditX)

    pca = PCA(random_state=5)
    nn_results = run_NN(dims_digits, pca, creditX_tr, creditY)
    nn_results.to_csv('./P4_Neural_Networks_Reduced/Credit_PCA_nn_results.csv')

'''Neural net with PCA using gridsearch'''
def run_NN(dims, clf, X, Y):
    nn_arch = [(12,), (12, 12), (12, 12, 12), (23), (23, 23), (23, 23, 23), (46, 23, 46)]
    nn_lr = [.001, .006, .01, .06, .1, .6, 1]
    grid ={'clf__n_components':dims, 'NN__learning_rate_init':nn_lr, 'NN__hidden_layer_sizes':nn_arch}
    print(grid)
    mlp = MLPClassifier(activation='logistic', max_iter=2000, early_stopping=True, random_state=5)
    pipe = Pipeline([('clf', clf), ('NN', mlp)])
    gs = GridSearchCV(pipe, grid, verbose=10, cv=5)
    gs.fit(X, Y)
    nn_results = pd.DataFrame(gs.cv_results_)
    return nn_results



'''Neural net of original vars for comparison to above with PCA'''
def run_NN_credit_base(X, Y):
    nn_arch =  [(12,), (12, 12), (12, 12, 12), (23), (23, 23), (23, 23, 23), (46, 23, 46)]
    nn_lr = [.001, .006, .01, .06, .1, .6, 1]
    grid ={'NN__learning_rate_init':nn_lr, 'NN__hidden_layer_sizes':nn_arch}
    mlp = MLPClassifier(activation='logistic', max_iter=2000, early_stopping=True, random_state=5)
    pipe = Pipeline([('Scale',StandardScaler()), ('NN', mlp)])
    gs = GridSearchCV(pipe, grid, verbose=10, cv=5)
    gs.fit(X, Y)
    pd.DataFrame(gs.cv_results_).to_csv('./P4_Neural_Networks_Reduced/Credit_Base_nn_results.csv')


# %% Part 2B & 4B - Run Dimensionality Reduction Algorithm ICA, Run NN with reduced dims
def run_credit_ICA(creditX, creditY):
    dims_digits = [1, 2, 3,4, 5,6,7,8, 9,10, 11,12, 13,14, 15,16,17,18,19,20, 21,22, 23]
    print('Part 2B & 4B - Starting ICA for dataset...credit')
    ica = FastICA(random_state=5)
    kurt = {}
    for dim in dims_digits:
        ica.set_params(n_components=dim)
        tmp = ica.fit_transform(creditX)
        tmp = pd.DataFrame(tmp)
        tmp2 = tmp.kurt(axis=0)
        kurt[dim] = tmp2.abs().mean()

    kurt = pd.Series(kurt)
    kurt.to_csv('./P2_Dimensionality_Reduction/Credit_ICA_kurtosis.csv')

    # Run Neural Networks
    # Transform X data
    sc = StandardScaler()
    creditX_tr = sc.fit_transform(creditX)

    nn_results = run_NN(dims_digits, ica, creditX_tr, creditY)
    nn_results.to_csv('./P4_Neural_Networks_Reduced/Credit_ICA_nn_results.csv')


def run_credit_RCA(creditX, creditY):
    dims_digits = [1,2,3,4,5,6,7,8]  #8, 10 ] #, 12, 14, 18, 20, 22, 23]
    print('Part 2C - Starting RP, pairwise distance correlation, for dataset...credit')
    tmp = defaultdict(dict)
    for i, dim in product(range(2), dims_digits):
        print i,dim
        rp = SparseRandomProjection(random_state=i, n_components=dim)
        tmp[dim][i] = pairwiseDistCorr(rp.fit_transform(creditX), creditX)
    tmp = pd.DataFrame(tmp).T
    tmp.to_csv('./P2_Dimensionality_Reduction/Credit_RP_pairwise_distance_corrB.csv')

    # Run Neural Networks
    rp = SparseRandomProjection(random_state=5)
    nn_results = run_NN(dims_digits, rp, creditX, creditY)
    nn_results.to_csv('./P4_Neural_Networks_Reduced/Credit_RP_nn_resultsB.csv')


def pairwiseDistCorr(X1, X2):
    assert X1.shape[0] == X2.shape[0]
    d1 = pairwise_distances(X1)
    d2 = pairwise_distances(X2)
    return np.corrcoef(d1.ravel(), d2.ravel())[0, 1]


#http://datascience.stackexchange.com/questions/6683/feature-selection-using-feature-importances-in-random-forests-with-scikit-learn
class ImportanceSelect(BaseEstimator, TransformerMixin):
    def __init__(self, model, n=1):
        self.model = model
        self.n = n

    def fit(self, *args, **kwargs):
        self.model.fit(*args, **kwargs)
        return self

    def transform(self, X):
        return X[:, self.model.feature_importances_.argsort()[::-1][:self.n]]


def run_credit_RF(creditX, creditY):
    rfc = RandomForestClassifier(n_estimators=100, class_weight='balanced', random_state=5)
    print('Part 2D - Starting RF for dataset...credit')
    rf_credit = rfc.fit(creditX, creditY).feature_importances_

    tmp = pd.Series(np.sort(rf_credit)[::-1])
    tmp.to_csv('./P2_Dimensionality_Reduction/Credit_RF_feature_importance.csv')

    # Select important features
    rfc_filtr = ImportanceSelect(rfc)
    dims_digits = [1, 2, 3,4, 5,6,7,8,9, 10,11, 12,13, 14,15,16,17, 18,19, 20,21, 22, 23]
    nn_arch = [(12,), (12, 12), (12, 12, 12), (23), (23, 23), (23, 23, 23), (46, 23, 46)]
    nn_lr = [.001, .006, .01, .06, .1, .6, 1]

    # Transform X data
    sc = StandardScaler()
    creditX_tf = sc.fit_transform(creditX)

    # Run Neural Networks
    grid = {'filter__n': dims_digits, 'NN__learning_rate_init': nn_lr, 'NN__hidden_layer_sizes': nn_arch}
    mlp = MLPClassifier(activation='logistic', max_iter=2000, early_stopping=True, random_state=5)
    pipe = Pipeline([('filter', rfc_filtr), ('NN', mlp)])
    gs = GridSearchCV(pipe, grid, verbose=10, cv=5)
    gs.fit(creditX_tf, creditY)
    nn_results = pd.DataFrame(gs.cv_results_)
    nn_results.to_csv('./P4_Neural_Networks_Reduced/Credit_RF_nn_results.csv')


def run_credit_datasets(creditX, creditY, dims):
    rfc = RandomForestClassifier(n_estimators=100, class_weight='balanced', random_state=5, n_jobs=7)

    algo_name = ['PCA', 'ICA', 'RP', 'RF']
    print('Part 2E - Storing dimensionally reduced datasets for credit dataset...')
    algos_digits = [PCA(n_components=dims["credit"]["PCA"], random_state=10),
           FastICA(n_components=dims["credit"]["ICA"], random_state=10),
           SparseRandomProjection(n_components=dims["credit"]["RCA"], random_state=5),
           ImportanceSelect(rfc, dims["credit"]["RF"])]

    for i in range(len(algos_digits)):
        if i == 3:
            digitsX2 = algos_digits[i].fit_transform(creditX, creditY)
        else:
            digitsX2 = algos_digits[i].fit_transform(creditX)
        digits2 = pd.DataFrame(np.hstack((digitsX2,np.atleast_2d(creditY).T)))
        cols = list(range(digits2.shape[1]))
        cols[-1] = 'Class'
        digits2.columns = cols
        digits2.to_hdf('datasets.hdf','credit_'+algo_name[i], complib='blosc', complevel=9)



def run_credit_analysis_dim_red():
    algo_name = ['PCA', 'ICA', 'RP', 'RF']
    clusters = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12] #, 13, 14, 16, 18, 20, 25, 30]
    # %% Part 3 - Run k-means and EM clustering algorithms on each dimensionally reduced dataset

    print('Part 3 - Running clustering algoirthms on dimensionally reduced datasets...credit')
    for i in range(len(algo_name)):
        # load datasets
        credit = pd.read_hdf('datasets.hdf', 'credit_' + algo_name[i])
        creditX = credit.drop('Class', 1).copy().values
        creditY = credit['Class'].copy().values

        SSE = defaultdict(dict)
        BIC = defaultdict(dict)
        homo = defaultdict(lambda: defaultdict(dict))
        compl = defaultdict(lambda: defaultdict(dict))
        adjMI = defaultdict(lambda: defaultdict(dict))
        km = kmeans(random_state=5)
        gmm = GMM(random_state=5)

        st = clock()
        for k in clusters:
            km.set_params(n_clusters=k)
            gmm.set_params(n_components=k)
            km.fit(creditX)
            gmm.fit(creditX)
            SSE[k]['Credit SSE'] = km.score(creditX)
            BIC[k]['Credit BIC'] = gmm.bic(creditX)
            homo[k]['Credit']['Kmeans'] = homogeneity_score(creditY, km.predict(creditX))
            homo[k]['Credit']['GMM'] = homogeneity_score(creditY, gmm.predict(creditX))
            compl[k]['Credit']['Kmeans'] = completeness_score(creditY, km.predict(creditX))
            compl[k]['Credit']['GMM'] = completeness_score(creditY, gmm.predict(creditX))
            adjMI[k]['Credit']['Kmeans'] = ami(creditY, km.predict(creditX))
            adjMI[k]['Credit']['GMM'] = ami(creditY, gmm.predict(creditX))
            print(k, clock() - st)

        SSE = (-pd.DataFrame(SSE)).T
        BIC = pd.DataFrame(BIC).T
        homo = pd.Panel(homo)
        compl = pd.Panel(compl)
        adjMI = pd.Panel(adjMI)

        SSE.to_csv('./P3_Clustering_Algorithms_Reduced/Credit/Credit_SSE_' + algo_name[i] + '.csv')
        BIC.to_csv('./P3_Clustering_Algorithms_Reduced/Credit/Credit_BIC_' + algo_name[i] + '.csv')
        homo.ix[:, :, 'Credit'].to_csv('./P3_Clustering_Algorithms_Reduced/Credit/credit_' + algo_name[i] + '_homo.csv')
        compl.ix[:, :, 'Credit'].to_csv('./P3_Clustering_Algorithms_Reduced/Credit/credit_' + algo_name[i] + '_compl.csv')
        adjMI.ix[:, :, 'Credit'].to_csv('./P3_Clustering_Algorithms_Reduced/Credit/credit_' + algo_name[i] + '_adjMI.csv')