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

    dims_digits = [1, 2, 4, 8, 10, 12, 14, 18, 20, 22, 23]
    print('Part 2A - Starting PCA for digits dataset...credit')
    pca = PCA(random_state=5)
    pca.fit(creditX)
    tmp = pd.Series(data=pca.explained_variance_ratio_, index=range(1, 24))
    tmp.to_csv('./P2_Dimensionality_Reduction/Credit_PCA_explained_variance_ratio.csv')

    # Run Neural Networks
    pca = PCA(random_state=5)
    nn_results = run_NN(dims_digits, pca, creditX, creditY)
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
    dims_digits = [1, 2, 4, 8, 10, 12, 14, 18, 20, 22, 23]
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

    nn_results = run_NN(dims_digits, ica, creditX, creditY)
    nn_results.to_csv('./P4_Neural_Networks_Reduced/Credit_ICA_nn_results.csv')


def run_credit_RCA(creditX, creditY):
    dims_digits = [1, 2] #, 8, 10 ]#, 12, 14, 18, 20, 22, 23]
    print('Part 2C - Starting RP, pairwise distance correlation, for dataset...credit')
    tmp = defaultdict(dict)
    for i, dim in product(range(10), dims_digits):
        print i,dim
        rp = SparseRandomProjection(random_state=i, n_components=dim)
        tmp[dim][i] = pairwiseDistCorr(rp.fit_transform(creditX), creditX)
    tmp = pd.DataFrame(tmp).T
    tmp.to_csv('./P2_Dimensionality_Reduction/Credit_RP_pairwise_distance_corr.csv')

    print('Part 2C - Starting RP, reconstruction error, for  dataset...credit')
    tmp = defaultdict(dict)
    for i, dim in product(range(10), dims_digits):
        rp = SparseRandomProjection(random_state=i, n_components=dim)
        rp.fit(creditX)
        tmp[dim][i] = reconstructionError(rp, creditX)
    tmp = pd.DataFrame(tmp).T
    tmp.to_csv('./P2_Dimensionality_Reduction/Credit_RP_reconstruction_error.csv')

    # Run Neural Networks
    rp = SparseRandomProjection(random_state=5)
    nn_results = run_NN(dims_digits, rp, creditX, creditY)
    nn_results.to_csv('./P4_Neural_Networks_Reduced/Credit_RP_nn_results.csv')


def pairwiseDistCorr(X1, X2):
    assert X1.shape[0] == X2.shape[0]
    d1 = pairwise_distances(X1)
    d2 = pairwise_distances(X2)
    return np.corrcoef(d1.ravel(), d2.ravel())[0, 1]


def reconstructionError(projections,X):
    W = projections.components_
    if sps.issparse(W):
        W = W.todense()
    p = pinv(W)
    reconstructed = (np.matmul( np.matmul(p,W) , (X.T))).T # Unproject projected data
    errors = np.square(X-reconstructed)
    return np.nanmean(errors)
