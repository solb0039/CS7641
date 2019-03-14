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
'''K-means and E-M'''
def run_adult_analysis(adultX, adultY):
    np.random.seed(0)
    clusters =  [2,3,4,5,6,7,8,9,10,11,12,13,14,16,18,20,25,30]

    print('Part 1 - Running clustering algoirthms on original datasets...adult')
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
        km.fit(adultX)
        gmm.fit(adultX)
        SSE[k]['Adult SSE'] = km.score(adultX)
        BIC[k]['Adult BIC'] = gmm.bic(adultX)  #BAYESIAN INFORMATION CRITERION
        #true = np.transpose(adultY)
        #pred = np.transpose(km.predict(adultX))
        homo[k]['Adult']['Kmeans'] = homogeneity_score(adultY, km.predict(adultX))  # Agreement of labels
        homo[k]['Adult']['GMM'] = homogeneity_score(adultY, gmm.predict(adultX))
        compl[k]['Adult']['Kmeans'] = completeness_score(adultY, km.predict(adultX)) #A clustering result satisfies completeness if all the data points that are members of a given class are elements of the same cluster
        compl[k]['Adult']['GMM'] = completeness_score(adultY, gmm.predict(adultX))
        adjMI[k]['Adult']['Kmeans'] = ami(adultY, km.predict(adultX))   #ADJUSTED MUTUAL INFORMATION
        adjMI[k]['Adult']['GMM'] = ami(adultY, gmm.predict(adultX))

        print(k, clock() - st)

    SSE = (-pd.DataFrame(SSE)).T
    BIC = pd.DataFrame(BIC).T
    homo = pd.Panel(homo)
    compl = pd.Panel(compl)
    adjMI = pd.Panel(adjMI)

    SSE.to_csv('./P1_Clustering_Algorithms_Non_Transformed/Adult_Cluster_Select_Kmeans.csv')
    BIC.to_csv('./P1_Clustering_Algorithms_Non_Transformed/Adult_Cluster_Select_GMM.csv')
    homo.ix[:, :, 'Adult'].to_csv('./P1_Clustering_Algorithms_Non_Transformed/Adult_homo.csv')
    compl.ix[:, :, 'Adult'].to_csv('./P1_Clustering_Algorithms_Non_Transformed/Adult_compl.csv')
    adjMI.ix[:, :, 'Adult'].to_csv('./P1_Clustering_Algorithms_Non_Transformed/Adult_adjMI.csv')


'''PCA of dataset'''
def run_adult_PCA(adultX, adultY):
    # %% Part 2A & 4A - Run Dimensionality Reduction Algorithm PCA, Run NN with reduced dims

    dims_digits = [1, 2, 4, 8, 10, 12, 14]
    print('Part 2A - Starting PCA for  dataset...adult')
    pca = PCA(random_state=5)
    pca.fit(adultX)
    tmp = pd.Series(data=pca.explained_variance_ratio_, index=range(1, 15))
    tmp.to_csv('./P2_Dimensionality_Reduction/Adult_PCA_explained_variance_ratio.csv')

    # Run Neural Networks
    pca = PCA(random_state=5)
    nn_results = run_NN(dims_digits, pca, adultX, adultY)
    nn_results.to_csv('./P4_Neural_Networks_Reduced/Adult_PCA_nn_results.csv')


'''Neural net with PCA using gridsearch'''
def run_NN(dims, clf, X, Y):
    nn_arch = [(8,), (8, 8), (8, 8, 8), (14), (14, 14), (14, 14, 14), (28, 14, 28)]
    nn_lr = [.001, .006, .01, .06, .1, .6, 1]
    grid ={'clf__n_components':dims, 'NN__learning_rate_init':nn_lr, 'NN__hidden_layer_sizes':nn_arch}
    print(grid)
    mlp = MLPClassifier(activation='logistic', max_iter=2000, early_stopping=True, random_state=5)
    pipe = Pipeline([('clf', clf), ('NN', mlp)])
    gs = GridSearchCV(pipe, grid, verbose=10, cv=5)
    gs.fit(X, Y)
    nn_results = pd.DataFrame(gs.cv_results_)
    return nn_results

    #test_score = gs.score(tstX, tstY)
    #with open('./P4_Neural_Networks_Reduced/test_results.csv', 'a') as f:
    #    f.write('{},{},{},{}\n'.format('ANN', 'Adult', test_score, gs.best_params_))


'''Neural net of original vars for comparison to above with PCA'''
def run_NN_base(X, Y):
    nn_arch = [(8,), (8, 8), (8, 8, 8), (14), (14, 14), (14, 14, 14), (28, 14, 28)]
    nn_lr = [.001, .006, .01, .06, .1, .6, 1]
    grid ={'NN__learning_rate_init':nn_lr, 'NN__hidden_layer_sizes':nn_arch}
    mlp = MLPClassifier(activation='logistic', max_iter=2000, early_stopping=True, random_state=5)
    pipe = Pipeline([('Scale',StandardScaler()), ('NN', mlp)])
    gs = GridSearchCV(pipe, grid, verbose=10, cv=5)
    gs.fit(X, Y)
    pd.DataFrame(gs.cv_results_).to_csv('./P4_Neural_Networks_Reduced/Adult_Base_nn_results.csv')


'''Neural net with ICA using gridsearch'''
# %% Part 2B & 4B - Run Dimensionality Reduction Algorithm ICA, Run NN with reduced dims
def run_adult_ICA(adultX, adultY):
    dims_digits = [1, 2, 4, 8, 10, 12, 14]
    print('Part 2B & 4B - Starting ICA for dataset...adult')
    ica = FastICA(random_state=5)
    kurt = {}
    svm = {}
    for dim in dims_digits:
        ica.set_params(n_components=dim)
        tmp = ica.fit_transform(adultX)
        tmp = pd.DataFrame(tmp)
        tmp2 = tmp.kurt(axis=0)
        kurt[dim] = tmp2.abs().mean()

    kurt = pd.Series(kurt)
    kurt.to_csv('./P2_Dimensionality_Reduction/Adult_ICA_kurtosis.csv')

    nn_results = run_NN(dims_digits, ica, adultX, adultY)
    nn_results.to_csv('./P4_Neural_Networks_Reduced/Adult_ICA_nn_results.csv')



def run_adult_RCA(adultX, adultY):
    dims_digits = [8, 10, 12, 14] #[1, 2, 4]#,
    print('Part 2C - Starting RP, pairwise distance correlation, for dataset...adult')
    tmp = defaultdict(dict)
    for i, dim in product(range(10), dims_digits):
        print i,dim
        rp = SparseRandomProjection(random_state=i, n_components=dim)
        tmp[dim][i] = pairwiseDistCorr(rp.fit_transform(adultX), adultX)
    tmp = pd.DataFrame(tmp).T
    tmp.to_csv('./P2_Dimensionality_Reduction/Adult_RP_pairwise_distance_corrB.csv')

    print('Part 2C - Starting RP, reconstruction error, for  dataset...adult')
    tmp = defaultdict(dict)
    for i, dim in product(range(10), dims_digits):
        rp = SparseRandomProjection(random_state=i, n_components=dim)
        rp.fit(adultX)
        tmp[dim][i] = reconstructionError(rp, adultX)
    tmp = pd.DataFrame(tmp).T
    tmp.to_csv('./P2_Dimensionality_Reduction/Adult_RP_reconstruction_errorB.csv')

    # Run Neural Networks
    rp = SparseRandomProjection(random_state=5)
    nn_results = run_NN(dims_digits, rp, adultX, adultY)
    nn_results.to_csv('./P4_Neural_Networks_Reduced/Adult_RP_nn_resultsB.csv')


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