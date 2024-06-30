### Imports ###
import numpy as np
from sklearn_extra.cluster import KMedoids
from scipy.optimize import minimize
from fastdtw import fastdtw
import matplotlib.pyplot as plt
from scipy.ndimage import gaussian_filter1d

def smooth(X, bandwidth):
    X_smooth = np.zeros(X.shape)
    for i in range(X.shape[0]):
        X_smooth[i] = gaussian_filter1d(X[i], bandwidth)
    return X_smooth

def get_dissimilarity_matrix(X):
    dissimilarity_matrix = np.zeros((X.shape[0], X.shape[0]))
    for i in range(X.shape[0]):
        for j in range(X.shape[0]):
            dissimilarity_matrix[i, j], _ = fastdtw(X[i], X[j], dist=2)
    return dissimilarity_matrix

def kernel_clustering(X, lmbda, n_clusters):
    X_smooth = smooth(X, lmbda)
    dissimilarity_matrix = get_dissimilarity_matrix(X_smooth)
    kmedoids = KMedoids(n_clusters=n_clusters, metric="precomputed", random_state=42)
    kmedoids.fit(dissimilarity_matrix)
    return kmedoids.inertia_ + abs(lmbda), kmedoids.labels_, kmedoids.cluster_centers_

def optimize_lambda(X, n_clusters, bounds, initial_lambda):
    def objective(lmbda):
        return kernel_clustering(X, lmbda, n_clusters)[0]
    result = minimize(objective, initial_lambda, method='Nelder-Mead', bounds=bounds)
    return result.x[0], result.fun