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

def custom_metric(x, y):
    return np.linalg.norm(x - y)

def custom_metric_dtw(x, y):
    distance, _ = fastdtw(x, y, dist=2)
    return distance

def get_dissimilarity_matrix(X):
    dissimilarity_matrix = np.zeros((X.shape[0], X.shape[0]))
    for i in range(X.shape[0]):
        for j in range(X.shape[0]):
            #dissimilarity_matrix[i, j], _ = fastdtw(X[i], X[j], dist=2)
            dissimilarity_matrix[i, j] = np.linalg.norm(X[i] - X[j])
    return dissimilarity_matrix

def kernel_clustering(X, lmbda, n_clusters):
    X_smooth = smooth(X, lmbda)
    #dissimilarity_matrix = get_dissimilarity_matrix(X_smooth)
    kmedoids = KMedoids(n_clusters=n_clusters, metric="euclidean", random_state=42)
    kmedoids.fit(X_smooth)
    return (1/X.shape[0])*kmedoids.inertia_ + lmbda, kmedoids.labels_, kmedoids

def optimize_lambda(X, n_clusters, bounds, initial_lambda):
    def objective(lmbda):
        return kernel_clustering(X, lmbda, n_clusters)[0]
    result = minimize(objective, initial_lambda, method='Nelder-Mead', bounds=bounds)
    return result.x[0], result.fun

def predict_cluster(new_data_point, kmedoids):
    medoids = kmedoids.cluster_centers_
    distances = np.array([custom_metric(new_data_point, medoid) for medoid in medoids])
    return np.argmin(distances)