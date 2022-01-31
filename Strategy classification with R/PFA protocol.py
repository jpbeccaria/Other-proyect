# -*- coding: utf-8 -*-
"""
Created on Wed Jan 19 09:48:27 2022

@author: Juan
"""

import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from matplotlib import*
import matplotlib.pyplot as plt
from matplotlib.cm import register_cmap
from scipy import stats
from sklearn.decomposition import PCA as sklearnPCA


def PFA(data):
    '''
    Data (variables x cases) have to be presented as Pandas DataFrame
    From a Pandas' DataFrame, it returns eigenvectors matrix
    '''
    # Data standarization
    data_std = StandardScaler().fit_transform(data)
    
    # Creating co-variance matrix 
    mean_vec = np.mean(data_std, axis=0)
    cov_mat = (data_std - mean_vec).T.dot((data_std - mean_vec)) / (data_std.shape[0]-1)
    
    # 'eigendecomposition' 
    cov_mat = np.cov(data_std.T)
    eig_vals, eig_vecs = np.linalg.eig(cov_mat)
    eig_vecs_cuad = eig_vecs **2
    
    return pd.DataFrame(eig_vecs_cuad)

def seleccion_variables (data, matriz):
    '''
    From Pandas DataFrame of original data + eigenvectors matrix it returns
    a list of the 3 variables that contribute the most to PCA
    '''
    variables_pos = matriz.idxmax()[:3]
    lista_variables_pos = []
    for e in variables_pos:
        lista_variables_pos.append(e)
    columnas = data.columns
    lista_variables_nombres = columnas[lista_variables_pos]
    return lista_variables_nombres

def truncar_data (data, variables):
    'From the original Dataframe it truncates the columns that are not in the list of 3 selected variables'
    data_truncada = data[variables]
    return data_truncada


###  Example

data_for_PFA = pd.read_excel('dia9_controls.xlsx')

matrix = PFA(data_for_PFA)
variables = seleccion_variables(data_for_PFA, matrix)
data_truncada = truncar_data(data_for_PFA, variables)

data_truncada.to_excel('data_truncada.xlsx', index = False)

