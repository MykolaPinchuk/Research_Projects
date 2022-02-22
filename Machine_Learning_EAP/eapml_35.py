""" This is the main script for eap ml project, which plays with all ml methods, applicable for cpcrsp analysis
Created on Sat Feb 12 15:19:54 2022
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os
import warnings
import random
import gc
import dill
import time
from stargazer.stargazer import Stargazer

import statsmodels.api as sm
from sklearn.svm import SVR


from sklearn.model_selection import train_test_split, cross_val_score, cross_val_predict, GridSearchCV
from sklearn.linear_model import LinearRegression, Lasso, Ridge, ElasticNetCV
from sklearn import svm
from sklearn.decomposition import PCA, KernelPCA
from sklearn.neighbors import KNeighborsRegressor
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestRegressor
from sklearn.preprocessing import StandardScaler, PolynomialFeatures
from sklearn.impute import SimpleImputer
from sklearn.metrics import r2_score
from sklearn.tree import DecisionTreeRegressor, plot_tree

from xgboost import XGBRegressor

import tensorflow as tf
import keras
from keras.models import Sequential
from keras.layers import Dense, SimpleRNN, LSTM, Activation, Dropout, BatchNormalization
from tensorflow.keras.callbacks import EarlyStopping

plt.style.use('ggplot')
warnings.simplefilter(action='ignore')
pd.set_option('display.max_columns', 40)
gc.enable()

os.chdir('H:\Dropbox\ML\eapml_project')

# can skip data loading and cleaning steps and go straigth to modelling, around line 200.

#%% Load data ###

cpcrsp = pd.read_csv('IMLEAP_v2.csv')
cpcrsp.dropna(axis=0, subset=['bm', 'lbm', 'llme', 'lvol12m', 'lop', 'op', 'linv', 'mom122',
                              'beta_bw', 'l_beta_bw'], inplace=True)
cpcrsp.head()
cpcrsp.isna().sum()
cpcrsp.describe()

cpcrsp['id'] = np.arange(cpcrsp.shape[0])
cpcrsp.rename(columns={'ivol_capm':'ivol', 'l_ivol_capm':'livol'}, inplace=True)

#%% Clean data ###

# using SimpleImputer() will be faster, but maybe less flexible. I will impute evth manually. should replace it later?
cpcrsp.loc[cpcrsp.mom11.isnull(),'mom11'] = cpcrsp.mom11.median()
cpcrsp.loc[cpcrsp.lmom11.isnull(),'lmom11'] = cpcrsp.lmom11.median()
cpcrsp.loc[cpcrsp.mom122.isnull(),'mom122'] = cpcrsp.mom11.median()
cpcrsp.loc[cpcrsp.mom242.isnull(),'mom242'] = cpcrsp.mom242.median()
cpcrsp.loc[cpcrsp.mom482.isnull(),'mom482'] = cpcrsp.mom482.median()

cpcrsp.loc[cpcrsp.lbm.isnull(),'lbm'] = cpcrsp.bm
cpcrsp.loc[cpcrsp.llme.isnull(),'llme'] = cpcrsp.size

cpcrsp.loc[cpcrsp.op.isnull(),'op'] = cpcrsp.op.median()
cpcrsp.loc[cpcrsp.lop.isnull(),'lop'] = cpcrsp.lop.median()

cpcrsp.loc[cpcrsp.gp.isnull(),'gp'] = cpcrsp.gp.median()
cpcrsp.loc[cpcrsp.lgp.isnull(),'lgp'] = cpcrsp.lgp.median()

cpcrsp.loc[cpcrsp.inv.isnull(),'inv'] = cpcrsp.inv.median()
cpcrsp.loc[cpcrsp.linv.isnull(),'linv'] = cpcrsp.linv.median()

cpcrsp.loc[cpcrsp.amhd.isnull(),'amhd'] = cpcrsp.amhd.median()
cpcrsp.loc[cpcrsp.lamhd.isnull(),'lamhd'] = cpcrsp.lamhd.median()

cpcrsp.loc[cpcrsp.ivol.isnull(),'ivol'] = cpcrsp.ivol.median()
cpcrsp.loc[cpcrsp.livol.isnull(),'livol'] = cpcrsp.livol.median()

cpcrsp.loc[cpcrsp.beta_bw.isnull(),'beta_bw'] = cpcrsp.beta_bw.median()
cpcrsp.loc[cpcrsp.l_beta_bw.isnull(),'l_beta_bw'] = cpcrsp.l_beta_bw.median()

cpcrsp.loc[cpcrsp.vol12m.isnull(),'vol12m'] = cpcrsp.vol12m.median()
cpcrsp.loc[cpcrsp.lvol12m.isnull(),'lvol12m'] = cpcrsp.lvol12m.median()

cpcrsp.loc[cpcrsp.BAspr.isnull(),'BAspr'] = cpcrsp.BAspr.median()
cpcrsp.loc[cpcrsp.lBAspr.isnull(),'lBAspr'] = cpcrsp.lBAspr.median()

cpcrsp.loc[cpcrsp.MAX.isnull(),'MAX'] = cpcrsp.MAX.median()
cpcrsp.loc[cpcrsp.vol1m.isnull(),'vol1m'] = cpcrsp.vol1m.median()

print(cpcrsp.shape)

cpcrsp.replace(-np.inf, np.nan, inplace=True)
cpcrsp.dropna(inplace=True)
print(cpcrsp.shape)



#%% Summary stat and visualization ###

# output from the project:
# t1  description of variables.
# t2  summary stat of features.
# f1  histogram of returns
# f2  plot of smaple size over years
# f3  binned scatterplots of returns vs each feature

# t3  results table
# f4  results barplot x3 types of smples

# t4, t5, t6, t7 coefficients from glm.
# f5 abs coefficients from glm, barplot.
# t8 coefficients from svr.
# f6 abs coeffs from svr.
# f7 tree
# ... 


# how to gte feature importances:
# svr:   model_svr.coef_
# rf:    https://scikit-learn.org/stable/auto_examples/ensemble/plot_forest_importances.html
# bt:    xgb.feature_importances_,sorted_idx = xgb.feature_importances_.argsort()
#        plt.barh(boston.feature_names[sorted_idx], xgb.feature_importances_[sorted_idx])
#        plt.xlabel("Xgboost Feature Importance")
# ann:   https://stackoverflow.com/questions/44119207/is-there-any-way-to-get-variable-importance-with-keras
    

data = cpcrsp.copy()
data = data.iloc[:,0:19]
data.drop(columns = ['PERMNO', 'prd'], inplace=True)
model_results.loc[8, 'Model'] = 'BoostedTree'

# table 2
t2 = data.describe().transpose()
print(t2.to_latex(float_format="%.2f"))

# table 1
t1 = t2.copy()
t1 = t1.iloc[:,0:1]
print(t1.to_latex(float_format="%.2f"))

# figure 1
data.RET.hist(bins=20)
plt.savefig('fig1.png', dpi = 200)

# figure 2
cpcrsp['year']=np.floor((cpcrsp.prd+23500-1)/12)
fig2 = pd.DataFrame((np.floor(cpcrsp.groupby('year').size()/12)).astype(int))
fig2.columns = ['Count']
fig2.reset_index(inplace=True)
sns.lineplot(data=fig2, x='year', y='Count')
plt.savefig('fig2.png', dpi = 200)

# figure 3
x_varrs = list(data.columns)
x_varrs.remove('RET')
#plot_kws={"s": 3}
sns.pairplot(data=data, y_vars=['RET'], x_vars=x_varrs[0:4], plot_kws={"s": 1})
plt.savefig('fig3a.png', dpi = 300)
sns.pairplot(data=data, y_vars=['RET'], x_vars=x_varrs[4:8], plot_kws={"s": 1})
plt.savefig('fig3b.png', dpi = 300)
sns.pairplot(data=data, y_vars=['RET'], x_vars=x_varrs[8:12], plot_kws={"s": 1})
plt.savefig('fig3c.png', dpi = 300)
sns.pairplot(data=data, y_vars=['RET'], x_vars=x_varrs[12:16], plot_kws={"s": 1})
plt.savefig('fig3d.png', dpi = 300)


# get results:
    
#ann_r = pd.read_csv('mleap_ann_results.csv')
#dill.load_session('eapml_afterBT_3.pkl')
#model_results.loc[model_results.Model=='ANN',model_results.columns[1:]] = list(ann_r.iloc[:,1])
#model_results.to_csv('model_results_full.csv')
# dt = pd.read_csv('temp_results_correcttree.csv')
# model_results.loc[model_results.Model=='Tree',model_results.columns] = dt.loc[dt.Model=='Tree',dt.columns[1:]]

# table 3
t3 = model_results.copy()
t3.iloc[:,1:] = (100*t3.iloc[:,1:])
t3[t3.columns[1:]] = t3[t3.columns[1:]].apply(pd.to_numeric)
t3 = t3.round(2)
t3.drop(index=5, axis=0, inplace=True)
print(t3.to_latex(index=False))

# figures 4,5,6

#sns.set_context("paper", rc={"font.size":8,"axes.titlesize":5,"axes.labelsize":10})   
#sns.barplot(data=t3, x='Model', y='s_IS')

f4a = sns.barplot(data=t3, x='Model', y='s_IS')
f4a.set(xlabel="Model", ylabel = "$R^2$, %")
plt.savefig('f4a.pdf', dpi=300)
plt.clf()

f4b = sns.barplot(data=t3, x='Model', y='s_OOS')
f4b.set(xlabel="Model", ylabel = "$R^2$, %")
plt.savefig('f4b.pdf', dpi=300)
plt.clf()

f5a = sns.barplot(data=t3, x='Model', y='sp_IS')
f5a.set(xlabel="Model", ylabel = "$R^2$, %")
plt.savefig('f5a.pdf', dpi=300)
plt.clf()

f5b = sns.barplot(data=t3, x='Model', y='sp_OOS')
f5b.set(xlabel="Model", ylabel = "$R^2$, %")
plt.savefig('f5b.pdf', dpi=300)
plt.clf()

f6a = sns.barplot(data=t3, x='Model', y='lp_IS')
f6a.set(xlabel="Model", ylabel = "$R^2$, %")
plt.savefig('f6a.pdf', dpi=300)
plt.clf()

f6b = sns.barplot(data=t3, x='Model', y='lp_OOS')
f6b.set(xlabel="Model", ylabel = "$R^2$, %")
plt.savefig('f6b.pdf', dpi=300)
plt.clf()


### Now feature importance results ###

# figure 7 #

feature_names = (list(cpcrsp_train.columns))
feature_names.remove('RET')

t_ols = pd.DataFrame(list(zip(feature_names, ols_s.coef_*100)))
t_ols.columns = ['fname', 'coef']
t_ols.fname=t_ols.fname.astype('category')


f7 = sns.barplot(t_ols.coef, t_ols.fname)
f7.set(xlabel="Feature Importance, %", ylabel = "Feature")
plt.gca().set_aspect(2)
plt.savefig('f7.pdf', dpi=300)
plt.clf()

dill.load_session('eapml_temp_214_mrn_afterKNN_2.pkl')

# for lasso and ridge run 'eapml_fi_1', it is fast.

## figure 8 ##

t_lasso = pd.DataFrame(list(zip(feature_names, lasso.coef_*100)))
t_lasso.columns = ['fname', 'coef']
f8 = sns.barplot(t_lasso.coef, t_lasso.fname)
f8.set(xlabel="Feature Importance, %", ylabel = "Feature")
plt.gca().set_aspect(2)
plt.savefig('f8.pdf', dpi=300)
plt.clf()


## figure 9 ##

t_ridge = pd.DataFrame(list(zip(feature_names, ridge.coef_*100)))
t_ridge.columns = ['fname', 'coef']
f9 = sns.barplot(t_ridge.coef, t_ridge.fname)
f9.set(xlabel="Feature Importance, %", ylabel = "Feature")
plt.gca().set_aspect(2)
plt.savefig('f9.pdf', dpi=300)
plt.clf()


## figure 10 ##

t_en = pd.DataFrame(list(zip(feature_names, elasticNetCV_.coef_*100)))
t_en.columns = ['fname', 'coef']
f10 = sns.barplot(t_en.coef, t_en.fname)
f10.set(xlabel="Feature Importance, %", ylabel = "Feature")
plt.gca().set_aspect(2)
plt.savefig('f10.pdf', dpi=300)
plt.clf()

## figure 11, tree ##

plot_tree(dtr1_)
plt.savefig('f11tree.pdf', dpi=600)
plt.clf()

## figure 11 ##

# function for feature importance in a tree 
# (https://stackoverflow.com/questions/49170296/scikit-learn-feature-importance-calculation-in-decision-trees)

def dt_feature_importance(model,normalize=True):

    left_c = model.tree_.children_left
    right_c = model.tree_.children_right

    impurity = model.tree_.impurity    
    node_samples = model.tree_.weighted_n_node_samples 

    # Initialize the feature importance, those not used remain zero
    feature_importance = np.zeros((model.tree_.n_features,))

    for idx,node in enumerate(model.tree_.feature):
        if node >= 0:
            # Accumulate the feature importance over all the nodes where it's used
            feature_importance[node]+=impurity[idx]*node_samples[idx]- \
                                   impurity[left_c[idx]]*node_samples[left_c[idx]]-\
                                   impurity[right_c[idx]]*node_samples[right_c[idx]]

    # Number of samples at the root node
    feature_importance/=node_samples[0]

    if normalize:
        normalizer = feature_importance.sum()
        if normalizer > 0:
            feature_importance/=normalizer

    return feature_importance


dtr1_.tree_.compute_feature_importances(normalize=...)

t_dt = pd.DataFrame(list(zip(feature_names, dtr1_.tree_.compute_feature_importances(normalize=...)*100)))
t_dt.columns = ['fname', 'coef']
f11 = sns.barplot(t_dt.coef, t_dt.fname)
f11.set(xlabel="Feature Importance, %", ylabel = "Feature")
plt.gca().set_aspect(2)
plt.savefig('f11.pdf', dpi=300)
plt.clf()

## figure 12 ##

start_time = time.time()
importances = model_rf_.feature_importances_
std = np.std([tree.feature_importances_ for tree in model_rf_.estimators_], axis=0)
elapsed_time = time.time() - start_time

print(f"Elapsed time to compute the importances: {elapsed_time:.3f} seconds")

forest_importances = pd.Series(importances, index=feature_names)

t_rf = pd.DataFrame(list(zip(feature_names, importances*100)))
t_rf.columns = ['fname', 'coef']
f12 = sns.barplot(t_rf.coef, t_rf.fname)
f12.set(xlabel="Feature Importance, %", ylabel = "Feature")
#plt.gca().set_aspect(2)
plt.savefig('f12.pdf', dpi=300)
plt.clf()

## figure 13 ##

# bt:    xgb.feature_importances_,sorted_idx = xgb.feature_importances_.argsort()
#        plt.barh(boston.feature_names[sorted_idx], xgb.feature_importances_[sorted_idx])
#        plt.xlabel("Xgboost Feature Importance")

t_bt = pd.DataFrame(list(zip(feature_names, bt_.feature_importances_*100)))
t_bt.columns = ['fname', 'coef']
f13 = sns.barplot(t_bt.coef, t_bt.fname)
f13.set(xlabel="Feature Importance, %", ylabel = "Feature")
#plt.gca().set_aspect(2)
plt.savefig('f13.pdf', dpi=300)
plt.clf()








#%% Train/test split ###

temp = cpcrsp[cpcrsp.prd>=369]
random.seed(10)
test_ids = random.sample(list(temp.id), 100000)

cpcrsp_test = cpcrsp[cpcrsp.id.isin(test_ids)]
cpcrsp_train = cpcrsp[np.logical_not(cpcrsp.id.isin(test_ids))]

cpcrsp_train.reset_index(inplace=True)
cpcrsp_test.reset_index(inplace=True)

cpcrsp_train.drop(columns=['index', 'PERMNO', 'prd', 'id'], inplace=True)
cpcrsp_test.drop(columns=['index', 'PERMNO', 'prd', 'id'], inplace=True)


#%% Do scaling and Create 2 versions of original sample ###

scl = StandardScaler()

s_train = cpcrsp_train.copy()
s_test = cpcrsp_test.copy()

s_train_y = s_train['RET']
s_train_X = s_train.drop(columns=['RET'])
s_test_y = s_test['RET']
s_test_X = s_test.drop(columns=['RET'])

s_train_X = scl.fit_transform(s_train_X)
s_test_X = scl.transform(s_test_X)
ymean = s_train_y.mean()
ysd = s_train_y.std()
s_train_y = np.array((s_train_y-ymean)/ysd)
s_test_y = np.array((s_test_y-ymean)/ysd)

poly = PolynomialFeatures(2)
l_train_X = poly.fit_transform(s_train_X)
l_test_X = poly.transform(s_test_X)

#%% PCA, create 2 altsamples ###
################################

### First, PCA for small sample ###
# in _v8 there is for loop across numbe iof PCs.

PCAmod = PCA(n_components=10)
sp_train_X = PCAmod.fit_transform(pd.DataFrame(s_train_X.copy()))
sp_test_X = PCAmod.transform(pd.DataFrame(s_test_X.copy()))


# temp: time PCA vs kernel PCA

#temp = pd.DataFrame(s_test_X).iloc[0:5000,:]
#PCAmod = PCA(n_components=10)
#KPCAmod = KernelPCA(n_components=10,kernel='rbf')

#ttemp = PCAmod.fit_transform(temp)
#ttemp = KPCAmod.fit_transform(temp)
# even when there is no RAM crash, execution time is at least 1 om more.


#%% PCA for large sample ###

# again, see _v8 for different PCs number.

l_train_X = scl.fit_transform(l_train_X)
l_test_X = scl.transform(l_test_X)

PCAmod = PCA(n_components=50)
lp_train_X = PCAmod.fit_transform(pd.DataFrame(l_train_X.copy()))
lp_test_X = PCAmod.transform(pd.DataFrame(l_test_X.copy()))

del l_train_X
del l_test_X
gc.collect()

#filepath = 'eapml_cleandata_1.pkl'
#dill.dump_session(filepath) # Save the session



#####################################
#%% Start here for modelling part ###
#####################################

# can run this after the first block.

dill.load_session('eapml_cleandata_1.pkl')

######################
#%% Fitting models ###
######################

model_results = pd.DataFrame(columns=['Model', 's_IS', 's_OOS', 'sp_IS', 'sp_OOS', 'lp_IS', 'lp_OOS'], index=range(10))
model_results.Model = ['OLS', 'Lasso', 'Ridge', 'ElasticNet', 'KNN', 'SVR', 'Tree', 'RF', 'BoosetedTree', 'ANN']

y0 = np.zeros(len(s_test_y))
r2_score(y0, s_test_y)

#%% OLS ###

## Small sample ##

ols_s = LinearRegression().fit(s_train_X, s_train_y)
print(ols_s.score(s_train_X, s_train_y))
print(ols_s.score(s_test_X, s_test_y))
# i have verified, this is equivalent to .predict on any sample.

model_results.loc[model_results.Model=='OLS',['s_IS', 's_OOS']] = [ols_s.score(s_train_X, s_train_y), ols_s.score(s_test_X, s_test_y)]

# looking at coefficients:
X = s_train_X.copy()
X = sm.add_constant(X)
sm_ols_s = sm.OLS(s_train_y,X)
results_ols = sm_ols_s.fit()
colnamess = (list(cpcrsp_test.columns))
colnamess.remove('RET')
colnamess.insert(0,'const')
print (results_ols.summary(xname=colnamess))

## small PCA sample ##

ols_sp = LinearRegression().fit(sp_train_X, s_train_y)
print(ols_sp.score(sp_train_X, s_train_y))
print(ols_sp.score(sp_test_X, s_test_y))

model_results.loc[model_results.Model=='OLS',['sp_IS', 'sp_OOS']] = [ols_sp.score(sp_train_X, s_train_y), ols_sp.score(sp_test_X, s_test_y)]

## large PCA sample ##

ols_lp = LinearRegression().fit(lp_train_X, s_train_y)
print(ols_lp.score(lp_train_X, s_train_y))
print(ols_lp.score(lp_test_X, s_test_y))

model_results.loc[model_results.Model=='OLS',['lp_IS', 'lp_OOS']] = [ols_lp.score(lp_train_X, s_train_y), ols_lp.score(lp_test_X, s_test_y)]


#%% LASSO ###

lasso = Lasso(max_iter=10000)
params = {
    'alpha': [0.0001, 0.003, 0.001, 0.03]
}

grid_ls = GridSearchCV(lasso, params, cv=10)
grid_lsp = GridSearchCV(lasso, params, cv=10)
grid = GridSearchCV(lasso, params, cv=10)

grid_ls.fit(s_train_X, s_train_y)
print(grid_ls.best_score_, grid_ls.best_params_)

yhat = grid_ls.predict(s_test_X)
r2_score(s_test_y, yhat)
# i have verified, this is actually the best model with coefs estimated on train sample.

model_results.loc[model_results.Model=='Lasso',['s_IS', 's_OOS']] = [grid_ls.best_score_, r2_score(s_test_y, yhat)]


## small PCA sample ##

grid_lsp.fit(sp_train_X, s_train_y)
print(grid_lsp.best_score_, grid_lsp.best_params_)
yhat = grid_lsp.predict(sp_test_X)
r2_score(s_test_y, yhat)

model_results.loc[model_results.Model=='Lasso',['sp_IS', 'sp_OOS']] = [grid_lsp.best_score_, r2_score(s_test_y, yhat)]


## large PCA sample ##

grid.fit(lp_train_X, s_train_y)
print(grid.best_score_, grid.best_params_)
yhat = grid.predict(lp_test_X)
r2_score(s_test_y, yhat)

model_results.loc[model_results.Model=='Lasso',['lp_IS', 'lp_OOS']] = [grid.best_score_, r2_score(s_test_y, yhat)]


#%% Ridge ###

ridge = Ridge()
params = {
    'alpha': [400,600,900,1500]
}

grid_r = GridSearchCV(ridge, params, cv=10)
grid_r.fit(s_train_X, s_train_y)
print(grid_r.best_score_, grid_r.best_params_)
yhat = grid_r.predict(s_test_X)
r2_score(s_test_y, yhat)

model_results.loc[model_results.Model=='Ridge',['s_IS', 's_OOS']] = [grid_r.best_score_, r2_score(s_test_y, yhat)]

## small PCA sample ##

grid.fit(sp_train_X, s_train_y)
print(grid.best_score_, grid.best_params_)
yhat = grid.predict(sp_test_X)
r2_score(s_test_y, yhat)

model_results.loc[model_results.Model=='Ridge',['sp_IS', 'sp_OOS']] = [grid.best_score_, r2_score(s_test_y, yhat)]

## large PCA sample ##

grid.fit(lp_train_X, s_train_y)
print(grid.best_score_, grid.best_params_)
yhat = grid.predict(lp_test_X)
r2_score(s_test_y, yhat)

model_results.loc[model_results.Model=='Ridge',['lp_IS', 'lp_OOS']] = [grid.best_score_, r2_score(s_test_y, yhat)]


#%% Elastic Net ###

alphas2 = np.array([0.00001, 0.0001, 0.001, 0.01])
l1_ratios = [0.05,0.1,0.5, 0.9]

elasticNetCV_ = ElasticNetCV(alphas=alphas2, 
                            l1_ratio=l1_ratios,
                            max_iter=1e4).fit(s_train_X, s_train_y)

print(r2_score(s_train_y, elasticNetCV_.predict(s_train_X)),r2_score(s_test_y, elasticNetCV_.predict(s_test_X)))

model_results.loc[model_results.Model=='ElasticNet',['s_IS', 's_OOS']] = \
[r2_score(s_train_y, elasticNetCV_.predict(s_train_X)), r2_score(s_test_y, elasticNetCV_.predict(s_test_X))]

## small PCA sample ##

elasticNetCV = ElasticNetCV(alphas=alphas2, 
                            l1_ratio=l1_ratios,
                            max_iter=1e4).fit(sp_train_X, s_train_y)

print(r2_score(s_train_y, elasticNetCV.predict(sp_train_X)),r2_score(s_test_y, elasticNetCV.predict(sp_test_X)))

model_results.loc[model_results.Model=='ElasticNet',['sp_IS', 'sp_OOS']] = \
[r2_score(s_train_y, elasticNetCV.predict(sp_train_X)), r2_score(s_test_y, elasticNetCV.predict(sp_test_X))]

## large PCA sample ##

elasticNetCV = ElasticNetCV(alphas=alphas2, 
                            l1_ratio=l1_ratios,
                            max_iter=1e4).fit(lp_train_X, s_train_y)

print(r2_score(s_train_y, elasticNetCV.predict(lp_train_X)),r2_score(s_test_y, elasticNetCV.predict(lp_test_X)))

model_results.loc[model_results.Model=='ElasticNet',['lp_IS', 'lp_OOS']] = \
[r2_score(s_train_y, elasticNetCV.predict(lp_train_X)), r2_score(s_test_y, elasticNetCV.predict(lp_test_X))]



# %% temp ###

#save data to export to kaggle

#df_sp_test_X = pd.DataFrame(sp_test_X)
#df_s_test_y = pd.DataFrame(s_test_y)
#df_sp_test_X.to_csv('sp_test_X.csv', index=False)
#df_s_test_y.to_csv('s_test_y.csv', index=False)

tempx = pd.DataFrame(sp_train_X)
tempy = pd.DataFrame(s_train_y)

tempxt = pd.DataFrame(sp_test_X)
tempyt = pd.DataFrame(s_test_y)

rand_obs = random.sample(list(np.arange(tempy.shape[0])), 50000)
rand_obs2 = random.sample(list(np.arange(tempyt.shape[0])), 10000)

tempx = tempx.iloc[rand_obs,:]
tempy = tempy.iloc[rand_obs,:]

tempxt = tempxt.iloc[rand_obs2,:]
tempyt = tempyt.iloc[rand_obs2,:]

#tempx.to_csv('ss_sp_train_X.csv')
#tempy.to_csv('ss_s_train_y.csv')


#pd.DataFrame(s_train_X).to_csv('s_train_X.csv')
#pd.DataFrame(s_train_y).to_csv('s_train_y.csv')
#pd.DataFrame(sp_train_X).to_csv('sp_train_X.csv')
#pd.DataFrame(lp_train_X).to_csv('lp_train_X.csv')



#%% KNN ###

# see previous scripts for knn


#%% SVR ###

# initial grid search suggests c=0.5, epsilon=1.
# all 3 specifications will probably take 5 hours or more to run

svrm_ = SVR(C=0.5, epsilon=0.5)
svrm_.fit(s_train_X, s_train_y)

model_results.loc[model_results.Model=='SVR',['s_IS', 's_OOS']] = \
[r2_score(s_train_y, svrm_.predict(s_train_X)), r2_score(s_test_y, svrm_.predict(s_test_X))]

## small PCA sample ##

svrm = SVR(C=0.5, epsilon=0.5)
svrm.fit(sp_train_X, s_train_y)

model_results.loc[model_results.Model=='SVR',['sp_IS', 'sp_OOS']] = \
[r2_score(s_train_y, svrm.predict(sp_train_X)), r2_score(s_test_y, svrm.predict(sp_test_X))]

## large PCA sample ##

svrm = SVR(C=0.5, epsilon=0.5)
svrm.fit(lp_train_X, s_train_y)

model_results.loc[model_results.Model=='SVR',['lp_IS', 'lp_OOS']] = \
[r2_score(s_train_y, svrm.predict(lp_train_X)), r2_score(s_test_y, svrm.predict(lp_test_X))]


filepath = 'eapml_afterSVR_3.pkl'
dill.dump_session(filepath) # Save the session



#%% Simple Tree ###
# should run for 3-4 minutes

dtr1_ = DecisionTreeRegressor(max_depth=4, random_state=1)
dtr1_.fit(s_train_X, s_train_y)

model_results.loc[model_results.Model=='Tree',['s_IS', 's_OOS']] = \
[r2_score(s_train_y, dtr1_.predict(s_train_X)), r2_score(s_test_y, dtr1_.predict(s_test_X))]

## small PCA sample ##

dtr1 = DecisionTreeRegressor(max_depth=4, random_state=1)
dtr1.fit(sp_train_X, s_train_y)

model_results.loc[model_results.Model=='Tree',['sp_IS', 'sp_OOS']] = \
[r2_score(s_train_y, dtr1.predict(sp_train_X)), r2_score(s_test_y, dtr1.predict(sp_test_X))]

## large PCA sample ##

dtr1 = DecisionTreeRegressor(max_depth=4, random_state=1)
dtr1.fit(lp_train_X, s_train_y)

model_results.loc[model_results.Model=='Tree',['lp_IS', 'lp_OOS']] = \
[r2_score(s_train_y, dtr1.predict(lp_train_X)), r2_score(s_test_y, dtr1.predict(lp_test_X))]

#model_results.to_csv('temp_results_correcttree.csv')

#filepath = 'eapml_afterTree_.pkl'
#dill.dump_session(filepath) # Save the session

#%% Random Forest ###

# 5-15 minutes for one model on s_train.
# initial grid: depth 8-10-12, max_features 0.5
# results for 3-4-5 are much worse.

model_rf_ = RandomForestRegressor(random_state=42, max_depth=15, max_features=0.3, n_estimators=200)
model_rf_.fit(s_train_X, s_train_y)
model_results.loc[model_results.Model=='RF',['s_IS', 's_OOS']] = \
[r2_score(s_train_y, model_rf_.predict(s_train_X)), r2_score(s_test_y, model_rf_.predict(s_test_X))]

## small PCA ##

model_rf = RandomForestRegressor(random_state=42, max_depth=10, max_features=0.3, n_estimators=200)
model_rf.fit(sp_train_X, s_train_y)
model_results.loc[model_results.Model=='RF',['sp_IS', 'sp_OOS']] = \
[r2_score(s_train_y, model_rf.predict(sp_train_X)), r2_score(s_test_y, model_rf.predict(sp_test_X))]

## large PCA ##

model_rf = RandomForestRegressor(random_state=42, max_depth=20, max_features=0.3, n_estimators=200)
model_rf.fit(lp_train_X, s_train_y)
model_results.loc[model_results.Model=='RF',['lp_IS', 'lp_OOS']] = \
[r2_score(s_train_y, model_rf.predict(lp_train_X)), r2_score(s_test_y, model_rf.predict(lp_test_X))]





#model_results.to_csv('temp_results_RF_3.csv')

# 10-8-12 depth, 0.5


#(pd.DataFrame(s_test_X)).to_csv('s_test_X.csv')
#(pd.DataFrame(lp_test_X)).to_csv('lp_test_X.csv')

#%% Boosted Tree ###

# depth=2, eta=0.1, subsample=0.50.7, colsample_bytree=0.5

bt_ = XGBRegressor(n_estimators=3000, max_depth=2, eta=0.05, subsample=0.5, colsample_bytree=0.4, nthread=4)
bt_.fit(s_train_X, s_train_y)
model_results.loc[model_results.Model=='BoosetedTree',['s_IS', 's_OOS']] = \
[r2_score(s_train_y, bt_.predict(s_train_X)), r2_score(s_test_y, bt_.predict(s_test_X))]

print([r2_score(s_train_y, bt_.predict(s_train_X)), r2_score(s_test_y, bt_.predict(s_test_X))])


## small PCA ##

bt = XGBRegressor(n_estimators=2000, max_depth=2, eta=0.05, subsample=0.5, colsample_bytree=0.5, nthread=4)
bt.fit(sp_train_X, s_train_y)
model_results.loc[model_results.Model=='BoosetedTree',['sp_IS', 'sp_OOS']] = \
[r2_score(s_train_y, bt.predict(sp_train_X)), r2_score(s_test_y, bt.predict(sp_test_X))]

## large PCA ##

bt = XGBRegressor(n_estimators=2000, max_depth=2, eta=0.05, subsample=0.5, colsample_bytree=0.5, nthread=4)
bt.fit(lp_train_X, s_train_y)
model_results.loc[model_results.Model=='BoosetedTree',['lp_IS', 'lp_OOS']] = \
[r2_score(s_train_y, bt.predict(lp_train_X)), r2_score(s_test_y, bt.predict(lp_test_X))]


#model_results.to_csv('temp_results_BT_1.csv')

#filepath = 'eapml_afterBT_5.pkl'
#dill.dump_session(filepath) # Save the session

#%% Load workspace ###

#dill.load_session('eapml_afterBT_3.pkl')




#%% ANN ###

# due to need for gpu to speed up runtime, i will train ann exclusively on kaggle.


#%% Generate accuracy tables for all models ###



#%% Produce feature importance plots for all models ###


#%%  ###


#%%  ###


#%%  ###


#%%  ###




