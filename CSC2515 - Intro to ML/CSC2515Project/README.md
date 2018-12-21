# CSC2515Project - Predicting Fama-French Factors Using Machine Learning Techniques

In this repository you will find the code behind our paper "Predicting Fama-French Factors Using Machine Learning Techniques".

A quick breakdown of how this repository is organized.

Report (Folder: 'Report')
- This folder contains a PDF copy of our report plus the LaTeX files used to produce it.

Modelling ('AdaBoosting.py', 'Gradient Boosting.py', 'GRU.ipynb', 'Logistic Regression.ipynb', 'SVM_RF_MLP.ipynb').
- These files contain the underlying code used to come up with the predictions made for the Fama-French Factors.

Data ('fama_french_data.csv')
- This CSV contains all of the data (features and targets) used in training/validation/testing. Note that descriptions of what each variable mean can be found in the paper.

Results (Folder: 'Results')
- This folder contains 'Predicts_all.xlsx', an Excel file containing all of the models' predictions.
- This folder also contains 'monthly_returns.csv' which breaks down the performance of all passive/active strategies based on these models. A description of these trading strategies can be found in the paper.
- The folder 'Feature Importance' contain csv files of the feature importances for the random forest and logistic regression models.

Other ('CumulativeReturns.ipynb', 'FeatureImportances.ipynb')
- These are the scripts which calculated cumulative returns and random forest feature importances.
