
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt 
import pandas as pd
import numpy as np
import seaborn as sns
from sklearn.model_selection import train_test_split
get_ipython().run_line_magic('matplotlib', 'inline')
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.preprocessing import MinMaxScaler
from sklearn.ensemble import AdaBoostClassifier
from sklearn.feature_selection import RFE
import matplotlib.pyplot as plt



# # Read Data

data = pd.read_csv('../Data/Cleaned Data/MLDATA_Cleaned_SW_v2.csv', index_col=0, dtype={0:'str'})
data.index = pd.to_datetime(data.index, format='%Y%m')
features = data.iloc[:,8:]
SMB_bin = data['SMB_bin']
HML_bin = data['HML_bin']

feature_names = features.columns
index_names = features.index

index_names[-10*12:]
scaler = MinMaxScaler()
X_scale = scaler.fit_transform(features)
random_state = 302

# # Prediction

def select_features(features, X_train_all, y_train_all):
    learningrates = [0.05, 0.1, 0.5, 1]
    valid_score = pd.DataFrame(index = ['accuracy'], columns = features)
    learning_rates = pd.DataFrame(index = ['accuracy'], columns = learningrates)
    valid_score[:] = 0
    learning_rates[:] = 0
    
    for j in reversed(range(1,51,1)):
        print(j)
        X_train = X_train_all[:-j,:]
        X_valid = X_train_all[-j,:]

        y_train = y_train_all[:-j]
        y_valid = y_train_all[-j]
        
        for f in features:
            for lr in learningrates:
                estimator = AdaBoostClassifier(learning_rate = lr, random_state=random_state)
                selector = RFE(estimator, f, step=1)
                selector = selector.fit(X_train, y_train)
                valid_score.loc['accuracy',f] += 1/(90*len(learningrates)) * selector.score(X_valid.reshape(1, -1), y_valid.flatten())
                learning_rates.loc['accuracy',lr] += 1/(90*len(features)) * selector.score(X_valid.reshape(1,-1), y_valid.flatten())
                print("number {}, feature number {}, learning rate{}, accuracy {}".format(j, f, lr, selector.score(X_valid.reshape(1, -1), y_valid.flatten())))
    print(float(learning_rates.idxmax(axis=1)))
    print(int(valid_score.idxmax(axis=1)))
    estimator_select = AdaBoostClassifier(learning_rate = lr, random_state=random_state)
    selector_select = RFE(estimator_select, int(valid_score.idxmax(axis=1)), step=1)
    selector_select_fit = selector_select.fit(X_train_all, y_train_all)
    #valid_error = 1 - valid_score.max(axis=1)
    #print('validation error is:', valid_error)
    return selector_select_fit, float(learning_rates.idxmax(axis=1))

# In[74]:


def annual_prediction_model(features, sliding_window, years, X_all, y_all):
    feature_pick = pd.DataFrame(index=range(1, years+1), columns=feature_names)
    test_acc = pd.DataFrame(index = ['No_Features', 'Train_accu','Learning_accu', 'Learning_Rate'], columns = range(1, years+1))
    test_acc[:] = 0
    pred_period = index_names[-years*12:]
    pred_array = np.empty(0)
    for year in reversed(range(1,years+1,1)):
        print('year', year)
        train_year_end = - 12 * year
        train_year_beg = - sliding_window + train_year_end
        X_year = X_all[train_year_beg:train_year_end,:]
        y_year = y_all[train_year_beg:train_year_end]

        X_year_train = X_year[:-12,:] 
        X_year_test = X_year[-12:,:]
        
        y_year_train = y_year[:-12]
        y_year_test = y_year[-12:]
        
        model_year, lr  = select_features(features, X_year_train, y_year_train)
        accuracy_year = model_year.score(X_year_test, y_year_test)
        predicts = model_year.predict(X_year_test)
        test_acc.loc['Test_accu', year] = accuracy_year
        test_acc.loc['Train_accu', year] = model_year.score(X_year_train, y_year_train)
        test_acc.loc['No_Features', year] = model_year.n_features_
        test_acc.loc['Learning_Rate', year] = lr
        feature_pick.loc[year,] = model_year.ranking_
        pred_array = np.append(pred_array, model_year.predict(X_year_test))
        plt.figure()
        plt.plot(y_year_test, label="True")
        plt.plot(predicts, label='Predicted')
        
    
    pred_array = pred_array.reshape(-1,1)
    predictions = pd.DataFrame(pred_array, index = pred_period, columns = ['Predictions'])
                               
    return feature_pick, test_acc, predictions

# Prediction
features = [5, 10, 15, 20, 25]
feature_pick_SMB, test_acc_SMB, pred_SMB = annual_prediction_model(features, 400, 30, X_scale, SMB_bin)
feature_pick_HML, test_acc_HML, pred_HML = annual_prediction_model(features, 400, 30, X_scale, HML_bin)
pred_SMB.to_csv('AdaBoosting_SMB_30yr.csv')
pred_HML.to_csv('AdaBoosting_HML_30yr.csv')
test_acc_SMB.to_csv('AdaBoosting_SMB_acc_30yr.csv')
test_acc_HML.to_csv('AdaBoosting_HML_acc_30yr.csv')

y_SMB_HML = np.where((data['SMB'] - data['HML']) >= 0, 1, 0)
feature_pick_SMBHML, test_acc_SMBHML, pred_SMBHML = annual_prediction_model(features, 400, 30, X_scale, y_SMB_HML)
pred_SMBHML.to_csv('AdaBoosting_SMBHML_30yr.csv')
test_acc_SMBHML.to_csv('AdaBoosting_SMBHML_acc_30yr.csv')



fig, ax = plt.subplots(figsize=(10,6)) 
sns.heatmap(feature_pick_SMB, ax=ax, vmin = 1, vmax=3)
ax.set_title('SMB Features Selected')


fig, ax = plt.subplots(figsize=(10,6)) 
sns.heatmap(feature_pick_HML, ax=ax, vmin = 1, vmax=3)
ax.set_title('HML Features Selected')



SMB_invest = pred_SMB.merge(pd.DataFrame(data.loc[:,'SMB']), right_index=True, left_index=True, how='left')
SMB_invest['Active'] = np.where(SMB_invest['Predictions'] == 1, 1 + SMB_invest['SMB'], 1 - 1 * SMB_invest['SMB'])
SMB_invest['Passive'] = 1 + SMB_invest['SMB']

SMB_return = SMB_invest.loc[:,['Active', 'Passive']]
SMB_invest_annual = (SMB_return.reset_index(drop=True)
                               .groupby(by = np.arange(len(SMB_return.index))//12, axis=0).prod())

SMB_invest_annual


# In[38]:


HML_invest = pred_HML.merge(pd.DataFrame(data.loc[:,'HML']), right_index=True, left_index=True, how='left')
HML_invest['Active'] = np.where(HML_invest['Predictions'] == 1, 1 + HML_invest['HML'], 1 - 1 * HML_invest['HML'])
HML_invest['Passive'] = 1 + HML_invest['HML']

HML_return = HML_invest.loc[:,['Active', 'Passive']]
HML_invest_annual = (HML_return.reset_index(drop=True)
                               .groupby(by = np.arange(len(HML_return.index))//12, axis=0).prod())
HML_invest_annual


# In[40]:


SMBHML_invest = pred_SMBHML.merge(data.loc[:,['SMB','HML']], right_index=True, left_index=True, how='left')
SMBHML_invest['Active'] = np.where(SMBHML_invest['Predictions'] == 1, 1 + SMBHML_invest['SMB'], 1 + 1 * SMBHML_invest['HML'])
SMBHML_invest['Passive_SMB'] = 1 + SMBHML_invest['SMB']
SMBHML_invest['Passive_HML'] = 1 + SMBHML_invest['HML']
SMBHML_invest['Passive_Equal_Weight'] = 0.5* (SMBHML_invest['Passive_SMB'] + SMBHML_invest['Passive_HML'])

SMBHML_return = SMBHML_invest.loc[:,['Active', 'Passive_SMB','Passive_HML','Passive_Equal_Weight']]
SMBHML_invest_annual = (SMBHML_return.reset_index(drop=True)
                               .groupby(by = np.arange(len(SMBHML_return.index))//12, axis=0).prod())
SMBHML_invest_annual


#returns
SMB_return.reset_index(inplace = True)
SMB_return['Returns_passive'] = np.NaN
SMB_return.loc[0,'Returns_passive'] = SMB_return.loc[0,'Passive']
for i in range(1, len(SMB_return)):
    SMB_return.loc[i,'Returns_passive'] = SMB_return.loc[i,'Passive']*SMB_return.loc[i-1,'Returns_passive']

SMB_return['Returns_active'] = np.NaN
SMB_return.loc[0,'Returns_active'] = SMB_return.loc[0,'Active']
for i in range(1, len(SMB_return)):
    SMB_return.loc[i,'Returns_active'] = SMB_return.loc[i,'Active']*SMB_return.loc[i-1,'Returns_active']
HML_return.reset_index(inplace = True)

HML_return['Returns_passive'] = np.NaN
HML_return.loc[0,'Returns_passive'] = HML_return.loc[0,'Passive']
for i in range(1, len(HML_return)):
    HML_return.loc[i,'Returns_passive'] = HML_return.loc[i,'Passive']*HML_return.loc[i-1,'Returns_passive']

HML_return['Returns_active'] = np.NaN
HML_return.loc[0,'Returns_active'] = HML_return.loc[0,'Active']
for i in range(1, len(SMB_return)):
    HML_return.loc[i,'Returns_active'] = HML_return.loc[i,'Active']*HML_return.loc[i-1,'Returns_active']
    
SMBHML_return.reset_index(inplace = True)
SMBHML_return['Returns_passive_SMB'] = np.NaN
SMBHML_return.loc[0,'Returns_passive_SMB'] = SMBHML_return.loc[0,'Passive_SMB']
for i in range(1, len(SMBHML_return)):
    SMBHML_return.loc[i,'Returns_passive_SMB'] = SMBHML_return.loc[i,'Passive_SMB']*SMBHML_return.loc[i-1,'Returns_passive_SMB']

SMBHML_return['Returns_passive_HML'] = np.NaN
SMBHML_return.loc[0,'Returns_passive_HML'] = SMBHML_return.loc[0,'Passive_HML']
for i in range(1, len(SMBHML_return)):
    SMBHML_return.loc[i,'Returns_passive_HML'] = SMBHML_return.loc[i,'Passive_HML']*SMBHML_return.loc[i-1,'Returns_passive_HML']

SMBHML_return['Returns_passive_EQ'] = np.NaN
SMBHML_return.loc[0,'Returns_passive_EQ'] = SMBHML_return.loc[0,'Passive_Equal_Weight']
for i in range(1, len(SMBHML_return)):
    SMBHML_return.loc[i,'Returns_passive_EQ'] = SMBHML_return.loc[i,'Passive_Equal_Weight']*SMBHML_return.loc[i-1,'Returns_passive_EQ']
    
SMBHML_return['Active_SMBHML'] = np.NaN
SMBHML_return.loc[0,'Active_SMBHML'] = SMBHML_return.loc[0,'Active']
for i in range(1, len(SMBHML_return)):
    SMBHML_return.loc[i,'Active_SMBHML'] = SMBHML_return.loc[i,'Active']*SMBHML_return.loc[i-1,'Active_SMBHML']
SMB_return.to_csv('AdaBoosting_SMB_return.csv')
HML_return.to_csv('AdaBoosting_HML_return.csv')
SMBHML_return.to_csv('AdaBoosting_SMBHML_return.csv')

plt.plot(SMBHML_return['Returns_passive_SMB'],color = 'g', label = 'SMB_passive')
plt.plot(SMBHML_return['Returns_passive_HML'],color = 'r', label = 'HML_passive')
plt.plot(SMBHML_return['Returns_passive_EQ'], color = 'b', label = 'EQ_passsive')
plt.plot(SMBHML_return['Active_SMBHML'], color = 'black', label = 'SMBHML_switch')
plt.legend()

plt.plot(SMB_return['Returns_passive'],color = 'g', label = 'SMB_passive')
plt.plot(SMB_return['Returns_active'],color = 'r', label = 'SMB_active')
plt.plot(HML_return['Returns_passive'], color = 'b', label = 'HML_passive')
plt.plot(HML_return['Returns_active'], color = 'black', label = 'HML_active')
plt.plot(SMBHML_return['Active_SMBHML'], color = 'purple', label = 'SMBHML_switch')
plt.legend()