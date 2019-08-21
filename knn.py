import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
from sklearn.neighbors import KNeighborsRegressor
from sklearn.metrics import mean_squared_error

# Input data files are available in the "../input/" directory.
train_path =  "train.csv"
test_path =  "test.csv"
# Read in the data
train_csv = pd.read_csv(train_path)
test_csv = pd.read_csv(test_path)

y_col = "SalePrice"
# X_cols = [col for col in train_csv.columns if col not in y_col]
X_cols = ["LotArea", "YearBuilt", "OverallQual"]

y_train = train_csv[y_col]
X_train = train_csv[X_cols]
X_test = test_csv[X_cols]

neigh = KNeighborsRegressor(n_neighbors = 5)

neigh.fit(X_train, y_train)

test_predict = pd.Series(neigh.predict(X_test))
print("vecino" ,test_predict)
test_ids = test_csv["Id"]

# submission = pd.DataFrame(data = test_predict, index = test_ids)
submission = pd.concat([test_ids, test_predict], axis=1, keys=['Id', 'SalePrice'])

submission.to_csv("submission.csv", index = True )
# test_error = mean_squared_error(y_test, test_predict)