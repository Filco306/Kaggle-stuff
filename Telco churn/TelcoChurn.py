import pandas as pd
import numpy as np

data = pd.read_csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Convert all to categories that should be such
for i in range(data.shape[1]):
    if i not in [0,5,18,19]:
        data[data.columns[i]] = data[data.columns[i]].astype("category")


from sklearn.linear_model import LogisticRegressionCV

model = LogisticRegressionCV(solver="saga")
model.fit(data[data.columns[1:20]],data[data.columns[20]])
