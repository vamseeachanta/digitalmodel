import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import six

df = pd.DataFrame()
df['date'] = ['2016-04-01', '2016-04-02', '2016-04-03']
df['calories'] = [2200.555, 2100, 1500]
df['sleep hours'] = [2200.555, 2100, 1500]
df['gym'] = [True, False, False]

print(df)

decimals = pd.Series([0, 2, 2, 0], index=df.columns)

print(df.round(1))
