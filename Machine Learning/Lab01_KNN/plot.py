import pandas as pd
import matplotlib.pyplot as plt
# filename = 'res_all.csv'
# filename = 'res_fixed.csv'
filename = 'res/res_naive.csv'

dataset = pd.read_csv(filename)
xLabel = dataset.columns[0]
x = dataset.loc[:, xLabel].tolist()
y = dataset.loc[:, 'f1'].tolist()
plt.xlabel(xLabel)
plt.ylabel('f1')
plt.plot(x, y)
plt.show()
