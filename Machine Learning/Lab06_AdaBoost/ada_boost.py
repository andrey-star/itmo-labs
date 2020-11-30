import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn.base import BaseEstimator
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score
from mlxtend.plotting import plot_decision_regions


def read(file):
    df = pd.read_csv(file)
    values_str = df["class"].values
    y = np.zeros(len(values_str))
    for i in range(len(values_str)):
        y[i] = 1 if values_str[i] == 'P' else -1
    return df.drop("class", axis=1).values, y


class AdaBoost(BaseEstimator):

    def __init__(self, T):
        self.T = T
        self.b = []
        self.a = []
        self.accs = []

    def fit(self, X, y):
        w = np.repeat(1 / len(y), len(y))
        for t in range(self.T):
            self.b.append(DecisionTreeClassifier(max_depth=3).fit(X, y, sample_weight=w))
            y_pred = self.b[t].predict(X)
            n_t = sum([w[i] if y[i] * y_pred[i] < 0 else 0 for i in range(len(y))])
            self.a.append(0.5 * np.log((1 - n_t) / n_t))
            sum_w = 0
            for i in range(len(y)):
                w[i] *= np.exp(-self.a[t] * y[i] * y_pred[i])
                sum_w += w[i]
            for i in range(len(y)):
                w[i] /= sum_w
            self.accs.append(accuracy_score(y, self.predict(X)))

    def predict(self, X):
        y_pred = np.zeros(len(X))
        for t in range(len(self.b)):
            y_pred_t = self.b[t].predict(X)
            for i in range(len(y_pred_t)):
                y_pred[i] += self.a[t] * y_pred_t[i]
        y_pred_sign = []
        for y in y_pred:
            y_pred_sign.append(-1 if y < 0 else 1)
        return y_pred_sign


def plot_ab(file, title):
    X, y = read(file)
    ab = AdaBoost(100)
    ab.fit(X, y)

    # y_2 = np.vectorize(lambda t: 1 if t == 1 else 0)(y)
    # plot_decision_regions(X, y.astype(np.integer), clf=ab, legend=2)

    plt.plot(range(100), ab.accs)
    plt.title(title)
    plt.xlabel('iter')
    plt.ylabel('acc')
    plt.show()


chips = "dataset/chips.csv"
geyser = "dataset/geyser.csv"
# plot_ab(chips, 'chips')
# plot_ab(geyser, 'geyser')
