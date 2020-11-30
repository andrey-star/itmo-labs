import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score


def read(file):
    df = pd.read_csv(file)
    return df.drop("y", axis=1).values, df["y"].values


def train_test(file_num):
    numf = "%02d" % file_num
    base = 'dataset/'
    X_train, y_train = read(base + numf + '_train.csv')
    X_test, y_test = read(base + numf + '_test.csv')
    return X_train, X_test, y_train, y_test


def best_depth_by_acc(file_num):
    X_train, X_test, y_train, y_test = train_test(file_num)
    acc = 0
    depth = 0
    criterion = 0
    splitter = 0
    for cur_depth in range(1, 10):
        for cur_criterion in ["gini", "entropy"]:
            for cur_splitter in ["best", "random"]:
                tree = DecisionTreeClassifier(max_depth=cur_depth, criterion=cur_criterion, splitter=cur_splitter)
                tree.fit(X_train, y_train)
                y_pred = tree.predict(X_test)
                cur_acc = accuracy_score(y_test, y_pred)
                if acc < cur_acc:
                    acc = cur_acc
                    depth = cur_depth
                    criterion = cur_criterion
                    splitter = cur_splitter
                # print("File: %02d, depth: %d, acc: %.5f, crit: %s, split: %s" % (i, cur_depth, cur_acc,
                #                                                                  cur_criterion, cur_splitter))
    return depth, acc, criterion, splitter


def plot_acc_by_depth(file_num, criterion, splitter):
    X_train, X_test, y_train, y_test = train_test(file_num)
    depths = []
    accs_train = []
    accs_test = []
    for depth in range(1, 25):
        tree = DecisionTreeClassifier(max_depth=depth, criterion=criterion, splitter=splitter)
        tree.fit(X_train, y_train)
        depths.append(depth)
        accs_train.append(accuracy_score(y_train, tree.predict(X_train)))
        accs_test.append(accuracy_score(y_test, tree.predict(X_test)))
    plt.plot(depths, accs_train, label="train")
    plt.plot(depths, accs_test, label="test")
    plt.xlabel('depth')
    plt.ylabel('acc')
    plt.title('File: %02d, crit: %s, split: %s' % (file_num, criterion, splitter))
    plt.legend(loc='best')
    plt.show()


def choose(X, y):
    indices = np.random.choice(len(X), len(X))
    return X[indices], y[indices]


def random_forest(file_num, forest_size):
    X_train, X_test, y_train, y_test = train_test(file_num)
    preds_train = []
    preds_test = []
    for i in range(0, forest_size):
        X_train_choose, y_train_choose = choose(X_train, y_train)
        tree = DecisionTreeClassifier(max_features="sqrt")
        tree.fit(X_train_choose, y_train_choose)
        preds_train.append(tree.predict(X_train))
        preds_test.append(tree.predict(X_test))

    max_votes_train = []
    max_votes_test = []
    for i in range(0, len(preds_train[0])):
        votes = []
        for pred in preds_train:
            votes.append(pred[i])
        max_vote = np.argmax(np.bincount(votes))
        max_votes_train.append(max_vote)
    for i in range(0, len(preds_test[0])):
        votes = []
        for pred in preds_test:
            votes.append(pred[i])
        max_vote = np.argmax(np.bincount(votes))
        max_votes_test.append(max_vote)
    return accuracy_score(y_train, max_votes_train), accuracy_score(y_test, max_votes_test)


def plot_acc_by_forest_size(file_num):
    forest_sizes = []
    accs_train = []
    accs_test = []
    acc_train, acc_test = random_forest(file_num, 1)
    forest_sizes.append(1)
    accs_train.append(acc_train)
    accs_test.append(acc_test)
    for forest_size in range(10, 100, 10):
        acc_train, acc_test = random_forest(file_num, forest_size)
        forest_sizes.append(forest_size)
        accs_train.append(acc_train)
        accs_test.append(acc_test)
    plt.plot(forest_sizes, accs_train, label="train")
    plt.plot(forest_sizes, accs_test, label="test")
    plt.xlabel('forest size')
    plt.ylabel('acc')
    plt.title('File: %02d, crit: %s, split: %s' % (11, "gini", "best"))
    plt.legend(loc='best')
    plt.show()


# for i in range(1, 22):
#     depth, acc, criterion, splitter = best_depth_by_acc(i)
#     print("File: %02d, depth: %d, acc: %.5f, crit: %s, split: %s" % (i, depth, acc, criterion, splitter))

# acc_by_depth(11, 'gini', 'best')
# acc_by_depth(12, 'entropy', 'best')

# for i in range(1, 22):
#     acc_train, acc_test = random_forest(i, 100)
#     print("File: %02d, acc_train: %.5f, acc_test: %.5f" % (i, acc_train, acc_test))

# plot_acc_by_forest_size(15)
