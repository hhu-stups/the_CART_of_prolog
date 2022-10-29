import time
import sys
from sklearn.model_selection import train_test_split
from sklearn import tree
from sklearn import datasets
from matplotlib import pyplot as plt


california = datasets.fetch_california_housing()
iris = datasets.load_iris()
diabetes = datasets.load_diabetes()
digits = datasets.load_digits()

california_feature_train1, california_feature_test1, california_target_train1, california_target_test1 = train_test_split(california['data'],california['target'],test_size=0.3,random_state=18)
california_feature_train2, california_feature_test2, california_target_train2, california_target_test2 = train_test_split(california['data'],california['target'],test_size=0.3,random_state=19)
california_feature_train3, california_feature_test3, california_target_train3, california_target_test3 = train_test_split(california['data'],california['target'],test_size=0.3,random_state=20)
california_feature_train4, california_feature_test4, california_target_train4, california_target_test4 = train_test_split(california['data'],california['target'],test_size=0.3,random_state=21)
california_feature_train5, california_feature_test5, california_target_train5, california_target_test5 = train_test_split(california['data'],california['target'],test_size=0.3,random_state=22)

iris_feature_train1, iris_feature_test1, iris_target_train1, iris_target_test1 = train_test_split(iris['data'],iris['target'],test_size=0.3,random_state=18)
iris_feature_train2, iris_feature_test2, iris_target_train2, iris_target_test2 = train_test_split(iris['data'],iris['target'],test_size=0.3,random_state=19)
iris_feature_train3, iris_feature_test3, iris_target_train3, iris_target_test3 = train_test_split(iris['data'],iris['target'],test_size=0.3,random_state=20)
iris_feature_train4, iris_feature_test4, iris_target_train4, iris_target_test4 = train_test_split(iris['data'],iris['target'],test_size=0.3,random_state=21)
iris_feature_train5, iris_feature_test5, iris_target_train5, iris_target_test5 = train_test_split(iris['data'],iris['target'],test_size=0.3,random_state=22)

diabetes_feature_train1, diabetes_feature_test1, diabetes_target_train1, diabetes_target_test1 = train_test_split(diabetes['data'],diabetes['target'],test_size=0.3,random_state=18)
diabetes_feature_train2, diabetes_feature_test2, diabetes_target_train2, diabetes_target_test2 = train_test_split(diabetes['data'],diabetes['target'],test_size=0.3,random_state=19)
diabetes_feature_train3, diabetes_feature_test3, diabetes_target_train3, diabetes_target_test3 = train_test_split(diabetes['data'],diabetes['target'],test_size=0.3,random_state=20)
diabetes_feature_train4, diabetes_feature_test4, diabetes_target_train4, diabetes_target_test4 = train_test_split(diabetes['data'],diabetes['target'],test_size=0.3,random_state=21)
diabetes_feature_train5, diabetes_feature_test5, diabetes_target_train5, diabetes_target_test5 = train_test_split(diabetes['data'],diabetes['target'],test_size=0.3,random_state=22)

digits_feature_train1, digits_feature_test1, digits_target_train1, digits_target_test1 = train_test_split(digits['data'],digits['target'],test_size=0.3,random_state=18)
digits_feature_train2, digits_feature_test2, digits_target_train2, digits_target_test2 = train_test_split(digits['data'],digits['target'],test_size=0.3,random_state=19)
digits_feature_train3, digits_feature_test3, digits_target_train3, digits_target_test3 = train_test_split(digits['data'],digits['target'],test_size=0.3,random_state=20)
digits_feature_train4, digits_feature_test4, digits_target_train4, digits_target_test4 = train_test_split(digits['data'],digits['target'],test_size=0.3,random_state=21)
digits_feature_train5, digits_feature_test5, digits_target_train5, digits_target_test5 = train_test_split(digits['data'],digits['target'],test_size=0.3,random_state=22)

def precision(preds, test):
    l = len(preds)
    akk = 0
    for i in range(l):
        if(preds[i] == test[i]):
            akk += 1
    return akk / l

def train_test_classification(feature_train,target_train,feature_test,target_test):
    t1 = time.process_time()
    clf = tree.DecisionTreeClassifier()
    clf = clf.fit(feature_train,target_train)
    t2 = time.process_time()
    trainTime = t2 - t1
    print('Train Time:       {time} sec.'.format(time=trainTime))
    preds = clf.predict(feature_test)
    prec = precision(preds, target_test)
    print('Precision:        {p}'.format(p = prec))
    return trainTime, prec

def mean(x):
    return sum(x) / len(x)

def metrics(targets):
    l = len(targets)
    m = mean(targets)
    sumsq = 0
    for i in range(l):
        sumsq += (targets[i] - m)**2
    variance = sumsq / (l - 1)
    abw = variance**(1/2)
    err = abw / (l**(1/2))
    return variance, abw, err

def mean_sq_err(preds, tests):
    l = len(preds)
    acc = 0
    for i in range(l):
        acc += (preds[i] - tests[i])**2
    return acc / l

def train_test_regression(feature_train,target_train,feature_test,target_test):
    t1 = time.process_time()
    clf = tree.DecisionTreeRegressor()
    clf = clf.fit(feature_train,target_train)
    t2 = time.process_time()
    trainTime = t2 - t1
    print('Train Time:                    {time} sec.'.format(time=trainTime))
    preds = clf.predict(feature_test)
    prec = mean_sq_err(preds, target_test)
    variance, abw, err = metrics(target_train)
    print('Mean Squared Error:            {p}'.format(p = prec))
    """
    print('Sample Variance:               {v}'.format(v = variance))
    print('Sample Standard Deviation:     {s}'.format(s = abw))
    print('Standard Error of Mean         {e}'.format(e = err))
    """
    return trainTime, prec, variance, abw, err

def run_iris():
    print("Training iris: Set1")
    t1, p1 = train_test_classification(iris_feature_train1, iris_target_train1, iris_feature_test1, iris_target_test1)
    print('----------------------------------------------------------')
    print("Training iris: Set2")
    t2, p2 = train_test_classification(iris_feature_train2, iris_target_train2, iris_feature_test2, iris_target_test2)
    print('----------------------------------------------------------')
    print("Training iris: Set3")
    t3, p3 = train_test_classification(iris_feature_train3, iris_target_train3, iris_feature_test3, iris_target_test3)
    print('----------------------------------------------------------')
    print("Training iris: Set4")
    t4, p4 = train_test_classification(iris_feature_train4, iris_target_train4, iris_feature_test4, iris_target_test4)
    print('----------------------------------------------------------')
    print("Training iris: Set5")
    t5, p5 = train_test_classification(iris_feature_train5, iris_target_train5, iris_feature_test5, iris_target_test5)
    print('----------------------------------------------------------')
    mt = mean([t1,t2,t3,t4,t5])
    mp = mean([p1,p2,p3,p4,p5])
    var, abw, err = metrics([t1,t2,t3,t4,t5])
    print('Average Train Time:            {t} sec.'.format(t = mt))
    print('Standard Error of Mean(Time):  {err}'.format(err = err))
    print('Average Precision:             {p}'.format(p = mp))
    print()
    print()



def run_digits():
    print("Training digits: Set1")
    t1, p1 = train_test_classification(digits_feature_train1, digits_target_train1, digits_feature_test1, digits_target_test1)
    print('----------------------------------------------------------')
    print("Training digits: Set2")
    t2, p2 = train_test_classification(digits_feature_train2, digits_target_train2, digits_feature_test2, digits_target_test2)
    print('----------------------------------------------------------')
    print("Training digits: Set3")
    t3, p3 = train_test_classification(digits_feature_train3, digits_target_train3, digits_feature_test3, digits_target_test3)
    print('----------------------------------------------------------')
    print("Training digits: Set4")
    t4, p4 = train_test_classification(digits_feature_train4, digits_target_train4, digits_feature_test4, digits_target_test4)
    print('----------------------------------------------------------')
    print("Training digits: Set5")
    t5, p5 = train_test_classification(digits_feature_train5, digits_target_train5, digits_feature_test5, digits_target_test5)
    print('----------------------------------------------------------')
    mt = mean([t1,t2,t3,t4,t5])
    mp = mean([p1,p2,p3,p4,p5])
    var, abw, err = metrics([t1,t2,t3,t4,t5])
    print('Average Train Time:            {t} sec.'.format(t = mt))
    print('Standard Error of Mean(Time):  {err}'.format(err = err))
    print('Average Precision:             {p}'.format(p = mp))
    print()
    print()



def run_diabetes():
    print("Training diabetes: Set1")
    t1, p1, v1, a1, e1 = train_test_regression(diabetes_feature_train1, diabetes_target_train1, diabetes_feature_test1, diabetes_target_test1)
    print('----------------------------------------------------------')
    print("Training diabetes: Set2")
    t2, p2, v2, a2, e2 = train_test_regression(diabetes_feature_train2, diabetes_target_train2, diabetes_feature_test2, diabetes_target_test2)
    print('----------------------------------------------------------')
    print("Training diabetes: Set3")
    t3, p3, v3, a3, e3 = train_test_regression(diabetes_feature_train3, diabetes_target_train3, diabetes_feature_test3, diabetes_target_test3)
    print('----------------------------------------------------------')
    print("Training diabetes: Set4")
    t4, p4, v4, a4, e4 = train_test_regression(diabetes_feature_train4, diabetes_target_train4, diabetes_feature_test4, diabetes_target_test4)
    print('----------------------------------------------------------')
    print("Training diabetes: Set5")
    t5, p5, v5, a5, e5 = train_test_regression(diabetes_feature_train5, diabetes_target_train5, diabetes_feature_test5, diabetes_target_test5)
    print('----------------------------------------------------------')
    mt = mean([t1,t2,t3,t4,t5])
    mp = mean([p1,p2,p3,p4,p5])
    mv = mean([v1,v2,v3,v4,v5])
    ma = mean([a1,a2,a3,a4,a5])
    me = mean([e1,e2,e3,e4,e5])
    var, abw, err = metrics([t1,t2,t3,t4,t5])
    print('Average Train Time:            {t} sec.'.format(t = mt))
    print('Standard Error of Mean(Time):  {err}'.format(err = err))
    print('Average Mean Squared Error:    {p}'.format(p = mp))
    """
    print('Average Variance:              {v}'.format(v = mv))
    print('Average Standard Error:        {a}'.format(a = ma))
    print('Average Error of Mean:         {p}'.format(p = me))
    """
    print()
    print()


def run_california():
    print("Training california: Set1")
    t1, p1, v1, a1, e1 = train_test_regression(california_feature_train1, california_target_train1, california_feature_test1, california_target_test1)
    print('----------------------------------------------------------')
    print("Training california: Set2")
    t2, p2, v2, a2, e2 = train_test_regression(california_feature_train2, california_target_train2, california_feature_test2, california_target_test2)
    print('----------------------------------------------------------')
    print("Training california: Set3")
    t3, p3, v3, a3, e3 = train_test_regression(california_feature_train3, california_target_train3, california_feature_test3, california_target_test3)
    print('----------------------------------------------------------')
    print("Training california: Set4")
    t4, p4, v4, a4, e4 = train_test_regression(california_feature_train4, california_target_train4, california_feature_test4, california_target_test4)
    print('----------------------------------------------------------')
    print("Training california: Set5")
    t5, p5, v5, a5, e5 = train_test_regression(california_feature_train5, california_target_train5, california_feature_test5, california_target_test5)
    print('----------------------------------------------------------')
    mt = mean([t1,t2,t3,t4,t5])
    mp = mean([p1,p2,p3,p4,p5])
    mv = mean([v1,v2,v3,v4,v5])
    ma = mean([a1,a2,a3,a4,a5])
    me = mean([e1,e2,e3,e4,e5])
    var, abw, err = metrics([t1,t2,t3,t4,t5])
    print('Average Train Time:            {t} sec.'.format(t = mt))
    print('Standard Error of Mean(Time):  {err}'.format(err = err))
    print('Average Mean Squared Error:    {p}'.format(p = mp))
    """
    print('Average Variance:              {v}'.format(v = mv))
    print('Average Standard Error:        {a}'.format(a = ma))
    print('Average Error of Mean:         {p}'.format(p = me))
    """
    print()
    print()


def run_all():
    sys.stdout = open("Iris_tree_Py.txt", "w")
    run_iris()
    sys.stdout = open("Digits_tree_Py.txt", "w")
    run_digits()
    sys.stdout = open("Diabetes_tree_Py.txt", "w")
    run_diabetes()
    sys.stdout = open("California_tree_Py.txt", "w")
    run_california()
    sys.stdout.close()

run_all()





