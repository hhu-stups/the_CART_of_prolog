from sklearn import datasets
from sklearn.model_selection import train_test_split
import pandas as pd

# exportiert die benoetigten Datensets

california = datasets.fetch_california_housing()

df = pd.DataFrame(data=california['data'], columns = california['feature_names'])
df['Y'] = california['target']
df.to_csv('california.csv', sep = ',', index = False)

iris = datasets.load_iris()

df = pd.DataFrame(data=iris['data'], columns = iris['feature_names'])
df['class'] = iris['target']
df.to_csv('iris.csv', sep = ',', index = False)

diabetes = datasets.load_diabetes()

df = pd.DataFrame(data=diabetes['data'], columns = diabetes['feature_names'])
df['Y'] = diabetes['target']
df.to_csv('diabetes.csv', sep = ',', index = False)

digits = datasets.load_digits()

df = pd.DataFrame(data=digits['data'], columns = digits['feature_names'])
df['digit'] = digits['target']
df.to_csv('digits.csv', sep = ',', index = False)

# erstellt die train-test-splits
california_feature_train1, california_feature_test1, california_target_train1, california_target_test1 = train_test_split(california['data'],california['target'],test_size=0.3,random_state=18)

df = pd.DataFrame(data=california_feature_train1, columns = california['feature_names'])
df['Y'] = california_target_train1
df.to_csv('california_train1.csv', sep = ',', index = False)

df = pd.DataFrame(data=california_feature_test1, columns = california['feature_names'])
df['Y'] = california_target_test1
df.to_csv('california_test1.csv', sep = ',', index = False)

california_feature_train2, california_feature_test2, california_target_train2, california_target_test2 = train_test_split(california['data'],california['target'],test_size=0.3,random_state=19)

df = pd.DataFrame(data=california_feature_train2, columns = california['feature_names'])
df['Y'] = california_target_train2
df.to_csv('california_train2.csv', sep = ',', index = False)

df = pd.DataFrame(data=california_feature_test2, columns = california['feature_names'])
df['Y'] = california_target_test2
df.to_csv('california_test2.csv', sep = ',', index = False)


california_feature_train3, california_feature_test3, california_target_train3, california_target_test3 = train_test_split(california['data'],california['target'],test_size=0.3,random_state=20)

df = pd.DataFrame(data=california_feature_train3, columns = california['feature_names'])
df['Y'] = california_target_train3
df.to_csv('california_train3.csv', sep = ',', index = False)

df = pd.DataFrame(data=california_feature_test3, columns = california['feature_names'])
df['Y'] = california_target_test3
df.to_csv('california_test3.csv', sep = ',', index = False)


california_feature_train4, california_feature_test4, california_target_train4, california_target_test4 = train_test_split(california['data'],california['target'],test_size=0.3,random_state=21)

df = pd.DataFrame(data=california_feature_train4, columns = california['feature_names'])
df['Y'] = california_target_train4
df.to_csv('california_train4.csv', sep = ',', index = False)

df = pd.DataFrame(data=california_feature_test4, columns = california['feature_names'])
df['Y'] = california_target_test4
df.to_csv('california_test4.csv', sep = ',', index = False)


california_feature_train5, california_feature_test5, california_target_train5, california_target_test5 = train_test_split(california['data'],california['target'],test_size=0.3,random_state=22)

df = pd.DataFrame(data=california_feature_train5, columns = california['feature_names'])
df['Y'] = california_target_train5
df.to_csv('california_train5.csv', sep = ',', index = False)

df = pd.DataFrame(data=california_feature_test5, columns = california['feature_names'])
df['Y'] = california_target_test5
df.to_csv('california_test5.csv', sep = ',', index = False)




iris_feature_train1, iris_feature_test1, iris_target_train1, iris_target_test1 = train_test_split(iris['data'],iris['target'],test_size=0.3,random_state=18)

df = pd.DataFrame(data=iris_feature_train1, columns = iris['feature_names'])
df['class'] = iris_target_train1
df.to_csv('iris_train1.csv', sep = ',', index = False)

df = pd.DataFrame(data=iris_feature_test1, columns = iris['feature_names'])
df['class'] = iris_target_test1
df.to_csv('iris_test1.csv', sep = ',', index = False)


iris_feature_train2, iris_feature_test2, iris_target_train2, iris_target_test2 = train_test_split(iris['data'],iris['target'],test_size=0.3,random_state=19)

df = pd.DataFrame(data=iris_feature_train2, columns = iris['feature_names'])
df['class'] = iris_target_train2
df.to_csv('iris_train2.csv', sep = ',', index = False)

df = pd.DataFrame(data=iris_feature_test2, columns = iris['feature_names'])
df['class'] = iris_target_test2
df.to_csv('iris_test2.csv', sep = ',', index = False)


iris_feature_train3, iris_feature_test3, iris_target_train3, iris_target_test3 = train_test_split(iris['data'],iris['target'],test_size=0.3,random_state=20)

df = pd.DataFrame(data=iris_feature_train3, columns = iris['feature_names'])
df['class'] = iris_target_train3
df.to_csv('iris_train3.csv', sep = ',', index = False)

df = pd.DataFrame(data=iris_feature_test3, columns = iris['feature_names'])
df['class'] = iris_target_test3
df.to_csv('iris_test3.csv', sep = ',', index = False)


iris_feature_train4, iris_feature_test4, iris_target_train4, iris_target_test4 = train_test_split(iris['data'],iris['target'],test_size=0.3,random_state=21)

df = pd.DataFrame(data=iris_feature_train4, columns = iris['feature_names'])
df['class'] = iris_target_train4
df.to_csv('iris_train4.csv', sep = ',', index = False)

df = pd.DataFrame(data=iris_feature_test4, columns = iris['feature_names'])
df['class'] = iris_target_test4
df.to_csv('iris_test4.csv', sep = ',', index = False)


iris_feature_train5, iris_feature_test5, iris_target_train5, iris_target_test5 = train_test_split(iris['data'],iris['target'],test_size=0.3,random_state=22)

df = pd.DataFrame(data=iris_feature_train5, columns = iris['feature_names'])
df['class'] = iris_target_train5
df.to_csv('iris_train5.csv', sep = ',', index = False)

df = pd.DataFrame(data=iris_feature_test5, columns = iris['feature_names'])
df['class'] = iris_target_test5
df.to_csv('iris_test5.csv', sep = ',', index = False)




diabetes_feature_train1, diabetes_feature_test1, diabetes_target_train1, diabetes_target_test1 = train_test_split(diabetes['data'],diabetes['target'],test_size=0.3,random_state=18)

df = pd.DataFrame(data=diabetes_feature_train1, columns = diabetes['feature_names'])
df['Y'] = diabetes_target_train1
df.to_csv('diabetes_train1.csv', sep = ',', index = False)

df = pd.DataFrame(data=diabetes_feature_test1, columns = diabetes['feature_names'])
df['Y'] = diabetes_target_test1
df.to_csv('diabetes_test1.csv', sep = ',', index = False)


diabetes_feature_train2, diabetes_feature_test2, diabetes_target_train2, diabetes_target_test2 = train_test_split(diabetes['data'],diabetes['target'],test_size=0.3,random_state=19)

df = pd.DataFrame(data=diabetes_feature_train2, columns = diabetes['feature_names'])
df['Y'] = diabetes_target_train2
df.to_csv('diabetes_train2.csv', sep = ',', index = False)

df = pd.DataFrame(data=diabetes_feature_test2, columns = diabetes['feature_names'])
df['Y'] = diabetes_target_test2
df.to_csv('diabetes_test2.csv', sep = ',', index = False)


diabetes_feature_train3, diabetes_feature_test3, diabetes_target_train3, diabetes_target_test3 = train_test_split(diabetes['data'],diabetes['target'],test_size=0.3,random_state=20)

df = pd.DataFrame(data=diabetes_feature_train3, columns = diabetes['feature_names'])
df['Y'] = diabetes_target_train3
df.to_csv('diabetes_train3.csv', sep = ',', index = False)

df = pd.DataFrame(data=diabetes_feature_test3, columns = diabetes['feature_names'])
df['Y'] = diabetes_target_test3
df.to_csv('diabetes_test3.csv', sep = ',', index = False)


diabetes_feature_train4, diabetes_feature_test4, diabetes_target_train4, diabetes_target_test4 = train_test_split(diabetes['data'],diabetes['target'],test_size=0.3,random_state=21)

df = pd.DataFrame(data=diabetes_feature_train4, columns = diabetes['feature_names'])
df['Y'] = diabetes_target_train4
df.to_csv('diabetes_train4.csv', sep = ',', index = False)

df = pd.DataFrame(data=diabetes_feature_test4, columns = diabetes['feature_names'])
df['Y'] = diabetes_target_test4
df.to_csv('diabetes_test4.csv', sep = ',', index = False)


diabetes_feature_train5, diabetes_feature_test5, diabetes_target_train5, diabetes_target_test5 = train_test_split(diabetes['data'],diabetes['target'],test_size=0.3,random_state=22)

df = pd.DataFrame(data=diabetes_feature_train5, columns = diabetes['feature_names'])
df['Y'] = diabetes_target_train5
df.to_csv('diabetes_train5.csv', sep = ',', index = False)

df = pd.DataFrame(data=diabetes_feature_test5, columns = diabetes['feature_names'])
df['Y'] = diabetes_target_test5
df.to_csv('diabetes_test5.csv', sep = ',', index = False)




digits_feature_train1, digits_feature_test1, digits_target_train1, digits_target_test1 = train_test_split(digits['data'],digits['target'],test_size=0.3,random_state=18)

df = pd.DataFrame(data=digits_feature_train1, columns = digits['feature_names'])
df['class'] = digits_target_train1
df.to_csv('digits_train1.csv', sep = ',', index = False)

df = pd.DataFrame(data=digits_feature_test1, columns = digits['feature_names'])
df['class'] = digits_target_test1
df.to_csv('digits_test1.csv', sep = ',', index = False)


digits_feature_train2, digits_feature_test2, digits_target_train2, digits_target_test2 = train_test_split(digits['data'],digits['target'],test_size=0.3,random_state=19)

df = pd.DataFrame(data=digits_feature_train2, columns = digits['feature_names'])
df['class'] = digits_target_train2
df.to_csv('digits_train2.csv', sep = ',', index = False)

df = pd.DataFrame(data=digits_feature_test2, columns = digits['feature_names'])
df['class'] = digits_target_test2
df.to_csv('digits_test2.csv', sep = ',', index = False)


digits_feature_train3, digits_feature_test3, digits_target_train3, digits_target_test3 = train_test_split(digits['data'],digits['target'],test_size=0.3,random_state=20)

df = pd.DataFrame(data=digits_feature_train3, columns = digits['feature_names'])
df['class'] = digits_target_train3
df.to_csv('digits_train3.csv', sep = ',', index = False)

df = pd.DataFrame(data=digits_feature_test3, columns = digits['feature_names'])
df['class'] = digits_target_test3
df.to_csv('digits_test3.csv', sep = ',', index = False)


digits_feature_train4, digits_feature_test4, digits_target_train4, digits_target_test4 = train_test_split(digits['data'],digits['target'],test_size=0.3,random_state=21)

df = pd.DataFrame(data=digits_feature_train4, columns = digits['feature_names'])
df['class'] = digits_target_train4
df.to_csv('digits_train4.csv', sep = ',', index = False)

df = pd.DataFrame(data=digits_feature_test4, columns = digits['feature_names'])
df['class'] = digits_target_test4
df.to_csv('digits_test4.csv', sep = ',', index = False)


digits_feature_train5, digits_feature_test5, digits_target_train5, digits_target_test5 = train_test_split(digits['data'],digits['target'],test_size=0.3,random_state=22)

df = pd.DataFrame(data=digits_feature_train5, columns = digits['feature_names'])
df['class'] = digits_target_train5
df.to_csv('digits_train5.csv', sep = ',', index = False)

df = pd.DataFrame(data=digits_feature_test5, columns = digits['feature_names'])
df['class'] = digits_target_test5
df.to_csv('digits_test5.csv', sep = ',', index = False)



