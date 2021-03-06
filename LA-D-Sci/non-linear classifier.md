#機器學習實驗-分類器2


##分類問題

我們在[機器學習實驗-分類器1](https://datasciencetaiwan.wordpress.com/2016/03/14/%E6%A9%9F%E5%99%A8%E5%AD%B8%E7%BF%92%E5%AF%A6%E9%A9%97-%E5%88%86%E9%A1%9E%E5%99%A8/)時提過，分類問題為機器學習裡面最最基礎的問題，但當數據無法用一條直線分類時，要怎樣分類？這時候非線性的分類器就可以被應用。本文將介紹基礎的非線性分類器，但注意**所有線性分類器皆可**用**核技法(Kernel Trick)**轉成非線性分類器，並不是死板板的一條直線，之後會再介紹此技法。好那我們看下圖，請問要怎樣分出綠色紅色的點？

![alt text](http://openclassroom.stanford.edu/MainFolder/courses/MachineLearning/exercises/ex8materials/ex8a_dataonly.png)

當然，把這張圖片拿到一個小朋友面前問他這個問題，小朋友很直覺的反應就是拿支筆沿著紅色綠色交界處劃下 界線然後區隔紅色綠色的點。對我們來說很直覺的方式但要怎樣教會你的電腦去做這件事情？那就要引用非線性分類器的特性來做分類。


以下，就是幾種基礎非線性分類器。

**二次判別分析Quadratic Discriminant Analysis(QDA)**
二次判別分析是在機器學習中，用二次曲面來將你的數據分成兩個含以上的分類。 先回去複習一下上次講的X對應到Ｙ。再來，使用這個技法的前提是每個X對應的Y是由以下公式決定：

$$x^T A x + b^T x + c$$

也就是說這樣的觀測會用圓錐曲面切割成不同的分類。

好一樣我們用我們最熟悉的鳶尾花來玩：

# load the package
library(MASS)
data(iris)
# fit model
fit <- qda(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])$class
# summarize accuracy
table(predictions, iris$Species)



你可以看到QDA在這個鳶尾花分類裡面表現非常好。有可能是因為鳶尾花分類並不複雜，一個曲面就hold著整個分類


**Regularized Discriminant Analysis(RDA)規範分類法**
這個分類法其實就是LDA的變種，尤其在當數據量很小時，卻又想要有個準確的結果時，就要引入**(Regularisation)規範**。而公式可以表現為

$$\Sigma = (1-\lambda) \Sigma+\lambda I$$

I可視為同個矩陣，Lambda就是規範。

以下為RDA的試做：

# load the package
library(klaR)
data(iris)
# fit model
fit <- rda(Species~., data=iris, gamma=0.05, lambda=0.01)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])$class
# summarize accuracy
table(predictions, iris$Species)
# load the package


**K-nearest neighbors(KNN)最近鄰居法**
此分類法就是找多數表決的一個算法。可以參考此文章[Big-O](http://enginebai.logdown.com/posts/241676/knn)。而以下就是實作：

#install.packages("lattice")
#install.packages("ggplot2")

library(caret)
library("lattice")
library("ggplot2")
data(iris)
# fit model
fit <- knn3(Species~., data=iris, k=5)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4], type="class")
# summarize accuracy
table(predictions, iris$Species)
# load the package



**Naive Bayes 樸素貝葉斯模型**
貝氏分類器60年代被用在文詞分類後就一直是此領域的熱門方法。基本上此分類法是以字詞出現頻率為特徵判斷文件所屬類別或其他(如垃圾郵件等)的問題。如果有適當的預處理，此算法的精確度可以與這個領域更先進的方法(如SVM)互爭高下。之後會有專文討論，公式就先收下吧！

![alt text](http://www.saedsayad.com/images/Bayes_rule.png)

實作：

#install.packages("e1071")
library(e1071)
data(iris)
# fit model
fit <- naiveBayes(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)




