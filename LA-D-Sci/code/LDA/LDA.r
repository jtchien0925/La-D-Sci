# load the package
library(MASS)
library(ggplot2)
library(scales)
library(gridExtra)
data(iris)
# fit model
fit <- lda(Species~., data=iris, prior= c(1,1,1)/3)
# summarize the fit
summary(fit)
prop.lda = fit$svd^2/sum(fit$svd^2)

# make predictions
predictions <- predict(fit, newdata=iris)


r2 <- lda(Species ~ ., 
          data = iris, 
          prior = c(1,1,1)/3,
          CV = TRUE)

train <- sample(1:150, 75)

r3 <- lda(Species ~ ., # training model
          iris, 
          prior = c(1,1,1)/3, 
          subset = train)

plda = predict(object = r, # predictions
               newdata = iris[-train, ])


dataset = data.frame(species = iris[,"Species"],lda = predictions$x)


plot(fit)
p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = species, shape = species), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))


