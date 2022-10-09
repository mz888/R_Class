Class 6 Demo
================
Mike Zhu
9/12/2021

## Predictive Analytics

``` r
knitr::opts_chunk$set(echo = TRUE)

library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mlbench)
library(rpart)
library(rpart.plot)
library(class)

titanic <- read.csv("https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv")
housing <- data(BostonHousing)

titanic <- titanic %>% mutate(Female = ifelse(Sex == "female", 1, 0))
titanic <- titanic %>% select(Survived, Pclass, Female, Age,
                              Siblings.Spouses.Aboard,
                              Parents.Children.Aboard, Fare)
```

### Sample

``` r
BH.sample <- sample(nrow(BostonHousing), 300)
BH.train <- BostonHousing[sample(BH.sample),]
BH.test <- BostonHousing[-sample(BH.sample),]

TT.sample <- sample(nrow(titanic), 600)
TT.train <- titanic[sample(TT.sample),]
TT.test <- titanic[-sample(TT.sample),]
```

## Baseline Models

``` r
# Linear regression
lm.housing <- lm(medv ~ ., BH.train)
summary(lm.housing)
```

    ## 
    ## Call:
    ## lm(formula = medv ~ ., data = BH.train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.8068 -2.7345 -0.6755  1.7899 24.4103 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 26.392896   6.413919   4.115 5.07e-05 ***
    ## crim        -0.108683   0.033564  -3.238 0.001345 ** 
    ## zn           0.042214   0.016827   2.509 0.012672 *  
    ## indus        0.040027   0.073089   0.548 0.584362    
    ## chas1        4.088172   1.086015   3.764 0.000203 ***
    ## nox         -9.785556   4.799843  -2.039 0.042397 *  
    ## rm           4.820077   0.536433   8.985  < 2e-16 ***
    ## age         -0.035837   0.016540  -2.167 0.031083 *  
    ## dis         -1.317409   0.245788  -5.360 1.72e-07 ***
    ## rad          0.269797   0.082905   3.254 0.001273 ** 
    ## tax         -0.014417   0.004610  -3.127 0.001947 ** 
    ## ptratio     -0.816945   0.163505  -4.996 1.02e-06 ***
    ## b            0.005446   0.003184   1.710 0.088268 .  
    ## lstat       -0.457808   0.062945  -7.273 3.38e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.427 on 286 degrees of freedom
    ## Multiple R-squared:  0.7765, Adjusted R-squared:  0.7663 
    ## F-statistic: 76.44 on 13 and 286 DF,  p-value: < 2.2e-16

``` r
# predict
lm.pred <- predict(lm.housing, BH.test %>% select(-"medv"))

# Get the MSE of predictions
print(paste("Training MSE:", mean(lm.housing$residuals^2))) # train
```

    ## [1] "Training MSE: 18.6860703866392"

``` r
print(paste("Test MSE:", mean((BH.test$medv - lm.pred)^2))) # test
```

    ## [1] "Test MSE: 29.0092477421984"

``` r
# Logistic regression
lgm.titanic <- glm(Survived ~ ., TT.train, family = "binomial")
summary(lgm.titanic)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ ., family = "binomial", data = TT.train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8478  -0.6135  -0.3834   0.6343   2.5123  
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              2.662786   0.605298   4.399 1.09e-05 ***
    ## Pclass                  -1.259772   0.182957  -6.886 5.75e-12 ***
    ## Female                   2.733403   0.244748  11.168  < 2e-16 ***
    ## Age                     -0.044958   0.009180  -4.898 9.70e-07 ***
    ## Siblings.Spouses.Aboard -0.382500   0.127100  -3.009  0.00262 ** 
    ## Parents.Children.Aboard -0.070079   0.138141  -0.507  0.61194    
    ## Fare                     0.003390   0.003119   1.087  0.27707    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 798.81  on 599  degrees of freedom
    ## Residual deviance: 523.45  on 593  degrees of freedom
    ## AIC: 537.45
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
# predict 
lgm.trainpred <- predict(lgm.titanic, TT.train %>% select(-"Survived"), type = "response") # train
lgm.testpred <- predict(lgm.titanic, TT.test %>% select(-"Survived"), type = "response") # test

TT.train["lgm_pred"] <- if_else(lgm.trainpred > 0.5, 1, 0) # if prob > 0.5, then assume prediction = 1
TT.test["lgm_pred"] <- if_else(lgm.testpred > 0.5, 1, 0)

# get the accuracy of model on train and test sets
print(paste("Training accuracy:", sum(1*(TT.train$lgm_pred == TT.train$Survived)) / nrow(TT.train)))
```

    ## [1] "Training accuracy: 0.808333333333333"

``` r
print(paste("Test accuracy:", sum(1*(TT.test$lgm_pred == TT.test$Survived)) / nrow(TT.test)))
```

    ## [1] "Test accuracy: 0.801393728222996"

## Trees

``` r
# Regression tree
tree.housing <- rpart(medv ~ ., BH.train)

tree.trainpred <- predict(tree.housing, BH.train %>% select(-"medv"))
tree.testpred <- predict(tree.housing, BH.test %>% select(-"medv"))

print(paste("Training MSE:", mean((BH.train$medv - tree.trainpred)^2)))
```

    ## [1] "Training MSE: 12.4291612304726"

``` r
print(paste("Test MSE:", mean((BH.test$medv - tree.testpred)^2)))
```

    ## [1] "Test MSE: 25.756955342992"

``` r
rpart.plot(tree.housing)
```

![](class6_demo_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## K-Nearest Neighbors

``` r
# KNN classification
knn.train <- knn(TT.train %>% select(-"Survived"), TT.train %>% select(-"Survived"), 
                 cl = TT.train[,1], k=3)

knn.test <- knn(TT.train %>% select(-"Survived"), TT.test %>% select(-"Survived"), 
                 cl = TT.train[,1], k=3)

##score accuracy
knn.trainacc <- data.frame(knn.train, as.factor(TT.train[,1]))
knn.testacc <- data.frame(knn.test, as.factor(TT.test[,1]))

print(paste("Training accuracy:", sum(1*(knn.trainacc[,1] == knn.trainacc[,2])) / nrow(knn.trainacc)))
```

    ## [1] "Training accuracy: 0.845"

``` r
print(paste("Test accuracy:", sum(1*(knn.testacc[,1] == knn.testacc[,2])) / nrow(knn.testacc)))
```

    ## [1] "Test accuracy: 0.693379790940767"
