###################################################################################################
#Script: 03_mort_pred
#Inputs: "tidy_mort_dat.Rdata"
#Outputs: N/A
#Author: CM
#Date: 5/13/2020
###################################################################################################

setwd("~/Documents/_Michigan/_Summer 2020/applied qual/qr_analysis")

library(randomForest)
library(glmnet)
library(tidyverse)
library(gbm)
library(plotly)
library(ggplot2)
library(MASS)

###################################################################################################

load("temp/tidy_mort_dat.Rdata")

dat$month <- factor(dat$month)

set.seed(12345)
train <- sample(seq(1,nrow(dat),1),.8*nrow(dat),replace=F)

lmmf <- lm(log(mort_10) ~ year + month + sex + age_group +  population +
             sex:age_group + sex:month + age_group:year, data=dat[train,])

predtst <- exp(predict(lmmf, newdata = dat[-train,]))
predtrn <- exp(lmmf$fitted.values)

mean((predtst - dat$mort_10[-train])^2)
mean((predtrn - dat$mort_10[train])^2)


glmf <- glm(mort_10 ~ year + month + sex + age_group + population + 
              sex:age_group + sex:month + age_group:year, 
            data=dat[train,],
            family = gaussian(link = "log"))

predtst <- exp(predict(glmf, newdata = dat[-train,]))
predtrn <- glmf$fitted.values

mean((predtst - dat$mort_10[-train])^2)
mean((predtrn - dat$mort_10[train])^2)


########################### RANDOM FOREST ###########################
y_var <- which(names(dat)=="mort_10")
pred_vars <- which(names(dat) %in% c("year", "age_group", "month", "population", "sex"))
rf.mod <- randomForest(dat[train,pred_vars], y=dat[train,y_var], ntree=100, importance = TRUE, mtry=2)
pred5 <- predict(rf.mod)
mse5 <- sum((pred5-dat$mort_10[train])^2)/length(train)

pred5t <- predict(rf.mod, newdata = dat[-train,])
mse5t <- sum((pred5t-dat$mort_10[-train])^2)/nrow(X[-train,])

mean((pred5t-dat$mort_10[-train])^2)

cv_random_forest <- function(X, y, mtry, n_trees, K = 5){
  
  N <- nrow(X)
  folds <- cut(sample(N),breaks=K,labels=FALSE)
  
  n_try <- length(mtry)
  n_ttest <- length(n_trees)
  
  #set up vector to store sum of errors for each choice of lambda
  ave_error <- matrix(rep(0, n_ttest*n_try), nrow=n_ttest)
  
  for (i in 1:n_ttest){
    for (j in 1: n_try){
      for (k in 1:K){
        rf <- randomForest(X[(folds != k),], y[(folds != k)], ntree = n_trees[i], mtry= mtry[j])
        pred <- predict(rf, newdata = X[(folds == k),])
        ave_error[i,j] <- ave_error[i,j] + mean((pred - y[(folds == k)])^2)
      }
    }
  }
  
  #take average of error rates
  ave_error <- ave_error/K
  
  return(rbind(mtry, ave_error))
}

cv_random_forest2 <- function(X, y, mtry, n_trees, K = 5){
  
  N <- nrow(X)
  folds <- cut(sample(N),breaks=K,labels=FALSE)
  
  n_try <- length(mtry)
  n_ttest <- length(n_trees)
  
  #set up vector to store sum of errors for each choice of lambda
  ave_error <- matrix(rep(0, n_ttest*n_try), nrow=n_ttest)
  
  for (i in 1:n_ttest){
    for (j in 1: n_try){
      for (k in 1:K){
        rf <- randomForest(X[(folds != k),], log(y[(folds != k)]), ntree = n_trees[i], mtry= mtry[j])
        pred <- exp(predict(rf, newdata = X[(folds == k),]))
        ave_error[i,j] <- ave_error[i,j] + mean((pred - y[(folds == k)])^2)
      }
    }
  }
  
  #take average of error rates
  ave_error <- ave_error/K
  
  return(rbind(mtry, ave_error))
}

#cv for maxnodes and n trees
mtry <- c(1,2,3,4)
n_trees <- c(75, 100, 125, 150, 175, 200, 300, 500, 600)
rf_cv <- cv_random_forest(X = dat[train,pred_vars], y = dat[train,y_var], mtry = mtry, n_trees = n_trees)
rf_cv2 <- cv_random_forest2(X = dat[train,pred_vars], y = dat[train,y_var], mtry = mtry, n_trees = n_trees)


#plot cross validation results
cvdat <- data.frame(rf_cv[-1,])
names(cvdat) <- c(mtry)
cvdat$ntree <- n_trees
plotdat <- cvdat %>% gather("mtry", "err", 1:4)
cv.opt <- plotdat %>% filter(err== min(err))
ggplot(aes(x=ntree, y=err, col=mtry), data =plotdat %>% filter(mtry!=1)) + geom_line()

pdat2 <- plotdat %>% filter(ntree == 300)
ggplot(aes(x=mtry, y=err),data = pdat2) + geom_point()


cvdat2 <- data.frame(rf_cv2[-1,])
names(cvdat2) <- c(mtry)
cvdat2$ntree <- n_trees
plotdat2 <- cvdat2 %>% gather("mtry", "err", 1:4)
#cv.opt2 <- plotdat2 %>% filter(err== min(err))
ggplot(aes(x=ntree, y=err, col=mtry), data =plotdat2 %>% filter(mtry!=1)) + geom_line()

pdat2 <- plotdat2 %>% filter(ntree == 150, mtry>1)
ggplot(aes(x=mtry, y=err),data = pdat2) + geom_point()


rf.mod <- randomForest(dat[train,pred_vars], y=log(dat[train,y_var]), ntree=150, importance = TRUE, mtry=3)
pred5 <- exp(predict(rf.mod))
mse5 <- sum((pred5-dat$mort_10[train])^2)/length(train)

pred5t <- exp(predict(rf.mod, newdata = dat[-train,]))
mse5t <- sum((pred5t-dat$mort_10[-train])^2)/nrow(X[-train,])


#mean accuracy variable importance
imp <- sort(rf.mod$importance[,1]/sum(rf.mod$importance[,1]))
imp <- data.frame(imp)
imp$var <- c("Month", "Year",  "Sex", "Population", "Age Group")

png("figures/vi_rf.png", width = 6, height = 4, units = 'in', res = 300)
g <- ggplot(data=imp) + geom_col(aes(x=reorder(var,imp), y=imp), fill="#2C7BB6")
g + coord_flip() + xlab("Feature") + ylab("Variable Importance (Standardized)") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()
########################### GRADIENT BOOSTING ###########################

gbm.mod <- gbm(mort_10 ~ year + month + sex + age_group + population,
               data = dat[train,], n.trees = 500, shrinkage = .1, 
               interaction.depth = 2)

#cross validation for depth and n_trees
cv_gb <- function(dt, inter_depth, n_trees, K = 5, shrink=1){
  
  N <- nrow(dt)
  folds <- cut(sample(N),breaks=K,labels=FALSE)
  
  n_depth <- length(inter_depth)
  n_ttest <- length(n_trees)
  
  #set up vector to store sum of errors for each choice of lambda
  ave_error <- matrix(rep(0, n_ttest*n_depth), nrow=n_ttest)
  
  for (i in 1:n_ttest){
    for (j in 1: n_depth){
      for (k in 1:K){
        gbm_mod <- gbm(mort_10 ~ year + month + sex + age_group + population, distribution = "gaussian", data = dt[(folds != k),],
                       n.trees = n_trees[i], shrinkage = shrink, interaction.depth = inter_depth[j])
        pred <- predict(gbm_mod, n.trees = n_trees[i], newdata = dt[(folds == k),], type = "response")
        ave_error[i,j] <- ave_error[i,j] + mean((pred-dat$mort_10[(folds == k)])^2)
      }
    }
  }
  
  #take average of error rates
  ave_error <- ave_error/K
  
  return(ave_error)
}
cv_gb2 <- function(dt, inter_depth, n_trees, K = 5, shrink=1){
  
  set.seed(12345)
  N <- nrow(dt)
  folds <- cut(sample(N),breaks=K,labels=FALSE)
  
  n_depth <- length(inter_depth)
  n_ttest <- length(n_trees)
  
  #set up vector to store sum of errors for each choice of lambda
  ave_error <- matrix(rep(0, n_ttest*n_depth), nrow=n_ttest)
  
  for (i in 1:n_ttest){
    for (j in 1: n_depth){
      for (k in 1:K){
        gbm_mod <- gbm(log(mort_10) ~ year + month + sex + age_group + population, distribution = "gaussian", data = dt[(folds != k),],
                       n.trees = n_trees[i], shrinkage = shrink, interaction.depth = inter_depth[j])
        pred <- exp(predict(gbm_mod, n.trees = n_trees[i], newdata = dt[(folds == k),], type = "response"))
        ave_error[i,j] <- ave_error[i,j] + mean((pred-dt$mort_10[(folds == k)])^2)
      }
    }
  }
  
  #take average of error rates
  ave_error <- ave_error/K
  
  return(ave_error)
}


id <- c(1,2,3,4,5)
nt <- c(75, 100, 125, 150, 175, 200, 300, 500)
cv_results_gbm <- cv_gb(dat, inter_depth =id , n_trees=nt, shrink = 1)
cv_results_gbm2 <- cv_gb2(dat[train,], inter_depth =id , n_trees=nt, shrink = 1)


cvdatgbm <- data.frame(cv_results_gbm)
cvdatgbm$ntree <- nt
names(cvdatgbm) <- c(id, "ntree")
plotdatgbm <- cvdatgbm %>% gather("depth", "err", 1:6)
cv.opt.gbm <- plotdatgbm %>% filter(err== min(err))
ggplot(aes(x=ntree, y=err, col=depth), data = plotdatgbm) +geom_line()


cvdatgbm2 <- data.frame(cv_results_gbm2)
cvdatgbm2$ntree <- nt
names(cvdatgbm2) <- c(id, "ntree")
plotdatgbm2 <- cvdatgbm2 %>% gather("depth", "err", 1:5)
cv.opt.gbm2 <- plotdatgbm2 %>% filter(err== min(err))
ggplot(aes(x=ntree, y=err, col=depth), data = plotdatgbm2 %>% filter(depth>1)) +geom_line()


#final model
set.seed(12345)
gbm.mod <- gbm(log(mort_10) ~ year + month + sex + age_group + population,
               data = dat[train,], n.trees = 300, shrinkage = 1, 
               interaction.depth = 2)

pred6 <- exp(predict(gbm.mod, n.trees = 300, type = "response"))
mean((pred6-dat$mort_10[train])^2)

pred6t <- exp(predict(gbm.mod, newdata = dat[-train,], n.trees = 300, type = "response"))
mean((pred6t-dat$mort_10[-train])^2)

imp <- summary(gbm.mod)$rel.inf/100
imp <- data.frame(imp)
summary(gbm.mod)$var
imp$var <- c("Age Group", "Sex", "Month", "Population", "Year")

#this is nicer...
png("figures/vi_gb.png", width = 6, height = 4, units = 'in', res = 300)
g <- ggplot(data=imp) + geom_col(aes(x=reorder(var,imp), y=imp), fill="#2C7BB6")
g + coord_flip() + xlab("") + ylab("Variable Importance (Standardized)") + 
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

