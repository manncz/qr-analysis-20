###################################################################################################
#Script: 05.01_mort_pred_compare
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
perc_err <- function(pred, obs){
  mpe <- mean(abs(pred-obs)/obs)
}


set.seed(12345)
train <- sample(seq(1,nrow(dat),1),.8*nrow(dat),replace=F)
y_var <- which(names(dat)=="mort_10")
pred_vars <- which(names(dat) %in% c("year", "age_group", "month", "population", "sex"))

final_cv_rf_log <- function(X, y, K = 5, seed=12345, ntree, mtry, trn){
  
  N <- nrow(X)
  
  set.seed(seed)
  if(K > 1){
    folds <- cut(sample(N),breaks=K,labels=FALSE)
  }else{
    folds <- 1-as.numeric(1:N %in% trn)
  }
  
  
  #set up vector to store sum of errors for each choice of lambda
  trn_error <- 0
  tst_error <- 0
  trn_err_perc <- 0
  tst_err_perc <- 0
  
  for (k in 1:K){
    
    rf <- randomForest(X[(folds != k),], log(y[(folds != k)]), ntree = ntree, mtry = mtry)
    pred_tst <- exp(predict(rf, newdata = X[(folds == k),]))
    pred_trn <- exp(rf$predicted)
    
    trn_error <- trn_error + mean((y[(folds != k)] - pred_trn)^2)
    tst_error <- tst_error + mean((pred_tst - y[(folds == k)])^2)
    
    trn_err_perc <- trn_err_perc + perc_err(pred = pred_trn, obs = y[(folds != k)])
    tst_err_perc <- tst_err_perc + perc_err(pred = pred_tst, obs = y[(folds == k)])
    
  }
  
  #take average of error rates
  trn_error <- trn_error/K
  tst_error <- tst_error/K
  trn_error_p <- trn_err_perc/K
  tst_error_p <- tst_err_perc/K
  
  return(list("ave_trn_err" = trn_error, "ave_tst_err" = tst_error, 
              "ave_trn_perc_err" = trn_error_p, "ave_tst_perc_err" = tst_error_p))
}

f.cv.rf <- final_cv_rf(X=dat[,pred_vars], y = dat[,y_var], ntree=300, mtry=3)
f.cv.rf

f.cv.rf.log <- final_cv_rf_log(X=dat[,pred_vars], y = dat[,y_var], ntree=300, mtry=3)
f.cv.rf.log

f.cv.rf.log2 <- final_cv_rf_log(X=dat[,pred_vars], y = dat[,y_var], K=1, ntree=300, mtry=3, trn=train)
f.cv.rf.log2

#final cv for gradient boosting
final_cv_gb <- function(dt, K = 5, seed=12345, inter_depth, n_trees){
  
  N <- nrow(dt)
  
  set.seed(seed)
  folds <- cut(sample(N),breaks=K,labels=FALSE)
  
  #set up vector to store sum of errors for each choice of lambda
  trn_error <- 0
  tst_error <- 0
  trn_err_perc <- 0
  tst_err_perc <- 0
  
  for (k in 1:K){
    
    
    gbm_mod <- gbm(mort_10 ~ year + month + sex + age_group + population, distribution = "gaussian", data = dt[(folds != k),],
                   n.trees = n_trees, shrinkage = 1, interaction.depth = inter_depth)
    
    predtst <- predict(gbm_mod, n.trees = n_trees, newdata = dt[(folds == k),], type = "response")
    predtrn <- predict(gbm_mod, n.trees = n_trees, newdata = dt[(folds != k),], type = "response")
    
    tst_error <- tst_error + mean((predtst - dt$mort_10[(folds == k)])^2)
    trn_error <- trn_error + mean((predtrn - dt$mort_10[(folds != k)])^2)
    
    trn_err_perc <- trn_err_perc + perc_err(pred = predtrn, obs = dt$mort_10[(folds != k)])
    tst_err_perc <- tst_err_perc + perc_err(pred = predtst, obs = dt$mort_10[(folds == k)])
    
  }
  
  #take average of error rates
  trn_error <- trn_error/K
  tst_error <- tst_error/K
  trn_error_p <- trn_err_perc/K
  tst_error_p <- tst_err_perc/K
  
  return(list("ave_trn_err" = trn_error, "ave_tst_err" = tst_error, 
              "ave_trn_perc_err" = trn_error_p, "ave_tst_perc_err" = tst_error_p))
}

final_cv_gb_log <- function(dt, K = 5, seed=12345, inter_depth, n_trees, trn){
  
  N <- nrow(dt)
  
  set.seed(seed)
  if(K > 1){
    folds <- cut(sample(N),breaks=K,labels=FALSE)
  }else{
    folds <- 1-as.numeric(1:N %in% trn)
  }
  
  #set up vector to store sum of errors for each choice of lambda
  trn_error <- 0
  tst_error <- 0
  trn_err_perc <- 0
  tst_err_perc <- 0
  
  for (k in 1:K){
    
    gbm_mod <- gbm(log(mort_10) ~ year + month + sex + age_group + population, distribution = "gaussian", data = dt[(folds != k),],
                   n.trees = n_trees, shrinkage = 1, interaction.depth = inter_depth)
    
    predtst <- exp(predict(gbm_mod, n.trees = n_trees, newdata = dt[(folds == k),], type = "response"))
    predtrn <- exp(predict(gbm_mod, n.trees = n_trees, newdata = dt[(folds != k),], type = "response"))
    
    tst_error <- tst_error + mean((predtst - dt$mort_10[(folds == k)])^2)
    trn_error <- trn_error + mean((predtrn - dt$mort_10[(folds != k)])^2)
    
    trn_err_perc <- trn_err_perc + perc_err(pred = predtrn, obs = dt$mort_10[(folds != k)])
    tst_err_perc <- tst_err_perc + perc_err(pred = predtst, obs = dt$mort_10[(folds == k)])
    
  }
  
  #take average of error rates
  trn_error <- trn_error/K
  tst_error <- tst_error/K
  trn_error_p <- trn_err_perc/K
  tst_error_p <- tst_err_perc/K
  
  return(list("ave_trn_err" = trn_error, "ave_tst_err" = tst_error, 
              "ave_trn_perc_err" = trn_error_p, "ave_tst_perc_err" = tst_error_p))
}

f.cv.gb <- final_cv_gb(dt=dat, inter_depth = 2, n_trees = 150)
f.cv.gb

f.cv.gb.log <- final_cv_gb_log(dt=dat, inter_depth = 2, n_trees = 300)
f.cv.gb.log

f.cv.gb.log2 <- final_cv_gb_log(dt=dat, inter_depth = 2, K=1, n_trees = 300, trn =train)
f.cv.gb.log2

final_cv_lm <- function(dt, K = 5, seed=12345, trn){
  
  N <- nrow(dt)
  
  set.seed(seed)
  if(K > 1){
    folds <- cut(sample(N),breaks=K,labels=FALSE)
  }else{
    folds <- 1-as.numeric(1:N %in% trn)
  }
  #set up vector to store sum of errors for each choice of lambda
  trn_error <- 0
  tst_error <- 0
  trn_err_perc <- 0
  tst_err_perc <- 0
  
  for (k in 1:K){
    
    lm_mod <-  lm(log(mort_10) ~ year + month + sex + age_group +  population +
                     sex:age_group + sex:month + age_group:year, data=dat[(folds != k),])
    
    predtst <- exp(predict(lm_mod, newdata = dt[(folds == k),]))
    predtrn <- exp(lm_mod$fitted.values)
    
    tst_error <- tst_error + mean((predtst - dt$mort_10[(folds == k)])^2)
    trn_error <- trn_error + mean((predtrn - dt$mort_10[(folds != k)])^2)
    
    trn_err_perc <- trn_err_perc + perc_err(pred = predtrn, obs = dt$mort_10[(folds != k)])
    tst_err_perc <- tst_err_perc + perc_err(pred = predtst, obs = dt$mort_10[(folds == k)])
  }
  
  #take average of error rates
  trn_error <- trn_error/K
  tst_error <- tst_error/K
  trn_error_p <- trn_err_perc/K
  tst_error_p <- tst_err_perc/K
  
  return(list("ave_trn_err" = trn_error, "ave_tst_err" = tst_error, 
              "ave_trn_perc_err" = trn_error_p, "ave_tst_perc_err" = tst_error_p))
}

f.cv.lm <- final_cv_lm(dt=dat, K=5)
f.cv.lm

f.cv.lm2 <- final_cv_lm(dt=dat, K=1, trn=train)
f.cv.lm2

final_cv_glm <- function(dt, K = 5, seed=12345, trn){
  
  N <- nrow(dt)
  
  set.seed(seed)
  if(K > 1){
    folds <- cut(sample(N),breaks=K,labels=FALSE)
  }else{
    folds <- 1-as.numeric(1:N %in% trn)
  }
  
  #set up vector to store sum of errors for each choice of lambda
  trn_error <- 0
  tst_error <- 0
  trn_err_perc <- 0
  tst_err_perc <- 0
  
  for (k in 1:K){
    
    glm_mod <-  glm(mort_10 ~ year + month + sex + age_group + population + 
                     sex:age_group + sex:month + age_group:year, 
                   data=dat[(folds != k),],
                   family = gaussian(link = "log"))
    
    predtst <- exp(predict(glm_mod, newdata = dt[(folds == k),]))
    predtrn <- glm_mod$fitted.values
    
    tst_error <- tst_error + mean((predtst - dt$mort_10[(folds == k)])^2)
    trn_error <- trn_error + mean((predtrn - dt$mort_10[(folds != k)])^2)
    trn_err_perc <- trn_err_perc + perc_err(pred = predtrn, obs = dt$mort_10[(folds != k)])
    tst_err_perc <- tst_err_perc + perc_err(pred = predtst, obs = dt$mort_10[(folds == k)])
  }
  
  #take average of error rates
  trn_error <- trn_error/K
  tst_error <- tst_error/K
  trn_error_p <- trn_err_perc/K
  tst_error_p <- tst_err_perc/K
  
  return(list("ave_trn_err" = trn_error, "ave_tst_err" = tst_error, 
              "ave_trn_perc_err" = trn_error_p, "ave_tst_perc_err" = tst_error_p))
}

f.cv.glm <- final_cv_glm(dt=dat)
f.cv.glm

f.cv.glm2 <- final_cv_glm(dt=dat, K=1, trn=train)
f.cv.glm2


err.plt <- data.frame(glm = unlist(f.cv.glm),
                      lm = unlist(f.cv.lm),
                      rf = unlist(f.cv.rf.log),
                      gb = unlist(f.cv.gb.log))
err.plt$meas = rownames(err.plt)

err.dt <- err.plt %>%
  filter(!str_detect(meas, "perc")) %>%
  gather(key = "model", value = "err", - meas) %>%
  mutate(meas = str_extract(meas, "(?<=\\_)[:lower:]+(?=\\_)"))

err.dt$model <- as.factor(err.dt$model)
err.dt$model <- factor(err.dt$model,levels = levels(err.dt$model)[c(3,2,1,4)], labels = c("Linear", "GLM", "Gradient\nBoosting", "Random\nForest"))

png("figures/mod_errs.png", width = 6, height = 4, units = 'in', res = 300)
g <- ggplot(data= err.dt, aes(x=model, y=err)) + 
  geom_bar(aes(fill = meas), stat="identity", position="dodge")
g + xlab("Model") + ylab("Mean Squared Prediction Error") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(name = "Train / Test", values = c( "#7bccc4","#2b8cbe"), labels = c("Train", "Test")) 
dev.off()




err.plt2 <- data.frame(glm = unlist(f.cv.glm2),
                      lm = unlist(f.cv.lm2),
                      rf = unlist(f.cv.rf.log2),
                      gb = unlist(f.cv.gb.log2))
err.plt2$meas = rownames(err.plt2)

err.dt2 <- err.plt2 %>%
  filter(!str_detect(meas, "perc")) %>%
  gather(key = "model", value = "err", - meas) %>%
  mutate(meas = str_extract(meas, "(?<=\\_)[:lower:]+(?=\\_)"))

err.dt2$model <- as.factor(err.dt2$model)
err.dt2$model <- factor(err.dt2$model,levels = levels(err.dt2$model)[c(3,2,1,4)], labels = c("Linear", "GLM", "Gradient\nBoosting", "Random\nForest"))

png("figures/mod_errs2.png", width = 6, height = 4, units = 'in', res = 300)
g <- ggplot(data= err.dt2, aes(x=model, y=err)) + 
  geom_bar(aes(fill = meas), stat="identity", position="dodge")
g + xlab("Model") + ylab("Mean Squared Prediction Error") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(name = "Train / Test", values = c( "#9ebcda","#2b8cbe"), labels = c("Train", "Test")) 
dev.off()

#####################  SEE HOW ERROR DEPENDS ON GROUP ################

rf.mod <- randomForest(dat[train,pred_vars], y= log(dat[train,y_var]), ntree=150, mtry=3)

tst.dt <- dat[-train,]
rf.tst.pred <- exp(predict(rf.mod, newdata = dat[-train,pred_vars]))

tst.dt$pred <- rf.tst.pred
tst.dt.sum.rf <- tst.dt %>%
  mutate(sq_err1 = abs(mort_10-pred)/mort_10) %>%
  group_by(age_group, sex) %>%
  summarize(err1 = mean(sq_err1))

#MAYBE USE
png("figures/err_rf.png", width = 6, height = 4, units = 'in', res = 300)
ggplot(data = tst.dt.sum.rf, aes(x = age_group, y=err1, col = sex)) + geom_point() +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1))+
  xlab("Age Group") + ylab("Mean Absolute Percentage Error") +
  scale_color_manual(name = "Sex", values = c("#2b8cbe","#7bccc4")) 
dev.off()

gbm.mod <- gbm(log(mort_10) ~ year + month + sex + age_group + population,
               data = dat[train,], n.trees = 300, shrinkage = 1, 
               interaction.depth = 2)

gbm.tst.pred <- exp(predict(gbm.mod, n.trees = 300, newdata = dat[-train,], type = "response"))

tst.dt <- dat[-train,]
tst.dt$pred <- gbm.tst.pred
tst.dt.sum.gb <- tst.dt %>%
  mutate(sq_err1 = abs(mort_10-pred)/mort_10) %>%
  group_by(age_group, sex) %>%
  summarize(err1 = mean(sq_err1))

png("figures/err_gb.png", width = 6, height = 4, units = 'in', res = 300)
ggplot(data = tst.dt.sum.gb, aes(x = age_group, y=err1, col = sex)) + geom_point() +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "None")+
  xlab("Age Group") + ylab("Mean Absolute Percentage Error") +
  scale_color_manual( values = c("#2b8cbe","#7bccc4")) 
dev.off()


lmmf <- lm(log(mort_10) ~ year + month + sex + age_group +  population +
             sex:age_group + sex:month + age_group:year, data=dat[train,])

glmf <- glm(mort_10 ~ year + month + sex + age_group + population + 
              sex:age_group + sex:month + age_group:year, 
            data=dat[train,],
            family = gaussian(link = "log"))

glm.tst.pred <- exp(predict(glmf, newdata = dat[-train,]))
lm.tst.pred <- exp(predict(lmmf, newdata = dat[-train,]))

tst.dt <- dat[-train,]
tst.dt$pred <- glm.tst.pred
tst.dt.sum.glm <- tst.dt %>%
  mutate(sq_err1 = abs(mort_10-pred)/mort_10) %>%
  group_by(age_group, sex) %>%
  summarize(err1 = mean(sq_err1))

png("figures/err_gb.png", width = 6, height = 4, units = 'in', res = 300)
ggplot(data = tst.dt.sum.glm, aes(x = age_group, y=err1, col = sex)) + geom_point() +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "None")+
  xlab("Age Group") + ylab("Mean Absolute Percentage Error") +
  scale_color_manual( values = c("#2b8cbe","#7bccc4")) 
dev.off()

tst.dt <- dat[-train,]
tst.dt$pred <- lm.tst.pred
tst.dt.sum.lm <- tst.dt %>%
  mutate(sq_err1 = abs(mort_10-pred)/mort_10) %>%
  group_by(age_group, sex) %>%
  summarize(err1 = mean(sq_err1))

png("figures/err_lm.png", width = 6, height = 4, units = 'in', res = 300)
ggplot(data = tst.dt.sum.lm, aes(x = age_group, y=err1, col = sex)) + geom_point() +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "None")+
  xlab("Age Group") + ylab("Mean Absolute Percentage Error") +
  scale_color_manual( values = c("#2b8cbe","#7bccc4")) 
dev.off()

perc_err_obs <- function(pred, obs){
  mpe <- abs(pred-obs)/obs
}

tst.dt <- dat[-train,]
plt.dt <- tst.dt %>%
  mutate(errlm = perc_err_obs(pred = lm.tst.pred, obs = mort_10),
         errglm = perc_err_obs(pred = glm.tst.pred, obs = mort_10),
         errrf = perc_err_obs(pred = rf.tst.pred, obs = mort_10),
         errgb = perc_err_obs(pred = gbm.tst.pred, obs = mort_10)) %>%
  dplyr::select(age_group, sex, errlm, errglm, errrf, errgb) %>%
  group_by(age_group, sex) %>%
  summarize_all(mean) %>%
  gather(key = "mod", value = "err", -age_group, -sex)

plt.dt$mod <- as.factor(plt.dt$mod)  
levels(plt.dt$mod)
plt.dt$mod <- factor(plt.dt$mod, levels = levels(plt.dt$mod)[c(3,2,1,4)], labels = c("Linear", "GLM", "Gradient Boosting", "Random Forest"))

png("figures/err_groups.png", width = 10, height = 6, units = 'in', res = 300)
ggplot(data = plt.dt, aes(x = age_group, y=err, col = sex)) + geom_point() +
  facet_wrap(~mod, nrow=2)+
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1))+
  xlab("Age Group") + ylab("Mean Absolute Percentage Error") +
  scale_color_manual(name = "Sex", values = c("#2b8cbe","#7bccc4")) 
dev.off()


check <- tst.dt %>%
  group_by(age_group) %>%
  summarise(mort = mean(mort_10), n=n())

check2 <- dat %>%
  group_by(age_group) %>%
  summarise(mort = mean(mort_10), n=n())

