###################################################################################################
#Script: 03_mort_model
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

lmm1 <- lm(log(mort_10) ~ year + month + sex + age_group + population, data=dat)

summary(lmm1)

#lmm1.1 <- lm(log(mort_10) ~ year + month + sex + age_group + population + year:month, data=dat)
#anova(lmm1, lmm1.1)

lmm2 <- lm(log(mort_10) ~ year + month + sex + age_group + population + year:month +
            sex:age_group + sex:year + sex:month + age_group:year, data=dat)

summary(lmm2)

lmm3 <- lm(log(mort_10) ~ year + month + sex + age_group + population +
             sex:age_group + sex:year + sex:month + age_group:year, data=dat)

anova(lmm2, lmm3)
AIC(lmm2)
AIC(lmm3)
BIC(lmm2)
BIC(lmm3) #BIC says remove

#best model - need to decide if including sex:year though...
lmm4 <- lm(log(mort_10) ~ year + month + sex + age_group + population +
             sex:age_group + sex:month + age_group:year, data=dat)

anova(lmm4, lmm3) #can remove sex:year

#lmm5 <- lm(log(mort_10) ~ year + month + sex + age_group + 
             sex:age_group + sex:month + age_group:year, data=dat)

#anova(lmm5, lmm4) #keep population

lmm6 <- lm(log(mort_10) ~ year + month + sex + age_group + population +
             sex:age_group + age_group:year, data=dat)

lmm7 <- lm(log(mort_10) ~ year + month + sex + age_group + population +
            sex:month + age_group:year, data=dat)

anova(lmm4, lmm6)
anova(lmm4, lmm7)

lmm8 <- lm(log(mort_10) ~ year + month + sex + age_group + population +
             sex:age_group + sex:month, data=dat)
anova(lmm4, lmm8)

######################### SOME DIAGNOSTICS AND COMPARISONS ####################
qqnorm(resid(lmm4))
qqline(resid(lmm4))
hist(resid(lmm4))

models <- list(lm2 = lmm2,
               lm3 = lmm3,
               lm4 = lmm4,
               lm6 = lmm6,
               lm7 = lmm7,
               lm8= lmm8,
               lm1 = lmm1)

aic_bic2 <- imap_dfr(models, ~tibble(Model = .y, AIC = AIC(.x), BIC = BIC(.x)))
aic_bic2

indicators.included <- data.frame(
  Model = paste0("Model ", 1:7),
  year.month = c('X','','','','','',''),
  sex.year = c('X','X','','','','',''),
  sex.month = c('X','X','X','','X','X',''),
  sex.age = c('X','X','X','X','','X',''),
  age.year = c('X','X','X','X','X','','')
)

aic <- data.frame(aic_bic2[,-1])


aic_xtable <- xtable(cbind(indicators.included, aic), caption = "AIC and BIC for Model Selection", 
                    table.placement = "h",
                    label="tab:reg-model-fit",
                    digits = 0)

#other options:
#align - Use "l", "r", and "c" to denote left, right, and center alignment, respectively. 


print(aic_xtable,
      #file='figures/example.tex',
      include.rownames=F,
      include.colnames=F,
      hline.after=c(-1,0,6,7),
      sanitize.colnames.function = identity,
      size='tabcolsep=0.11cm\\small',
      table.placement='t')

X <- model.matrix(lmm4)
ncol(X) # 77 variables w hot encoding

leverage <- hatvalues(lmm4)
n <- nobs(lmm4)
p <- ncol(X)
cutoff <- (2*(p+1)/n)
big_lev <- which(leverage > cutoff) #no large leverage

influence <- cooks.distance(lmm4)
sum(influence > .5) #no large influence

############################################################################

#gaussian but log link
glmm <- glm(mort_10 ~ year + month + sex + age_group + population +
              sex:age_group + sex:month + age_group:year, 
            data=dat,
            family = gaussian(link = "log"))

#residual qq plot
qqnorm(resid(glmm))
qqline(resid(glmm))
hist(resid(glmm))

train <- sample(seq(1,nrow(dat),1),.8*nrow(dat),replace=F)

lmmf <- lm(log(mort_10) ~ year + month + sex + age_group +  population +
             sex:age_group + sex:month + age_group:year, data=dat[train,])


glmf <- glm(mort_10 ~ year + month + sex + age_group + population + 
              sex:age_group + sex:month + age_group:year, 
            data=dat[train,],
            family = gaussian(link = "log"))


pred_tstl <- exp(predict(lmmf, newdata = dat[-train,]))
pred_tstg <- exp(predict(glmf, newdata = dat[-train,]))
pred_trnl <- exp(predict(lmmf))
pred_trng <- exp(predict(glmf))

mse_tstl <- sum((pred_tstl-dat$mort_10[-train])^2)/nrow(dat[-train,])
mse_tstg <- sum((pred_tstg-dat$mort_10[-train])^2)/nrow(dat[-train,])
mse_trnl <- sum((pred_trnl-dat$mort_10[train])^2)/nrow(dat[train,])
mse_trng <- sum((pred_trng-dat$mort_10[train])^2)/nrow(dat[train,])

print(c(mse_tstl, mse_trnl, mse_tstg, mse_trng))

summary(dat$mort_10[-train])

########################################################################
dat$mort_10r <- round(dat$mort_10)
plm2 <- glm(mort_10r ~ year + month + sex + age_group + population + 
              sex:age_group + sex:month + age_group:year, 
            data=dat,
            family = poisson(link = "log"))

groups <- cut(1:nrow(dat),breaks=100,labels=FALSE)
pred <- exp(predict(plm2))

#indicates need for negative binomial model
over.check <- dat %>%
  mutate(pois.pred = pred) %>%
  arrange(pois.pred) %>%
  mutate(group = groups) %>%
  group_by(group) %>%
  summarize(d.var = var(deaths), d.mean = mean(deaths),
            p.var = var(pois.pred), p.mean= mean(pois.pred),
            ratio = d.var/d.mean)

fun1 <-  function(x, theta){x*(1+x/theta)}
fun2 <- function(x){x}

ggplot(data=over.check %>% filter(d.mean < 20000), aes(x=d.mean, y=d.var))+geom_point()+
  stat_function(fun = fun1, args = list(theta = 187099.8))

check2 <- over.check %>% filter(d.mean < 20000)

#doesn't work graet either
nbin2 <- glm.nb(mort_10r ~ year + month + sex + age_group + population + 
                  sex:age_group + sex:year + sex:month + age_group:year, data=dat,
                init.theta = 50)
nbin2$theta


my.lm <- lm(log(mort_10) ~ year+month+sex+age_group+population, data=dat[train,])
summary(my.lm)

pred <- exp(predict(my.lm, newdata = dat[-train,]))

mean((exp(my.lm$fitted.values)-dat$mort_10[train])^2)
mean((pred-dat$mort_10[-train])^2)
