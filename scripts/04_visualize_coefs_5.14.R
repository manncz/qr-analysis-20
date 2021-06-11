###################################################################################################
#Script: 04_visualize_coefs.R
#Inputs: "tidy_mort_dat.Rdata"
#Outputs: N/A
#Author: CM
#Date: 5/12/2020
###################################################################################################

setwd("~/Documents/_Michigan/_Summer 2020/applied qual/qr_analysis")

coef.df <- data.frame(summary(lmm4)$coefficients)
coef.df$var = row.names(coef.df)
colnames(coef.df) <- c("est", "se", "t", "p","var")
coef.df <- coef.df %>%
  mutate(sig = ifelse(p < .05, "Yes", "No"),
         coef.exp = exp(est))

#############################  MONTH ########################################
#impact of month for males as compared to females
coefs.month <- coef.df %>%
  filter(str_detect(var, "month\\d{1,2}"))%>%
  mutate(sex = as.numeric(str_detect(var,"Male")),
         mnth = ifelse(sex == 1, str_remove(var, ":sexMale"), var)) %>%
  dplyr::select(sex, mnth, coef.exp) %>%
  spread(key=sex, value = coef.exp) %>%
  mutate(male = `1`*`0`,
         female = `0`,
         month = as.numeric(str_extract(mnth, "\\d+"))) %>%
  dplyr::select(month, Male, Female)%>%
  gather(key = sex, value = effect, -month)
 
png("figures/month_effect.png", width = 6, height = 4, units = 'in', res = 300)
g <- ggplot(data=coefs) + geom_line(aes(x=month, y=effect, color = sex))
g + xlab("Month (January = 1)") + ylab("Multiplicative Effect on Mortality") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  ylim(.88,1.001) + scale_y_continuous(breaks = c(.88, .9, .92, .94, .96, .98, 1)) +
  geom_hline(yintercept = 1) +
  scale_color_manual(name = "Sex", values =c("#2b8cbe", "#7bccc4"), labels = c("Female", "Male"))
dev.off()

#############################  SEX ########################################
#interacts with year, and month, and age

gender.coef <- coef.df %>%
  filter(str_detect(var, "sex")) %>%
  mutate(meas = ifelse(str_detect(var,"age"), str_extract(var, "\\d{2}-\\d{2}"), 
                              ifelse(str_detect(var,"month"), str_extract(var, "month\\d+"), "bs"))) %>%
  dplyr::select(meas, cest=coef.exp, sig)

bs <- gender.coef$cest[gender.coef$meas == "bs"]
mnth <- gender.coef %>% filter(str_detect(meas,"month"))
mnth.rep <- data.frame(month = rep(mnth$meas, 17), est = rep(mnth$cest, 17))
age <-  gender.coef %>% filter(str_detect(meas,"\\d{2}-\\d{2}"))
age.rep <- data.frame(age = rep(age$meas, each = 12), est = rep(age$cest, each = 12))

plot.dat <- data.frame(month = rep(c("month1", mnth$meas), 18), 
                       age = rep(c("00-04",age$meas), each = 12),
                       estm = rep(c(1, mnth$cest), 18), 
                       esta = rep(c(1, age$cest), each = 12), estbs = bs) %>%
  mutate(estimate = estm*esta*estbs,
         month = as.numeric(str_extract(month, "\\d+")))

#plot for impact of gender
cols <- colorRampPalette(brewer.pal(11,"Spectral"))(18)
cols <- colorRampPalette(brewer.pal(9,"GnBu"))(18)

png("figures/gender_effect_month.png", width = 6, height = 4, units = 'in', res = 300)
ggplot(data = plot.dat %>% filter(age %in% c("70-74","75-79", "80-84", "85-99")), aes(x = month, y = estimate, color = age)) + 
  geom_line() +
  xlab("Month (January = 1)") + ylab("Multiplicative Effect on Mortality") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  ylim(.95,1.55) +
  scale_color_manual(values = cols[c(7,10,13,16)], name = "Age Group") +
  geom_hline(yintercept = 1)
dev.off()

plot.dat$age <- factor(plot.dat$age)
ggplot(data = plot.dat %>% filter(month %in% c(1,4,7,10)), aes(x = as.numeric(age), y = estimate, color = as.factor(month))) + geom_line() +
  scale_x_continuous(breaks = 1:18, labels = c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                                               "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-99"))+
  scale_color_manual(values = brewer.pal(8, "GnBu")[4:8])

ggplot(data = plot.dat, aes(x = month, y = estimate, color = age)) + geom_line() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  scale_color_manual(values = cols)


plot.dat$age <- factor(plot.dat$age)

age.sig <- c("N/A", age$sig)

oct.coef <- plot.dat %>% filter(month == 10) %>%
  mutate(sig = age.sig)

png("figures/gender_effect_age1.png", width = 6, height = 4, units = 'in', res = 300)
ggplot(data = oct.coef, 
       aes(x = age, y = estimate,fill = sig)) + 
  geom_bar(stat="identity", position="dodge" )+
  xlab("Age Group") + ylab("Multiplicative Effect on Mortality") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1)) +
  geom_hline(yintercept = 1) +
  scale_fill_manual(name = "Interaction\nsignificant?", values = c("grey","#fdae61", "#2C7BB6")) 
dev.off()


png("figures/gender_effect_age2.png", width = 6, height = 4, units = 'in', res = 300)
ggplot(data = plot.dat %>% filter(month == 2), 
       aes(x = as.numeric(age), y = estimate)) + 
  geom_line()+
  xlab("Age Group") + ylab("Multiplicative Effect on Mortality") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_x_continuous(breaks = 1:18, labels = c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                                               "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-99"))+
  geom_hline(yintercept = 1)
dev.off()


#############################  AGE GROUP  #################################
#interacts with gender and year
#impact of age group for males as compared to females - for a fixed year 2017

sigage <- coef.df %>%
  filter(str_detect(var, "age_group") & !str_detect(var, "year")) %>%
  dplyr::select(sig, var) %>%
  mutate(age = str_extract(var, "\\d{2}-\\d{2}"),
         sex = ifelse(str_detect(var, "Male"),"Male","Female")) %>%
  dplyr::select(-var)

coefs2018 <- coef.df %>%
  filter(str_detect(var, "age_group")) %>%
  mutate(meas = ifelse(str_detect(var,"Male"), "sx", 
                       ifelse(str_detect(var,"year"),"yr", "bs")),
         cest = ifelse(meas=="yr", coef.exp^2018, coef.exp),
         age = str_extract(var, "\\d{2}-\\d{2}"))%>%
  dplyr::select(meas, cest, age) %>%
  spread(key=meas, value = cest) %>%
  mutate(Female = yr*bs,
         Male = Female*sx)%>%
  dplyr::select(age, Male, Female)%>%
  gather(key = sex, value = effect, -age) %>%
  left_join(sigage, by=c("sex","age")) %>%
  mutate(label = ifelse(sig == "Yes", "*", ""),
         place = ifelse(sex == "Female", row_number()-17-.25, row_number()+.25),
         yplace = ifelse(effect-1 <0, effect-1.07, effect-1))


png("figures/age_effect_all.png", width = 6, height = 4, units = 'in', res = 300)
g <- ggplot(data=coefs2018, aes(x=factor(age), y=effect)) + geom_bar(aes( fill = sex), stat="identity", position="dodge")
g + xlab("Age Group") + ylab("Multiplicative Effect on Mortality") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_fill_manual(name = "Sex", values = c("#2b8cbe", "#7bccc4"))+
  annotate("rect", xmin = 0, xmax =9.5, ymin = -1, ymax = 4, 
           alpha = .4) +
  geom_text(aes(y=effect+1, label = label, x= place))
dev.off()

coefs2018$age <- factor(coefs2018$age)

png("figures/age_effect_young.png", width = 6, height = 4, units = 'in', res = 300)
g <- ggplot(data= coefs2018 %>% filter(as.numeric(age)< 10), aes(x=age, y=effect)) + 
  geom_bar(aes( fill = sex), stat="identity", position="dodge")
g + xlab("Age Group") + ylab("Multiplicative Effect on Mortality per 100,000") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_fill_manual(name = "Sex", values = c("#2b8cbe", "#7bccc4")) +
  geom_hline(yintercept=1)
dev.off()

png("figures/age_effect_young2.png", width = 6, height = 4, units = 'in', res = 300)
g <- ggplot(data= coefs2018 %>% filter(as.numeric(age)< 10), aes(x=age, y=effect-1)) + geom_bar(aes( fill = sex), stat="identity", position="dodge")
g + xlab("Age Group") + ylab("Multiplicative Effect on Mortality - 1") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_fill_manual(name = "Sex",labels = c("Female","Male"), values = c("#2b8cbe", "#7bccc4"))+
  geom_text(aes(y=yplace, label = label, x= place))
dev.off()

  geom_text(aes(x=age, y=effect,label=round(effect,1)),position=position_dodge(width = 1),
            show.legend=F, color="black", size=2.5)
  
  
  
  coefs2011 <- coef.df %>%
    filter(str_detect(var, "age_group")) %>%
    mutate(meas = ifelse(str_detect(var,"Male"), "sx", 
                         ifelse(str_detect(var,"year"),"yr", "bs")),
           cest = ifelse(meas=="yr", coef.exp^2011, coef.exp),
           age = str_extract(var, "\\d{2}-\\d{2}"))%>%
    dplyr::select(meas, cest, age) %>%
    spread(key=meas, value = cest) %>%
    mutate(Female = yr*bs,
           Male = Female*sx)%>%
    dplyr::select(age, Male, Female)%>%
    gather(key = sex, value = effect, -age) %>%
    left_join(sigage, by=c("sex","age")) %>%
    mutate(label = ifelse(sig == "Yes", "*", ""),
           place = ifelse(sex == "Female", row_number()-17-.25, row_number()+.25),
           yplace = ifelse(effect-1 <0, effect-1.07, effect-1))
  coefs2011$age <- factor(coefs2011$age)
  g <- ggplot(data= coefs2011 %>% filter(as.numeric(age)< 10), aes(x=age, y=effect-1)) + geom_bar(aes( fill = sex), stat="identity", position="dodge")
  g + xlab("Age Group") + ylab("Multiplicative Effect on Mortality - 1") +
    theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle=45, hjust = 1)) +
    scale_fill_manual(name = "Sex",labels = c("Female","Male"), values = c("#2b8cbe", "#7bccc4"))+
    geom_text(aes(y=yplace, label = label, x= place))

#trend doesn't change

##########################      YEAR      #################################

coefsyear <- coef.df %>%
  filter(str_detect(var, "year")) %>%
  mutate(meas = ifelse(str_detect(var,"age_group"), "age_group", "bs"),
         coef_bs = ifelse(meas=="bs", coef.exp, NA)) %>%
  fill(coef_bs) %>%
  mutate(effect = ifelse(meas == "bs", coef.exp, coef.exp*coef_bs)) %>%
  dplyr::select(var, effect, meas, sig) %>%
  mutate(age = ifelse(meas == "bs", "00-04", str_extract(var, "\\d{2}-\\d{2}")),
         text.place = ifelse(effect-1 > 0, effect -.999, effect-1.001))

png("figures/year_effect.png", width = 6, height = 4, units = 'in', res = 300)
g <- ggplot(data=coefsyear, aes(x=age)) + geom_bar(aes(y=effect-1, fill = sig), stat ="identity", position="dodge")
g + xlab("Age Group") + ylab("Multiplicative Effect on Mortality -1") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_fill_manual(name = "Interaction\nsignificant?", values = c("#fdae61", "#2C7BB6")) +
  geom_text(aes(y = text.place, label=round(effect,3)), show.legend=F, color="black", size=2.5)
dev.off()

