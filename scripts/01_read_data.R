###################################################################################################
#Script: 01_read_data
#Inputs: "pop_mort.csv"
#Outputs: "tidy_mort_dat.Rdata"
#Author: CM
#Date: 5/12/2020
###################################################################################################

setwd("~/Documents/_Michigan/_Summer 2020/applied qual/qr_analysis")

###################################################################################################

mort.dat <- read.csv("input/pop_mort.csv")
#no missing data
sum(complete.cases(mort.dat))

names(mort.dat) <- str_to_lower(names(mort.dat))

#years are from 2007-2018
table(mort.dat$year)
table(mort.dat$month)
table(mort.dat$age_group)

#add a couple of calculated variables
dat <- mort.dat %>%
   mutate(ym = ymd(paste(as.character(year), sprintf('%02d',month), sep = "-"),truncated = 1),
          mort_rate = deaths/population,
          mort_10 = mort_rate*100000,
          age_ordered = as.ordered(age_group),
          risk_age = ifelse(age_group %in% c("80_84", "85_99"), "elderly", 
                            ifelse(age_group %in% c("60_64", "65_69", "70_74", "75_79"), "old adult",
                                   ifelse(age_group %in% c("40_44", "45_49", "50_54", "55_59"), "middle aged", "premature"))),
          age_group = str_replace(age_group, "_","-"))
dat$month <- as.factor(dat$month)
dat$age_group <- as.factor(dat$age_group)

save(dat, file= "temp/tidy_mort_dat.Rdata")
