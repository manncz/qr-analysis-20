###################################################################################################
#Script: 02_eda
#Inputs: "tidy_mort_dat.Rdata"
#Outputs: N/A
#Author: CM
#Date: 5/12/2020
###################################################################################################

setwd("~/Documents/_Michigan/_Summer 2020/applied qual/qr_analysis")
library(grid)
###################################################################################################

load("temp/tidy_mort_dat.Rdata")

cols <- brewer.pal(10,"Spectral")
cols1 <- c(cols[1:5], "#d9f0a3", cols[7:10])
cols2 <- c(cols[1:5], cols[7:10])


png("figures/eda_youngl.png", width = 7, height = 4, units = 'in', res = 300)
ggplot(data = dat %>% filter(as.numeric(age_group) <= 10 ))+
   geom_line(aes(x=ym, y=log(mort_10), color=age_group)) + facet_grid(~sex) + 
   xlab("Year / Month") + ylab("Deaths per 100,000 population")+
   scale_color_manual(values = cols1, name = "Age Group")
dev.off()

png("figures/eda_olderl.png", width = 7, height = 4, units = 'in', res = 300)
ggplot(data = dat %>% filter(as.numeric(age_group) > 10 )) + 
   geom_line(aes(x=ym, y=log(mort_10), color=age_group))+ facet_grid(~sex)+
   xlab("Year / Month") + ylab("Deaths per 100,000 population") +
   scale_color_manual(values = cols2, name = "Age Group")
dev.off()

png("figures/eda.png", width = 14, height = 8, units = 'in', res = 300)

p2 <- ggplot(data = dat %>% filter(as.numeric(age_group) <= 10 ))+
   geom_line(aes(x=ym, y=mort_10, color=age_group)) + facet_grid(~sex) + 
   xlab("") + ylab("Deaths per 100,000 population")+
   scale_color_manual(values = cols1, name = "Age Group")

p1 <- ggplot(data = dat %>% filter(as.numeric(age_group) > 10 )) + 
   geom_line(aes(x=ym, y=mort_10, color=age_group))+ facet_grid(~sex)+
   xlab("Year / Month") + ylab("Deaths per 100,000 population") +
   scale_color_manual(values = cols2, name = "Age Group")

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

dev.off()

