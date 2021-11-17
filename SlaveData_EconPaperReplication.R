library(haven)
library(ggplot2)
library(tidyverse)
slave_trade_QJE <- read_dta("Downloads/slave_trade_QJE.dta")

slave_data = slave_trade_QJE

#qplot(slave_trade_QJE$ln_export_area, slave_trade_QJE$ln_maddison_pcgdp2000, 
      #main= "Relationships between Slave Exports and GDP", 
      #xlab = "log of exports/area",
      #ylab = "log of per capita GDP in 2000")


#OLS estimates of GDP and slave exports
lm(ln_maddison_pcgdp2000 ~ ln_export_area, data = slave_trade_QJE)


#Plot relationship between GDP and slave exports/area
ggplot(data = slave_data, 
       aes(x = ln_export_area, y =ln_maddison_pcgdp2000 )) + geom_smooth(method = 'lm', se =FALSE) + geom_text(aes(label=isocode), check_overlap = T)

 #Plot the dots of the relationship between GDP and slave exports/area                                                                                                                                        
plot(slave_data$ln_export_area, slave_data$ln_maddison_pcgdp2000) 

#OLS estimates of slave exports/area and colonizer effects
ols1 = lm(ln_maddison_pcgdp2000 ~ ln_export_area + 
     colony1 + colony2 + colony3 + colony4 + colony5 + colony6 + 
      colony7 , 
   data = slave_data)

#OLS colonizer and geographic effects
ols2 = lm(ln_maddison_pcgdp2000 ~ ln_export_area + 
      abs_latitude + longitude + rain_min + humid_max + low_temp + ln_coastline_area + colony1 + colony2 + colony3 + colony4 + colony5 + colony6 + colony7, 
   data = slave_data)

summary(ols1)
summary(ols2)

library(usethis)
edit_git_config()
library(usethis)
create_github_token()
library(gitcreds)
gitcreds_set()
library(usethis)
use_github()
 