library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(readr)
library(fastDummies)

source("pct_cleaning.R") #cleaning and prepping census data for precincts

source("ccrb_cleaning.R") #cleaning and prepping CCRB complaint data

#-----Regression specifications!--------------
citywide_reg <- lm(log(comp_rate_p100k) ~ lag(subst_rate, 1) + lag(trunc_rate, 1) + rec_yr + adams, 
                   data = citywide_stat2)
summary(citywide_reg) 

citywide_reg2 <- lm(log(comp_count) ~ lag(subst_count, 1) + lag(trunc_count, 1) + rec_yr + adams, data = citywide_stat2)
summary(citywide_reg2)
#B(lag(subst_rate)) = 0.132 - 0.05 significant effect of prior year substantiations
#but assoc. with INCREASED complaint rate!
#possibly due to reporting effects?
#Adj R2 = 0.96



#both demographics and precincts
both_reg <- lm(log(comp_rate_p100k) ~ lag(subst_rate, 1) + lag(trunc_rate, 1) + rec_yr + adams + precinct + rec_yr:precinct +
                 share_nhwh + share_nhbl + share_hisp + share_asian + share_bir, data = ccrb_comps_pc)
summary(both_reg)
#can't reg demographics and precincts at the same time - too closely correlated!

#regression by demographic characteristics
demo_reg <- lm(log(comp_rate_p100k) ~ lag(subst_rate, 1) + lag(trunc_rate, 1) + rec_yr +adams + 
                 share_nhwh + share_nhbl + share_hisp + share_asian + share_bir, 
               data = ccrb_comps_pc)
summary(demo_reg) #Adjusted R-squared = .264 - B(subst) = +1.865

#regression by precinct - certain ones stick out
pct_reg <- lm(log(comp_count) ~ lag(subst_rate, 1) + lag(trunc_rate, 1) + precinct + rec_yr +rec_yr:precinct + adams, data = ccrb_comps_pc)

summary(pct_reg) #Adjusted R-squared = .2331 - much stronger, catching other effects too, like poverty, crime
#B(subst) = +2.277

pct_reg2 <- lm(log(comp_count) ~ lag(subst_count, 1) + lag(trunc_count, 1) + precinct + rec_yr +rec_yr:precinct + adams + pct_pop, data = ccrb_comps_pc)

summary(pct_reg2)

#separating out borough effects
bor_reg <- lm(log(comp_rate_p100k) ~ lag(subst_rate, 1) + lag(trunc_rate, 1) + rec_yr + adams + boro + boro:rec_yr
              , data = ccrb_comps_pc)
summary(bor_reg) #Looks like strong borough effects, but... (B(subst) = +0.015) (r2 =0.03)

pctbor_reg <- lm(log(comp_rate_p100k) ~ lag(subst_rate, 1) + lag(trunc_rate, 1) + rec_yr + adams
              + precinct + boro+ rec_yr:precinct + rec_yr:boro
              , data = ccrb_comps_pc)
summary(pctbor_reg) #...insignificant when precincts are factored in (B(subst) = 2.394), R2 = 0.3

#---------Some visualizations

citywide_scatter1 <- ggplot(citywide_stat2) +
  geom_point(aes(x = rec_yr, y = comp_rate_p100k)) +
  labs(
    title = "Complaint Rates dropped until 2021, then rise",
    subtitle = "citywide complaints 100k residents, 2017-2024ytd",
    x = "log of substantiation rate",
    y = "complaints per 100k",
    caption = "source: https://data.cityofnewyork.us/Public-Safety/Civilian-Complaint-Review-Board-Complaints-Against/2mby-ccnw/about_data"
  ) +
  theme_minimal()

citywide_scatter1

citywide_line<- ggplot(citywide_stat2) +
  geom_line(aes(x = rec_yr, y = comp_count)) +
  labs(
    title = "Complaints dropped until 2021, then rise",
    subtitle = "citywide complaints 100k residents, 2017-2024ytd",
    x = "year",
    y = "complaints",
    caption = "source: https://data.cityofnewyork.us/Public-Safety/Civilian-Complaint-Review-Board-Complaints-Against/2mby-ccnw/about_data"
  ) +
  theme_minimal()
citywide_line

citywide_line2<- ggplot(citywide_stat2) +
  geom_line(aes(x = rec_yr, y = comp_rate_p100k)) +
  labs(
    title = "Complaints dropped until 2021, then rise",
    subtitle = "citywide complaints per 100k residents, 2017-2024ytd",
    x = "year",
    y = "complaints per 100k",
    caption = "source: https://data.cityofnewyork.us/Public-Safety/Civilian-Complaint-Review-Board-Complaints-Against/2mby-ccnw/about_data"
  ) +
  theme_minimal()
citywide_line2

subst_line <- ggplot(citywide_stat2) +
  geom_line(aes(x = rec_yr, y = subst_count)) +
  labs(
    title = "Complaints dropped until 2021, then rise",
    subtitle = "citywide substantiated complaints, 2017-2024ytd",
    x = "year",
    y = "substantiations",
    caption = "source: https://data.cityofnewyork.us/Public-Safety/Civilian-Complaint-Review-Board-Complaints-Against/2mby-ccnw/about_data"
  ) +
  geom_line(aes(x = rec_yr, y = comp_count)) +
  geom_line(aes(x = rec_yr, y = trunc_count)) +
  theme_minimal()
subst_line

pct_scatter1 <- ggplot(ccrb_comps_pc) +
  geom_jitter(aes(x = subst_rate, y = comp_rate_p100k)) +
  labs(
    title = "complaints vs substantiations by precinct",
    subtitle = "citywide substantiated complaints, 2017-2024ytd",
    x = "complaints per capita",
    y = "substantiations",
    caption = "source: https://data.cityofnewyork.us/Public-Safety/Civilian-Complaint-Review-Board-Complaints-Against/2mby-ccnw/about_data"
  )
pct_scatter1

pct_scatter_pop <- ggplot(ccrb_comps_pc) +
  geom_jitter(aes(x = lag(subst_rate, 1), y = comp_rate_p100k)) +
  labs(
    title = "truncations vs substantiations by precinct",
    subtitle = "citywide substantiated complaints, 2017-2024ytd",
    x = "substantiations",
    y = "complaints per capita",
    caption = "source: https://data.cityofnewyork.us/Public-Safety/Civilian-Complaint-Review-Board-Complaints-Against/2mby-ccnw/about_data"
  )
pct_scatter_pop





