library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(readr)
library(fastDummies)

ccrb_comp_raw <- read.csv("Civilian_Complaint_Review_Board__Complaints_Against_Police_Officers_20240623.csv")

ccrb_comp_cleanv1 <- ccrb_comp_raw %>% 
  mutate(inc_date = Incident.Date,
         rec_date = CCRB.Received.Date,
         close_date = Close.Date,
         boro = Borough.Of.Incident.Occurrence,
         precinct = Precinct.Of.Incident.Occurrence,
         disp = CCRB.Complaint.Disposition,
         comp_id = Complaint.Id) %>% 
  select(comp_id, inc_date, rec_date, close_date, boro, precinct, disp)
#Mostly renamings here; keeping comp_id and precinct/boro identifiers so
#I can join them to other sets later

#date-timing the date complaints get received - incidents could be far in the past 
#and investigations can sometimes take years so this is a good approximator
ccrb_comp_cleanv2 <- ccrb_comp_cleanv1 %>%
  mutate(rec_dt = mdy_hms(rec_date))

ccrb_comp_cleanv3 <- ccrb_comp_cleanv2 %>%
  filter(rec_dt > as.Date('2017-01-01'), 
         !is.na(precinct), 
         precinct != "", 
         precinct != "Precinct 0", 
         precinct != "483",
         !is.na(boro),
         boro != "",
         boro != "Outside NYC")

#binary-ifying dispositions of interest so I can analyze them numerically
ccrb_comps <- ccrb_comp_cleanv3 %>% 
  select(comp_id, precinct, boro, disp, rec_dt) %>% 
  mutate(truncated = if_else(
    disp == "Complainant Uncooperative" |
      disp == "Complainant Unavailable" | 
      disp == "Complaint Withdrawn" |
      disp == "Closed - Pending Litigation", 1, 0),
    subst = if_else(
      disp == "Substantiated (Command Discipline A)" |
        disp == "Substantiated (Command Discipline B)" |
        disp == "Substantiated (Command Lvl Instructions)" |
        disp == "Substantiated (Formalized Training)", 1, 0),
    rec_yr = year(rec_dt),
    adams = if_else(rec_yr > 2021, 1, 0)
  ) #Year helpful, but covers multiple administrations "adams" can isolate that effect

ccrb_comps_dum <- ccrb_comps %>% 
  dummy_cols(select_columns = c("precinct", "rec_yr", "boro"))

ccrb_grouped <- ccrb_comps_dum %>% 
  group_by(precinct, rec_yr, boro, adams) %>% 
  summarize(trunc_rate = mean(truncated)*100,
            subst_rate = mean(subst)*100,
            subst_count = sum(subst),
            trunc_count = sum(truncated),
            ln_sr = log(subst_rate),
            ln_tr = log(trunc_rate),
            comp_count = n())
#substantiation rate is *100 to make the regression more legible as a percent

#join the census data
ccrb_pct_join <- left_join(ccrb_grouped, pct_stats_v2, by = "precinct")

#compute per-capita complaints
ccrb_comps_pc <- ccrb_pct_join %>% 
  mutate(comp_rate_p100k = comp_count/pct_pop * 100000)

#un/regrouping the stats to do a citywide regression

citywide_stat1 <- ccrb_comps %>% 
  group_by(rec_yr, adams) %>% 
  summarize(trunc_rate = mean(truncated)*100,
            subst_rate = mean(subst)*100,
            subst_count = sum(subst),
            trunc_count = sum(truncated),
            ln_sr = log(subst_rate),
            ln_tr = log(trunc_rate),
            comp_count = n())

citywide_stat2 <- citywide_stat1 %>% 
  mutate(comp_rate_p100k = comp_count/8800000 * 100000)




