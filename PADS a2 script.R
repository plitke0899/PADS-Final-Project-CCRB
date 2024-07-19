library(tidyverse)
library(janitor)
library(lubridate)

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

ccrb_comp_cleanv2 <- ccrb_comp_cleanv1 %>% 
  mutate(inc_dt = ymd(inc_date),
         rec_dt = mdy_hms(rec_date),
         close_dt = mdy_hms(close_date))

ccrb_comp_cleanv3 <- ccrb_comp_cleanv2 %>%
  filter(rec_dt > as.Date('2017-01-01'), 
         !is.na(precinct), 
         precinct != "", 
         precinct != "Precinct 0", 
         precinct != "483",
         !is.na(boro),
         boro != "",
         boro != "Outside NYC")

ccrb_comp_explo <- ccrb_comp_cleanv3 %>% 
  group_by(precinct) %>% 
  summarize(comp_count = n())

write.csv(ccrb_comp_explo, 'litkePADSa2R.csv')


  
