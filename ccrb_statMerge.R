library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)

pct_stats <- pct_pop_shares %>% 
  mutate(pct = as.character(precinct)) %>% 
  select(pct, pct_pop, pop_white, pop_nhwhite, 
         pop_black, pop_nhblack, pop_hisp, pop_nonhisp, 
         pop_asian, pop_nhasian, pop_nat, pop_birac, 
         share_wh, share_nhwh, share_bl, share_nhbl, 
         share_hisp, share_nonhisp, share_asian, share_nat, share_bir) %>%
  rename(precinct = pct)

ccrb_comps <- ccrb_comp_cleanv3 %>% 
  select(comp_id, precinct, boro, disp, inc_dt, rec_dt, close_dt) %>% 
  mutate(truncated = case_when(
    disp == "Complainant Uncooperative" ~ "Truncated",
    disp == "Complainant Unavailable" ~ "Truncated",
    disp == "Complaint Withdrawn" ~ "Truncated",
    disp == "Closed - Pending Litigation" ~ "Truncated",
    TRUE ~ "Not Truncated"
  ))

ccrb_co_joint2 <- left_join(ccrb_comps, ccrb_comp_explo, by = "precinct")

comp_stat_merge <- left_join
