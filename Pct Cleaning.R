library(tidyverse)
library(janitor)
library(lubridate)

pct_pop <- read.csv("nyc_precinct_2020pop.csv") #data and data dictionaries found here: https://github.com/jkeefe/census-by-precincts/blob/master/data/nyc/nyc_precinct_2020pop.csv

pct_pop_slim <- pct_pop %>% #selecting only relevant demographic features
  select(precinct, P1_001N, P1_003N, P1_004N, P1_005N, P1_006N, P1_007N, P1_008N, P1_009N, P1_010N, P2_001N, P2_002N, P2_003N, P2_004N, P2_005N, P2_006N, P2_007N, P2_008N, P2_009N, P2_010N, P2_011N)

pct_pop_clean <- pct_pop_slim %>% #cleaning up the names so I can use them without referencing the documentation
  mutate(pct_pop = P1_001N, pop_white = P1_003N, pop_black = P1_004N, pop_nat = P1_005N, pop_asian = P1_006N, pop_oth = P1_008N, pop_birac = P1_009N, pop_hisp = P2_002N, pop_nonhisp = P2_003N, pop_nhwhite = P2_005N, pop_nhblack = P2_006N, pop_nhnat = P2_007N, pop_nhasian = P2_008N) %>%
  select(precinct, pct_pop, pop_white, pop_nhwhite, pop_black, pop_nhblack, pop_hisp, pop_nonhisp, pop_asian, pop_nhasian, pop_nat, pop_nhnat, pop_birac)

write.csv(pct_pop_clean, 'pct_pop_clean.csv')

pct_pop_shares <- pct_pop_clean %>% 
  mutate(pct = precinct, share_wh = pop_white/pct_pop, share_nhwh = pop_nhwhite/pct_pop, share_bl = pop_black/pct_pop, share_nhbl = pop_nhblack/pct_pop, share_hisp = pop_hisp/pct_pop, share_nonhisp = pop_nonhisp/pct_pop, share_asian = pop_asian/pct_pop, share_nat = pop_nat/pct_pop, share_bir = pop_birac/pct_pop)

write.csv(pct_pop_clean, 'pct_pop_shares.csv')
