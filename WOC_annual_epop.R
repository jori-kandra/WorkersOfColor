library(tidyverse)
library(epiextractr)
library(epidatatools)
library(here)
library(blsR)
library(openxlsx)

## CPS data ##
basic_raw <- load_basic(2000:2024, year, month, basicwgt, age, raceorig, hispanic, unemp, emp, lfstat)

basic_clean <- basic_raw %>% 
  filter(age >= 16) %>% 
  mutate(bhaa = case_when(raceorig == 2 & hispanic == 0 ~ "black_alone_nh",
                          raceorig == 3 ~ "aian_alone",
                          (raceorig == 4 | raceorig == 5 | raceorig == 6) & hispanic == 0 ~ "aapi_alone_nh",
                          hispanic == 1 ~ "hispanic",
                          TRUE ~ "other"),
         bhaa2 = case_when(hispanic == 1 ~ "hispanic",
                           raceorig == 2 & hispanic == 0 ~ "black_alone_nh",
                           raceorig == 3 ~ "aian_alone",
                           raceorig == 5 & hispanic == 0 ~ "asian_alone_nh",
                           raceorig == 6 ~ "nhpi_alone",
                           TRUE ~ "other"),
         date = as.Date(paste(year, month, 1, sep = "-"), "%Y-%m-%d"))

cps_annual_epop <- basic_clean %>% 
  filter(bhaa2 != "other") %>% 
  summarize(epop = round(weighted.mean(emp, 
                                       # adjust weight for annual estimates
                                       w = basicwgt/12, na.rm = TRUE), 3),
            # create weighted sums
            n = sum(emp * (basicwgt/12), na.rm = TRUE),
            # .by keeps grouping for just this operation 
            .by = c(year, bhaa2)) %>% 
  pivot_wider(id_cols = year, names_from = bhaa2, values_from = c("epop", "n"))

cps_annual_paepop <- basic_clean %>% 
  filter(bhaa2 != "other", age >= 25 & age <= 54) %>%
  summarize(pa_epop = round(weighted.mean(emp, w = basicwgt/12, na.rm = TRUE), 3),
            n = n(),
            .by = c(year, bhaa2)) %>%
  pivot_wider(id_cols = date, names_from = bhaa2, values_from = c("pa_epop", "n"))
  
civilian_lf_benchmark <- basic_clean %>% 
  filter(lfstat == 1 | lfstat == 2) %>% 
  mutate(wbhaa = case_when(raceorig == 1 & hispanic == 0 ~ "white_nh",
                           raceorig == 2 & hispanic == 0 ~ "black_nh",
                          (raceorig == 4 | raceorig == 5) & hispanic == 0 ~ "asian_nh",
                           raceorig == 3 ~ "AIAN_only",
                          TRUE ~ NA)) %>% 
  arrange(year, wbhaa) %>% 
  summarize(laborforce = sum(basicwgt/12, na.rm = TRUE),
            .by = c(year, wbhaa)) %>% 
  pivot_wider(id_cols = year, names_from = wbhaa, values_from = laborforce)


cps_annual_laborforce <- basic_clean %>% 
  filter(lfstat == 1 | lfstat == 2) %>%
  mutate(wbhaa = case_when(raceorig == 1 & hispanic == 0 ~ "white_alone_nh",
                           raceorig == 2 & hispanic == 0 ~ "black_alone_nh",
                           raceorig == 3 ~ "aian_alone",
                           (raceorig == 4 | raceorig == 5 | raceorig == 6) & hispanic == 0 ~ "aapi_alone_nh",
                           hispanic == 1 ~ "hispanic",
                           TRUE ~ "other")) %>% 
  arrange(year, wbhaa) %>% 
  summarize(laborforce = sum(basicwgt/12, na.rm = TRUE),
            .by = c(year, wbhaa)) %>%  
  pivot_wider(id_cols = year, names_from = wbhaa, values_from = laborforce) 
  
cps_annual_laborforce_all <- basic_clean %>% 
  filter(lfstat == 1 | lfstat == 2) %>% 
  summarize(laborforce_all = sum(basicwgt/12, na.rm = TRUE), .by = year) %>%  
  left_join(cps_annual_laborforce, by = c("year")) %>% 
  select(-other) %>% 
  mutate(across(contains("alone") | contains("hispanic"), ~.x / laborforce_all))




  