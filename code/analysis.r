# analysis
# 2019-03-14

# load -------

library(tidyverse)
library(readxl)
library(sf)
library(scales)

jcps_adv_prog <- read_excel("data/jcpsdbk204.xlsx")
jcps_hs_zoned <- read_excel("data/jcpsdbk205.xlsx")
jcps_hs_zip <- read_excel("data/jcpsdbk207.xlsx")

ky_zip <- st_read("data/ZIPKY/zipky.shp") %>%
  rename(zipcode = ZCTA5CE) %>%
  mutate(zipcode = as.character(zipcode)) %>%
  select(zipcode, geometry)

mhi_data <- read_csv("data/ACS_17_5YR_S1903/ACS_17_5YR_S1903_with_ann.csv") %>%
  select(`GEO.id2`, HC03_EST_VC02) %>%
  rename(zipcode = `GEO.id2`,
         mhi_est = HC03_EST_VC02) %>%
  mutate(zipcode = as.character(zipcode),
         mhi_est = as.numeric(mhi_est))

# clean ------

zip_hs <- jcps_hs_zip %>%
  rename(zipcode = `Zip Code`) %>%
  gather(hs, students, -zipcode) %>%
  mutate(magnet = ifelse(hs %in% c("Dupont Manual", "Butler Traditional",
                                   "Central High School", "Brown School",
                                   "Louisville Male"), "Magnet", "Zoned"))

zip_choice <- zip_hs %>%
  group_by(zipcode, magnet) %>%
  summarise(students = sum(students, na.rm = TRUE)) %>% 
  spread(magnet, students) %>%
  mutate(Total = Magnet + Zoned,
         pct_mag = Magnet/ Total) %>%
  left_join(mhi_data)

zip_lou <- ky_zip %>%
  filter(zipcode %in% (zip_hs %>% unique() %>% pull(zipcode))) %>%
  select(zipcode, geometry)

write_rds(zip_lou, "data/zip_lou.rda")

dupont_odds <- zip_hs %>%
  mutate(dupont = ifelse(hs == "Dupont Manual", "Dupont", "Other")) %>%
  group_by(zipcode, dupont) %>%
  summarise(students = sum(students, na.rm = TRUE)) %>% 
  spread(dupont, students) %>%
  mutate(Total = Dupont + Other,
         pct_dm = Dupont/ Total) %>%
  left_join(mhi_data) %>%
  filter(Total > 30)

dupont_double <- dupont_odds %>%
  filter(pct_dm > (2 * .03528))

dupont_double_list<- dupont_double %>%
  select(zipcode) %>%
  mutate(rate = "Double")

dupont_double_summary <- dupont_double %>%
  mutate(mhi_weight = sum(mhi_est * Total)) %>%
  ungroup() %>%
  summarise(Dupont = sum(Dupont),
            Other = sum(Other),
            Total = sum(Total),
            pct_dm = Dupont / Total,
            mhi_avg = sum(mhi_weight )/ sum(Total))


dupont_half <- dupont_odds %>%
  filter(pct_dm < ( .03528 / 2))

dupont_half_list<- dupont_half %>%
  select(zipcode) %>%
  mutate(rate = "Half")

dupont_list <- dupont_double_list %>%
  bind_rows(dupont_half_list)

dupont_mid <- dupont_odds %>% 
  anti_join(dupont_list)


dupont_mid_list <- dupont_mid %>%
  select(zipcode) %>%
  mutate(rate = "About Average")

dupont_list <- dupont_list %>%
  bind_rows(dupont_mid_list) %>%
  filter(zipcode != "High Total")
  
dupont_mid_summary <- dupont_mid %>%
  filter(zipcode != "High Total") %>%
  mutate(mhi_weight = sum(mhi_est * Total)) %>%
  ungroup() %>%
  summarise(Dupont = sum(Dupont),
            Other = sum(Other),
            Total = sum(Total),
            pct_dm = Dupont / Total,
            mhi_avg = sum(mhi_weight )/ sum(Total))

dupont_half_summary <- dupont_half %>%
  mutate(mhi_weight = sum(mhi_est * Total)) %>%
  ungroup() %>%
  summarise(Dupont = sum(Dupont),
            Other = sum(Other),
            Total = sum(Total),
            pct_dm = Dupont / Total,
            mhi_avg = sum(mhi_weight )/ sum(Total))

dupont_geom <- dupont_odds %>%
  left_join(dupont_list) %>%
  left_join(zip_lou)

ggplot(dupont_geom %>%
         filter(zipcode != "High Total"),
       aes(fill = rate))+
  geom_sf()+
  coord_sf(datum = NA) +
  theme_void()

ggplot(zip_choice %>%
         filter(Total > 10), aes(x = mhi_est, pct_mag, size = Total)) +
  geom_point()+
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = percent) +
  scale_size_area(max_size = 10)+ 
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)


ggplot(dupont_odds %>%
       filter(zipcode != "High Total") %>%
       filter(Total > 10),
       aes(x = mhi_est, pct_dm, size = Total)) +
  geom_point(alpha = .6)+
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = percent) +
  scale_size_area(max_size = 10)+ 
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE,
              color = "firebrick", linetype = 3) +
  geom_hline(yintercept = .035278, color = "firebrick")+
  geom_vline(xintercept = 52237, color = "firebrick") +
  labs(x = "Median Household Income",
       y = "Percent of JCPS HS Students @ Dupont Manual",
       title = "Access to Dupont Manual by Louisville Zip Code", 
       size = "JCPS HS Students\nin Zip Code")

male_odds <- zip_hs %>%
  mutate(male = ifelse(hs == "Louisville Male", "Male", "Other")) %>%
  group_by(zipcode, male) %>%
  summarise(students = sum(students, na.rm = TRUE)) %>% 
  spread(male, students) %>%
  mutate(Total = Male + Other,
         pct_male = Male/ Total) %>%
  left_join(mhi_data)


ggplot(male_odds %>% filter(Total > 10), aes(x = mhi_est, pct_male, size = Total)) +
  geom_point()+
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = percent) +
  scale_size_area(max_size = 10)+ 
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)

zip_hs_mhi <- zip_hs %>%
  left_join(mhi_data)

ggplot(zip_hs_mhi %>%
         filter(hs == "Dupont Manual"),
       aes(x = mhi_est, y = students)) +
  geom_point()+
  facet_wrap(~hs, scales = "free_y")


zip_hs_res <- zip_hs %>%
  filter(hs != "Resides") %>%
  filter(hs != "Total") %>%
  left_join(zip_lou)

ggplot(zip_hs_res %>% filter(hs == "Dupont Manual"),
       aes(fill = students)) +
  geom_sf() 
