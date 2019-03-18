# butler.R
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
  st_transform(crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>%
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

central_odds <- zip_hs %>%
  mutate(central = ifelse(hs == "Central High School",
                         "Central", "Other")) %>%
  group_by(zipcode, central) %>%
  summarise(students = sum(students, na.rm = TRUE)) %>% 
  spread(central, students) %>%
  mutate(Total = Central + Other,
         pct_central = Central / Total) %>%
  left_join(mhi_data) %>%
  filter(Total > 30)


# extract district totals
central_dist <- central_odds %>%
  filter(zipcode == "High Total")

# pull district rate
central_dist_rt <- central_dist %>% pull(pct_central)

# remove district total
central_odds <- central_odds %>%
  filter(zipcode != "High Total") %>%
  mutate(dist_comp = pct_central / central_dist_rt,
         dist_bin = cut(dist_comp,
                        breaks = c(0,.5,.75,1.25,2,10),
                        labels = c("Less than half district rate",
                                   "50-75% of the district rate",
                                   "Similar to district rate",
                                   "125-200% of the district rate",
                                   "More than double the district rate")))




central_double <- central_odds %>%
  filter(pct_central > (2 * central_dist_rt))

central_double_list<- central_double %>%
  select(zipcode) %>%
  mutate(rate = "Double")

central_double_summary <- central_double %>%
  mutate(mhi_weight = sum(mhi_est * Total)) %>%
  ungroup() %>%
  summarise(Central = sum(Central),
            Other = sum(Other),
            Total = sum(Total),
            pct_central = Central / Total,
            mhi_avg = sum(mhi_weight )/ sum(Total))


central_half <- central_odds %>%
  filter(pct_central < ( central_dist_rt / 2))

central_half_list<- central_half %>%
  select(zipcode) %>%
  mutate(rate = "Half")


central_list <- central_double_list %>%
  bind_rows(central_half_list)

central_mid <- central_odds %>% 
  anti_join(central_list)

central_mid_list <- central_mid %>%
  select(zipcode) %>%
  mutate(rate = "About Average")

central_list <- central_list %>%
  bind_rows(central_mid_list) %>%
  filter(zipcode != "High Total")

central_mid_summary <- central_mid %>%
  filter(zipcode != "High Total") %>%
  mutate(mhi_weight = sum(mhi_est * Total)) %>%
  ungroup() %>%
  summarise(Central = sum(Central),
            Other = sum(Other),
            Total = sum(Total),
            pct_central = Central / Total,
            mhi_avg = sum(mhi_weight )/ sum(Total))

central_half_summary <- central_half %>%
  mutate(mhi_weight = sum(mhi_est * Total)) %>%
  ungroup() %>%
  summarise(Central = sum(Central),
            Other = sum(Other),
            Total = sum(Total),
            pct_central = Central / Total,
            mhi_avg = sum(mhi_weight )/ sum(Total))

central_geom <- central_odds %>%
  left_join(central_list) %>%
  left_join(zip_lou)
write_rds(central_geom, "data/central_geom.rda")
write_rds(central_odds, "data/central_odds.rda")
write_rds(central_dist, "data/central_dist.rda")

# plot ----------


library(kydistgeo)
library(kysrc)

ggplot(central_odds %>%
         filter(zipcode != "High Total") %>%
         filter(Total > 10),
       aes(x = mhi_est, pct_central, size = Total)) +
  geom_point(alpha = .6)+
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = percent) +
  scale_size_area(max_size = 10)+ 
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE,
              color = "firebrick", linetype = 3) +
  geom_hline(yintercept = central_dist_rt, color = "firebrick")+
  geom_vline(xintercept = 52237, color = "firebrick") +
  labs(x = "Median Household Income",
       y = "Percent of JCPS HS Students @ Central",
       title = "Access to Central HS by Louisville Zip Code", 
       size = "JCPS HS Students\nin Zip Code",
       caption = "Vertical red line = Median Household Income for Jefferson County\nHorizontal red line = percent of all JCPS HS students attending Central HS\nDotted red line = linear relationship between zip income & enrollment")

ggplot(central_geom)+
  geom_sf(aes(fill = dist_bin))+
  geom_sf(data = ky_dist_geo %>% filter(sch_id == "275"),
          color = "orange", fill = NA)+
  
  geom_point(data = profile_sch %>% filter(year == "2016-2017" & sch_id == "275179"),
             aes(x = long, y = lat),
             shape = 16, color = "blue", size = 4) +
  scale_fill_manual(values = c("firebrick",
                               "lightcoral",
                               "grey80",
                               "seagreen3",
                               "seagreen4"
  )) +
  coord_sf(datum = NA) +
  theme_void()+
  labs(title = "Access to Central HS by Zip Code",
       fill = "Zip code access rate\ncompared to district rate",
       caption = "Zip codes with fewer than 30 students total or no students sent are not shaded.\nHS location shown as blue point. Orange line indicates JCPS boundary.") +
  theme(legend.position = c(0.12,.8),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.9, size = 8))
