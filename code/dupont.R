# dupont
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

# gather hs df
zip_hs <- jcps_hs_zip %>%
  rename(zipcode = `Zip Code`) %>%
  gather(hs, students, -zipcode) %>%
  mutate(magnet = ifelse(hs %in% c("Dupont Manual", "Butler Traditional",
                                   "Central High School", "Brown School",
                                   "Louisville Male"), "Magnet", "Zoned"))
# get zipcodes for lou hs's
zip_lou <- ky_zip %>%
  filter(zipcode %in% (zip_hs %>% unique() %>% pull(zipcode))) %>%
  select(zipcode, geometry)

# calculate ------

# determine odds a student from a given zip code will attend 
# dupont manual hs
dupont_odds <- zip_hs %>%
  mutate(dupont = ifelse(hs == "Dupont Manual", "Dupont", "Other")) %>%
  group_by(zipcode, dupont) %>%
  summarise(students = sum(students, na.rm = TRUE)) %>% 
  spread(dupont, students) %>%
  mutate(Total = Dupont + Other,
         pct_dm = Dupont/ Total) %>%
  # add mhi data
  left_join(mhi_data) %>%
  filter(Total > 30)

# extract district totals
dupont_dist <- dupont_odds %>%
  filter(zipcode == "High Total")

# pull district rate
dupont_dist_rt <- dupont_dist %>% pull(pct_dm)

# remove district total
dupont_odds <- dupont_odds %>%
  filter(zipcode != "High Total") %>%
  mutate(dist_comp = pct_dm / dupont_dist_rt,
         dist_bin = cut(dist_comp,
                        breaks = c(0,.5,.75,1.25,2,10),
                        labels = c("Less than half district rate",
                                   "50-75% of the district rate",
                                   "Similar to district rate",
                                   "125-200% of the district rate",
                                   "More than double the district rate")),
         equity_students = Total * dupont_dist_rt,
         equity_gap = Dupont - equity_students,
         equity_pct = equity_gap / equity_students) 


# calculations for zips that are 2x district rate
dupont_double <- dupont_odds %>%
  filter(pct_dm > (2 * dupont_dist_rt))

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

# calculations for zips that are 1/2 district rate
dupont_half <- dupont_odds %>%
  filter(pct_dm < ( dupont_dist_rt / 2))

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
  bind_rows(dupont_mid_list) 

dupont_mid_summary <- dupont_mid %>%
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

# save data

write_rds(dupont_geom, "data/dupont_geom.rda")
write_rds(dupont_odds, "data/dupont_odds.rda")
write_rds(dupont_dist, "data/dupont_dist.rda")

# plot ---------

library(kydistgeo)
library(kysrc)

ggplot(dupont_geom)+
  geom_sf(aes(fill = Total))+
  geom_sf(data = ky_dist_geo %>% filter(sch_id == "275"),
          color = "orange", fill = NA)+
  scale_fill_gradientn(colors = c("firebrick",
                                  "lightcoral",
                                  "grey80",
                                  "seagreen3",
                                  "seagreen4"),
                       labels = comma)+
  coord_sf(datum = NA) +
  theme_void() +
  labs(title = "JCPS HS Student Residence by Zip Code",
       fill = "JCPS HS Students\nin Zip Code",
       caption = "Zip codes with fewer than 30 JCPS HS students total are not shaded. Orange line indicates JCPS boundary.") +
  theme(legend.position = c(0.12,.8),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.9, size = 8))

ggplot(dupont_geom)+
  geom_sf(aes(fill = mhi_est))+
  geom_sf(data = ky_dist_geo %>% filter(sch_id == "275"),
          color = "orange", fill = NA)+
  scale_fill_gradientn(colors = c("firebrick",
                               "lightcoral",
                               "grey80",
                               "seagreen3",
                               "seagreen4",
                               "seagreen4"),
                       labels = dollar)+
  coord_sf(datum = NA) +
  theme_void() +
  labs(title = "Louisville Median Household Income by Zip Code",
       fill = "Median Household Income",
       caption = "Zip codes with fewer than 30 JCPS HS students total are not shaded. Orange line indicates JCPS boundary.") +
  theme(legend.position = c(0.12,.8),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.9, size = 8))

ggsave("figures/jcps_zip_mhi.png", height=6, width = 8, units = "in")


ggplot(dupont_geom)+
  geom_sf(aes(fill = dist_bin))+
  geom_sf(data = ky_dist_geo %>% filter(sch_id == "275"),
   color = "orange", fill = NA)+
  
  geom_point(data = profile_sch %>% filter(year == "2016-2017" & sch_id == "275200"),
             aes(x = long, y = lat),
             shape = 16, color = "blue", size = 4) +
  scale_fill_manual(values = c("firebrick",
                               "lightcoral",
                               "grey80",
                               "seagreen3",
                               "seagreen4"
  )) +
  coord_sf(datum = NA) +
  theme_void() +
  labs(title = "Access to Dupont Manual HS by Zip Code",
       fill = "Zip code access rate\ncompared to district rate",
       caption = "Zip codes with fewer than 30 students total or no students sent are not shaded.\nHS location shown as blue point. Orange line indicates JCPS boundary.") +
  theme(legend.position = c(0.12,.8),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.9, size = 8))

ggsave("figures/dupont_odds_map.png", height=6, width = 8, units = "in")


ggplot(dupont_odds,
       aes(x = mhi_est, pct_dm, size = Total)) +
  geom_point(alpha = .6)+
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = percent) +
  scale_size_area(max_size = 10)+ 
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE,
              color = "firebrick", linetype = 3) +
  geom_hline(yintercept = dupont_dist_rt, color = "firebrick")+
  geom_vline(xintercept = 52237, color = "firebrick") +
  labs(x = "Median Household Income",
       y = "Percent of JCPS HS Students @ Dupont Manual",
       title = "Access to Dupont Manual by Louisville Zip Code", 
       size = "JCPS HS Students\nin Zip Code",
       caption = "Vertical red line = Median Household Income for Jefferson County\nHorizontal red line = percent of all JCPS HS students attending Dupont Manual HS\nDotted red line = linear relationship between zip income & enrollment")
ggsave("figures/dupont_odds_plot.png", height=6, width = 8, units = "in")


ggplot(dupont_odds, aes(x = mhi_est, y = equity_pct,
                        size = Total))+
  geom_point(alpha = .6) +
  scale_size_area(max_size = 10) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 52237, color = "firebrick") +
  theme_bw()

       