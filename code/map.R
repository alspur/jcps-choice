# map.R

# load -------
library(tidyverse)
library(sf)
library(scales)
library(kysrc)
library(kydistgeo)

brown_odds <- read_rds("data/brown_odds.rda")
butler_odds <- read_rds("data/butler_odds.rda")
central_odds <- read_rds("data/central_odds.rda")
dupont_odds <- read_rds("data/dupont_odds.rda")
male_odds <- read_rds("data/male_odds.rda")

brown_dist <- read_rds("data/brown_dist.rda")
butler_dist <- read_rds("data/butler_dist.rda")
central_dist <- read_rds("data/central_dist.rda")
dupont_dist <- read_rds("data/dupont_dist.rda")
male_dist <- read_rds("data/male_dist.rda")

ky_zip <- st_read("data/ZIPKY/zipky.shp") %>%
  st_transform(crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>%
  rename(zipcode = ZCTA5CE) %>%
  mutate(zipcode = as.character(zipcode)) %>%
  select(zipcode, geometry) 

# clean ---------

sch_loc <- profile_sch %>% 
  filter(year == "2016-2017") %>%
  filter(sch_id == "275200" | 
           sch_id == "275047" | 
           sch_id == "275165" | 
           sch_id == "275045" | 
           sch_id == "275179") %>%
  mutate(sch_name = str_replace_all(sch_name, " High", ""),
         sch_name = str_replace_all(sch_name, " Magnet Career Academy", ""),
         sch_name = str_replace_all(sch_name, "J. Graham Brown School", "Brown"),
         sch_name = str_replace_all(sch_name, "Louisville ", ""),
         sch_name = str_replace_all(sch_name, " Traditional", ""))

brown_odds <- brown_odds %>%
  mutate(sch_name = "Brown") %>%
  rename(pct_odds = pct_brown) %>%
  select(sch_name, everything())

butler_odds <- butler_odds %>%
  mutate(sch_name = "Butler") %>%
  rename(pct_odds = pct_butler) %>%
  select(sch_name, everything())

central_odds <- central_odds %>%
  mutate(sch_name = "Central") %>%
  rename(pct_odds = pct_central) %>%
  select(sch_name, everything())

dupont_odds <- dupont_odds %>%
  mutate(sch_name = "duPont Manual") %>%
  rename(pct_odds = pct_dm) %>%
  select(sch_name, everything())

male_odds <- male_odds %>%
  mutate(sch_name = "Male") %>%
  rename(pct_odds = pct_male) %>%
  select(sch_name, everything())


mega_odds <- bind_rows(brown_odds, butler_odds, central_odds, dupont_odds, male_odds) %>%
  ungroup()

zip_lou <- ky_zip %>%
  filter(zipcode %in% (mega_odds %>% unique() %>% pull(zipcode))) %>%
  select(zipcode, geometry)

mega_geom <- mega_odds %>%
  left_join(zip_lou)

brown_dist <- brown_dist %>%
  mutate(sch_name = "Brown") %>%
  rename(pct_odds = pct_brown) %>%
  select(sch_name, everything())

butler_dist <- butler_dist %>%
  mutate(sch_name = "Butler") %>%
  rename(pct_odds = pct_butler) %>%
  select(sch_name, everything())

central_dist <- central_dist %>%
  mutate(sch_name = "Central") %>%
  rename(pct_odds = pct_central) %>%
  select(sch_name, everything())

dupont_dist <- dupont_dist %>%
  mutate(sch_name = "duPont Manual") %>%
  rename(pct_odds = pct_dm) %>%
  select(sch_name, everything())

male_dist <- male_dist %>%
  mutate(sch_name = "Male") %>%
  rename(pct_odds = pct_male) %>%
  select(sch_name, everything())

mega_dist <- bind_rows(brown_dist, butler_dist, central_dist, dupont_dist, male_dist) %>%
  ungroup()

# plot ---------

ggplot(mega_geom)+
  geom_sf(aes(fill = dist_bin))+
  scale_fill_manual(values = c("firebrick",
                               "lightcoral",
                               "grey80",
                               "seagreen3",
                               "seagreen4"
  )) +

  geom_sf(data = ky_dist_geo %>% filter(sch_id == "275"),
          color = "orange", fill = NA)+
  geom_point(data = sch_loc,
             aes(x = long, y = lat),
             shape = 16, color = "blue", size = 3) +
  coord_sf(datum = NA) +
  facet_wrap(~sch_name) +
  theme_bw() +
  labs(title = "Access to JCPS Magnet High Schools by Zip Code",
       fill = "Zip Code Access Rate\nCompared to District Rate",
       x = "", y= "",
       caption = "Zip codes with fewer than 30 students total or no students sent are not shaded.\nHS locations are shown as blue points. Orange line indicates JCPS boundary.") +
  theme(legend.position = c(0.85,.2),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.9, size = 8))
ggsave("figures/mega_map.png", height = 6, width = 8, units = "in")

ggplot(mega_odds,
       aes(x = mhi_est, pct_odds, size = Total)) +
  geom_point(alpha = .6)+
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = percent) +
  scale_size_area(max_size = 10)+ 
  facet_wrap(~sch_name) +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE,
              color = "firebrick", linetype = 3) +
  geom_vline(xintercept = 52237, color = "firebrick") +
  geom_hline(data = mega_dist,aes(yintercept = pct_odds), color = "firebrick") +
  labs(x = "Zip Code Median Household Income",
       y = "Percent of JCPS HS Students within Zip Code Attending Particular Magnet HS",
       title = "Access to JCPS Magnet High Schools by Louisville Zip Code Median Household Income", 
       size = "JCPS HS Students\nin Zip Code",
       caption = "Vertical red line = Median Household Income for Jefferson County.\nHorizontal red line = percent of all JCPS HS students attending particular magnet HS.\nDotted red line = linear relationship between zip income & enrollment.") +
  theme(legend.position = c(0.85,.2),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.9, size = 8))

ggsave("figures/mega_plot.png", height = 6, width = 8, units = "in")


ggplot(mega_geom %>%
         filter(sch_name == "Butler" | sch_name == "Male"))+
  geom_sf(aes(fill = dist_bin))+
  scale_fill_manual(values = c("firebrick",
                               "lightcoral",
                               "grey80",
                               "seagreen3",
                               "seagreen4"
  )) +
  
  geom_sf(data = ky_dist_geo %>% filter(sch_id == "275"),
          color = "orange", fill = NA)+
  geom_point(data = sch_loc %>%
               filter(sch_name == "Butler" | sch_name == "Male"),
             aes(x = long, y = lat),
             shape = 16, color = "blue", size = 3) +
  coord_sf(datum = NA) +
  facet_wrap(~sch_name) +
  theme_bw() +
  labs(title = "Access to JCPS Traditional Magnet High Schools by Zip Code",
       fill = "Zip Code Access Rate\nCompared to District Rate",
       x = "", y= "",
       caption = "Zip codes with fewer than 30 students total or no students sent are not shaded.\nHS locations are shown as blue points. Orange line indicates JCPS boundary.") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.9, size = 8))
ggsave("figures/trad_map.png", height = 6, width = 8, units = "in")

ggplot(mega_geom %>%
         filter(sch_name == "Brown"))+
  geom_sf(aes(fill = dist_bin))+
  scale_fill_manual(values = c("firebrick",
                               "lightcoral",
                               "grey80",
                               "seagreen3",
                               "seagreen4"
  )) +
  
  geom_sf(data = ky_dist_geo %>% filter(sch_id == "275"),
          color = "orange", fill = NA)+
  geom_point(data = sch_loc %>%
               filter(sch_name == "Brown"),
             aes(x = long, y = lat),
             shape = 16, color = "blue", size = 3) +
  coord_sf(datum = NA) +
  facet_wrap(~sch_name) +
  theme_bw() +
  labs(title = "Access to Brown Magnet HS by Zip Code",
       fill = "Zip Code Access Rate\nCompared to District Rate",
       x = "", y= "",
       caption = "Zip codes with fewer than 30 students total or no students sent are not shaded.\nHS location shown as a blue point. Orange line indicates JCPS boundary.") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.9, size = 8))
ggsave("figures/brown_map.png", height = 6, width = 8, units = "in")

ggplot(mega_geom %>%
         filter(sch_name == "duPont Manual" | sch_name == "Central"))+
  geom_sf(aes(fill = dist_bin))+
  scale_fill_manual(values = c("firebrick",
                               "lightcoral",
                               "grey80",
                               "seagreen3",
                               "seagreen4"
  )) +
  
  geom_sf(data = ky_dist_geo %>% filter(sch_id == "275"),
          color = "orange", fill = NA)+
  geom_point(data = sch_loc %>%
               filter(sch_name == "duPont Manual" | sch_name == "Central"),
             aes(x = long, y = lat),
             shape = 16, color = "blue", size = 3) +
  coord_sf(datum = NA) +
  facet_wrap(~sch_name) +
  theme_bw() +
  labs(title = "Access to Central & duPont Manual Magnet High Schools by Zip Code",
       fill = "Zip Code Access Rate\nCompared to District Rate",
       x = "", y= "",
       caption = "Zip codes with fewer than 30 students total or no students sent are not shaded.\nHS locations are shown as blue points. Orange line indicates JCPS boundary.") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.9, size = 8))
ggsave("figures/dm_central_map.png", height = 6, width = 8, units = "in")

ccr_sch %>%
  filter(sch_id == "275200" | 
           sch_id == "275047" | 
           sch_id == "275165" | 
           sch_id == "275045" | 
           sch_id == "275179") %>%
  filter(year == "2016-2017") %>%
  filter(student_group == "All Students") %>%
  mutate(sch_name = str_replace_all(sch_name, " High", ""),
         sch_name = str_replace_all(sch_name, " Magnet Career Academy", ""),
         sch_name = str_replace_all(sch_name, "J. Graham Brown School", "Brown"),
         sch_name = str_replace_all(sch_name, "Louisville ", ""),
         sch_name = str_replace_all(sch_name, " Traditional", "")) %>%
  ggplot(aes(x = sch_name, y= ccr_pct)) +
  geom_col() +
  geom_hline(yintercept = .57, color = "firebrick") +
  scale_y_continuous(expand = c(0,0), limits = c(0,1), labels = percent ) + 
  theme_bw()+
  labs(x = "", y = "Percent College/Career Ready",
       title = "JCPS Magnet High School College/Career Readiness, 2016-2017",
       caption = "Red line = JCPS CCR rate of 57%")

ggsave("figures/ccr_mag_hs.png", height = 5, width = 6, units = "in")

act_sch %>%
  filter(sch_id == "275200" | 
           sch_id == "275047" | 
           sch_id == "275165" | 
           sch_id == "275045" | 
           sch_id == "275179") %>%
  filter(year == "2016-2017") %>%
  filter(student_group == "All Students") %>%
  mutate(sch_name = str_replace_all(sch_name, " High", ""),
         sch_name = str_replace_all(sch_name, " Magnet Career Academy", ""),
         sch_name = str_replace_all(sch_name, "J. Graham Brown School", "Brown"),
         sch_name = str_replace_all(sch_name, "Louisville ", ""),
         sch_name = str_replace_all(sch_name, " Traditional", "")) %>%
  ggplot(aes(x = sch_name, y= act_comp_mean)) +
  geom_col() +
  geom_hline(yintercept = 19.1, color = "firebrick") +
  scale_y_continuous(expand = c(0,0), breaks = c(seq(0,30,2)), limits = c(0,30) ) + 
  theme_bw()+
  labs(x = "", y = "Mean ACT Composite Score",
       title = "JCPS Magnet High School ACT Scores, 2016-2017",
       caption = "Red line = JCPS ACT mean of 19.1")

ggsave("figures/act_mag_hs.png", height = 5, width = 6, units = "in")
