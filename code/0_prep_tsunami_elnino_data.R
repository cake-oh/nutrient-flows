#### goal: process + visualize other variables ####
## el niño data (oni); tsunami; SDGs


#### [0] prep ####
library(tidyverse)

## load data
# external variables
oni_og <- read_csv("data/oni.csv")
tsunami_og <- read_tsv("data/tsunamis-2024-07-30_09-52-34_-0600.tsv")
sdg2_og <- read_csv("data/Goal2.csv")
sdg3_og <- read_csv("data/Goal3.csv")


#### [1] process + plot oni ####
oni_avg <- oni_og %>% mutate(annual_mean = rowMeans(select(., 2:13), na.rm = TRUE))

p_oni <- ggplot(oni_avg,aes(Year,annual_mean)) + geom_line() +
  scale_x_continuous(breaks = seq(1996, 2022, by = 2), limits = c(1996, 2022)) +
  labs(title = "Oceanic Niño Index", x = "Year", y = "Annual Mean") +
  geom_hline(yintercept = c(-0.5, 0.5), linetype = "dashed", color = c("blue","red")) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
plot(p_oni)
# ggsave("output/oni_plot.jpg")

# write.csv(oni_avg,"output/oni_avg.csv",row.names = FALSE)


#### [2] process + plot tsunami data ####
# clean, make node list
tsunami_clean <- tsunami_og %>% select(-1) %>% slice(-1)
tsunami_nodes <- tsunami_clean %>% # grab relevant variables, convert country names to iso
  select(Country,Year,`Maximum Water Height (m)`,
         `Earthquake Magnitude`,Latitude,Longitude) %>%
  arrange(Country,Year) %>% 
  mutate(COUNTRY = countrycode::countrycode(Country, "country.name", "iso3c"),
         COUNTRY = ifelse(Country == "KERMADEC ISLANDS", "NZL", COUNTRY))

## plot n
p_tsunami <- tsunami_nodes %>%
  count(COUNTRY) %>%
  ggplot(aes(x = reorder(COUNTRY, n), y = n)) +
  geom_bar(stat = "identity") + coord_flip() +
  labs(title = "Tsunamis per Country 2000-2022",
       x = "Country",
       y = "n") + theme_minimal()
plot(p_tsunami)

# plot average intensity
average_water_height <- tsunami_nodes %>%
  group_by(COUNTRY) %>%
  summarise(average_max_water_height = mean(`Maximum Water Height (m)`, na.rm = TRUE))

# Create a bar chart of the average maximum water height per country
p_water_height <- ggplot(average_water_height, aes(x = reorder(COUNTRY, average_max_water_height), y = average_max_water_height)) +
  geom_bar(stat = "identity") + coord_flip() +
  labs(title = "Tsunami Height 2000-2022",
       x = "Country",
       y = "Average Maximum Water Height (m)") + theme_minimal()
plot(p_water_height)

# write.csv(tsunami_nodes,"output/tsunami_nodes.csv",row.names = FALSE)




#### [3] process + plot SDG data ####
## process
nut_net <- read_csv("output/nut_pict_hs160414.csv")
country_list <- unique(c(nut_net$importer_iso3c,nut_net$exporter_iso3c))

# sdg 2
sdg2.1 <- sdg2_og %>%
  filter(SeriesDescription == "Prevalence of undernourishment (%)" & TimePeriod >= 2000 & TimePeriod <= 2020) %>%
  select(Goal, Target, Indicator, SeriesDescription, GeoAreaName, TimePeriod, Value) %>%
  mutate(COUNTRY = countrycode::countrycode(GeoAreaName, "country.name", "iso3c"),
    Value = as.numeric(Value)) %>%
  filter(COUNTRY %in% country_list) %>%
  filter(!is.na(Value)) %>%
  mutate(Value = ifelse(Value < 1, 1, ifelse(Value < 2.5, 2.5, Value)),
         Unit="Percentage",SeriesDescription="Prevalence of undernourishment (percentage)") %>%
  arrange(COUNTRY, TimePeriod) 


# sdg 3
sdg3.1 <- sdg3_og %>% 
  filter(SeriesDescription == "Mortality rate attributed to cardiovascular disease, cancer, diabetes or chronic respiratory disease (probability)", 
           TimePeriod >= 2000 & TimePeriod <= 2020) %>%
  select(Goal, Target, Indicator, SeriesDescription, GeoAreaName, TimePeriod, Value) %>%
  mutate(COUNTRY = countrycode::countrycode(GeoAreaName, "country.name", "iso3c"),
    Value = as.numeric(Value)) %>%
  filter(COUNTRY %in% country_list) %>%
  filter(!is.na(Value)) %>%
  mutate(Value = ifelse(Value < 1, 1, ifelse(Value < 2.5, 2.5, Value))) %>%
  arrange(COUNTRY, TimePeriod)  %>% # Ensure the data is ordered by COUNTRY and TimePeriod
  group_by(COUNTRY,TimePeriod) %>%
  mutate(Value = mean(Value),
         Unit="Probability",
         SeriesDescription="Non-communicable disease mortality (probability)") %>% distinct()
  

# combine all SDGs
sdg_full <- bind_rows(sdg2.1,sdg3.1)
  
  

## plot
p_sdg <- ggplot(sdg_full,aes(TimePeriod,Value,group=COUNTRY,color=COUNTRY)) +
  geom_path() + theme_minimal()+
  labs(title="Sustainable Development Goal Metrics: Food Security and Health",x="Year",y=NULL)+
  scale_x_continuous(limits=c(2000,2020),breaks=seq(2000,2020,2),expand=c(0,.1))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~SeriesDescription,scales = "free_y")
plot(p_sdg)
# ggsave("output/sdg_plots.jpg")






