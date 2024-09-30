#### [0] prep ####
library(tidyverse)
library(countrycode)
library(rfishbase)
library(ggalluvial)

#### [1] pull consumption data ####
# define the path to your data folder and the file pattern to match
data_path <- "data/consumption/"
file_pattern <- "consumption_midpoint_HS07_\\d{4}\\.csv"  

# list all files in the folder that match the pattern for all years
file_list <- list.files(path = data_path, pattern = file_pattern, full.names = TRUE)

# define the PICTs to filter for
picts <- c("FJI", "PNG", "SLB", "VUT", "FSM", "PLW", "MHL", "KIR", "NRU", "WSM", "TON", "TUV")

# define a function to read and filter each file
load_and_filter <- function(file) {
  read_csv(file) %>%
    filter(
      consumer_iso3c %in% picts |
        exporter_iso3c %in% picts |
        source_country_iso3c %in% picts,
      habitat == "marine",
      method == "capture"
    ) %>%
    mutate(
      source_country_short = if_else(
        str_length(countrycode(source_country_iso3c, "iso3c", "country.name")) > 6, 
        source_country_iso3c, 
        countrycode(source_country_iso3c, "iso3c", "country.name")
      ),
      exporter_country_short = if_else(
        str_length(countrycode(exporter_iso3c, "iso3c", "country.name")) > 6, 
        exporter_iso3c, 
        countrycode(exporter_iso3c, "iso3c", "country.name")
      ),
      consumer_country_short = if_else(
        str_length(countrycode(consumer_iso3c, "iso3c", "country.name")) > 6, 
        consumer_iso3c, 
        countrycode(consumer_iso3c, "iso3c", "country.name")
      )
    )
}

# combine into a single dataframe
combined_data <- bind_rows(lapply(file_list, load_and_filter))

## save it - annual
# write.csv(combined_data,paste0("output/consumption_annual_",unique(combined_data$hs_version), ".csv"),row.names = FALSE)

## save it - years aggregated
consumption_allyears <- group_by(combined_data,
                           source_country_iso3c,exporter_iso3c,consumer_iso3c) %>%
  summarise(total_weight = sum(consumption_live_t))
# write.csv(consumption_allyears,paste0("output/consumption_allyears_",unique(combined_data$hs_version),".csv"),row.names = FALSE)




#### [2] merge w/ nutrients ####
## load consumption data from above
combined_data <- read_csv("output/consumption_annual_HS07.csv")
## load nutrient data
afcd_og <- read_csv("data/nutrient_updated.csv") # nutrient contents per 100g; new data from Jessica Aug 1

## join
join_df <- left_join(combined_data,afcd_og)

## convert nutrient data to nutrients per tonne - ANNUAL
tonne_to_100g <- 1e4 # 100 grams to tonne

pertonne_annual_df <- join_df %>% # nutrient per tonne
  mutate(protein_g_pert = protein_g * tonne_to_100g, 
         fattyacids_g_pert = fattyacids_g * tonne_to_100g,
         calcium_mg_pert = calcium_mg * tonne_to_100g,
         zinc_mg_pert = zinc_mg * tonne_to_100g,
         iron_mg_pert = iron_mg * tonne_to_100g,
         vitamina_mcg_pert = vitamina_mcg * tonne_to_100g,
         vitaminb12_mcg_pert = vitaminb12_mcg * tonne_to_100g)

converted_annual_df <- pertonne_annual_df %>% # total nutrients
  mutate(protein_total_g = protein_g_pert * consumption_live_t,
         fattyacids_total_g = fattyacids_g_pert * consumption_live_t,
         calcium_total_mg = calcium_mg_pert * consumption_live_t,
         zinc_total_mg = zinc_mg_pert * consumption_live_t,
         iron_total_mg = iron_mg_pert * consumption_live_t,
         vitamina_total_mcg = vitamina_mcg_pert * consumption_live_t,
         vitaminb12_total_mcg = vitaminb12_mcg_pert * consumption_live_t) 


## convert nutrient data to nutrients per tonne - AGGREGATE
pertonne_allyears_df <- join_df %>% # sum total volumes across years
  group_by(source_country_iso3c,exporter_iso3c,consumer_iso3c,sciname) %>%
  mutate(summed_weight_t = sum(consumption_live_t)) %>% ungroup() %>%
  select(-c(consumption_live_t,year)) %>% distinct() %>%
  mutate(protein_g_pert = protein_g * tonne_to_100g, # convert for tonnes
         fattyacids_g_pert = fattyacids_g * tonne_to_100g,
         calcium_mg_pert = calcium_mg * tonne_to_100g,
         zinc_mg_pert = zinc_mg * tonne_to_100g,
         iron_mg_pert = iron_mg * tonne_to_100g,
         vitamina_mcg_pert = vitamina_mcg * tonne_to_100g,
         vitaminb12_mcg_pert = vitaminb12_mcg * tonne_to_100g)

converted_allyears_df <- pertonne_allyears_df %>% # convert to total nutrients 
  mutate(protein_total_g = protein_g_pert * summed_weight_t,
         fattyacids_total_g = fattyacids_g_pert * summed_weight_t,
         calcium_total_mg = calcium_mg_pert * summed_weight_t,
         zinc_total_mg = zinc_mg_pert * summed_weight_t,
         iron_total_mg = iron_mg_pert * summed_weight_t,
         vitamina_total_mcg = vitamina_mcg_pert * summed_weight_t,
         vitaminb12_total_mcg = vitaminb12_mcg_pert * summed_weight_t) 



## save it - consumption w/ nutrient flows
# write.csv(converted_annual_df,paste0("output/consumption_annual_",unique(combined_data$hs_version), "_nutrients.csv"),row.names = FALSE)
# write.csv(converted_allyears_df,paste0("output/consumption_allyears_",unique(combined_data$hs_version),"_nutrients.csv"),row.names = FALSE)


#### [3] 'splore ####
consumption_og <- read_csv("output/consumption_annual_HS07.csv")
nutrients_cons_og <- read_csv("output/consumption_allyears_HS07_nutrients.csv")

colSums(is.na(nutrients_cons_og)) # check where NA's are at
# why does the exporter iso3c have so many NA's?

table(nutrients_cons_og$consumption_source)
table(nutrients_cons_og$dom_source)
# Q: waht is dom source vs. consumption source?

## seems like - when consumption source = domestic, source = consumer; aka no exporting & local consumption
## and when - when dom source = domestic, source = exporter; aka no local production & foreign consumption (also consumption source = foreign, so confirmed)


#### [4] plots ####
## what are the common names of species?
scientific_names <- sort(unique(nutrients_cons_og$sciname))
scientific_names <- str_to_sentence(scientific_names)
species_info <- species(scientific_names,fields = c("SpecCode", "Genus", "Species", "FBname")) # use fishbase for common names

## what is the most common species?
# summarize and count the occurrences/tonnes of each species in sciname
species_counts <- nutrients_cons_og %>%
  group_by(sciname) %>%
  summarize(count = n(), consumption_live_t=sum(consumption_live_t),
            .groups = 'drop') %>%
  arrange(desc(count)) 

# prepare species_info with lowercase sciname for joining
species_info <- species(str_to_sentence(unique(nutrients_cons_og$sciname)), fields = c("Genus", "Species", "FBname")) %>%
  mutate(sciname = str_to_lower(paste(Species))) %>%  # create lowercase sciname
  select(sciname, CommonName = FBname)  # select relevant columns

# join species counts with common names
species_combined <- species_counts %>%
  mutate(sciname = str_to_lower(sciname)) %>%  # ensure sciname is lowercase
  left_join(species_info, by = "sciname") %>%
  slice_max(order_by = count, n = 20) %>%
  mutate(label_name = ifelse(!is.na(CommonName), paste(str_to_sentence(sciname), " / ", CommonName), str_to_sentence(sciname)))

# plot the bar chart for the top species - counts
ggplot(species_combined, aes(x = reorder(label_name, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # flip coordinates for better readability
  labs(title = "Most Traded Species (instances)",
       x = "Species",
       y = "Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust=0.5))
# ggsave("output/species_counts.jpg")

# plot the bar chart for the top species - tons
ggplot(species_combined, aes(x = reorder(label_name, consumption_live_t), y = consumption_live_t)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # flip coordinates for better readability
  labs(title = "Most Traded Species (volume)",
       x = "Species",
       y = "Tonnes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust=0.5))
# ggsave("output/species_weight.jpg")

## what roles are countries playing?
# pivot longer to get roles in one column
country_role_counts <- nutrients_cons_og %>%
  pivot_longer(cols = c(source_country_short, exporter_country_short, consumer_country_short),
               names_to = "role", values_to = "country") %>%
  filter(!is.na(country)) %>%
  group_by(country, role) %>%
  summarize(count = n(), .groups = 'drop')

# get the top 20 countries for each role
top_countries <- country_role_counts %>%
  group_by(role) %>%
  slice_max(order_by = count, n = 30)  %>%
  mutate(role = recode(role,
                       "source_country_iso3c" = "Source","source_country_short" = "Source",
                       "exporter_iso3c" = "Exporter","exporter_country_short" = "Exporter",
                       "consumer_iso3c" = "Consumer","consumer_country_short" = "Consumer"),
         role = factor(role, levels = c("Source", "Exporter", "Consumer")))

# plot the facet-wrapped bar plot
ggplot(top_countries, aes(x = tidytext::reorder_within(country, count, role), y = count, fill = role)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  coord_flip() +  # flip coordinates for better readability
  facet_wrap(~ role, scales = "free") +  # facet by role
  tidytext::scale_x_reordered() +  # adjust scale_x to reorder within each facet
  labs(title = "Country Roles",
       x = "Country",
       y = "Counts",
       fill = "Role") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title = element_text(hjust=0.5))
# ggsave("output/country_roles.jpg")



## sankey plots
# Add PICS/non-PICS IDs; NA exporter for domestic consumption
net_class <- nutrients_cons_og %>% 
  mutate(
    SourceClass = ifelse(source_country_iso3c %in% picts, "PICs", "Non-\nPICs"),
    ExporterClass = ifelse(is.na(exporter_iso3c),"Domestic",ifelse(exporter_iso3c %in% picts, "PICs", "Non-\nPICs")),
    ConsumerClass = ifelse(consumer_iso3c %in% picts, "PICs", "Non-\nPICs")
  )



# List of nutrient columns
nutrients <- c("protein_total_g","fattyacids_total_g","calcium_total_mg",
               "zinc_total_mg","iron_total_mg","vitamina_total_mcg","vitaminb12_total_mcg")

# Aggregate data across all years for each nutrient
net_agg <- net_class %>%
  # filter(!(SourceClass == "PICs" & ExporterClass == "PICs" & ConsumerClass == "PICs")) %>%
  group_by(SourceClass, ExporterClass, ConsumerClass) %>%
  summarise(across(all_of(nutrients), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = all_of(nutrients), names_to = "Nutrient", values_to = "Value") %>%
  ungroup() %>%
  mutate(Nutrient = str_replace(Nutrient, "_total", "")) %>%
  mutate(Nutrient = str_to_title(Nutrient)) %>%
  separate_wider_delim(Nutrient,delim="_",names=c("Nutrient","Unit")) 

alluvial_data <- net_agg %>%
  mutate(Flow = paste(SourceClass, "to", ExporterClass, "to", ConsumerClass)) %>%
  group_by(Flow, Nutrient) %>%
  summarise(Value = sum(Value)) %>%
  separate(Flow, into = c("SourceClass", "ExporterClass","ConsumerClass"), sep = " to ") %>%
  ungroup() %>%  group_by(SourceClass,ExporterClass,ConsumerClass)%>%
  mutate(Triad_proportion = Value / sum(Value)) %>%
  mutate(Nutrient = factor(Nutrient, levels = unique(Nutrient)),
         SourceClass = factor(SourceClass, levels = unique(SourceClass)),
         ExporterClass = factor(ExporterClass, levels = unique(ExporterClass)),
         ConsumerClass = factor(ConsumerClass, levels = unique(ConsumerClass))) %>%
  droplevels() %>% ungroup() %>%
  mutate(Total_proportion = Value / sum(Value))
  

# Create labels for proportions
alluvial_data <- alluvial_data %>%
  mutate(Label_triad_prop = paste0(round(Triad_proportion * 100, 1), "%"),
         Label_total_prop = paste0(round(Total_proportion * 100, 1), "%"))

exporter_labels <- alluvial_data %>%
  group_by(ExporterClass) %>%
  mutate(mid_y = cumsum(Value) + 1 * Value)


## Plot triple sankey
# library(RColorBrewer)
# colors <- brewer.pal(6,"Set2") # establish color theme

ggplot(alluvial_data,
       aes(axis1 = SourceClass, axis2 = ExporterClass, axis3= ConsumerClass, y = Value, fill = Nutrient)) +
  geom_alluvium(width = 1/12, knot.pos = 0.5) +
  geom_stratum(aes(color = ExporterClass), fill="grey95",width = 1/12, show.legend = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, vjust = -0.5) +
  # geom_text(aes(label = Label_triad_prop,x=1.2), size = 5, color = "black", position = position_stack(vjust = .3),check_overlap = TRUE) +
  scale_x_discrete(limits = c("SourceClass", "ExporterClass","ConsumerClass"), expand = c(0.1, 0.1),
                   labels = c("SourceClass" = "Source","ExporterClass" = "Exporter","ConsumerClass"="Consumer")) +
  labs(title = "Nutrient Flows Between PICs and Non-PICs",x=NULL,
       y = NULL, fill = "Nutrient") + 
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size=17)) +
  guides(fill = guide_legend(title = "Nutrient"),color="none")

# ggsave("output/sankey_nutrients_consumption.jpg")
#



## barplot (?) of flows











