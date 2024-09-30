#### goal: merge together trade + nutrient data ####

#### [0] prep ####
library(tidyverse)

# load data
artis_og <- read_csv("data/artis_request_nomura.csv") # trade flows
afcd_og <- read_csv("data/nutrient_updated.csv") # nutrient contents per 100g; new data from Jessica Aug 1
# afcd_og <- read_csv("data/nutrient_metadata.csv") # 

#### [1] join data ####
## join trade and nutrient data
join_df <- left_join(artis_og,afcd_og)

# clean? 
join_df <- join_df %>% filter(dom_source!="error", 
                              method=="capture",
                              habitat=="marine")

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
  mutate(protein_total_g = protein_g_pert * product_weight_t,
         fattyacids_total_g = fattyacids_g_pert * product_weight_t,
         calcium_total_mg = calcium_mg_pert * product_weight_t,
         zinc_total_mg = zinc_mg_pert * product_weight_t,
         iron_total_mg = iron_mg_pert * product_weight_t,
         vitamina_total_mcg = vitamina_mcg_pert * product_weight_t,
         vitaminb12_total_mcg = vitaminb12_mcg_pert * product_weight_t) 


## convert nutrient data to nutrients per tonne - AGGREGATE
pertonne_allyears_df <- join_df %>% # sum total volumes across years
  group_by(importer_iso3c,exporter_iso3c,sciname) %>%
  mutate(summed_weight_t = sum(product_weight_t)) %>% ungroup() %>%
  select(-c(product_weight_t,live_weight_t,year)) %>% distinct() %>%
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


#### save output ####
# write.csv(converted_annual_df,"output/nutrient_flows_annual_df.csv",row.names = FALSE)
# write.csv(converted_allyears_df,"output/nutrient_flows_allyears_df.csv",row.names=FALSE)

