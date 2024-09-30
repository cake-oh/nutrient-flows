#### goal: explore nutrient flows / ID major import species for PICTs ####
# previous: 1_merge_nutrients.R


#### [0] prep ####
library(tidyverse)
library(vegan) # for shannon's diversity index

nutrient_flows_allyears_og <- read_csv("output/nutrient_flows_allyears_df.csv")
nutrient_flows_annual_og <- read_csv("output/nutrient_flows_annual_df.csv")


#### [1] filter for PICTs and hs codes ####
picts <- c("FJI", "PNG", "SLB", "VUT", "FSM", "PLW", "MHL", "KIR", "NRU", "WSM", "TON", "TUV")
hs_codes <- 160414

# filter PICTs in importers or exporters (could also include source data) + hs codes
nut_pict_hs <- nutrient_flows_annual_og %>% 
  filter(importer_iso3c %in% picts |
           exporter_iso3c %in% picts) %>% 
  filter(hs6==hs_codes)

# aggregate - sum total for each nutrient
nut_pict_hs_sum <- nut_pict_hs %>% group_by(importer_iso3c,exporter_iso3c,year) %>%
  summarise(total_weight_t = sum(product_weight_t),
            protein_sum=sum(protein_total_g),
            fattyacids_sum=sum(fattyacids_total_g),
            calcium_sum=sum(calcium_total_mg),
            zinc_sum=sum(zinc_total_mg),
            iron_sum=sum(iron_total_mg),
            vita_sum=sum(vitamina_total_mcg),
            vitb12_sum=sum(vitaminb12_total_mcg))

#### [2] calculate diversity metrics by normalized dietary contributions ####
## get dietary recommendations
## values from nih https://ods.od.nih.gov/HealthInformation/nutrientrecommendations.aspx
## values for fatty acids averaged for male adults over 4 years old from nih https://ods.od.nih.gov/factsheets/Omega3FattyAcids-HealthProfessional/#h2
dietrec_df <- data_frame(category=c("protein","fattyacids","calcium",
                                    "zinc","iron","vita","vitb12"),
                         rec_intake_adults=c(50,1.4,1300,
                                             11,18,900,2.4),
                         unit=c("g","g","mg",
                                "mg","mg","mcg","mcg"))
# write.csv(dietrec_df,"output/dietrec_df.csv",row.names = FALSE)
  
## normalize and calculate diversity
# extract nutrient columns ending with "_sum"
nutrient_columns <- grep("_sum$", names(nut_pict_hs_sum), value = TRUE)

# create dictionary for quick lookup of recommended intake values
rec_intake_dict <- setNames(dietrec_df$rec_intake_adults, dietrec_df$category)

# normalize each nutrient column
nut_pict_hs_div <- nut_pict_hs_sum

for (col in nutrient_columns) {
  nutrient <- sub("_sum$", "", col)
  if (nutrient %in% names(rec_intake_dict)) {
    recommended_intake <- rec_intake_dict[[nutrient]]
    new_col_name <- paste0(nutrient, "_norm")
    nut_pict_hs_div[[new_col_name]] <- nut_pict_hs_div[[col]] / recommended_intake
  }
}

# calculate diversity
normalized_columns <- grep("_norm$", names(nut_pict_hs_div), value = TRUE)

nut_pict_hs_div$shannon_diversity <- apply(nut_pict_hs_div[normalized_columns], 1,
                                           function(x) diversity(x, index = "shannon"))
nut_pict_hs_div$norm_sum <- rowSums(nut_pict_hs_div[normalized_columns])


## aggregate year version - find overall top volumes
# nut_pict_all <- nutrient_flows_allyears_og %>% 
#   filter(importer_iso3c %in% picts |
#            exporter_iso3c %in% picts) 
# nut_pict_all_top <- nut_pict_all %>%
#   select(-c(importer_iso3c,exporter_iso3c,source_country_iso3c)) %>%
#   group_by(sciname) %>% #summarise(total_weight=sum(summed_weight_t)) %>%
#   arrange(desc(total_weight)) %>%
#   slice_head(n=10)

#### save output ####
# PICT names
# write.csv(picts,"output/pict_names.csv",row.names = FALSE) 
# top species
# write.csv(nut_pict_all_top,"output/top_sciname.csv",row.names = FALSE) 

# pruned annual data at hs level 160414
# write.csv(nut_pict_hs_div,"output/nut_pict_hs160414.csv",row.names = FALSE) 


