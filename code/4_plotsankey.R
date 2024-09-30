#### goal: make a sankey diagram with nutrient categories flowing pics <-> non-pics ####

#### [0] prep ####
library(tidyverse)
# library(networkD3)
library(ggalluvial)


nut_net_og <- read_csv("output/nut_pict_hs160414.csv")




#### [1] process - ggalluvial version #####
picts <- c("FJI", "PNG", "SLB", "VUT", "FSM", "PLW", "MHL", "KIR", "NRU", "WSM", "TON", "TUV")

# Add PICS/non-PICS IDs
net_class <- nut_net_og %>% 
  mutate(
    ImporterClass = ifelse(importer_iso3c %in% picts, "PICs", "Non-\nPICs"),
    ExporterClass = ifelse(exporter_iso3c %in% picts, "PICs", "Non-\nPICs")
  )


# List of nutrient columns
nutrients <- c("protein_sum", "zinc_sum", "fattyacids_sum",
               "iron_sum", "vita_sum","vitb12_sum","calcium_sum")

# Aggregate data across all years for each nutrient
net_agg <- net_class %>%
  filter(!(ExporterClass == "PICs" & ImporterClass == "PICs")) %>%
  group_by(ExporterClass, ImporterClass) %>%
  summarise(across(all_of(nutrients), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = all_of(nutrients), names_to = "Nutrient", values_to = "Value") %>%
  ungroup() %>%
  mutate(Nutrient = str_replace(Nutrient, "_sum$", "")) %>%
  mutate(Nutrient = str_to_title(Nutrient))

alluvial_data <- net_agg %>%
  mutate(Flow = paste(ExporterClass, "to", ImporterClass)) %>%
  group_by(Flow, Nutrient) %>%
  summarise(Value = sum(Value)) %>%
  separate(Flow, into = c("ExporterClass", "ImporterClass"), sep = " to ") %>%
  ungroup() %>%  group_by(ExporterClass,ImporterClass)%>%
  mutate(Proportion = Value / sum(Value)) %>%
  mutate(Nutrient = factor(Nutrient, levels = unique(Nutrient)),
         ExporterClass = factor(ExporterClass, levels = unique(ExporterClass)),
         ImporterClass = factor(ImporterClass, levels = unique(ImporterClass))) %>%
  droplevels()

# Create labels for proportions
alluvial_data <- alluvial_data %>%
  mutate(Label = paste0(round(Proportion * 100, 1), "%"))

exporter_labels <- alluvial_data %>%
  group_by(ExporterClass) %>%
  mutate(mid_y = cumsum(Value) + 1 * Value)


# Create the plot
ggplot(alluvial_data,
       aes(axis1 = ExporterClass, axis2 = ImporterClass, y = Value, fill = Nutrient)) +
  geom_alluvium(width = 1/12, knot.pos = 0.5) +
  geom_stratum(aes(color = ExporterClass), fill="grey95",width = 1/12, show.legend = FALSE) +
  # geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, vjust = -0.5) +
  geom_text(aes(label = Label,x=1.2), size = 5, color = "black", position = position_stack(vjust = .3),check_overlap = TRUE) +
  scale_x_discrete(limits = c("ExporterClass", "ImporterClass"), expand = c(0.1, 0.1),
                   labels = c("ExporterClass" = "Exporter", "ImporterClass" = "Importer")) +
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

# ggsave("output/sankey_nutrients.jpg")
#



