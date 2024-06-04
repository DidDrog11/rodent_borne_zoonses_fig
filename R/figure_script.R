if (!require("pacman")) install.packages("pacman")

pkgs =
  c("countrycode",
    "cowplot",
    "ggnewscale",
    "ggrepel",
    "ggridges",
    "here",
    "hrbrthemes",
    "readxl",
    "svglite",
    "tidyverse"
  )

pacman::p_load(pkgs, character.only = T)


# Prepare data ---------------------------------------------------------------

disease_timelines <- read_xlsx(here("data", "timelines_data_v4.xlsx")) %>%
  filter(str_detect(category, "Oldest infection|Transmission|Pathogen"))

yersinia_publications <- read_csv(here("data", "plague_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Plague")
yersinia_rodent_publications <- read_csv(here("data", "plague_rodent_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Plague") %>%
  rename("rodent_count" = Count)
yersinia <- full_join(yersinia_publications, yersinia_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

leptospirosis_publications <- read_csv(here("data", "leptospirosis_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Leptospirosis")
leptospirosis_rodent_publications <- read_csv(here("data", "leptospirosis_rodent_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Leptospirosis") %>%
  rename("rodent_count" = Count)
leptospirosis <- full_join(leptospirosis_publications, leptospirosis_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

toxoplasmosis_publications <- read_csv(here("data", "toxoplasmosis_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Toxoplasmosis")
toxoplasmosis_rodent_publications <- read_csv(here("data", "toxoplasmosis_rodent_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Toxoplasmosis") %>%
  rename("rodent_count" = Count)
toxoplasmosis <- full_join(toxoplasmosis_publications, toxoplasmosis_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

lyme_publications <- read_csv(here("data", "lyme_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lyme borreliosis")
lyme_rodent_publications <- read_csv(here("data", "lyme_rodent_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lyme borreliosis") %>%
  rename("rodent_count" = Count)
lyme <- full_join(lyme_publications, lyme_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

lassa_publications <- read_csv(here("data", "lassa_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lassa fever")
lassa_rodent_publications <- read_csv(here("data", "lassa_rodent_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lassa fever") %>%
  rename("rodent_count" = Count)
lassa <- full_join(lassa_publications, lassa_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

schistosomiasis_publications <- read_csv(here("data", "schistosomiasis_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Schistosomiasis")
schistosomiasis_rodent_publications <- read_csv(here("data", "schistosomiasis_rodent_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Schistosomiasis") %>%
  rename("rodent_count" = Count)
schistosomiasis <- full_join(schistosomiasis_publications, schistosomiasis_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

hantavirus_publications <- read_csv(here("data", "hantavirus_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Haemorrhagic Fever with Renal Syndrome/Hantavirus Pulmonary Syndrome")
hantavirus_rodent_publications <- read_csv(here("data", "hantavirus_rodent_pubmed_2024-01-18.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Haemorrhagic Fever with Renal Syndrome/Hantavirus Pulmonary Syndrome") %>%
  rename("rodent_count" = Count)
hantavirus <- full_join(hantavirus_publications, hantavirus_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

combined_data <- bind_rows(yersinia,
          leptospirosis,
          toxoplasmosis,
          lyme,
          lassa,
          schistosomiasis,
          hantavirus) %>%
  full_join(disease_timelines, by = c("Year", "disease")) %>%
  group_by(disease) %>%
  fill(region, .direction = "downup") %>%
  mutate(region = factor(region, levels = c("Global", "Northern Hemisphere", "Tropical/Sub-tropical", "West Africa")),
         disease = factor(disease, levels = c("Plague", "Leptospirosis", "Toxoplasmosis", "Lyme borreliosis", "Haemorrhagic Fever with Renal Syndrome/Hantavirus Pulmonary Syndrome", "Schistosomiasis", "Lassa fever")),
         Count = replace_na(Count, 0)) 

split_data <- combined_data %>%
  group_by(disease) %>%
  group_split()

names(split_data) <- lapply(split_data, function(x) { paste(unique(x$disease)) })


# Version 1 ---------------------------------------------------------------
# 
# timelines <- lapply(split_data, function(x) {
#   
#   # Expand years and set pub counts as 0
#   first_infection <- min(x %>%
#                            pull(Year))
#   
#   expand_years <- tibble(Year = rep(seq(first_infection, max(combined_data$Year) - 1, 1), each = 2),
#                          search = rep(c("Count", "rodent_count"), times = length(seq(first_infection, max(combined_data$Year) - 1, 1)))) %>%
#     full_join(x, by = c("Year", "search")) %>%
#     fill(disease, .direction = "downup") %>%
#     fill(region, .direction = "downup") %>%
#     mutate(Count = replace_na(Count, 0))
#   
#   # Remove 2024 from the data
#   x = expand_years %>%
#     filter(Year != 2024)
#   
#   ggplot(data = x) +
#     geom_line(data = x %>% 
#                 distinct(Year, search, Count, disease, region) %>%
#                 group_by(Year, disease, region) %>%
#                 summarise(Count = sum(Count, na.rm = TRUE)) %>%
#                 ungroup() %>%
#                 filter(!Year %in% tail(unique(Year), 1)),
#               aes(x = Year, y = Count), linetype = "solid") +
#     geom_line(data = x %>% 
#                 distinct(Year, search, Count, disease, region) %>%
#                 group_by(Year, disease, region) %>%
#                 summarise(Count = sum(Count, na.rm = TRUE)) %>%
#                 ungroup() %>%
#                 filter(Year %in% tail(unique(Year), 2)),
#               aes(x = Year, y = Count), linetype = "dashed") +
#     geom_col(data =  x %>% 
#                distinct(Year, search, Count, disease, region),
#              aes(x = Year, y = Count, alpha = search, fill = region), position = position_stack()) +
#     scale_fill_viridis_d(limits = c("Global", "Northern Hemisphere", "Tropical/Sub-tropical", "West Africa")) +
#     guides(fill = "none") +
#     new_scale("fill") +
#     geom_point(data = x %>% 
#                  drop_na(label) %>%
#                  distinct() %>%
#                  group_by(Year, label, category) %>%
#                  summarise(Count = sum(Count)), aes(x = Year, y = Count, colour = category)) +
#     geom_label_repel(data = x %>% 
#                        drop_na(label) %>%
#                        distinct() %>%
#                        group_by(Year, label, category) %>%
#                        summarise(Count = sum(Count)), aes(x = Year, y = Count, fill = category, label = str_wrap(label, width = 18)), inherit.aes = FALSE,
#                      direction = "both",
#                      max.overlaps = 100,
#                      size = 3,
#                      ylim = c(80, 1400),
#                      force_pull = 0.1,
#                      nudge_y = 100) +
#     scale_fill_viridis_d(begin = 1, end = 0.4) +
#     scale_colour_viridis_d(begin = 1, end = 0.4) +
#     coord_cartesian(xlim = c(max(min(x$Year)-10, 1850), 2024)) +
#     theme_minimal() +
#     guides(colour = "none") +
#     labs(x = element_blank(),
#          y = "Number of publications",
#          fill = "Category",
#          title = paste(unique(x$disease))) +
#     theme(legend.position = "none")
#   
# })
# 
# #Plague
# save_plot(plot = timelines[[1]], filename = here("output", "plague.png"), base_height = 6, base_width = 10)
# save_plot(plot = timelines[[2]], filename = here("output", "leptospirosis.png"), base_height = 6, base_width = 10)
# save_plot(plot = timelines[[3]], filename = here("output", "toxoplasmosis.png"), base_height = 6, base_width = 10)
# save_plot(plot = timelines[[4]], filename = here("output", "lyme.png"), base_height = 6, base_width = 10)
# save_plot(plot = timelines[[5]], filename = here("output", "hantavirus.png"), base_height = 6, base_width = 10)
# save_plot(plot = timelines[[6]], filename = here("output", "schistosomiasis.png"), base_height = 6, base_width = 10)
# save_plot(plot = timelines[[7]], filename = here("output", "lassa.png"), base_height = 6, base_width = 10)



# Version 2 ---------------------------------------------------------------

timelines <- lapply(split_data, function(x) {
  
  # Expand years and set pub counts as 0
  first_infection <- min(x %>%
                           pull(Year))
  
  expand_years <- tibble(Year = rep(seq(first_infection, max(combined_data$Year) - 1, 1), each = 2),
                         search = rep(c("Count", "rodent_count"), times = length(seq(first_infection, max(combined_data$Year) - 1, 1)))) %>%
    full_join(x, by = c("Year", "search")) %>%
    fill(disease, .direction = "downup") %>%
    fill(region, .direction = "downup") %>%
    mutate(Count = replace_na(Count, 0))
})

hanta <- timelines$`Haemorrhagic Fever with Renal Syndrome/Hantavirus Pulmonary Syndrome`

lassa <- timelines$`Lassa fever`

lepto <- timelines$Leptospirosis

plague <- timelines$Plague

v2 <- bind_rows(hanta %>%
            mutate(disease = "HFRS/HPS"), lassa, lepto, plague) %>%
  filter(Year <= 2023) %>%
  mutate(search = case_when(search == "Count" ~ "All publications",
                            search == "rodent_count" ~ "Rodent publications")) %>%
  ggplot(aes(x = Year, fill = search, y = disease)) +
  geom_density_ridges(data = . %>%
                        drop_na(search), stat = "identity", scale = 1, aes(height = Count),
                      panel_scaling = FALSE) +
  geom_text_repel(data = . %>% 
                     drop_na(label) %>%
                     distinct(Year, disease, category, label), aes(x = Year, y = disease, label = str_wrap(label, width = 18)),
                   size = 2,
                   nudge_y = 0.2,
                   inherit.aes = FALSE,
                   direction = "both") +
  labs(y = element_blank(),
       fill = "Search") +
  theme_minimal() +
  theme(axis.line.x = element_line()) +
  coord_cartesian(ylim = c(1.4, 4.5),
                  xlim = c(1880, 2024))

ggsave(v2, filename = here("output", "v2.png"), width = 12)
