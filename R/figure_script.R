if (!require("pacman")) install.packages("pacman")

pkgs =
  c("countrycode",
    "cowplot",
    "ggnewscale",
    "ggrepel",
    "here",
    "readxl",
    "svglite",
    "tidyverse"
  )

pacman::p_load(pkgs, character.only = T)

disease_timelines <- read_xlsx(here("data", "timelines_data_v3.xlsx"))

yersinia_publications <- read_csv(here("data", "yersinia_pestis_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Plague")
yersinia_rodent_publications <- read_csv(here("data", "yersinia_pestis_and_rodent_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Plague") %>%
  rename("rodent_count" = Count)
yersinia <- full_join(yersinia_publications, yersinia_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

leptospirosis_publications <- read_csv(here("data", "leptospirosis_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Leptospirosis")
leptospirosis_rodent_publications <- read_csv(here("data", "leptospirosis_and_rodent_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Leptospirosis") %>%
  rename("rodent_count" = Count)
leptospirosis <- full_join(leptospirosis_publications, leptospirosis_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

toxoplasmosis_publications <- read_csv(here("data", "toxoplasmosis_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Toxoplasmosis")
toxoplasmosis_rodent_publications <- read_csv(here("data", "toxoplasmosis_and_rodent_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Toxoplasmosis") %>%
  rename("rodent_count" = Count)
toxoplasmosis <- full_join(toxoplasmosis_publications, toxoplasmosis_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

lyme_publications <- read_csv(here("data", "lyme_disease_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lyme borreliosis")
lyme_rodent_publications <- read_csv(here("data", "lyme_disease_and_rodent_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lyme borreliosis") %>%
  rename("rodent_count" = Count)
lyme <- full_join(lyme_publications, lyme_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

lassa_publications <- read_csv(here("data", "lassa_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lassa fever")
lassa_rodent_publications <- read_csv(here("data", "lassa_and_rodent_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lassa fever") %>%
  rename("rodent_count" = Count)
lassa <- full_join(lassa_publications, lassa_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

schistosomiasis_publications <- read_csv(here("data", "schistosomiasis_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Schistosomiasis")
schistosomiasis_rodent_publications <- read_csv(here("data", "schistosomiasis_and_rodent_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Schistosomiasis") %>%
  rename("rodent_count" = Count)
schistosomiasis <- full_join(schistosomiasis_publications, schistosomiasis_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

sin_nombre_publications <- read_csv(here("data", "sin_nombre_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Hantavirus Pulmonary Syndrome")
sin_nombre_publications <- read_csv(here("data", "sin_nombre_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Hantavirus Pulmonary Syndrome")
sin_nombre_rodent_publications <- read_csv(here("data", "sin_nombre_and_rodent_pubmed_citations.csv"), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Hantavirus Pulmonary Syndrome") %>%
  rename("rodent_count" = Count)
sin_nombre <- full_join(sin_nombre_publications, sin_nombre_rodent_publications, by = c("Year", "disease")) %>%
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
          sin_nombre) %>%
  full_join(disease_timelines, by = c("Year", "disease")) %>%
  group_by(disease) %>%
  fill(region, .direction = "downup") %>%
  mutate(region = factor(region, levels = c("Global", "Northern Hemisphere", "United States of America", "Tropical/Sub-tropical", "West Africa")),
         disease = factor(disease, levels = c("Plague", "Leptospirosis", "Toxoplasmosis", "Lyme borreliosis", "Hantavirus Pulmonary Syndrome", "Schistosomiasis", "Lassa fever")),
         Count = replace_na(Count, 0))

split_data <- combined_data %>%
  group_by(disease) %>%
  group_split()

names(split_data) <- lapply(split_data, function(x) { paste(unique(x$disease)) })

timelines <- lapply(split_data, function(x) {
  
  ggplot(data = x) +
    geom_line(data = x %>% 
                distinct(Year, search, Count, disease, region) %>%
                group_by(Year, disease, region) %>%
                summarise(Count = sum(Count, na.rm = TRUE)),
              aes(x = Year, y = Count)) +
    geom_col(data =  x %>% 
               distinct(Year, search, Count, disease, region),
             aes(x = Year, y = Count, alpha = search, fill = region), position = position_stack()) +
    scale_fill_viridis_d(limits = c("Global", "Northern Hemisphere", "United States of America", "Tropical/Sub-tropical", "West Africa")) +
    guides(fill = "none") +
    new_scale("fill") +
    geom_point(data = x %>% 
                 drop_na(label) %>%
                 distinct() %>%
                 group_by(Year, label, category) %>%
                 summarise(Count = sum(Count)), aes(x = Year, y = Count, colour = category)) +
    geom_label_repel(data = x %>% 
                       drop_na(label) %>%
                       distinct() %>%
                       group_by(Year, label, category) %>%
                       summarise(Count = sum(Count)), aes(x = Year, y = Count, label = label, fill = category), inherit.aes = FALSE,
                     direction = "y",
                     max.overlaps = 100,
                     size = 2,
                     ylim = c(200, 1400),
                     force_pull = 0.1,
                     nudge_y = 100) +
    scale_fill_viridis_d(alpha = 0.5) +
    scale_colour_viridis_d() +
    coord_cartesian(xlim = c(1850, 2023), ylim = c(0, 1400)) +
    theme_minimal() +
    guides(colour = "none") +
    labs(x = element_blank(),
         y = element_blank(),
         fill = "Category",
         title = paste(unique(x$disease))) +
    theme(legend.position = "none")
  
})

single_plot <- plot_grid(plotlist = timelines, ncol = 2)

save_plot(plot = single_plot, filename = here("output", "test_plot.svg"), base_height = 14, base_width = 18)
save_plot(plot = single_plot, filename = here("output", "test_plot.pdf"), base_height = 14, base_width = 18)
save_plot(plot = single_plot, filename = here("output", "test_plot.png"), base_height = 14, base_width = 18)
          