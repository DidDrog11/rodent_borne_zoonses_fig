if (!require("pacman")) install.packages("pacman")

pkgs =
  c("countrycode",
    "cowplot",
    "ggnewscale",
    "ggrepel",
    "ggridges",
    "here",
    "hrbrthemes",
    "patchwork",
    "readxl",
    "svglite",
    "tidyverse"
  )

pacman::p_load(pkgs, character.only = T)


# Prepare data ---------------------------------------------------------------

disease_timelines <- read_xlsx(here("data", "timelines_data_v6.xlsx")) %>%
  filter(str_detect(category, "Oldest infection|Host|Pathogen"))

search_date = "2024-09-03"

hantavirus_publications <- read_csv(here("data", paste0("hantavirus_pubmed_", search_date, ".csv")), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Haemorrhagic Fever with Renal Syndrome/Hantavirus Pulmonary Syndrome")
hantavirus_rodent_publications <- read_csv(here("data", paste0("hantavirus_rodent_pubmed_", search_date, ".csv")), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Haemorrhagic Fever with Renal Syndrome/Hantavirus Pulmonary Syndrome") %>%
  rename("rodent_count" = Count)
hantavirus <- full_join(hantavirus_publications, hantavirus_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

lassa_publications <- read_csv(here("data", paste0("lassa_pubmed_", search_date, ".csv")), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lassa fever")
lassa_rodent_publications <- read_csv(here("data", paste0("lassa_rodent_pubmed_", search_date, ".csv")), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lassa fever") %>%
  rename("rodent_count" = Count)
lassa <- full_join(lassa_publications, lassa_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

lyme_publications <- read_csv(here("data", paste0("lyme_pubmed_", search_date, ".csv")), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lyme borreliosis")
lyme_rodent_publications <- read_csv(here("data", paste0("lyme_rodent_pubmed_", search_date, ".csv")), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Lyme borreliosis") %>%
  rename("rodent_count" = Count)
lyme <- full_join(lyme_publications, lyme_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

mpox_publications <- read_csv(here("data", paste0("mpox_pubmed_", search_date, ".csv")), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "mpox")
mpox_rodent_publications <- read_csv(here("data", paste0("mpox_rodent_pubmed_", search_date, ".csv")), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "mpox") %>%
  rename("rodent_count" = Count)
mpox <- full_join(mpox_publications, mpox_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

yersinia_publications <- read_csv(here("data", paste0("plague_pubmed_", search_date, ".csv")), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Plague")
yersinia_rodent_publications <- read_csv(here("data", paste0("plague_rodent_pubmed_", search_date, ".csv")), show_col_types = FALSE, skip = 1) %>%
  mutate(disease = "Plague") %>%
  rename("rodent_count" = Count)
yersinia <- full_join(yersinia_publications, yersinia_rodent_publications, by = c("Year", "disease")) %>%
  mutate(rodent_count = replace_na(rodent_count, 0),
         Count = replace_na(Count, 0),
         Count = Count - rodent_count) %>%
  pivot_longer(cols = c("Count", "rodent_count"), names_to = "search", values_to = "Count")

combined_data <- bind_rows(hantavirus,
                           lassa,
                           lyme,
                           mpox,
                           yersinia) %>%
  full_join(disease_timelines %>%
              filter(disease %in% c("Haemorrhagic Fever with Renal Syndrome/Hantavirus Pulmonary Syndrome", "Lassa fever", "Lyme borreliosis", "mpox", "Plague")),
            by = c("Year", "disease")) %>%
  group_by(disease) %>%
  fill(region, .direction = "downup") %>%
  mutate(region = factor(region, levels = c("Global", "Northern Hemisphere", "Tropical/Sub-tropical", "West Africa")),
         disease = factor(disease, levels = c("Haemorrhagic Fever with Renal Syndrome/Hantavirus Pulmonary Syndrome", "Lassa fever", "Lyme borreliosis", "mpox", "Plague")),
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

lyme <- timelines$`Lyme borreliosis`

mpox <- timelines$mpox

plague <- timelines$Plague

v2 <- bind_rows(hanta %>%
            mutate(disease = "HFRS/HPS"), lassa, lyme, mpox, plague) %>%
  filter(Year <= 2024) %>%
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
  coord_cartesian(ylim = c(1.4, 5.5),
                  xlim = c(1880, 2025))

ggsave(v2, filename = here("output", "v2.png"), width = 12)


# Version 3 ---------------------------------------------------------------

# List of diseases to iterate over
diseases <- c("HFRS/HPS", "Lassa fever", "Lyme borreliosis", "mpox", "Plague")

# Combine data into one data frame
v3_data <- bind_rows(
  hanta %>% mutate(disease = "HFRS/HPS"), 
  lassa %>% mutate(disease = "Lassa fever"), 
  lyme %>% mutate(disease = "Lyme borreliosis"), 
  mpox %>% mutate(disease = "mpox"), 
  plague %>% mutate(disease = "Plague")
) %>%
  filter(Year <= 2024) %>%
  mutate(search = case_when(
    search == "Count" ~ "All publications",
    search == "rodent_count" ~ "Rodent publications"
  ))

# Create the function to generate the plot for each disease
generate_plot <- function(disease) {
  
  # Filter data for the current disease
  disease_data <- v3_data %>%
    filter(disease == !!disease)
  
  # Extract labels
  oldest_infection <- disease_data %>%
    filter(category == "Oldest infection")
  
  # Extract dates for pathogens and hosts
  line_dates <- disease_data %>% 
    filter(str_detect(category, "Pathogen|Host")) %>%
    distinct(Year, category, disease, .keep_all = TRUE)
  
  # Generate the plot for the current disease
  p <- ggplot(disease_data, aes(x = Year, fill = search, y = Count)) +
    geom_area(
      data = disease_data %>% drop_na(search),
      position = "identity",
      show.legend = TRUE  # Show only fill legend
    ) +
    geom_vline(
      data = line_dates, 
      aes(xintercept = Year, linetype = category), 
      lwd = 1
    )  +
    geom_text_repel(
      data = oldest_infection, 
      aes(x = Year, y = Count, label = str_wrap(label, width = 18)),
      size = 3,
      nudge_y = 0.2,
      inherit.aes = FALSE,
      direction = "both"
    ) +
    scale_y_log10() +
    labs(title = disease,
         y = "Number of publications (log scale)") +
    scale_linetype_manual(
      values = c("Pathogen" = "dashed", "Host" = "dotdash"), # Matching category names from line_dates
      labels = c("Pathogen" = "Pathogen discovered", "Host" = "Host(s) discovered"), # Custom labels for the legend
      guide = guide_legend(order = 1, override.aes = list(color = "black")) # Ensure correct line types
    ) +
    theme_minimal() +
    theme(
      axis.line.x = element_line(),
      legend.position = "none", # Remove individual legends for subplots
      legend.title = element_blank(),
      legend.background = element_rect(fill = "lightgrey", colour = "black")) +
    guides(linetype = guide_legend(override.aes = list(fill = c(NA, NA)))) +
    coord_cartesian(xlim = c(1880, 2025))
  
  return(p)
}

# Apply the function across all diseases and store the plots in a list
plot_list <- lapply(diseases, generate_plot)

# Combine all plots into a single figure with a shared legend
v3 <- wrap_plots(plot_list, ncol = 1) + 
  plot_layout(guides = "collect", axis_titles = "collect") & 
  theme(
    legend.position = "bottom", # Position the combined legend at the bottom
    legend.title = element_blank()
  )

ggsave(v3, filename = here("output", "v3.png"), height = 18, width = 16)
