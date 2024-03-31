# Load libraries ------------------------------------------------------
library(tidyverse)
library(ggrepel)
library(rnaturalearth)

# Set global theme
theme_set(theme_classic(base_family = "SF Pro Light", base_size = 16))

theme_update(
  plot.title.position = "plot",
  plot.title = element_text(size = 24),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 16)
)

# triadic color scheme
colors <- c("#ffb7c5", "#b7c5ff", "#c5ffb7")

# Download & prepare data ---------------------------------------------
# Sakura dates by region
# https://www.kaggle.com/datasets/ryanglasnapp/japanese-cherry-blossom-data
sakura_first_bloom_dates <- read_csv("data/sakura_first_bloom_dates.csv")
sakura_full_bloom_dates <- read_csv("data/sakura_full_bloom_dates.csv")

sakura_first_bloom_dates_long <- sakura_first_bloom_dates |> 
  select(-c("30 Year Average 1981-2010", "Notes")) |> 
  rename(location = `Site Name`,
         is_currently_observed = `Currently Being Observed`) |> 
  pivot_longer(cols = -c(location, is_currently_observed),
               names_to = "year", values_to = "first_bloom_date")

sakura_full_bloom_dates_long <- sakura_full_bloom_dates |> 
  select(-c("30 Year Average 1981-2010", "Notes")) |> 
  rename(location = `Site Name`,
         is_currently_observed = `Currently Being Observed`) |> 
  pivot_longer(cols = -c(location, is_currently_observed),
               names_to = "year", values_to = "full_bloom_date")

sakura_dates <- sakura_first_bloom_dates_long |> 
  full_join(sakura_full_bloom_dates_long, join_by(location, year, is_currently_observed)) |> 
  mutate(year = as.integer(year),
         first_bloom_day = as.integer(first_bloom_date - as.Date(paste(year, "-01-01", sep = ""))),
         full_bloom_day = as.integer(full_bloom_date - as.Date(paste(year, "-01-01", sep = ""))),
         time_to_full_bloom = as.integer(full_bloom_date - first_bloom_date))

# Location longitudes and latitudes
# https://github.com/Yuriko-Schumacher/statistical-analysis-of-cherry-blossom-first-bloom-date/tree/main/data
# observatory_locations <- read_csv("https://raw.githubusercontent.com/Yuriko-Schumacher/statistical-analysis-of-cherry-blossom-first-bloom-date/main/data/observatory-locations.csv")
# write_csv(observatory_locations, "data/observatory_locations.csv")
observatory_locations <- read_csv("data/observatory_locations.csv")

southern_islands <- c("Naze", "Ishigaki Island", "Miyakojima", "Naha", "Minami Daito Island")

# Sort locations into regions (used ChatGPT for labeling)
# https://en.wikipedia.org/wiki/List_of_regions_of_Japan
locations_region <- read_csv("data/locations_region.csv") |> 
  mutate(region = if_else(location %in% southern_islands, "Ryukyu Islands", region)) |> 
  mutate(region = factor(region, levels = c("Hokkaidō", "Honshū", "Shikoku", "Kyūshū", "Ryukyu Islands")))

# japan map (use polygon instead of sf for simplicity)
japan_map <- map_data("world", region = "japan") |> 
  as_tibble()

# Combine main sakura data
sakura_data <- sakura_dates |> 
  left_join(observatory_locations, join_by(location)) |> 
  left_join(locations_region, join_by(location)) |> 
  mutate(is_southern_island = location %in% southern_islands) |> 
  filter(is_currently_observed == TRUE) |> 
  filter(year >= 1954) |> # to have 7 full decades
  mutate(
    second_half = year > 1988,
    sample = case_when(
      year >= 1954 & year <= 1964 ~ "From 1954 to 1963", 
      year >= 2013 & year <= 2024 ~ "From 2013 to 2024"),
    sample_tile = ntile(year, 7),
    first_bloom_group = case_when(
      month(first_bloom_date) == 1 ~ "January or earlier",
      month(first_bloom_date) == 2 ~ "February",
      month(first_bloom_date) == 3  & days(first_bloom_date) <= 15 ~ "First half of March",
      month(first_bloom_date) == 3  & days(first_bloom_date) > 15 ~ "Second half of March",
      month(first_bloom_date) >= 4 ~ "April or later"
    ),
    time_to_full_group = case_when(
      time_to_full_bloom < 7 ~ "Less than 1 week",
      time_to_full_bloom >= 7 & time_to_full_bloom < 14 ~ "More than 1 week, less than 2",
      time_to_full_bloom >= 14 ~ "More than 2 weeks"
    ),
    first_bloom_group = factor(first_bloom_group, levels = c("January or earlier", "First half of March", "Second half of March", "April or later"))
  )

# Time to full bloom in Kyoto ----------------------------------------
today <- tibble(year = 2024, full_bloom_day = as.integer(as.Date("2024-04-03")-as.Date("2024-01-01")))
fig_sakura_kyoto <- sakura_data |> 
  filter(location == "Kyoto") |> 
  ggplot(aes(x = year, y = full_bloom_day)) +
  geom_point(color = colors[1], aes(size = time_to_full_group)) +
  geom_smooth(color = colors[2], se = FALSE) +
  geom_point(data = today, color = colors[2], size = 5) + 
  geom_text_repel(data = today, nudge_x = 4, nudge_y = 8,
                  color = colors[2], label = "2024\n(predicted)", fontface = 2, size = 6
  ) + 
  labs(title = expression("Day of the year with peak cherry tree blossom in Kyoto since 1953"),
       subtitle = "Bloom dates have significantly declined, but 2024 is no outlier in light of historical data",
       size = "Time from first to full bloom:",
       x = NULL, y = NULL) +
  scale_x_continuous(breaks = seq(1950, 2030, by = 20)) +
  scale_y_continuous(breaks = seq(75, 105, by = 15)) +
  coord_cartesian(ylim = c(75, 105)) +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    # axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  theme(legend.position = "bottom")
fig_sakura_kyoto

ggsave(plot = fig_sakura_kyoto, filename = "output/fig_sakura_kyoto.png",
       height = 8, width = 12)

# Time to full bloom --------------------------------------------------
fig_sakura_regions <- sakura_data |> 
  ggplot(aes(x = year, y = full_bloom_day)) +
  geom_point(color = colors[1], alpha = 0.5, size = 2) +
  geom_smooth(color = colors[2], se = FALSE, size = 2) +
  facet_wrap(~region, nrow = 1) + 
  labs(title = expression("Day of the year with peak cherry tree blossom for Japanese cities since 1953"),
       subtitle = "Cities in northern regions Hokkaidō and Honshū exhibit earlier full blooms, while Ryukyu Islands even later",
       x = NULL, y = NULL) +
  scale_x_continuous(breaks = seq(1950, 2030, by = 20)) +
  scale_y_continuous(breaks = seq(30, 150, by = 30)) +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    # axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) 
fig_sakura_regions

ggsave(plot = fig_sakura_regions, filename = "output/fig_sakura_regions.png",
       height = 8, width = 12)

# First bloom 2023 map ------------------------------------------------
# fig_map <- japan_map |> 
#   ggplot() + 
#   geom_polygon(data = japan_map, aes(x = long, y = lat, group = group), 
#                fill = "white", color = "black") +
#   geom_point(
#     data = sakura_data |> 
#       filter(year == 2023) |> 
#       drop_na(first_bloom_date, time_to_full_bloom), 
#     aes(x = long, y = lat, color = first_bloom_group, size = time_to_full_group)
#   ) + 
#   scale_color_manual(values = c(colors[3], colors[1], colors[2])) +
#   labs(x = NULL, y = NULL, 
#        color = "First bloom",
#        size = "Time between\nfirst & full bloom",
#        title = "First date of cherry blossom bloom & time to full bloom",
#        subtitle = "Southern islands have earlierst & longest blooms, northern regions latest & shortest") +
#   theme(axis.title.x = element_blank(), 
#         axis.title.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.line = element_blank())
# 
# ggsave(plot = fig_map, filename = "output/fig_map.png",
#        height = 8, width = 12)

# Full bloom dates in first half of sample vs second half -------------
# sakura_data |> 
#   filter(!is.na(sample_tile)) |> 
#   ggplot(aes(x = first_bloom_day)) +
#   geom_density(color = colors[1], fill = colors[1], alpha = 0.5) +
#   facet_grid(cols = vars(region), rows = vars(sample_tile), scales = "free") +
#   geom_vline(
#     data = sakura_data |> 
#       filter(!is.na(sample_tile)) |> 
#       group_by(sample_tile, region) |> 
#       summarize(median_first_bloom_day = median(first_bloom_day, na.rm = TRUE)),
#     aes(xintercept = median_first_bloom_day), color = colors[2], linetype = "dashed",
#     linewidth = 1
#   ) +
#   labs(x = NULL, y = NULL, 
#        color = "First bloom",
#        size = "Days between\nfirst and full bloom",
#        title = "Distribution of cherry blossom dates ",
#        subtitle = "The distribution first bloom dates shifted to the left over time") +
#   theme(axis.title.x = element_blank(), 
#         axis.title.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.line = element_blank(),
#   )

# Time to full bloom --------------------------------------------------
# fig_time_to_full_bloom <- sakura_data |> 
#   ggplot(aes(x = year, y = time_to_full_bloom)) +
#   geom_point(color = colors[1], alpha = 0.5) +
#   geom_smooth(color = colors[2], se = FALSE) +
#   facet_wrap(~region, nrow = 1) + 
#   labs(title = "Number of days between first and full bloom since 1953",
#        subtitle = "Hokkaidō exhibits the shortest and decreasing blooming periods, while Kyūshū's have lengthened",
#        x = NULL, y = NULL) +
#   scale_x_continuous(breaks = seq(1950, 2030, by = 20)) +
#   # scale_y_continuous(breaks = seq(7, 42, by = 7)) +
#   theme(
#     axis.title.x = element_blank(), 
#     axis.title.y = element_blank(),
#     # axis.text.x = element_blank(),
#     # axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.line = element_blank(),
#     strip.background = element_blank(),
#     strip.text = element_text(face = "bold")
#   )
# fig_time_to_full_bloom
# 
# ggsave(plot = fig_time_to_full_bloom, filename = "output/fig_time_to_full_bloom.png")

# Time to first bloom in Tokyo ----------------------------------------
# sakura_data |> 
#   filter(location == "Tokyo Japan") |> 
#   ggplot(aes(x = year, y = first_bloom_day)) +
#   geom_point(color = colors[1], aes(size = time_to_full_bloom)) +
#   geom_smooth(color = colors[2], se = FALSE) +
#   labs(title = "Cherry blossom first bloom dates and blooming periods in Tokyo from 1954 to 2023",
#        subtitle = "First bloom dates have decreased dramatically, while blooming periods show no pattern",
#        size = "Days between\nfirst and full bloom",
#        x = NULL, y = NULL) +
#   theme(
#     axis.title.x = element_blank(), 
#     axis.title.y = element_blank(),
#     # axis.text.x = element_blank(),
#     # axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.line = element_blank(),
#     strip.background = element_blank(),
#     strip.text = element_text(face = "bold")
#   )

# Change in time to first bloom dates over time -----------------------
# TODO: single out Tokyo, Kyoto and the outlier on the right and the one around 0
# TODO: add north / south to y axis
# Latitude 
# sakura_data |> 
#   filter(sample_tile == 1) |> 
#   group_by(location, lat) |> 
#   summarize(first_bloom_day_start = mean(first_bloom_day, na.rm = TRUE)) |> 
#   left_join(sakura_data |> 
#               filter(sample_tile == 7) |> 
#               group_by(location) |> 
#               summarize(first_bloom_day_end = mean(first_bloom_day, na.rm = TRUE)), join_by(location)) |> 
#   mutate(first_bloom_difference = first_bloom_day_end - first_bloom_day_start) |> 
#   ggplot(aes(x = first_bloom_difference, y = lat)) +
#   geom_point(color = colors[1], size = 2) +
#   geom_vline(aes(xintercept = 0), linetype = "dashed") +
#   labs(
#     title = "Title",
#     subtitle = "Subtitle",
#     x = NULL, y = NULL) +
#   theme(
#     axis.title.x = element_blank(), 
#     axis.title.y = element_blank(),
#     # axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.line = element_blank(),
#     strip.background = element_blank(),
#     strip.text = element_text(face = "bold")
#   )

# Change in time to first bloom vs temperature averages ---------------
# TODO: load temperature data
