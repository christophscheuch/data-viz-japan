library(tidyverse)
library(patchwork)
library(ggtext)

source("theme.R")

colors <- c("#BC002D", "#4D9DE0")

theme_update(
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold"),
  plot.title = element_markdown(),
)

# source: https://datafinder.qog.gu.se/dataset/gggi
files <- tibble(
  "file" = list.files("data", full.names = TRUE)
) |> 
  filter(str_detect(file, "gender_gap"))

gender_gap_raw <- map(files$file, read_csv2) |> 
  reduce(full_join)

gender_gap <- gender_gap_raw |> 
  select(country = cname, year, contains("gggi")) |> 
  pivot_longer(cols = contains("gggi")) |> 
  mutate(
    type = case_when(name == "gggi_eas" ~ "Educational Attainment",
                     name == "gggi_ggi" ~ "Overall",
                     name == "gggi_hss" ~ "Health and Survival",
                     name == "gggi_pes" ~ "Political Empowerment",
                     name == "gggi_pos" ~ "Economic Participation")
  ) |> 
  mutate(
    value = if_else(country == "Japan" & year == 2022 & type == "Overall", 0.65, value)
  )

# Gender GAP over time: Japan vs G7
# TODO: compute average for G7
countries_g7 <- c(
  "France", 
  "United States of America (the)", 
  "United Kingdom of Great Britain and Northern Ireland (the)", 
  "Germany", "Japan", "Italy", "Canada")
countries_g7_excl_japan <- countries_g7[countries_g7!="Japan"]
gender_gap_averages <- gender_gap |> 
  group_by(year, type) |> 
  summarize(
    average_global = mean(value, na.rm = TRUE),
    average_g7 = mean(value[country %in% countries_g7]),
    average_g7_excl_japan = mean(value[country %in% countries_g7_excl_japan]),
    .groups = "drop")

gender_gap_japan <- gender_gap |> 
  left_join(gender_gap_averages, join_by(year, type)) |> 
  filter(country == "Japan") 

fig_overall <- gender_gap_japan |> 
  filter(country == "Japan" & type == "Overall") |>  
  ggplot(aes(x = year)) +
  geom_line(aes(y = average_g7_excl_japan), 
            linetype = "dashed", color = colors[2],
            linewidth = 1) +
  # geom_line(aes(y = average_global), 
  #           linetype = "dotted", color = colors[2],
  #           linewidth = 1) +
  geom_line(aes(y = value), 
            color = colors[1],
            linewidth = 1) +
  facet_wrap(~type) 

fig_sub <- gender_gap_japan |> 
  filter(country == "Japan" & type != "Overall") |>  
  ggplot(aes(x = year)) +
  geom_line(aes(y = average_g7_excl_japan), 
            linetype = "dashed", 
            color = colors[2],
            linewidth = 1) +
  # geom_line(aes(y = average_global), 
  #           linetype = "dotted", color = colors[2],
  #           linewidth = 1) +
  geom_line(aes(y = value), 
            color = colors[1],
            linewidth = 1) +
  facet_wrap(~type, nrow = 1) +
  coord_cartesian(ylim = c(0, 1))

fig_gender_gap_time <- fig_overall / fig_sub +
  plot_annotation(
    title = paste0("Gender Gap Index since 2006 for <span style='color:", colors[1], ";'>**Japan**</span>  and <span style='color:", colors[2], ";'>**G7 countries**</span> (excl. Japan)"),
    subtitle = 'G7 countries moved closer to parity, excluding Japan, which stagnated in particular in political empowerment',
    caption = 'Data: The Global Gender Gap Report 2023, World Economic Forum, provided by The QoG Institute.'
  )
fig_gender_gap_time 

ggsave(plot = fig_gender_gap_time, filename = "output/fig_gender_gap_time.png",
       height = 8, width = 12)
