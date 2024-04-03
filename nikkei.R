library(tidyverse)
library(tidyquant)
library(ggtext)
library(extrafont)

# TODO: add dividend yields
# TODO: fix font usage for ggsave

# Set global theme
theme_set(theme_classic(base_family = "asap", base_size = 16))

theme_update(
  plot.title.position = "plot",
  plot.title = element_text(size = 24),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 16)
)

# triadic color scheme
colors <- c("#BC002D", "#4D9DE0")

# nikkei 225 vs msci world shaded years where nikkei outperformed
# Index price data from YahooFinance
start_date <- "1949-12-01"
end_date <- "2024-03-31"

# Since 1965: on YahooFinance
# Between 1949 and 1964: official archives (https://indexes.nikkei.co.jp/en/nkave/archives/data)
# Currency in JPY
nikkei <- tq_get("^N225", get = "stock.prices", from = start_date, to = end_date) |> 
  drop_na() |> 
  group_by(month = floor_date(date, "month")) |> 
  filter(date == max(date)) |> 
  select(date = month, value = adjusted) |> 
  bind_rows(
    read_csv2("data/nikkei_historical.csv") |> 
      filter(date >= start_date & date < "1965-01-01") |> 
      select(date, value = close) 
  ) |> 
  mutate(type = "Nikkei 225") |> 
  arrange(date)

# Yen to USD exchange rate
# From 1949 to 1971 pegged to 360 yen to dollar (https://en.wikipedia.org/wiki/Japanese_yen)
xrate_yenusd <- tq_get("EXJPUS", get = "economic.data", from = start_date, to = end_date) |> 
  drop_na() |> 
  group_by(month = floor_date(date, "month")) |> 
  filter(date == max(date)) |> 
  ungroup() |> 
  select(date = month, xrate = price)

# nikkei <- nikkei |> 
#   left_join(xrate_yenusd, join_by(date)) |> 
#   mutate(xrate = if_else(date < "1971-01-01", 360, xrate)) |> 
#   fill(xrate, .direction = "down") |> 
#   mutate(value = value * xrate) |> 
#   select(-xrate)

# S&P 500 since 1949 is on YahooFinance
sp500 <-  tq_get("^GSPC", get = "stock.prices",
                        from = start_date, to =end_date) |>
  drop_na() |> 
  group_by(month = floor_date(date, "month")) |> 
  filter(date == max(date)) |> 
  select(date = month, value = adjusted) |> 
  mutate(type = "S&P 500")

sp500 <- sp500 |>
  left_join(xrate_yenusd, join_by(date)) |>
  mutate(xrate = if_else(date < "1971-01-01", 360, xrate)) |>
  fill(xrate, .direction = "down") |>
  mutate(value = value * xrate) |>
  select(-xrate)

# Combine to monthly and annual index data
index_data_monthly <- nikkei |> 
  bind_rows(sp500) |> 
  group_by(type) |> 
  filter(date >= "1960-01-01") |>
  arrange(date) |> 
  mutate(value = value / value[1] * 100, 
         return = value / lag(value) - 1,
         year = year(date)) |> 
  ungroup()

# index_data_yearly <- index_data_monthly|> 
#   group_by(type, year) |> 
#   filter(date == max(date)) |> 
#   group_by(type) |> 
#   mutate(return = value / lag(value) - 1) |> 
#   ungroup()
# 
# nikkei_data_yearly <- index_data_yearly |> 
#   filter(type == "Nikkei 225") |> 
#   left_join(index_data_yearly |> 
#               filter(type == "S&P 500") |> 
#               select(year, return_sp500 = return), join_by(year)) |> 
#   mutate(has_outperformed = return > return_sp500)
#
# outperformance_years <- nikkei_data_yearly |> 
#   filter(has_outperformed == TRUE) |> 
#   distinct(year) |> 
#   mutate(start_date = as.Date(paste(year, "-01-01", sep="")),
#          end_date = as.Date(paste(year, "-12-31", sep="")))

fig_nikkei_vs_sp500 <- ggplot(index_data_monthly, aes(x = date, y = value, color = type)) +
  geom_line(linewidth = 1) +
  labs(
    title = paste0("<span style='color:", colors[1], ";'>**Nikkei 225**</span> and <span style='color:", colors[2], ";'>**S&P 500**</span> stock market performance since 1960"),
    subtitle = "Both indexes are denominated in Yen and start at 100. Data from Yahoo Finance and official Nikkei archives."
  ) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::comma) + 
  theme(
    legend.position = "none",
    plot.title = element_markdown(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )
fig_nikkei_vs_sp500

ggsave(plot = fig_nikkei_vs_sp500, filename = "output/fig_nikkei_vs_sp500.png",
       height = 8, width = 12)

# List of companies with market cap 
# Source: https://companiesmarketcap.com/japan/largest-companies-in-japan-by-market-cap/
# facet of biggest stocks by market cap in nikkei, stock price over time


# inflation vs investing in index
# see https://www.wikifolio.com/de/de/blog/inflationsschutz-mit-aktie-dax-bringt-realrendite
# inflation data: https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG?locations=JP


# Fama-French factors -------------------------------------------------
# library(frenchdata)
# 
# factors_ff5_monthly_raw_us <- download_french_data("Fama/French 5 Factors (2x3)")
# 
# factors_ff5_monthly_us <- factors_ff5_monthly_raw_us$subsets$data[[1]] |>
#   mutate(
#     month = floor_date(ymd(str_c(date, "01")), "month"),
#     across(c(RF, `Mkt-RF`, SMB, HML, RMW, CMA), ~as.numeric(.) / 100),
#     .keep = "none"
#   ) |>
#   rename_with(str_to_lower) |>
#   rename(mkt_excess = `mkt-rf`) |> 
#   filter(month >= start_date & month <= end_date)
# 
# factors_ff5_monthly_raw_japan <- download_french_data("Fama/French Japanese 5 Factors")
# 
# factors_ff5_monthly_japan <- factors_ff5_monthly_raw_japan$subsets$data[[1]] |>
#   mutate(
#     month = floor_date(ymd(str_c(date, "01")), "month"),
#     across(c(RF, `Mkt-RF`, SMB, HML, RMW, CMA), ~as.numeric(.) / 100),
#     .keep = "none"
#   ) |>
#   rename_with(str_to_lower) |>
#   rename(mkt_excess = `mkt-rf`) |> 
#   filter(month >= start_date & month <= end_date)
# 
# inner_join(
#   factors_ff5_monthly_us |> 
#     pivot_longer(cols = -month, names_to = "factor", values_to = "value_us") ,
#   factors_ff5_monthly_japan |> 
#     pivot_longer(cols = -month, names_to = "factor", values_to = "value_japan") 
# ) |> 
#   filter(factor != "rf") |> 
#   mutate(year = year(month)) |> 
#   ggplot(aes(x = value_us, y = value_japan)) +
#   facet_wrap(~factor) + 
#   geom_point() +
#   geom_smooth(method = "lm")
