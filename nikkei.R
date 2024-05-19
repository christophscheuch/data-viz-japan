library(tidyverse)
library(tidyquant)
library(tidytext)
library(ggtext)
library(readxl)
library(scales)
library(wesanderson)

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
colors_wesanderson <- colors <- wes_palette(
  "Zissou1",
  type = "continuous"
)

# Download & prepare data ---------------------------------------------
# Index price data from YahooFinance
start_date <- "1949-12-01" # First date of Nikkei 225
end_date <- "2024-03-31" # Last full month before script creation

# Since 1965: on YahooFinance
# Between 1949 and 1964: manual downloads from official archives 
# (https://indexes.nikkei.co.jp/en/nkave/archives/data)
# Currency in JPY
nikkei <- tq_get("^N225", get = "stock.prices", from = start_date, to = end_date) |> 
  drop_na() |> 
  group_by(month = floor_date(date, "month")) |> 
  filter(date == max(date)) |> 
  ungroup() |> 
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

# S&P 500 since 1949 is on YahooFinance
sp500 <-  tq_get("^GSPC", get = "stock.prices",
                 from = start_date, to =end_date) |>
  drop_na() |> 
  group_by(month = floor_date(date, "month")) |> 
  filter(date == max(date)) |> 
  ungroup() |> 
  select(date = month, value = adjusted) |> 
  mutate(type = "S&P 500")

sp500 <- sp500 |>
  left_join(xrate_yenusd, join_by(date)) |>
  mutate(xrate = if_else(date < "1971-01-01", 360, xrate)) |>
  fill(xrate, .direction = "down") |>
  mutate(value = value * xrate) |>
  select(-xrate)

sp500_tr <-  tq_get("^SP500TR", get = "stock.prices",
                    from = "1988-01-04", to = "2023-01-31") |>
  drop_na() |> 
  group_by(month = floor_date(date, "month")) |> 
  filter(date == max(date)) |> 
  ungroup() |> 
  select(date = month, value = adjusted) |> 
  mutate(type = "S&P 500 TR")

# Get dividend yield
temp <- tempfile(fileext = ".xls")

download.file(url = "http://www.econ.yale.edu/~shiller/data/ie_data.xls",
              destfile = temp, mode='wb')

shiller_historical <- read_excel(temp, sheet = "Data", skip = 7) |>
  transmute(date = ceiling_date(ymd(str_replace(str_c(Date, ".01"), "\\.1\\.", "\\.10\\.")), "month")-1,
            value = as.numeric(P),
            dividend = as.numeric(D)) |> 
  mutate(type = "S&P 500 TR") |> 
  arrange(date) |>
  mutate(ret = (value + dividend / 12) / lag(value) - 1)

sp500_historical <- sp500_tr |> 
  filter(date == min(date)) |>
  full_join(shiller_historical |>
              filter(date <= min(sp500_tr$date)), join_by(date, value, type)) |>
  arrange(desc(date)) |>
  mutate(ret = if_else(row_number() == 1, 0, ret),
         value = value[1] / cumprod(1 + ret))

sp500_tr <- sp500_tr |> 
  bind_rows(sp500_historical |> 
              select(date, value, type)) |> 
  distinct() |> 
  filter(date >= start_date)

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

fig_nikkei_vs_sp500 <- index_data_monthly |> 
  ggplot(aes(x = date, y = value, color = type)) +
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
files_companiesmarketcap <- tibble(
  file = list.files("data/companiesmarketcap", full.names = TRUE)
)
regions <- c("Japanese", "American", "German")

companiesmarketcap_raw <- list()
for (j in 1:length(regions)) {
  
  files_companiesmarketcap_sub <- files_companiesmarketcap |> 
    filter(str_detect(file, regions[j]))
  
  companiesmarketcap_raw[[j]] <- files_companiesmarketcap_sub$file |> 
    map(function(x) {
      if (str_detect(x, "market cap")) {
        read_csv(x) |>  
          mutate(shrout_estimated = round(marketcap / `price (USD)`)) |> 
          select(-c(Rank, `price (USD)`))
      } else {
        read_csv(x) |>  
          select(-c(Rank, `price (USD)`))
      }
      }) |> 
    reduce(full_join)
}
companiesmarketcap_raw <- bind_rows(companiesmarketcap_raw) 

companiesmarketcap <- companiesmarketcap_raw |> 
  janitor::clean_names() |> 
  # filter(!str_detect(symbol, "\\.T") &
  #          country == "Japan") |> 
  mutate(
    date = as.Date("2024-04-08"),
    symbol = case_when(
      name == "Toyota" ~ "7203.T",
      name == "Mitsubishi UFJ Financial" ~ "8306.T",
      name == "Sony" ~ "6758.T",
      name == "NTT (Nippon Telegraph &amp; Telephone)" ~ "9613.T",
      name == "Sumitomo Mitsui Financial Group" ~ "8316.T",
      name == "Honda" ~ "7267.T",
      name == "Mizuho Financial Group" ~ "8411.T",
      name == "Takeda Pharmaceutical" ~ "4502.T",
      name == "ORIX" ~ "8591.T",
      name == "Nomura Holdings" ~ "8604.T",
      name == "Chiba Bank" ~ "8331.T",
      name == "Nikon" ~ "7731.T",
      # name == "SYLA Technologies" ~ "",
      # name == "Lead Real Estate" ~ "",
      # name == "Pixie Dust Technologies" ~ "",
      # name == "Warrantee Inc." ~ "",
      # name == "Earlyworks" ~ "",
      .default = symbol
    )) 

# Nikkei components ---------------------------------------------------
# Download holdings from Blackrock
# https://www.blackrock.com/jp/individual-en/en/products/251897/ishares-nikkei-225-fund
nikkei_components <- read_csv("data/iShares Core Nikkei 225 ETF.csv", skip = 2) |> 
  janitor::clean_names() |> 
  select(ticker, name, sector, market_value) |> 
  filter(!ticker %in% c("JPY", "MSJP", "NKM4")) |> 
  drop_na() |> 
  mutate(ticker = paste0(as.character(ticker), ".T"))

nikkei_stocks <- tq_get(
  nikkei_components$ticker,
  get = "stock.prices",
  from = start_date,
  to = end_date
)

write_rds(nikkei_stocks, "data/nikkei_stocks.rds")

# All stocks active through 2024-03-29, youngest stock since 2022-10-12, oldest stock 1999-05-06
nikkei_stocks |> 
  group_by(symbol) |> 
  summarize(first_date = min(date), 
            last_date = max(date))

# Nikkei low point was on 2009-02-01
index_data_monthly |> 
  filter(date >= "1990-01-01" & type == "Nikkei 225") |> 
  filter(value == min(value))

nikkei_stocks_performance <- nikkei_stocks |> 
  filter(date >= "2009-02-01") |> 
  group_by(symbol) |> 
  arrange(date) |> 
  summarize(performance = last(adjusted) / first(adjusted) - 1,
            first_date = min(date), 
            last_date = max(date)) |> 
  left_join(nikkei_components, join_by(symbol == ticker)) |> 
  mutate(name = str_remove(name, "CORP|LTD|INC"),
         name = str_to_title(name),
         market_value_usd = market_value / xrate_yenusd |> filter(date == max(date)) |> pull(xrate))

nikkei_stocks_performance |> 
  anti_join(companiesmarketcap, join_by(symbol))

nikkei_best_and_worst <- bind_rows(
  nikkei_stocks_performance |> 
    arrange(performance) |> 
    slice(1:5) |> 
    mutate(type = "Worst performer"),
  nikkei_stocks_performance |> 
    arrange(-performance) |> 
    slice(1:5) |> 
    mutate(type = "Best performer")
) 

# TODO: add subtitle
# TODO: create two dots that signal the market cap and a line between them
fig_nikkei_best_and_worst <- nikkei_best_and_worst |> 
  # filter(type == "Best") |> 
  ggplot(aes(x = performance, 
             y = reorder_within(name, performance, type), 
             fill = sector)) +
  geom_col() +
  scale_y_reordered() +
  scale_x_continuous(labels = percent) + 
  facet_wrap(~type, ncol = 1, scales = "free") +
  scale_fill_manual(values = wes_palette("Zissou1", 6, "continuous")) + 
  labs(x = NULL, y = NULL, fill = NULL, color = NULL,
       title = "Best and worst performing stocks from the Nikkei 225 since February 2009",
       subtitle = "") +
  theme(
    plot.title = element_markdown(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )
fig_nikkei_best_and_worst

ggsave(plot = fig_nikkei_best_and_worst, filename = "output/fig_nikkei_best_and_worst.png",
       height = 8, width = 12)

nikkei_stocks_performance |> 
  ggplot(aes(x = performance, y = market_value)) +
  geom_point()

top_x <- 10

total_market_cap <- sum(nikkei_stocks_performance$market_value)

nikkei_top_market_cap <- nikkei_stocks_performance |> 
  arrange(-market_value) |> 
  slice(1:top_x) 

usd <- label_dollar(
  scale_cut = c(0, k = 1e3, m = 1e6, bn = 1e9, tn = 1e12)
)

# TOOD: use correct market cap list, not the wrong market value from ETF holdings
fig_largest_stocks <- nikkei_top_market_cap |> 
  ggplot() +
  geom_col(aes(x = market_value_usd, y = reorder(name, market_value), fill = sector)) +
  scale_fill_manual(values = wes_palette("Zissou1", 6, "continuous")) +
  labs(x = NULL, y = NULL, fill = NULL, color = NULL,
       title = paste0("Top ", top_x, " largest companies in the Nikkei 225 index"),
       subtitle = paste0("These companies make up ", percent(sum(nikkei_top_market_cap$market_value) / total_market_cap), " of the total market capitalisation")) +
  scale_x_continuous(labels = usd) + 
  theme(
    plot.title = element_markdown(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )
fig_largest_stocks

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
