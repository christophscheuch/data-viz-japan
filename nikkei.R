library(tidyverse)
library(tidyquant)

# nikkei 225 vs msci world shaded years where nikkei outperformed
# Index price data from YahooFinance
# TODO: convert currencies
# TODO: nikkei data available since 1949: https://indexes.nikkei.co.jp/en/nkave/archives/data?list=monthly
# TODO: use S&P 500 for comparison

start_date <- "1949-01-01" # start date of MSCI world
end_date <- "2024-03-31"

nikkei <- tq_get("^N225", get = "stock.prices", from = start_date, to = end_date) |> 
  drop_na() |> 
  group_by(month = floor_date(date, "month")) |> 
  filter(date == max(date)) |> 
  select(date = month, price = adjusted) |> 
  mutate(index = "Nikkei 225")

msci_world <- tq_get("^990100-USD-STRD", get = "stock.prices", from = start_date, to = end_date) |> 
  drop_na() |>
  group_by(month = floor_date(date, "month")) |> 
  filter(date == max(date)) |> 
  select(date = month, price = adjusted) |> 
  mutate(index = "MSCI World")

index_data_monthly <- nikkei |> 
  bind_rows(msci_world) |> 
  group_by(index) |> 
  arrange(date) |> 
  mutate(price = price / price[1], 
         return = price / lag(price) - 1,
         year = year(date)) |> 
  ungroup()

index_data_yearly <- index_data_monthly|> 
  group_by(index, year) |> 
  filter(date == max(date)) |> 
  group_by(index) |> 
  mutate(return = price / lag(price) - 1) |> 
  ungroup()

nikkei_data_yearly <- index_data_yearly |> 
  filter(index == "Nikkei 225") |> 
  left_join(index_data_yearly |> 
              filter(index == "MSCI World") |> 
              select(year, return_msci = return), join_by(year)) |> 
  mutate(has_outperformed = return > return_msci)


outperformance_years <- nikkei_data_yearly %>%
  filter(has_outperformed == TRUE) %>%
  distinct(year) %>%
  mutate(start_date = as.Date(paste(year, "-01-01", sep="")),
         end_date = as.Date(paste(year, "-12-31", sep="")))

p <- ggplot(index_data_monthly, aes(x = date, y = price, color = index)) +
  geom_line() +
  theme_minimal()

for(i in 1:nrow(outperformance_years)) {
  p <- p + geom_rect(data = outperformance_years[i, ], inherit.aes = FALSE,
                     aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
                     fill = "grey", alpha = 0.2)
}
 
# List of companies with market cap 
# Source: https://companiesmarketcap.com/japan/largest-companies-in-japan-by-market-cap/
# facet of biggest stocks by market cap in nikkei, stock price over time


# inflation vs investing in index
# see https://www.wikifolio.com/de/de/blog/inflationsschutz-mit-aktie-dax-bringt-realrendite
# inflation data: https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG?locations=JP


# Fama-French factors -------------------------------------------------
library(frenchdata)

factors_ff5_monthly_raw_us <- download_french_data("Fama/French 5 Factors (2x3)")

factors_ff5_monthly_us <- factors_ff5_monthly_raw_us$subsets$data[[1]] |>
  mutate(
    month = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(RF, `Mkt-RF`, SMB, HML, RMW, CMA), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |> 
  filter(month >= start_date & month <= end_date)

factors_ff5_monthly_raw_japan <- download_french_data("Fama/French Japanese 5 Factors")

factors_ff5_monthly_japan <- factors_ff5_monthly_raw_japan$subsets$data[[1]] |>
  mutate(
    month = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(RF, `Mkt-RF`, SMB, HML, RMW, CMA), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |> 
  filter(month >= start_date & month <= end_date)

inner_join(
  factors_ff5_monthly_us |> 
    pivot_longer(cols = -month, names_to = "factor", values_to = "value_us") ,
  factors_ff5_monthly_japan |> 
    pivot_longer(cols = -month, names_to = "factor", values_to = "value_japan") 
) |> 
  filter(factor != "rf") |> 
  mutate(year = year(month)) |> 
  ggplot(aes(x = value_us, y = value_japan)) +
  facet_wrap(~factor) + 
  geom_point() +
  geom_smooth(method = "lm")
