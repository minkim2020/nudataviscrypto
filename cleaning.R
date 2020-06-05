# Packages ----------------------------------------------------------------
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(RColorBrewer)


# Historical Prices -------------------------------------------------------
# source: https://www.kaggle.com/philmohun/cryptocurrency-financial-data

price_dat <- read_csv("data_processed/price_dat.csv")

price_dat <- price_dat %>%
  rename(slug = Currency,
         date = Date,
         open = Open,
         high = High,
         low = Low,
         close = Close,
         volume = Volume,
         market_cap = `Market Cap`)

price_dat$date <- mdy(price_dat$date)


# Names -------------------------------------------------------------------
# source: https://www.kaggle.com/afriblossom/crypto
names <- read_csv("data_unprocessed/crypto-markets.csv") %>%
  select(slug, symbol, name)

price_dat <- left_join(price_dat, names, by = "slug") %>% distinct()

price_dat <- price_dat %>%
  mutate(name = case_when(slug == "xrp" ~ "Ripple",
                          slug == "bitcoin-sv" ~ "Bitcoin SV",
                          TRUE ~ name),
         symbol = case_when(slug == "xrp" ~ "XRP",
                            slug == "bitcoin-sv" ~ "BSV",
                            TRUE ~ symbol))

write_csv(price_dat, "data_processed/price_dat.csv")


# fixing BTC missing values -----------------------------------------------

btc_dat <- read_csv("data_unprocessed/BTCUSD_day.csv") %>%
  mutate(Symbol = "BTC")

price_dat <- read_csv("data_processed/price_dat.csv")

price_dat <- left_join(price_dat, btc_dat, by = c("date" = "Date", "symbol" = "Symbol"))

price_dat <- price_dat %>%
  mutate(open = if_else(is.na(open), Open, open),
         high = if_else(is.na(high), High, high),
         low = if_else(is.na(low), Low, low),
         close = if_else(is.na(close), Close, close)) %>%
  select(-Open, -High, -Low, -Close, -`Volume BTC`, -`Volume USD`)

write_csv(price_dat, "data_processed/price_dat.csv")

# Awareness ---------------------------------------------------------------
# maybe: https://www.kaggle.com/kashnitsky/news-about-major-cryptocurrencies-20132018-40k
# google mentions?
