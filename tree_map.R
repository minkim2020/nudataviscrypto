
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(coinmarketcapr)
library(treemap)
library(janitor)
library(scales)

# Workspace ---------------------------------------------------------------

setup(api_key = "b66a77f4-a35c-4ef2-81a8-c9fabf528ccc")

all_coins <- get_crypto_listings() %>% as_tibble() %>% clean_names()

all_coins %>%
  select(name, symbol, usd_market_cap) %>%
  mutate(usd_market_cap_formated = paste0(name, '\n', "(",symbol, "-USD", ")", '\n', dollar(usd_market_cap))) %>%
  treemap(index = "usd_market_cap_formated", vSize = "usd_market_cap", title = "Cryptocurrency Market Cap", palette = "Purples")






