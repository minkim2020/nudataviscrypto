
# Packages ----------------------------------------------------------------
library(tidyquant)
library(tidyverse)
library(janitor)
library(skimr)
library(ggstance)
library(lubridate)
library(RColorBrewer)
library(splines)
library(ggrepel)
library(viridis)
library(sf)
library(statebins)


# data --------------------------------------------------------------------
price_dat <- read_csv("data_processed/price_dat.csv")

# theme -------------------------------------------------------------------
theme_global <- theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, family = "Arial", face = "bold", size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 70, hjust = 1),
        axis.text = element_text(family = "Arial", size = 7),
        axis.title = element_text(family = "Arial", size = 10, face = "bold"),
        legend.text = element_text(family = "Arial", size = 7),
        legend.title = element_text(family = "Arial", size = 10))


# MCAP over time ---------------------------------------------------------
# CAN WE MAKE A BOX-CHECK INPUT TO SHOW SPECIFIC CURRENCIES?
price_dat %>%
  ggplot(aes(x = date, y = market_cap, color = symbol)) +
  geom_line() + 
  ggtitle("Historical Market Cap") +
  labs(x = "Date", y = "Market Cap") +
  theme_global

price_dat %>%
  group_by(name) %>%
  count()

# candlestick (open/close over time) --------------------------------------
# INPUTS (date entry, selectbox, span/fit entry)
input_date_max <- date("2019-05-31")
input_date_min <- date("2019-01-01")
input_currency <- c("BTC")
input_span <- 0.3


price_dat %>%
  filter(symbol == input_currency,
         date < input_date_max & date > input_date_min) %>% 
  ggplot(aes(x = date, y = close)) +
  geom_smooth(size = 0.5, se = FALSE, span = input_span, color = "gold") +
  geom_candlestick(aes(open = open, high = high, low = low, close = close), 
                   colour_up = "darkgreen", colour_down = "darkred", 
                   fill_up  = "darkgreen", fill_down  = "darkred") +
  ggtitle("Historical Trading Prices") +
  labs(x = "Date", y = "Price (USD)") +
  theme_global



# maps (rename) -----------------------------------------------------------
google_geo_dat <- read_csv("data_processed/google_geo_dat.csv")
google_time_dat <- read_csv("data_processed/google_time_dat.csv")

#names(google_geo_dat) <- c("Region", "USDT", "XTZ", "XLM", "XRP", "LTC", "ETH", "EOS", "BCH", "BTC", "BNB", "BSV")
#names(google_time_dat) <- c("Month", "USDT", "XTZ", "XLM", "XRP", "LTC", "ETH", "EOS", "BSV", "BCH", "BTC", "BNB")

#write_csv(google_geo_dat, "data_processed/google_geo_dat.csv")
#write_csv(google_time_dat, "data_processed/google_time_dat.csv")

#load("data_processed/US_census.rda") 
#load("data_processed/US_income.rda") 

#US_income <- mutate(
#US_income,
#income_bins = cut(
#ifelse(is.na(median_income), 25000, median_income),
#breaks = c(0, 40000, 50000, 60000, 70000, 80000),
#labels = c("< $40k", "$40k to $50k", "$50k to $60k", "$60k to $70k", "> $70k"),
#right = FALSE))

#google_geo_dat <- left_join(google_geo_dat, US_income, by = c("Region" = "name"))
#write_csv(google_geo_dat, "data_processed/google_geo_dat.csv")

# Maps --------------------------------------------------------------------
# Inputs
google_currency <- "BTC" # which currency do we want to show the counts of?

google_geo_dat %>% 
  pivot_longer(cols = 2:12, names_to = "currency", values_to = "search_popularity") %>%
  filter(currency == google_currency) %>%
  ggplot(aes(fill = search_popularity, color = search_popularity, geometry = geometry)) +
  geom_sf(color = "grey80", size = 0.2) + 
  scale_fill_viridis(discrete = FALSE) + 
  theme_void()

google_geo_dat %>% 
  pivot_longer(cols = 2:12, names_to = "currency", values_to = "search_popularity") %>%
  filter(currency == google_currency) %>%
  ggplot(aes(fill = search_popularity, geometry = geometry)) +
  geom_statebins(aes(state = Region)) + 
  scale_fill_viridis(discrete = FALSE) + 
  theme_void()

# Would it be possible to make it possible to toggle between the above two graph types?