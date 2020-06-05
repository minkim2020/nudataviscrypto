
# Packages ----------------------------------------------------------------
library(tidyquant)
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(RColorBrewer)


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
