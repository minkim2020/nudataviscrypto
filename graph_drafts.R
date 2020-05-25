
# Packages ----------------------------------------------------------------
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(RColorBrewer)

<<<<<<< HEAD

# data --------------------------------------------------------------------
price_dat <- read_csv("data_processed/price_dat.csv")


=======
>>>>>>> 2ec3f19aecfd641b3d3c1b00095887eab455fa25
# plots (test) ------------------------------------------------------------
theme_global <- theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, family = "Arial", face = "bold", size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 70, hjust = 1),
        axis.text = element_text(family = "Arial", size = 7),
        axis.title = element_text(family = "Arial", size = 10, face = "bold"),
        legend.text = element_text(family = "Arial", size = 7),
        legend.title = element_text(family = "Arial", size = 10))


price_dat %>%
  ggplot(aes(x = date, y = market_cap, color = symbol)) +
  geom_line() + 
  ggtitle("Historical Market Cap") +
  labs(x = "Date", y = "Market Cap") +
  theme_global

