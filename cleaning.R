# Packages ----------------------------------------------------------------
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(RColorBrewer)
library(udpipe)
library(lattice)

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
geo1 <- read_csv("data_unprocessed/google/geoMap.csv", skip = 2)
geo2 <- read_csv("data_unprocessed/google/geoMap (1).csv", skip = 2)
geo3 <- read_csv("data_unprocessed/google/geoMap (2).csv", skip = 2)
geo4 <- read_csv("data_unprocessed/google/geoMap (3).csv", skip = 2)
geo5 <- read_csv("data_unprocessed/google/geoMap (4).csv", skip = 2)
geo6 <- read_csv("data_unprocessed/google/geoMap (5).csv", skip = 2)
geo7 <- read_csv("data_unprocessed/google/geoMap (6).csv", skip = 2)
geo8 <- read_csv("data_unprocessed/google/geoMap (7).csv", skip = 2)
geo9 <- read_csv("data_unprocessed/google/geoMap (8).csv", skip = 2)
geo10 <- read_csv("data_unprocessed/google/geoMap (9).csv", skip = 2)
geo11 <- read_csv("data_unprocessed/google/geoMap (11).csv", skip = 2)

# No bitcoin SV
google_geo_dat <- left_join(geo1, geo2)
google_geo_dat <- left_join(google_geo_dat, geo3)
google_geo_dat <- left_join(google_geo_dat, geo4)
google_geo_dat <- left_join(google_geo_dat, geo5)
google_geo_dat <- left_join(google_geo_dat, geo6)
google_geo_dat <- left_join(google_geo_dat, geo7)
google_geo_dat <- left_join(google_geo_dat, geo8)
google_geo_dat <- left_join(google_geo_dat, geo9)
google_geo_dat <- left_join(google_geo_dat, geo10)
google_geo_dat <- left_join(google_geo_dat, geo11)

# Time series
time1 <- read_csv("data_unprocessed/google/multiTimeline.csv", skip = 2)
time2 <- read_csv("data_unprocessed/google/multiTimeline (1).csv", skip = 2)
time3 <- read_csv("data_unprocessed/google/multiTimeline (2).csv", skip = 2)
time4 <- read_csv("data_unprocessed/google/multiTimeline (3).csv", skip = 2)
time5 <- read_csv("data_unprocessed/google/multiTimeline (4).csv", skip = 2)
time6 <- read_csv("data_unprocessed/google/multiTimeline (5).csv", skip = 2)
time7 <- read_csv("data_unprocessed/google/multiTimeline (6).csv", skip = 2)
time8 <- read_csv("data_unprocessed/google/multiTimeline (7).csv", skip = 2)
time9 <- read_csv("data_unprocessed/google/multiTimeline (8).csv", skip = 2)
time10 <- read_csv("data_unprocessed/google/multiTimeline (9).csv", skip = 2)
time11 <- read_csv("data_unprocessed/google/multiTimeline (10).csv", skip = 2)
time12 <- read_csv("data_unprocessed/google/multiTimeline (11).csv", skip = 2)

# No bitcoin SV
google_time_dat <- left_join(time1, time2)
google_time_dat <- left_join(google_time_dat, time3)
google_time_dat <- left_join(google_time_dat, time4)
google_time_dat <- left_join(google_time_dat, time5)
google_time_dat <- left_join(google_time_dat, time6)
google_time_dat <- left_join(google_time_dat, time7)
google_time_dat <- left_join(google_time_dat, time8)
google_time_dat <- left_join(google_time_dat, time9)
google_time_dat <- left_join(google_time_dat, time10)
google_time_dat <- left_join(google_time_dat, time11)

google_time_dat[is.na(google_time_dat)] <- 0
google_geo_dat[is.na(google_geo_dat)] <- 0

google_time_dat[google_time_dat == "<1"] <- 0.1
google_geo_dat[google_geo_dat == "<1"] <- 0.1

google_time_dat$`Tezos: (United States)`<- as.numeric(google_time_dat$`Tezos: (United States)`)
google_time_dat$`ripple crypto: (United States)`<- as.numeric(google_time_dat$`ripple crypto: (United States)`)
google_time_dat$`litecoin: (United States)`<- as.numeric(google_time_dat$`litecoin: (United States)`)
google_time_dat$`Ethereum: (United States)`<- as.numeric(google_time_dat$`Ethereum: (United States)`)
google_time_dat$`bitcoin cash: (United States)`<- as.numeric(google_time_dat$`bitcoin cash: (United States)`)
google_time_dat$`binance coin: (United States)`<- as.numeric(google_time_dat$`binance coin: (United States)`)

names(google_time_dat)
skim(google_time_dat)


write_csv(google_time_dat, "data_processed/google_time_dat.csv")
write_csv(google_geo_dat, "data_processed/google_geo_dat.csv")



# awareness (news) --------------------------------------------------------
# https://www.kaggle.com/kashnitsky/news-about-major-cryptocurrencies-20132018-40k
news_dat_1 <- read_csv("data_unprocessed/crypto_news_parsed_2013-2017_train.csv") %>%
  select(title, year, author, source)
news_dat_2 <- read_csv("data_unprocessed/crypto_news_parsed_2018_validation.csv") %>%
  select(title, year, author, source)

news_dat <- rbind(news_dat_1, news_dat_2)
write_csv(news_dat, "data_processed/news_dat.csv")


# udpipe ------------------------------------------------------------------
news_dat <- read_csv("data_processed/news_dat.csv")
model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = model)

year_annotator <- function(news_dat, category) {
  filtered <- news_dat %>% filter(year == category)
  annotated <- udpipe_annotate(udmodel_english, filtered$title)
  ant_df <- data.frame(annotated)
  return(ant_df)
}

# annotate by year
ant_13 <- year_annotator(news_dat, "2013")
ant_14 <- year_annotator(news_dat, "2014")
ant_15 <- year_annotator(news_dat, "2015")
ant_16 <- year_annotator(news_dat, "2016")
ant_17 <- year_annotator(news_dat, "2017")
ant_18 <- year_annotator(news_dat, "2018")

write_csv(ant_13, "data_processed/ant_13.csv")
write_csv(ant_14, "data_processed/ant_14.csv")
write_csv(ant_15, "data_processed/ant_15.csv")
write_csv(ant_16, "data_processed/ant_16.csv")
write_csv(ant_17, "data_processed/ant_17.csv")
write_csv(ant_18, "data_processed/ant_18.csv")