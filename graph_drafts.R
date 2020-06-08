
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
library(lattice)
library(wordcloud)
library(igraph)
library(ggraph)
library(textrank)

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


# searches over time --------------------------------------------------------
google_time_dat$Region <- as.Date(paste(google_time_dat$Region, "-01", sep = ""))

input_date_max_2 <- date("2019-05-31")
input_date_min_2 <- date("2016-01-01")
input_currency_2 <- c("BTC")
input_span_2 <- 0.3

google_time_dat %>%
  rename("date" = "Region") %>%
  pivot_longer(cols = 2:12, names_to = "symbol", values_to = "search_popularity") %>% 
  filter(symbol == input_currency_2,
         date < input_date_max_2 & date > input_date_min_2) %>% 
  ggplot(aes(x = date, y = search_popularity, color = symbol)) +
  geom_line() + 
  ggtitle("Historical Google Search Popularity") +
  labs(x = "Date", y = "Search Popularity") +
  theme_global


# news headlines (2013-18) ------------------------------------------------
ant_13 <- read_csv("data_processed/ant_13.csv")
ant_14 <- read_csv("data_processed/ant_14.csv")
ant_15 <- read_csv("data_processed/ant_15.csv")
ant_16 <- read_csv("data_processed/ant_16.csv")
ant_17 <- read_csv("data_processed/ant_17.csv")
ant_18 <- read_csv("data_processed/ant_18.csv")

#### Input (Which year?) ####
dataset <- ant_13

#### Top words in headlines of given year, by UPOS/Adjectives/Nouns/Verbs/Pairs ####
# Universal Parts of Speech (UPOS)
stats <- txt_freq(dataset$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
ggplot(stats, aes(x = key, y = freq)) + 
  geom_col(fill = "#656565") + 
  coord_flip() +
  ggtitle("UPOS (Universal Parts of Speech)\nfrequency of occurrence") + 
  labs(x = "Word", y = "Frequency") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Arial", face = "bold"),
        axis.text = element_text(family = "Arial", color = "black", size = 5),
        axis.title = element_text(family = "Arial", size = 8, face = "bold")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))


# Most common nouns
stats <- subset(dataset, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
ggplot(head(stats, 20), aes(x = key, y = freq)) + 
  geom_col(fill = "#6D9EEB") + 
  coord_flip() +
  ggtitle("Frequently Occurring Nouns") + 
  labs(x = "Word", y = "Frequency") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Arial", face = "bold"),
        axis.text = element_text(family = "Arial", color = "black", size = 5),
        axis.title = element_text(family = "Arial", size = 8, face = "bold")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))

# Most common adjectives
stats <- subset(dataset, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
ggplot(head(stats, 20), aes(x = key, y = freq)) + 
  geom_col(fill = "#FFD965") + 
  coord_flip() +
  ggtitle("Frequently Occurring Adjectives") + 
  labs(x = "Word", y = "Frequency") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Arial", face = "bold"),
        axis.text = element_text(family = "Arial", color = "black", size = 5),
        axis.title = element_text(family = "Arial", size = 8, face = "bold")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))

# Most common verbs
stats <- subset(dataset, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
ggplot(head(stats, 20), aes(x = key, y = freq)) + 
  geom_col(fill = "#E06566") + 
  coord_flip() +
  ggtitle("Frequently Occurring Verbs") + 
  labs(x = "Word", y = "Frequency") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Arial", face = "bold"),
        axis.text = element_text(family = "Arial", color = "black", size = 5),
        axis.title = element_text(family = "Arial", size = 8, face = "bold")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))

# Keyword (Unsupervised algorithm)
stats <- keywords_rake(x = dataset, term = "lemma", group = "doc_id", 
                       relevant = dataset$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
ggplot(head(subset(stats, freq > 3), 20), aes(x = key, y = rake)) + 
  geom_col(fill = "#93C47C") + 
  coord_flip() +
  ggtitle("Keywords identified by RAKE") + 
  labs(x = "Word", y = "Frequency") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Arial", face = "bold"),
        axis.text = element_text(family = "Arial", color = "black", size = 5),
        axis.title = element_text(family = "Arial", size = 8, face = "bold")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))

# Top noun-verb pairing
dataset$phrase_tag <- as_phrasemachine(dataset$upos, type = "upos")
stats <- keywords_phrases(x = dataset$phrase_tag, term = tolower(dataset$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
ggplot(head(stats, 20), aes(x = key, y = freq)) + 
  geom_col(fill = "#8E7CC3") + 
  coord_flip() +
  ggtitle("Keywords - simple noun phrases") + 
  labs(x = "Word", y = "Frequency") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Arial", face = "bold"),
        axis.text = element_text(family = "Arial", color = "black", size = 5),
        axis.title = element_text(family = "Arial", size = 8, face = "bold")) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .2)))


###### Co-occurence as word cloud (Here, we choose one of the 3 types of co-occurence, and then run the plot function at the bottom

# 1) Collocation (words following one another)
stats <- keywords_collocation(x = x, 
                              term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)

# 2) Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))

# 3) Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats <- cooccurrence(x = dataset$lemma, 
                      relevant = dataset$upos %in% c("NOUN", "ADJ"), skipgram = 2)
head(stats)


# Main plot function
wordnet <- head(stats, 30)
wordnet <- graph_from_data_frame(wordnet)
ggraph(wordnet, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "blue") +
  geom_node_text(aes(label = name), col = "purple", size = 4) +
  theme_graph(base_family = "Arial") +
  theme(legend.position = "none") +
  ggtitle("Cooccurrences within 3 words distance")


# Another option: Wordcloud
stats <- textrank_keywords(dataset$lemma, 
                           relevant = dataset$upos %in% c("NOUN", "ADJ", "VERB"), 
                           ngram_max = 10, sep = " ")
stats <- subset(stats$keywords, ngram > 1 & freq >= 2)
wordcloud(words = stats$keyword, freq = stats$freq)
