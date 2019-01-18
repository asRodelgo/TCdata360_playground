############################
# Pull indicators for all World Cup 2018 countries from TCdata360 and rank them
#
# asanchezrodelgo@worldbank.org
############################

## Use data360r package to pull all indicators by country and rank them
# install.packages("devtools","httr","curl")
# library(httr)
# library(curl)
# httr::set_config( config( ssl_verifypeer = 0L ) )
# devtools::install_github("mrpsonglao/data360r")library(data360r)
library(data360r)
library(tidyverse)

# declare helper functions
prepareData <- function(data) {
  data_output <- data %>%
    select(CountryCode = `Country ISO3`, CountryName = `Country Name`, Type = `Subindicator Type`, everything()) %>%
    group_by(CountryCode) %>%
    gather(Period, Value, -c(CountryCode, CountryName,Indicator,Type,Product,Partner)) %>%
    filter(!(Value=="" | Value %in% c("Yes","No") | is.na(as.numeric(Value)) | grepl("Rank",Type))) %>%
    mutate(Value = as.numeric(Value), Indicator = paste0(Indicator,"_",Product,"_",Partner,"_",Type)) %>%
    mutate(Value = ifelse(grepl("rank",tolower(Indicator)),-Value,Value)) %>%
    select(-c(Product,Partner,Type)) %>%
    as.data.frame()
  
  return(data_output)
}

getDataByChunks <- function(cou,timeFrame){
  
  dataComplete <- data.frame()
  for (t in timeFrame){
    thisData <- get_data360(country_iso3 = cou, timeframes = t) %>% mutate_at(vars(Product,Partner),as.character)
    if (nrow(dataComplete)>0) {
      if (nrow(thisData)>0) {
        dataComplete <- full_join(dataComplete,select(thisData,-starts_with("Country")), by = c("Indicator","Subindicator Type","Product","Partner"))
      }
    } else {
      dataComplete <- thisData
    }
  }
  return(dataComplete)
}

#get all country metadata in TCdata360
#df_countries <- get_metadata360(metadata_type = 'countries') %>% as_data_frame()

wc_countries <- c("Spain","Germany","Iceland","Portugal","France","Poland","Russia","United Kingdom","Sweden","Denmark","Croatia","Belgium","Switzerland","Serbia",
                  "Morocco","Tunisia","Egypt","Nigeria","Senegal","Australia","Japan","South Korea","Iran","Saudi Arabia","Mexico","Colombia","Panama",
                  "Costa Rica","Peru","Brazil","Argentina","Uruguay")

wc_data_lineup <- read.csv("wc_data_lineup.csv", stringsAsFactors = FALSE)
wc_data <- read.csv("wc_data.csv")
wc_data <- filter(wc_data, !grepl("currency",tolower(Indicator)))

# add ranks
wc_data_ranks <- group_by(wc_data, Indicator,Period) %>%
  filter(!is.na(CountryCode)) %>%
  mutate(rank = rank(Value,ties.method = "random")) %>%
  filter(rank <= 32)

# 
# 
# 
# 
# 
######## Matchup cou1 vs cou2 -------------------------------
######## Top 11 indicators for a given country
home_cou <- "SEN"
away_cou <- "COL"

data_vs <- inner_join(filter(wc_data_ranks, CountryCode == home_cou),filter(wc_data_ranks, CountryCode == away_cou), by = c("Indicator","Period")) %>%
  mutate(matchup = ifelse(Value.x-Value.y>0,1,ifelse(Value.x-Value.y<0 ,-1,0)),
         difference = (Value.x-Value.y)/abs((Value.x+Value.y+.001)),
         rank_diff = rank.x-rank.y) %>%
  distinct(Period,difference, .keep_all=TRUE) %>%
  group_by(Indicator) %>%
  filter(Period == max(Period)) %>%
  ungroup() %>%
  as.data.frame()

# Top 11 indicators where country 1 beats country 2
elite11_home <- select(data_vs, Indicator,Period,starts_with("Value"),starts_with("rank"), matchup,difference,rank_diff) %>%
  filter(matchup > 0) %>%
  top_n(30,difference) %>%
  arrange(desc(difference))

# Top 11 indicators where country 2 beats country 1
elite11_away <- select(data_vs, Indicator,Period,starts_with("Value"),starts_with("rank"),matchup,difference,rank_diff) %>%
  filter(matchup < 0) %>%
  top_n(30,-difference) %>%
  arrange(difference)

# Estimate matchup probabilites based on difference and sign for comparable indicators
prob_diff_cou1 <- round(100*sum(elite11_home$difference)/(sum(elite11_home$difference) - sum(elite11_away$difference)),1)
prob_matchup_cou1 <- round(100*nrow(filter(data_vs, matchup > 0))/(nrow(filter(data_vs, matchup > 0)) + nrow(filter(data_vs, matchup < 0))),1)

predicted_probs_diff <- paste0(home_cou,": ",prob_diff_cou1,"% - ",away_cou,": ",round(100-prob_diff_cou1,1),"%")
predicted_probs_matchup <- paste0(home_cou,": ",prob_matchup_cou1,"% - ",away_cou,": ",round(100-prob_matchup_cou1,1),"%")

cumulative_vs <- mutate(data_vs, cum_matchup = cumsum(matchup), cum_diff = cumsum(difference))
score_matchup_vs <- as.numeric(summarize(data_vs, sum(matchup)))
score_difference_vs <- as.numeric(summarize(data_vs, sum(difference)))

# Plotting
plot(cumulative_vs$cum_diff)

