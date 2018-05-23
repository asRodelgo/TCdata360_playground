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


######## Create full table all countries --------------------

wc_data <- data.frame()
for (c in wc_countries) {
  print(paste0("Processing... ",c))
  cou <- search_360(c, search_type = 'country')$slug[1]
  data1 <- getDataByChunks(cou, c(2017:2018))
  
  if (nrow(data1)>0){
    data1f <- prepareData(data1)
    if (nrow(wc_data)>0){
      wc_data <- bind_rows(wc_data,data1f) %>% as.data.frame() 
    } else {
      wc_data <- data1f
    }
  }
  print(paste0("Completed: ",c))
}
 
write.csv(wc_data, "wc_data.csv", row.names = FALSE)
# each indicator needs an extra indication of whether more is better than less and filtering and stuff
wc_data <- filter(wc_data, !grepl("currency",tolower(Indicator)))

# add ranks
wc_data_ranks <- group_by(wc_data, Indicator,Period) %>%
  filter(!is.na(CountryCode)) %>%
  mutate(rank = rank(Value,ties.method = "random")) %>%
  filter(rank <= 32)
 
######## Prepare data for lineup widget ---------------------

wc_data_lineup <- select(wc_data_ranks, -CountryCode, -rank) %>%
  distinct(CountryName,Indicator,Period, .keep_all=TRUE) %>%
  spread(Indicator,Value) %>%
  filter(Period == "2017")

write.csv(wc_data_lineup, "wc_data_lineup.csv", row.names = FALSE)
wc_data_lineup <- read.csv("wc_data_lineup.csv", stringsAsFactors = FALSE)

# probs diff matrix (uses top 11 indicators per country)
couCodes <- unique(wc_data_ranks$CountryCode)

matchup_matrix <- matrix(nrow = 32, ncol = 32, dimnames = list(countryA = couCodes, countryB = couCodes))
matchup_matrix_diff <- matrix(nrow = 32, ncol = 32, dimnames = list(countryA = couCodes, countryB = couCodes))
for (c1 in couCodes) {
  for (c2 in couCodes[-which(couCodes == c1)]) {
    print(paste0("Processing... ",c1," vs ", c2))
    data_vs <- inner_join(filter(wc_data_ranks, CountryCode == c1),filter(wc_data_ranks, CountryCode == c2), by = c("Indicator","Period")) %>%
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
      top_n(11,difference) %>%
      arrange(desc(difference))
    
    # Top 11 indicators where country 2 beats country 1
    elite11_away <- select(data_vs, Indicator,Period,starts_with("Value"),starts_with("rank"),matchup,difference,rank_diff) %>%
      filter(matchup < 0) %>%
      top_n(11,-difference) %>%
      arrange(difference)
    
    # Estimate matchup probabilites based on difference and sign for comparable indicators
    prob_diff_cou1 <- round(100*sum(elite11_home$difference)/(sum(elite11_home$difference) - sum(elite11_away$difference)),1)
    prob_matchup_cou1 <- round(100*nrow(filter(data_vs, matchup > 0))/(nrow(filter(data_vs, matchup > 0)) + nrow(filter(data_vs, matchup < 0))),1)
    matchup_matrix_diff[c1,c2] <- prob_diff_cou1
    matchup_matrix[c1,c2] <- prob_matchup_cou1
    print(paste0("Score: ",c1," ",round(prob_diff_cou1,2)," vs ", c2," ",round(100-prob_diff_cou1,2)))
  }
}

# heatmap visualization
library(d3heatmap)
d3heatmap(matchup_matrix, scale = "none", Rowv = FALSE, Colv = "Rowv", symm = TRUE, colors = "RdYlGn")


# 
# 
# 
# 
# 
# ######## Matchup cou1 vs cou2 -------------------------------
# ######## Top 11 indicators for a given country
# home_cou <- "PRT"
# away_cou <- "DEU"
# 
# data_vs <- inner_join(filter(wc_data_ranks, CountryCode == home_cou),filter(wc_data_ranks, CountryCode == away_cou), by = c("Indicator","Period")) %>%
#   mutate(matchup = ifelse(Value.x-Value.y>0,1,ifelse(Value.x-Value.y<0 ,-1,0)),
#          difference = (Value.x-Value.y)/abs((Value.x+Value.y+.001)),
#          rank_diff = rank.x-rank.y) %>%
#   distinct(Period,difference, .keep_all=TRUE) %>%
#   group_by(Indicator) %>%
#   filter(Period == max(Period)) %>%
#   ungroup() %>%
#   as.data.frame()
# 
# # Top 11 indicators where country 1 beats country 2
# elite11_home <- select(data_vs, Indicator,Period,starts_with("Value"),starts_with("rank"), matchup,difference,rank_diff) %>%
#   filter(matchup > 0) %>%
#   top_n(11,difference) %>%
#   arrange(desc(difference))
# 
# # Top 11 indicators where country 2 beats country 1
# elite11_away <- select(data_vs, Indicator,Period,starts_with("Value"),starts_with("rank"),matchup,difference,rank_diff) %>%
#   filter(matchup < 0) %>%
#   top_n(11,-difference) %>%
#   arrange(difference)
# 
# # Estimate matchup probabilites based on difference and sign for comparable indicators
# prob_diff_cou1 <- round(100*sum(elite11_home$difference)/(sum(elite11_home$difference) - sum(elite11_away$difference)),1)
# prob_matchup_cou1 <- round(100*nrow(filter(data_vs, matchup > 0))/(nrow(filter(data_vs, matchup > 0)) + nrow(filter(data_vs, matchup < 0))),1)
# 
# predicted_probs_diff <- paste0(home_cou,": ",prob_diff_cou1,"% - ",away_cou,": ",round(100-prob_diff_cou1,1),"%")
# predicted_probs_matchup <- paste0(home_cou,": ",prob_matchup_cou1,"% - ",away_cou,": ",round(100-prob_matchup_cou1,1),"%")
# 
# cumulative_vs <- mutate(data_vs, cum_matchup = cumsum(matchup), cum_diff = cumsum(difference))
# score_matchup_vs <- as.numeric(summarize(data_vs, sum(matchup)))
# score_difference_vs <- as.numeric(summarize(data_vs, sum(difference)))
# 
# # Plotting
# plot(cumulative_vs$cum_diff)
# 
