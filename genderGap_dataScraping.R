####################### WEF Gender gap data scraping
# From Reg:
# I did some snooping around the WEF GGGR 2018 site and saw their Data Explorer page. At the backend, it calls a JSON object with all the 
# country-level pillar and indicator data there (plus indicator descriptions!), so I think you can simply pull the JSON and tidy it into a dataframe 
# in R/Python. Here's the JSON link which I got via snooping around:
# http://reports.weforum.org/global-gender-gap-report-2018/wp-content/themes/wef-reports/wef-components/dist/static/data/ggi/2017-2018/data.json
#######################
#
library(tidyverse)
library(jsonlite)
library(magrittr)
#
data <- read_json("http://reports.weforum.org/global-gender-gap-report-2018/wp-content/themes/wef-reports/wef-components/dist/static/data/ggi/2017-2018/data.json", simplifyDataFrame = TRUE)
data <- as.tibble(data$countries)
data_ids <- map_df(data, extract, c("code","name"))
data_if <- map_if(data, )
#data_gender <- map_df(data, extract, c("Global Gender Gap score","Economic participation and opportunity","Educational attainment","Health and survival","Political empowerment","rank out of"))
data_gender <- map_dbl(data, c("score"))




data_gender <- map(data, extract, c("2018"))
data_gender <- map_df(data, extract, c("code","name","rank","score"))












