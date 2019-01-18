##################### E-Commerce data preparation
# Dear Alberto: Thanks for getting back to me!
# Since 2017 we have partnered with UNCTAD in producing the E-Trade For All Indicators, a dataset that gives national indicators for the enabling environment for e-trade for the most recent year available.
# 2019 will be the third year we have done this. The dataset is made available on WITS at https://wits.worldbank.org/analyticaldata/e-trade/Country/USA/ 
# and in another format on UNCTAD’s website. The first step in updating the data is to update the spreadsheet attached, which underlies the 2018 version of the database.
# Last year, we did this by means of combining data from TC360 and special sources available to UNCTAD.  UNCTAD provides us with data from the Universal Postal Union (UPU), UNCTAD itself, and the International Telecommunications Union (ITU).  The rest we were able to get from TC360 last year.  I believe that the other data are all either WBG or WEF.
# In addition UNCTAD has requested that we produce the original spreadsheet in a format without the cumbersome headers, and including the ISO codes.  Vicky Chemutai, who will coordinate with you on this, has the details.
# Our goal is to have the revision assembled by the end of February at the latest, in order that UNCTAD may update their version in time for E-Commerce Week in Geneva the week of April 1.  So if you can provide us with the variables *other* than those coming from UPU, UNCTAD, ITU, along with the ISO codes, we would be very grateful.
#####################
#
library(tidyverse)
library(data360r)
#
indicators <- read.csv("eCommerce_TCdata360.csv", stringsAsFactors = FALSE)
#
indicator_ids <- data.frame()
for (i in 1:nrow(indicators)) {
  c <- trimws(indicators$indicatorName[i])
  thisInd <- search_360(c, search_type = 'indicator', site = 'tc', limit_results = 1) %>%
    select(id,name)
  print(thisInd)
  if (nrow(indicator_ids)>0) indicator_ids <- bind_rows(indicator_ids, thisInd) else indicator_ids <- thisInd
}















