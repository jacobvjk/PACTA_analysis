library(tidyverse)
library(readr)

peer_pf <- read_csv("/Users/jacobkastl/Downloads/swiss_p2020_peergroup_Input.csv")


funds_squashed <- read_rds("/Users/jacobkastl/Dropbox (2° Investing)/PortCheck/00_Data/03_FundData/Lipper/data/2019Q4/outputs/reduced_fund_holdings_2019Q4.rds")


funds_matched_only <- funds_squashed %>% 
  semi_join(peer_pf, by = c("fund_isin" = "ISIN"))

write_rds(funds_matched_only, paste0("/Users/jacobkastl/Dropbox (2° Investing)/PortCheck/00_Data/07_AnalysisInputs/2019Q4_08052020_2020/funds_2019Q4_reduced_for_meta.rds"))

