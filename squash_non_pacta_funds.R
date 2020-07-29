library(tidyverse)
library(lobstr)

source("0_portfolio_input_check_initialisation.R")

fund_data <- read_csv("/Users/jacobkastl/Dropbox (2° Investing)/PortCheck/00_Data/03_FundData/Lipper/projects/EIOPA/2019Q4/outputs/EIOPA_fund_data_2019Q4.csv")
# fund_data <- read_rds("/Users/jacobkastl/Dropbox (2° Investing)/PortCheck/00_Data/03_FundData/Lipper/data/2019Q4/outputs/downloaded_fund_holdings_2019Q4.rds")
# fund_data <- fund_data %>%
#   select(fund_isin,holding_isin = isin,isin_weight = weight, fund_type = source)


lobstr::obj_size(fund_data)


-#----------------preprocess fund data - squash OTHER-------------------

# join the sector info of the fin data to the fund data and group all non pacta sectors into one row per fund
# in order to save rows/memory

# copied this from inside get_and_clean_fund_data()
fund_data <- fund_data %>% janitor::clean_names()
fund_data <- fund_data %>% filter(!is.na(holding_isin) & holding_isin != "")
fund_data <- normalise_fund_data(fund_data)


fin_data_prep <- get_and_clean_fin_data(fund_data)

fund_reduced <- fund_data %>% 
  left_join((fin_data_prep %>% select(isin, security_mapped_sector)), by = c("holding_isin" = "isin"))


non_pacta_sectors <- fund_reduced %>% 
  filter(security_mapped_sector =="Other") %>%
  mutate(holding_isin = "OtherSectors") %>% 
  group_by(fund_isin, holding_isin,
           fund_type, security_mapped_sector) %>% 
  summarise(isin_weight = sum(isin_weight, na.rm = T),
            fund_type = max(fund_type, na.rm = T)) %>% 
  ungroup()

non_pacta_sectors %>% ggplot(aes(isin_weight)) +
  geom_histogram()

fund_reduced_out <- fund_reduced %>%
  filter(security_mapped_sector != "Other") %>%
  bind_rows(non_pacta_sectors) %>% 
  normalise_fund_data()

fund_reduced_out_verify <- fund_reduced_out %>% 
  group_by(fund_isin) %>% 
  summarise(isin_weight = sum(isin_weight, na.rm = T)) %>% 
  ungroup()


fund_reduced_verify <- fund_reduced %>% 
  group_by(fund_isin) %>% 
  summarise(initial_weight = sum(isin_weight, na.rm = T)) %>% 
  ungroup()

fund_reduced_out_verify %>%
  full_join(fund_reduced_verify, by = c("fund_isin")) %>% 
  mutate(diff = isin_weight - initial_weight) %>%
  ggplot(aes(diff)) +
  geom_histogram()


# save the fund data with the squashed isins from non relevant sectors
fund_squashed <- fund_reduced_out %>% 
  select(-security_mapped_sector)

lobstr::obj_size(fund_squashed)

#write_rds(fund_squashed, "/Users/jacobkastl/Dropbox (2° Investing)/PortCheck/00_Data/03_FundData/Lipper/projects/EIOPA/2019Q4/outputs/downloaded_fund_holdings_2019Q4_reduced.rda")
write_rds(fund_squashed, "/Users/jacobkastl/Dropbox (2° Investing)/PortCheck/00_Data/03_FundData/Lipper/projects/EIOPA/2019Q4/outputs/EIOPA_fund_data_2019Q4_reduced.rda")
write_csv(fund_squashed, "/Users/jacobkastl/Dropbox (2° Investing)/PortCheck/00_Data/03_FundData/Lipper/projects/EIOPA/2019Q4/outputs/EIOPA_fund_data_2019Q4_reduced.csv")

