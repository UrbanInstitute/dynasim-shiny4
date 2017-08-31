## Libraries and Source Files
library(tidyverse)

options(scipen = 999)

# Load Data
income <- read_csv("data/incomes.csv",
                   col_types = cols(
                     .default = col_double(),
                     subgroup = col_character(),
                     year = col_integer(),
                     percentile = col_character(),
                     group = col_character(),
                     option = col_character(),
                     scale = col_character(),
                     baseline = col_character(),
                     comparison = col_character()
                   )
)

assets <- read_csv("data/assets.csv", 
                   col_types =  cols(
                     .default = col_double(),
                     group = col_character(),
                     subgroup = col_character(),
                     year = col_integer(),
                     percentile = col_character(),
                     option = col_character(),
                     scale = col_character(),
                     baseline = col_character(),
                     comparison = col_character()
                   )
)

# Clean and merge income and asset data
assets <- assets %>%
  filter(subgroup != "Other") %>%
  mutate(group = if_else(group == "All", "All Individuals", group),
         group = if_else(group == "Income Quintile", "Per Capita Income Quintile", group))

table(income$group %in% assets$group)
table(income$subgroup %in% assets$subgroup)     # PROBLEM!
table(income$year %in% assets$year)
table(income$percentile %in% assets$percentile)
table(income$option %in% assets$option)
table(income$scale %in% assets$scale)
table(income$baseline %in% assets$baseline)
table(income$comparison %in% assets$comparison)

distribution <- left_join(income, assets, by = c("group", "subgroup", "year", "percentile", "option", "scale", "baseline", "comparison"))

distribution <- distribution %>%
  select(-`DB Pension Income`, -`Earned Income`, -`HI Tax`, -`OASDI Tax`, 
         -`Other Family Member Income`, -`Own Benefit`, -`Own Earnings`,
         -`Dividend Income`, -`Interest Income`, -`Rental Income`, 
         -`Social Security Benefits`, -`Spouse Benefit`, -`Spouse Earnings`, 
         -`Home Equity`, -`Imputed Rental Income`) %>%
  filter(option != "Payable law")

rm(income, assets)
file.remove("data/incomes.csv")
file.remove("data/assets.csv")

write_csv(distribution, "data/distribution.csv")
