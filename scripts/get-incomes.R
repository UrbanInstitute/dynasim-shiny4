# Aaron Williams, Urban Institute Program on Retirement Policy

# This script reads mean income data for 64 groups, across 4 measures, in 6 
# different decades, for user-defined reform options, in two scales. It then 
# calculates dollar change against two different baselines. 
# The script also cleans the data and turns them into a long data frame 
# formatted for ggplot2 

# Library, Source, and Options Statements
library(tidyverse)
library(readxl)

options(scipen = 999)

# Read df with links to the Excel sheets with the mean income data
files <- read_csv("options-guide.csv",
                  col_types = cols(
                    option.name = col_character(),
                    option = col_character(),
                    scale = col_character(),
                    link = col_character(),
                    directory = col_character(),
                    file = col_character(),
                    bpc_boolean = col_logical()
                  )) %>%
  select(option, scale, link, bpc_boolean)

# Create functions that clean the clunky Excel files
distribution_scraper <- function(link, bpcpackage, option_label, scale_label) {

  distribution <- read_excel(link, 
                             sheet = "income Distribution by Source", 
                             skip = 4, col_names = FALSE)
  
  # Turn the data into untidy data frames for each year with no missing values
  cleanBPC <- function(column1, column2, column3, column4, year) {
    # Cleans raw data frame from read_excel and returns an untidy data frame
    #
    # Args: column names and year
    #
    # Returns: 
    
    temp <- distribution %>%
      select_(column1, column2, column3, column4)
    
    names(temp) <- c("subgroup", "income.source", "Percent with Income Source", temp[1, 4:ncol(temp)])
    
    temp <- temp %>%
      slice(-1) %>%
      mutate(year = year) %>%
      mutate(income.source = gsub("Per Capita ", "", income.source)) %>%
      mutate(income.source = gsub("Equivalent ", "", income.source)) %>%
      mutate(income.source = gsub("tax", "Tax", income.source))
    
    if (bpcpackage == TRUE) {
      temp[566, 1] <- "Bottom Quintile (Income)"
      temp[595, 1] <- "Quintile 2 (Income)"
      temp[624, 1] <- "Quintile 3 (Income)"
      temp[653, 1] <- "Quintile 4 (Income)"
      temp[682, 1] <- "Top Quintile (Income)"
      temp[750, 1] <- "Bottom Quintile (Lifetime Earnings)"
      temp[779, 1] <- "Quintile 2 (Lifetime Earnings)"
      temp[808, 1] <- "Quintile 3 (Lifetime Earnings)"
      temp[837, 1] <- "Quintile 4 (Lifetime Earnings)"
      temp[866, 1] <- "Top Quintile (Lifetime Earnings)"  
    }
    
    if (bpcpackage == FALSE) {
      temp[548, 1] <- "Bottom Quintile (Income)"
      temp[576, 1] <- "Quintile 2 (Income)"
      temp[604, 1] <- "Quintile 3 (Income)"
      temp[632, 1] <- "Quintile 4 (Income)"
      temp[660, 1] <- "Top Quintile (Income)"
      temp[726, 1] <- "Bottom Quintile (Lifetime Earnings)"
      temp[754, 1] <- "Quintile 2 (Lifetime Earnings)"
      temp[782, 1] <- "Quintile 3 (Lifetime Earnings)"
      temp[810, 1] <- "Quintile 4 (Lifetime Earnings)"
      temp[838, 1] <- "Top Quintile (Lifetime Earnings)"    
    }
    
    while (sum(is.na(temp$subgroup)) > 1) {
      
      temp <- temp %>%
        mutate(subgroup = ifelse(is.na(subgroup), lag(subgroup), subgroup))
      
    }
    
    temp <- temp %>%
      filter(!is.na(P10) & P10 != "P10") %>%
      filter(!is.na(income.source)) %>%
      mutate_all(funs(trimws)) %>%
      distinct()
    
    return(temp)
    
  }
  
  # TODO(awunderground): Add check to see if duplicated rows are only the repeated
  # charts in the Excel files
  
  distribution <- bind_rows(
    cleanBPC(column1 = "X__1",  column2 = "X__2",  column3 = "X__4", column4 = "X__6:X__14",  year = 2015),
    cleanBPC(column1 = "X__16", column2 = "X__17", column3 = "X__18", column4 = "X__20:X__28", year = 2025),
    cleanBPC(column1 = "X__30", column2 = "X__31", column3 = "X__32", column4 = "X__34:X__42", year = 2035),
    cleanBPC(column1 = "X__44", column2 = "X__45", column3 = "X__46", column4 = "X__48:X__56", year = 2045),
    cleanBPC(column1 = "X__58", column2 = "X__59", column3 = "X__60", column4 = "X__62:X__70", year = 2055),
    cleanBPC(column1 = "X__72", column2 = "X__73", column3 = "X__74", column4 = "X__76:X__84", year = 2065)
  )
  
  # create tidy data frame
  distribution <- distribution %>%
    gather(key = percentile, value = value, -subgroup, -income.source, -year) %>%
    spread(income.source, value) %>%
    arrange(subgroup, year, percentile)
  
  # Create a variable for groups
  distribution <- distribution %>%
    mutate(subgroup = ifelse(subgroup == "Male", "Males", subgroup)) %>%
    mutate(subgroup = ifelse(subgroup == "High School Graduate", "High School Graduates", subgroup)) %>%
    mutate(group = ifelse(subgroup == "All Individuals", "All Individuals", NA)) %>%
    mutate(group = ifelse(subgroup %in% c("Males", "Females"), "Sex", group)) %>%
    mutate(group = ifelse(subgroup %in% c("High School Dropouts",
                                          "High School Graduates", 
                                          "Some College", "College Graduates"),
                          "Education", group)) %>%
    mutate(group = ifelse(subgroup %in% c("African-Americans", 
                                          "Hispanics",
                                          "White, Non-Hispanics",
                                          "Other"),
                          "Race/Ethnicity", group)) %>%
    mutate(group = ifelse(subgroup %in% c("Never Married Individuals",
                                          "Divorced Individuals",
                                          "Married Individuals",
                                          "Widowed Individuals"),
                          "Marital Status", group)) %>%
    mutate(group = ifelse(subgroup %in% c("Bottom Quintile (Income)", 
                                          "Quintile 2 (Income)", 
                                          "Quintile 3 (Income)",
                                          "Quintile 4 (Income)", 
                                          "Top Quintile (Income)"), 
                          "Per Capita Income Quintile", group)) %>%
    mutate(group = ifelse(subgroup %in% c("Bottom Quintile (Lifetime Earnings)",
                                          "Quintile 2 (Lifetime Earnings)",
                                          "Quintile 3 (Lifetime Earnings)",
                                          "Quintile 4 (Lifetime Earnings)",
                                          "Top Quintile (Lifetime Earnings)"), 
                          "Lifetime Earnings Quintile", group))
  
  option_label <- enquo(option_label)
  scale_label <- enquo(scale_label)
  
  # Mutate numeric variables into class dbl, simplify quintiles, and add options/scales labels
  distribution <- distribution %>%
    mutate_at(vars(`Annuitized Financial Income`:`State Income Tax`), funs(as.numeric)) %>%
    mutate(subgroup = gsub(" \\(Lifetime Earnings\\)", "", subgroup),
           subgroup = gsub(" \\(Per Capita Income\\)", "", subgroup)) %>%
    mutate(option = !!option_label,
           scale = !!scale_label)
  
  # Drop the 99th percentile
  distribution <- distribution %>%
    filter(percentile != "P99")
  
  return(distribution)
}

# iterate function
final.distribution <- pmap(list(files$link, files$bpc_boolean, files$option, files$scale), distribution_scraper) %>%
  reduce(bind_rows)

# Should be 82,944 observations
# 24 subgroups * 6 years * 9 percentiles * 64 options
stopifnot(nrow(final.distribution) == 82944)

# Spread the data into long format
final.distribution <- final.distribution %>%
  gather(c(`Annuitized Financial Income`:`State Income Tax`), key = "incomes.taxes", value = "value") %>%
  select(-BMB)
# TODO(aaron): may require re-adding BMB

# Should be 979,776 observations
# 64 subgroups * 6 years * 9 percentiles * 28 options * 28 incomes/taxes/premiums
#stopifnot(nrow(final.distribution) == 979776)

# Create a baseline data frame
baselines <- final.distribution %>%
  filter(option == "Scheduled Law" | option == "RothIRA2") %>%
  rename(baseline.value = value, baseline.type = option)

# Create a options data frame
options <- final.distribution
# Should be 1,378,944 observations

final.distribution <- left_join(options, baselines, by = c("subgroup", "year", "percentile", "group", "scale", "incomes.taxes"))

# Should be 1,959,552 observations
# 24 subgroups * 6 years * 9 percentiles * 28 options * 28 incomes/taxes/premiums
#stopifnot(nrow(final.distribution) == 1959552)

# Calculate the dollar and percent changes
final.distribution <- final.distribution %>%
  mutate(dollar.change = value - baseline.value) %>%
  select(-baseline.value)

# Clean up baselines so it matches final.income
baselines <- baselines %>%
  rename(value = baseline.value) %>%
  rename(option = baseline.type) %>%
  mutate(dollar.change = 0) %>%
  mutate(baseline.type = option)

# Combine the baselines (with zeroes for changes) and the options
final.distribution <- union(final.distribution, baselines) %>%
#  filter(incomes.taxes != "BMB") %>%
  rename(baseline = baseline.type, level = value) %>%
  gather(level, dollar.change, key = "comparison", value = "value") %>%
  spread(key = incomes.taxes, value = value)

# Should be 145,152
# 24 subgroups * 6 years * 9 percentiles * 28 options * 2 scales * 2 baselines
#stopifnot(nrow(final.distribution) == 145152)

rm(files, options, baselines)

# Stop if any of the variables contain missing values
stopifnot(sum(sapply(final.distribution, function(x) sum(is.na(x))) != 0) == 0)

# If data directory does not exist, create data directory
if (!dir.exists("data")) {
  dir.create("data")
}

# Write tidy data frame
write_csv(final.distribution, "data/incomes.csv")
