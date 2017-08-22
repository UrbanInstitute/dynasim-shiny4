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
                             sheet = "Total Asset distribution", 
                             skip = 2, col_names = FALSE)
  
  # Turn the data into untidy data frames for each year with no missing values
  cleanBPC <- function(column1, column2, column3, column4, year) {
    # Cleans raw data frame from read_excel and returns an untidy data frame
    #
    # Args: column names and year
    #
    # Returns: 
    
    temp <- distribution %>%
      select_(column1, column2, column3, column4)
    
    names(temp) <- c("group", "subgroup", "Percent with Income Source", temp[1, 4:ncol(temp)])
    
    temp[2, "income.source"] <- "Total Assets"
    temp[83, "income.source"] <- "Financial Assets"
    temp[161, "income.source"] <- "Retirement Account Assets"
    temp[240, "income.source"] <- "Home Equity"
    
    temp <- temp %>%
      slice(-1) %>%
      mutate(year = year) %>%
      fill(group, income.source)
    
    temp <- temp %>%
      group_by(group) %>%
      mutate(subgroup = if_else(group == "All", "All", subgroup),
             subgroup = if_else(group == "Shared Income Quintile", paste(subgroup, "(Income)"), subgroup),
             subgroup = if_else(group == "Shared Lifetime Earnings Quintile", paste(subgroup, "(Lifetime Earnings)"), subgroup)) %>%
      ungroup()
    
    temp <- temp %>%
      filter(!is.na(P10) & P10 != "P10") %>%
      filter(!is.na(Mean) & !is.na(`Percent with Income Source`)) %>%
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
    gather(key = percentile, value = incomes.taxes, -group, -subgroup, -year, -income.source) %>%
    mutate(incomes.taxes = as.numeric(incomes.taxes)) %>%
    #    spread(income.source, value) %>%
    arrange(subgroup, year, percentile)
  
  # Create a variable for groups
  distribution <- distribution %>%
    mutate(subgroup = if_else(subgroup == "Male", "Males", subgroup),
           subgroup = if_else(subgroup == "High School Graduate", "High School Graduates", subgroup)) %>%
    mutate(group = if_else(group == "Race Ethnicity", "Race/Ethnicity", group),
           group = if_else(group == "Shared Income Quintile", "Income Quintile", group),
           group = if_else(group == "Shared Lifetime Earnings Quintile", "Lifetime Earnings Quintile", group))
  
  option_label <- enquo(option_label)
  scale_label <- enquo(scale_label)
  
  # Mutate numeric variables into class dbl, simplify quintiles, and add options/scales labels
  distribution <- distribution %>%
    #    mutate_at(vars(`Annuitized Financial Income`:`State Income Tax`), funs(as.numeric)) %>%
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

# Create a baseline data frame
baselines <- final.distribution %>%
  filter(option == "Scheduled Law" | option == "RothIRA2") %>%
  rename(baseline.value = incomes.taxes, baseline.type = option)

# Create a options data frame
options <- final.distribution

final.distribution <- left_join(options, baselines, by = c("subgroup", "year", "percentile", "group", "scale", "income.source"))

# Calculate the dollar and percent changes
final.distribution <- final.distribution %>%
  mutate(dollar.change = incomes.taxes - baseline.value) %>%
  select(-baseline.value)

# Clean up baselines so it matches final.income
baselines <- baselines %>%
  rename(incomes.taxes = baseline.value) %>%
  rename(option = baseline.type) %>%
  mutate(dollar.change = 0) %>%
  mutate(baseline.type = option)

# Combine the baselines (with zeroes for changes) and the options
final.distribution <- union(final.distribution, baselines) %>%
  filter(incomes.taxes != "BMB") %>%
  rename(baseline = baseline.type, level = incomes.taxes) %>%
  gather(level, dollar.change, key = "comparison", value = "value") %>%
  spread(key = income.source, value = value)

# Fix subgroups
final.distribution <- final.distribution %>%
  mutate(
    subgroup = if_else(subgroup == "All", "All Individuals", subgroup),
    subgroup = gsub("Female", "Females", subgroup),
    subgroup = if_else(subgroup == "Hispanic", "Hispanics", subgroup),    
    subgroup = gsub("Black nonHispanic", "African-Americans", subgroup),
    subgroup = gsub("White nonHispanic", "White, Non-Hispanics", subgroup),
    subgroup = gsub("No high school diploma", "High School Dropouts", subgroup),
    subgroup = gsub("High school graduate", "High School Graduates", subgroup),
    subgroup = gsub("Some college", "Some College", subgroup),  
    subgroup = gsub("College graduate", "College Graduates", subgroup), 
    subgroup = gsub("Married", "Married Individuals", subgroup), 
    subgroup = gsub("Never married", "Never Married Individuals", subgroup), 
    subgroup = gsub("Divorced", "Divorced Individuals", subgroup),   
    subgroup = gsub("Widowed", "Widowed Individuals", subgroup), 
    subgroup = gsub("Bottom quintile (income)", "Bottom Quintile (Income)", subgroup), 
    subgroup = gsub("2nd quintile (Income)", "Quintile 2 (Income)", subgroup), 
    subgroup = gsub("3rd quintile (Income)", "Quintile 3 (Income)", subgroup),
    subgroup = gsub("4th quintile (Income)", "Quintile 4 (Income)", subgroup),
    subgroup = gsub("Top quintile (Income)", "Top Quintile (Income)", subgroup),  
    subgroup = gsub("Bottom quintile", "Bottom Quintile", subgroup), 
    subgroup = gsub("2nd quintile", "Quintile 2", subgroup), 
    subgroup = gsub("3rd quintile", "Quintile 3", subgroup), 
    subgroup = gsub("4th quintile", "Quintile 4", subgroup), 
    subgroup = gsub("Top quintile", "Top Quintile", subgroup)
  ) %>%
  filter(group %in% c("All", "Sex", "Race/Ethnicity", "Education", "Marital Status", "Income Quintile", "Lifetime Earnings Quintile"))

rm(files, options, baselines)

# Stop if any of the variables contain missing values
stopifnot(sum(sapply(final.distribution, function(x) sum(is.na(x))) != 0) == 0)

# If data directory does not exist, create data directory
if (!dir.exists("data")) {
  dir.create("data")
}

# Write tidy data frame
write_csv(final.distribution, "data/assets.csv")
