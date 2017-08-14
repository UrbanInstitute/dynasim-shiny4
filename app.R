## Libraries and Source Files
library(shiny)
library(tidyverse)
library(scales)

options(scipen = 999)

# Source file for Windows
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64.exe")
source('urban_institute_themes/urban_theme_windows.R')

# Source file for Mac
#source('urban_institute_themes/urban_theme_mac.R')

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

scale_text <- read_csv("text/scale.csv",
  col_types = cols(
    scale = col_character(),
    text = col_character()
  )
)

baseline_text <- read_csv("text/baseline.csv",
  col_types = cols(
    baseline = col_character(),
    text = col_character()
  )
)

income_tax_premium_text <- read_csv("text/income_tax_premium.csv",
  col_types = cols(
    income_tax_premium = col_character(),
    text = col_character()
  )
)

option_text <- read_csv("text/option.csv",
  col_types = cols(
    option = col_character(),
    text = col_character()
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

income <- left_join(income, assets, by = c("group", "subgroup", "year", "percentile", "option", "scale", "baseline", "comparison"))
rm(income, assets)

# Gather the data
distribution <- distribution %>%
  mutate(group = gsub("Per Capita ", "", group)) %>%
  mutate(subgroup = gsub(" \\(Income\\)", "", subgroup)) %>% 
  mutate(percentile = factor(percentile, levels = c("Mean", "P5", "P10", "P25", "P50", "P75", "P90", "P95", "P99", "Percent with Income Source"))) %>%
  mutate(subgroup = factor(subgroup, levels = c("All Individuals",
                                                "Females",
                                                "Males",
                                                "African-Americans",
                                                "Hispanics",
                                                "White, Non-Hispanics",
                                                "Bottom Quintile",
                                                "Quintile 2",
                                                "Quintile 3",
                                                "Quintile 4",
                                                "Top Quintile",
                                                "Never Married Individuals",
                                                "Divorced Individuals",
                                                "Married Individuals",
                                                "Widowed Individuals",
                                                "High School Dropouts",
                                                "High School Graduates",
                                                "Some College",
                                                "College Graduates"),
                                      labels = c("All Individuals",
                                                 "Female",
                                                 "Male",
                                                 "Black",
                                                 "Hispanic",
                                                 "White, Non-Hispanic",
                                                 "Bottom Quintile",
                                                 "2nd Quintile",
                                                 "3rd Quintile",
                                                 "4th Quintile",
                                                 "Top Quintile",
                                                 "Never Married",
                                                 "Divorced",
                                                 "Married",
                                                 "Widowed",
                                                 "HS Dropout",
                                                 "HS Graduate",
                                                 "Some College",
                                                 "College Graduate"))) %>%
  gather(`Annuitized Financial Income`:`Total Assets`, key = income.tax.premium, value = value)

##
## SHINY
##

latoCSS <- "http://fonts.googleapis.com/css?family=Lato:300,400,700,900,300italic,400italic,700italic,900italic"

ui <- fluidPage(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = latoCSS)),
  tags$head(tags$script(src = "pym.min.js")),
  
  theme = "shiny.css",
  
  fluidRow(
    
    column(12,
           
           titlePanel("Exploring Pension Reform Options")
           )
  ),
  
  
  fluidRow(
    column(10,
           style = "position:relative",
           
           h4(textOutput("title")),
           h5(textOutput("subtitlea")),
           h5(textOutput("subtitleb")),
           
           plotOutput("chart", width = "100%", height = "400px")
           
           )
  ),
    
  fluidRow(
    column(6, 
           
           sliderInput(inputId = "year", 
                       label = "Year",
                       min = 2015,
                       max = 2065, 
                       step = 10,
                       value = 2015,
                       sep = "",
                       animate = animationOptions(loop = TRUE, interval = 1500))
           ),
    
    column(6,
           
           htmlOutput("text5")
           
           
    )
  ),
  
  fluidRow(
    
    column(6,
      selectInput(inputId = "option",
                  label = "Pension Reform",
                  choices = c("Payable Law" = "Payable Law",
                              "Scheduled Law" = "Scheduled Law",
                              "BPC Option" = "BPC Package",
                              "RothIRA2" = "RothIRA2",
                              "RothIRA2allpart" = "RothIRA2allpart",
                              "RothIRA2b" = "RothIRA2b",
                              "RothIRA2c" = "RothIRA2c",
                              "RothIRA2d" = "RothIRA2d",
                              "RothIRA2e" = "RothIRA2e",
                              "RothIRA2f" = "RothIRA2f",
                              "RothIRA2g" = "RothIRA2g",
                              "RothIRA2nocashout" = "RothIRA2nocashout",
                              "RothIRA3" = "RothIRA3",
                              "RothIRA3b" = "RothIRA3b",
                              "RothIRA3c" = "RothIRA3c",
                              "RothIRA3d" = "RothIRA3d",
                              "RothIRA4" = "RothIRA4",
                              "RothIRA4b" = "RothIRA4b",
                              "RothIRA4c" = "RothIRA4c",
                              "RothIRA4d" = "RothIRA4d",
                              "RothIRAHighLimits" = "RothIRAHighLimits",
                              "RothIRALimit2" = "RothIRALimit2",
                              "RothIRALimit2b" = "RothIRALimit2b",
                              "RothIRALimit2c" = "RothIRALimit2c",
                              "RothIRALimit2d" = "RothIRALimit2d",
                              "RothIRALimit2e" = "RothIRALimit2e",
                              "RothIRALimit3" = "RothIRALimit3",
                              "RothIRALimit3b" = "RothIRALimit3b",
                              "RothIRALimit4" = "RothIRALimit4",
                              "SAVEopt2" = "SAVEopt2",
                              "SaveOpt2b" = "SaveOpt2b",
                              "SaveOpt2firm10" = "SaveOpt2firm10",
                              "SaveOpt3" = "SaveOpt3",
                              "SAVEopt3b" = "SAVEopt3b",
                              "SAVEopt4" = "SAVEopt4",
                              "SAVEopt4b" = "SAVEopt4b")),           

      selectInput(inputId = "baseline",
                  label = "Baseline",
                  choices = c("Current Law Payable" = "Payable Law",
                              "Current Law Scheduled" = "Scheduled Law")),      
      
      selectInput(inputId = "income.tax.premium",
                  label = "Income, Tax, or Premium",
                  choices = c("Annuitized Financial Income" = "Annuitized Financial Income",
                              "DB Pension Income" = "DB Pension Income",
                              "Earned Income" = "Earned Income",
                              "Federal Income Tax" = "Federal Income Tax",
                              "HI Tax" = "HI Tax",
                              "Imputed Rental Income" = "Imputed Rental Income",
                              "Means and Non-Means Tested Benefits" = "Means+Nonmeans Benefits",
                              "Medicare Part B Premium" = "Medicare Part B Premium",
                              "Medicare Surtax" = "Medicare Surtax",
                              "Net Annuity Income" = "Net Annuity Income",
                              "Net Cash Income" = "Net Cash Income",
                              "OASDI Tax" = "OASDI Tax",
                              "Other Family Member Income" = "Other Family Member Income",
                              "Own Benefit" = "Own Benefit",
                              "Own Earnings" = "Own Earnings",
                              "Annuity Income" = "Annuity Income",
                              "Cash Income" = "Cash Income",
                              "Dividend Income" = "Dividend Income",
                              "Interest Income" = "Interest Income",
                              "IRA Withdrawal" = "IRA Withdrawal",
                              "Rental Income" = "Rental Income",
                              "Social Security Benefits" = "Social Security Benefits",
                              "Spouse Benefit" = "Spouse Benefit",
                              "Spouse Earnings" = "Spouse Earnings",
                              "SSI" = "SSI",
                              "State Income Tax" = "State Income Tax"))),

    column(6, 
      selectInput(inputId = "comparison",
                  label = "Comparison",
                  choices = c("Level" = "level",
                              "Dollar Change" = "dollar.change")),
      
      selectInput(inputId = "group",
                  label = "Demographic",
                  choices = c("All Individuals" = "All Individuals",
                              "Sex" = "Sex",
                              "Race & Ethnicity" = "Race/Ethnicity",
                              "Education" = "Education",
                              "Marital Status" = "Marital Status",
                              "Income Quintile" = "Income Quintile",
                              "Lifetime Earnings Quintile" = "Lifetime Earnings Quintile")),
      
      selectInput(inputId = "scale",
                  label = "Scale",
                  choices = c("Per Capita" = "per capita",
                              "Equivalent" = "equivalent")))),
  
  fluidRow(
    column(12,
           downloadButton('download_data', 'Download Charted Data')
    )
  ),
  
  
    fluidRow(
    
    column(12,
    
    # Explanation of Social Security Reform
    
    htmlOutput("text1")
    )
  ),
  
  fluidRow(
    
    column(12,
           
      # Explanation of Income, Tax, or Premium
      
      htmlOutput("text2")
    )
    
  ),
  
  fluidRow(
    
    column(12,
           
      # Explanation of Scales
      
      htmlOutput("text3")
    )
    
  ),
  
  fluidRow(
    
    column(12,
           
           # Explanation of Baseline
           
           htmlOutput("text4")
    )
  ),
  tags$script(src = "activatePym.js")
)

server <- function(input, output) {
  
  options(shiny.sanitize.errors = FALSE)
  
  output$title <- renderText({
    
    comparison <- ifelse(input$comparison == "level", "", "Change in ")
    
    incomes.taxes <- if (input$income.tax.premium == "Annuitized Financial Income") {"Annuitized Financial Income"} else
    if (input$income.tax.premium == "DB Pension Income") {"Defined Benefit Pension Income"} else
    if (input$income.tax.premium == "Earned Income") {"Earned Income"} else
    if (input$income.tax.premium == "Federal Income Tax") {"Federal Income Tax"} else
    if (input$income.tax.premium == "HI Tax") {"Hospital Insurance Tax"} else
    if (input$income.tax.premium == "Imputed Rental Income") {"Imputed Rental Income"} else
    if (input$income.tax.premium == "Means and Non-Means Tested Benefits") {"Means and Non-Means Tested Benefits"} else
    if (input$income.tax.premium == "Medicare Part B Premium") {"Medicare Part B Premium"} else
    if (input$income.tax.premium == "Medicare Surtax") {"Medicare Surtax"} else
    if (input$income.tax.premium == "Net Annuity Income") {"Net Annuity Income"} else
    if (input$income.tax.premium == "Net Cash Income") {"Net Cash Income"} else
    if (input$income.tax.premium == "OASDI Tax") {"OASDI Tax"} else
    if (input$income.tax.premium == "Other Family Member Income") {"Other Family Member Income"} else
    if (input$income.tax.premium == "Own Benefit") {"Own Benefit"} else
    if (input$income.tax.premium == "Own Earnings") {"Own Earnings"} else
    if (input$income.tax.premium == "Annuity Income") {"Annuity Income"} else
    if (input$income.tax.premium == "Cash Income") {"Cash Income"} else
    if (input$income.tax.premium == "Dividend Income") {"Dividend Income"} else
    if (input$income.tax.premium == "Interest Income") {"Interest Income"} else
    if (input$income.tax.premium == "IRA Withdrawal") {"IRA Withdrawal"} else
    if (input$income.tax.premium == "Rental Income") {"Rental Income"} else
    if (input$income.tax.premium == "Social Security Benefits") {"Social Security Benefits"} else
    if (input$income.tax.premium == "Spouse Benefit") {"Spouse Benefit"} else
    if (input$income.tax.premium == "Spouse Earnings") {"Spouse Earnings"} else
    if (input$income.tax.premium == "SSI") {"SSI"} else
    if (input$income.tax.premium == "State Income Tax") {"State Income Tax"}
    
    paste(comparison, as.character(input$year), stringr::str_to_title(input$scale), incomes.taxes)
    
  })
  
  output$subtitlea <- renderText({
    
    if (input$comparison == "level") {
      input$option
    } else {
      paste(input$option, "vs.", input$baseline)
    }
    
  })
  
  output$subtitleb <- renderText({
    
    if (input$group == "All Individuals") {"Everyone Ages 62+, 2015 dollars"} else
    if (input$group == "Sex") {"Ages 62+ by Sex, 2015 dollars"} else
    if (input$group == "Race/Ethnicity") {"Ages 62+ by Race & Ethnicity, 2015 dollars"} else
    if (input$group == "Education") {"Ages 62+ by Education, 2015 dollars"} else
    if (input$group == "Marital Status") {"Ages 62+ by Marital Status, 2015 dollars"} else
    if (input$group == "Income Quintile") {"Ages 62+ by Shared Income Quintile, 2015 dollars"} else
    if (input$group == "Lifetime Earnings Quintile") {"Ages 62+ by Shared Lifetime Earnings Quintile, 2015 dollars"}
  
    })

  data_subset <- reactive({
    distribution %>%
      filter(option == input$option) %>%
      filter(group == input$group) %>%  

      filter(comparison == input$comparison) %>%   
      filter(baseline == input$baseline) %>% 
      filter(scale == input$scale) %>%
      filter(income.tax.premium == input$income.tax.premium) %>%
      filter(percentile != "Percent with Income Source")
  })  
  
  output$chart <- renderPlot({  

    # Calculate the maximum for the y-axis (because of the animation)
    y.max <- data_subset() %>%
      summarize(max = max(value))

    # Calculate the minimum for the y-axis (because of the animation)
    y.min <- data_subset() %>%
      summarize(min = min(value))
    
    y.min <- min(0, as.numeric(y.min))
    
    print(y.max)
    print(y.min)

    graphr <- function(origin, line.placement, line.color){
    
      filter(data_subset(), year == input$year) %>%  
        ggplot() +
          geom_bar(aes(x = percentile, y = value, fill = subgroup), position = "dodge", stat = "identity") +
          scale_y_continuous(limits = c(y.min, as.numeric(y.max)), labels = scales::dollar) +
          labs(caption = "DYNASIM3",
               x = "Mean and Percentiles",
               y = NULL) +
          expand_limits(y = origin) +
          geom_hline(size = 0.5, aes(yintercept = line.placement), color = line.color) +
          theme(axis.ticks.length = unit(0, "points"),
                axis.line = element_blank())
    
    }
      
    if (input$comparison == "level") {
      graphr(origin = NULL, line.placement = 0, line.color = "black") 
    } 
    else if (input$comparison == "dollar.change") {
      graphr(origin = 0, line.placement = 0, line.color = "black")
    } 
    
  })  
  
    output$text1 <- renderText({
    
      as.character(
        option_text %>%
          filter(option == input$option) %>%
          select(text)
      )
    
    })
    
    output$text2 <- renderText({
      
      as.character(
        income_tax_premium_text %>%
          filter(income_tax_premium == input$income.tax.premium) %>%
          select(text)
      )
      
    })
    
    output$text3 <- renderText({
      
      as.character(
        scale_text %>%
          filter(scale == input$scale) %>%
          select(text)
      )
  
    })
    
    output$text4 <- renderText({
      
      as.character(
        baseline_text %>%
          filter(baseline == input$baseline) %>%
          select(text)
      )
      
    })
    
    output$text5 <- renderUI({
      
      percent <- distribution %>%
        filter(option == input$option) %>%
        filter(group == "All Individuals") %>%  
        filter(year == input$year) %>%
        filter(comparison == input$comparison) %>%   
        filter(baseline == input$baseline) %>% 
        filter(scale == input$scale) %>%
        filter(income.tax.premium == input$income.tax.premium) %>%
        filter(percentile == "Percent with Income Source") %>% 
        select(value)
      
      if (input$comparison == "level") {
      HTML(paste("<div class='income-percent'>", as.character(round(percent * 100, 1)), "%", "</div>","<div class='income-text'>", "have", "<b>", input$income.tax.premium, "</b>", "</div>"))
      } else {}
        
    })
    
    output$download_data <- downloadHandler(
      filename = function() { paste0(input$option, '.csv') },
      content = function(file) {
        write_csv(data_subset(), file)
      }
    )
    
}
    
shinyApp(ui = ui, server = server)