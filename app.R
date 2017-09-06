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

distribution <- read_csv("data/distribution.csv",
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

demographic <- read_csv("text/demographic.csv",
  col_types = cols(
    demographic = col_character(),
    description = col_character()
  )
)

# Gather the data
distribution <- distribution %>%
  rename(`Medicare Surtax` = `Medicare SurTax`) %>%
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
                                      labels = c("All individuals",
                                                 "Female",
                                                 "Male",
                                                 "Black",
                                                 "Hispanic",
                                                 "White, non-Hispanic",
                                                 "Bottom quintile",
                                                 "2nd quintile",
                                                 "3rd quintile",
                                                 "4th quintile",
                                                 "Top quintile",
                                                 "Never married",
                                                 "Divorced",
                                                 "Married",
                                                 "Widowed",
                                                 "HS dropout",
                                                 "HS graduate",
                                                 "Some college",
                                                 "College graduate"))) %>%
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
           
           p("Defined-contribution pensions are an important source of 
             retirement savings for future retirees. How can more Americans 
             access the benefits of DC pensions and better prepare for 
             retirement? Use this interactive tool to explore the impacts of DC 
             pension reform across the income and wealth distributions for 
             different populations from 2015 to 2065.")
           
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
                  choices = c("Scheduled law" = "Scheduled law",
                              "BPC package" = "BPC package",
                              "Low fees" = "Low fees",
                              "Rebalance every 5 years" = "Rebalance every 5 years",
                              "Low participation" = "Low participation",
                              "High participation" = "High participation",
                              "Less risk" = "Less risk",
                              "More risk" = "More risk",
                              "No target date funds" = "No target date funds",
                              "No auto-enrollment" = "No auto-enrollment",
                              "No cash outs" = "No cash outs",
                              "All Roth-401k accounts #1" = "All Roth-401k accounts #1",
                              "All Roth-401k accounts #2" = "All Roth-401k accounts #2",
                              #"Mandated employer plans (60%)" = "Mandated employer plans (60%)",
                              #"Mandated employer plans (100%)" = "Mandated employer plans (100%)",
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
                  choices = c("Scheduled law" = "Scheduled law",
                              "RothIRA2" = "RothIRA2")),      
      
      selectInput(inputId = "income.tax.premium",
                  label = "Income, Tax, or Asset",
                  choices = c("Total assets" = "Total Assets",
                              "Financial assets" = "Financial Assets",
                              "Retirement account assets" = "Retirement Account Assets",
                              "Annuitized financial income" = "Annuitized Financial Income",
                              "Federal income tax" = "Federal Income Tax",
                              "Medicare Part B premium" = "Medicare Part B Premium",
                              "Medicare surtax" = "Medicare Surtax",
                              "Net annuity income" = "Net Annuity Income",
                              "Net cash income" = "Net Cash Income",
                              "Gross annuity income" = "Annuity Income",
                              "Gross cash income" = "Cash Income",
                              "IRA withdrawal" = "IRA Withdrawal",
                              "Supplemental Security Income" = "SSI",
                              "State income tax" = "State Income Tax"))),

    column(6, 
      selectInput(inputId = "comparison",
                  label = "Comparison",
                  choices = c("Level" = "level",
                              "Dollar change" = "dollar.change")),
      
      selectInput(inputId = "group",
                  label = "Demographic",
                  choices = c("All individuals" = "All Individuals",
                              "Sex" = "Sex",
                              "Race or ethnicity" = "Race/Ethnicity",
                              "Education" = "Education",
                              "Marital status" = "Marital Status",
                              "Income quintile" = "Income Quintile",
                              "Lifetime earnings quintile" = "Lifetime Earnings Quintile")),
      
      selectInput(inputId = "scale",
                  label = "Scale",
                  choices = c("Per capita" = "per capita",
                              "Equivalent" = "equivalent")))),
  
  fluidRow(
    column(12,
           downloadButton('download_data', 'Download charted data')
    )
  ),
  
  br(),
  
  fluidRow(
    
    column(12,
           
           # Explanation of Social Security Reform
           
           htmlOutput("text_option")
    )
  ),
  
  fluidRow(
    
    column(12,
           
           # Explanation of Baseline
           
           htmlOutput("text_baseline")
    )
  ),
  
  fluidRow(
    
    column(12,
           
           # Explanation of Income, Tax, or Premium
           
           htmlOutput("text_income_tax_premium")
    )
    
  ),
  
  fluidRow(
    
    column(12, 
           
           # Explanation of Demographic
           
           htmlOutput("text_demographic")
    )
  ),
  
  fluidRow(
    
    column(12,
           
           # Explanation of Scales
           
           htmlOutput("text_scales")
    )
    
  ),
  tags$script(src = "activatePym.js")
)

server <- function(input, output) {
  
  options(shiny.sanitize.errors = FALSE)
  
  output$title <- renderText({
    
    comparison <- ifelse(input$comparison == "level", "", "Change in ")
    
    incomes.taxes <- if (input$income.tax.premium == "Annuitized Financial Income") {"annuitized financial income"} else
    if (input$income.tax.premium == "DB Pension Income") {"defined-benefit pension income"} else
    if (input$income.tax.premium == "Earned Income") {"earned income"} else
    if (input$income.tax.premium == "Federal Income Tax") {"federal income tax"} else
    if (input$income.tax.premium == "HI Tax") {"Hospital Insurance Program tax"} else
    if (input$income.tax.premium == "Imputed Rental Income") {"imputed rental income"} else
    if (input$income.tax.premium == "Means and Non-Means Tested Benefits") {"means- and non-means tested benefits"} else
    if (input$income.tax.premium == "Medicare Part B Premium") {"Medicare Part B premium"} else
    if (input$income.tax.premium == "Medicare Surtax") {"Medicare surtax"} else
    if (input$income.tax.premium == "Net Annuity Income") {"net annuity income"} else
    if (input$income.tax.premium == "Net Cash Income") {"net cash income"} else
    if (input$income.tax.premium == "OASDI Tax") {"OASDI tax"} else
    if (input$income.tax.premium == "Other Family Member Income") {"other family member income"} else
    if (input$income.tax.premium == "Own Benefit") {"own benefit"} else
    if (input$income.tax.premium == "Own Earnings") {"own earnings"} else
    if (input$income.tax.premium == "Annuity Income") {"gross annuity income"} else
    if (input$income.tax.premium == "Cash Income") {"gross cash income"} else
    if (input$income.tax.premium == "Dividend Income") {"dividend income"} else
    if (input$income.tax.premium == "Interest Income") {"interest income"} else
    if (input$income.tax.premium == "IRA Withdrawal") {"IRA withdrawal"} else
    if (input$income.tax.premium == "Rental Income") {"rental income"} else
    if (input$income.tax.premium == "Social Security Benefits") {"Social Security benefits"} else
    if (input$income.tax.premium == "Spouse Benefit") {"spouse benefit"} else
    if (input$income.tax.premium == "Spouse Earnings") {"spouse earnings"} else
    if (input$income.tax.premium == "SSI") {"Supplemental Security Income"} else
    if (input$income.tax.premium == "State Income Tax") {"state income tax"} else
    if (input$income.tax.premium == "Financial Assets") {"financial assets"} else
    if (input$income.tax.premium == "Retirement Account Assets") {"retirement account assets"} else
    if (input$income.tax.premium == "Total Assets") {"total assets"}    
  
    paste(comparison, as.character(input$year), input$scale, incomes.taxes)
    
  })
  
  output$subtitlea <- renderText({
    
    if (input$comparison == "level") {
      input$option
    } else {
      paste(input$option, "vs.", tolower(input$baseline))
    }
    
  })
  
  output$subtitleb <- renderText({
    
    if (input$group == "All Individuals") {"Everyone ages 62+, 2015 dollars"} else
    if (input$group == "Sex") {"Ages 62+ by sex, 2015 dollars"} else
    if (input$group == "Race/Ethnicity") {"Ages 62+ by race or ethnicity, 2015 dollars"} else
    if (input$group == "Education") {"Ages 62+ by dducation, 2015 dollars"} else
    if (input$group == "Marital Status") {"Ages 62+ by marital status, 2015 dollars"} else
    if (input$group == "Income Quintile") {"Ages 62+ by shared income quintile, 2015 dollars"} else
    if (input$group == "Lifetime Earnings Quintile") {"Ages 62+ by shared lifetime earnings quintile, 2015 dollars"}
  
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
               x = "Mean and percentiles",
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
  
  output$text_option <- renderText({
    
    as.character(
      option_text %>%
        filter(option == input$option) %>%
        select(text)
    )
    
  })
  
  output$text_income_tax_premium <- renderText({
    
    as.character(
      income_tax_premium_text %>%
        filter(income_tax_premium == input$income.tax.premium) %>%
        select(text)
    )
    
  })
  
  output$text_scales <- renderText({
    
    as.character(
      scale_text %>%
        filter(scale == input$scale) %>%
        select(text)
    )
    
  })
  
  output$text_baseline <- renderText({
    
    as.character(
      baseline_text %>%
        filter(baseline == input$baseline) %>%
        select(text)
    )
    
  })
  
  output$text_demographic <- renderText({
    
    as.character(
      demographic %>%
        filter(demographic == input$group) %>%
        select(description)
    )
    
  })
  
  output$text_have_income <- renderUI({
    
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