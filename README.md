# dynasim-shiny4

Interactive visualizations of DYNASIM projections of the distributions of wealth and incomes under different employer pension reforms and counterfactuals. 

## data

Data source: Urban Institute's Dynamic Simulation of Income Model (DYNASIM), 2017

## Scripts

### get-assets.R

Pulls and cleans data from the sheet "Total Asset distribution". 

### get-incomes.R

Pulls and cleans data from the sheet "income Distribution by Source".

### clean-and-merge.R

Combines and cleans the data from `get-assets.R` and `get-incomes.R`.

### /www

The /www subdirectory contains `shiny.css`. Shiny applications automatically look for material in the www subdirectory. 

### themes

The R Shiny graphic is built using the [Urban Institute R theme](https://github.com/UrbanInstitute/urban_R_theme). The theme works better using Mac OSX than Windows so `urban_theme_mac.R` is used when publishing the Shiny graphic and `urban_theme_windows.R` is used for developing edits and new features. 

**Note:** Lines at the top of `app.R` need to be commented out when switching between operating systems. 

## Built With
* R
* [Shiny](https://shiny.rstudio.com/)

## Authors
* Aaron Williams
* Karen Smith