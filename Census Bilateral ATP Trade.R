###############################################################################################################################

# README
# Project:    Advanced Technology Products Trade between the US and Other Countries
# Objective:  From the Census API, download the US Census Bureau bilateral trade in ATP goods

# US Census API Setup
# Census API Key request here:          http://api.census.gov/data/key_signup.html

# In the API call: months must be specified as "\d\d" ("01", "02")
# The as.yearqtr function converts the underlying date value to the first day-month of the quarter 

# For help on Census international trade data API
# https://www.census.gov/foreign-trade/reference/guides/Guide%20to%20International%20Trade%20Datasets.pdf

# To run, replace "[key]" and "[path]" below

###############################################################################################################################

# Run before every program to clear your workspace
rm(list=ls())
cat("\014") 

# Root paths
folder <- "[path]"
analysis <- paste(folder, "Analysis\\", sep = "")

# Set beginning date
initial_year <- "2013"
initial_month <- "01"

# Set end date
final_year  <- "2018"
final_month <- "12"

### PATHS AND PARAMETERS ###

# Census key, obtained from Census website above
census_key <- "[key]"

# Set analysis parameters and vars you want to grab. Currently set to Germany.
# Parameters
ctry_code   <- 4280    
# Vars
import_vars <- paste("CTY_CODE","CTY_NAME","HITECH","HITECH_DESC","GEN_VAL_MO","GEN_VAL_YR", sep=",")
export_vars <- paste("CTY_CODE","CTY_NAME","HITECH","HITECH_DESC","ALL_VAL_MO","ALL_VAL_YR", sep=",")

###############################################################################################################################

### PULL DATA FROM CENSUS API###

# Set location
Sys.setlocale("LC_ALL","C")

# Set working directory
setwd(folder)

# Load libraries
library("tidyverse")  
library("RJSONIO")    
library("sqldf")      
library("ggthemes")   
library("zoo")        
library("dplyr")      
library("tools")
library("lubridate") 
library("reshape2")
library("formattable")

# Set current quarter and month
year_month  <- paste("from+",initial_year,"-",initial_month,"+to+",final_year,"-",final_month, sep="")
current_month <- as.Date(paste(final_year,"-",final_month,"-02",sep=""), format = "%Y-%m-%d")
current_quarter <- as.yearqtr(current_month)
format(current_quarter, format = "20%y Q%q")

# Function to extract imports data
get_imports <- function(census_key, year_month, import_vars, ctry_code) {
  
  imp_resURL <- paste("https://api.census.gov/data/timeseries/intltrade/imports/",
                      "hitech?get=",import_vars,"&time=",year_month, 
                      "&CTY_CODE=",ctry_code,"&key=",census_key,sep="")
  
  imp_lJSON <- fromJSON(imp_resURL) # convert JSON content to R objects
  
  imp_lJSON <- imp_lJSON[2:length(imp_lJSON)]         # keep everything but the 1st element (var names) in lJSON
  imp_lJSON.cc <- sapply(imp_lJSON,function(x) x[1])  # extract country code
  imp_lJSON.cn <- sapply(imp_lJSON,function(x) x[2])  # extract country name
  imp_lJSON.hic <- sapply(imp_lJSON,function(x) x[3]) # extract high tech product code
  imp_lJSON.hid <- sapply(imp_lJSON,function(x) x[4]) # extract high tech description
  imp_lJSON.avm <- sapply(imp_lJSON,function(x) x[5]) # extract all value month
  imp_lJSON.avy <- sapply(imp_lJSON,function(x) x[6]) # extract all value year
  imp_lJSON.t <- sapply(imp_lJSON,function(x) x[7])   # extract time
  
  imp_df <- data.frame(as.Date(paste(imp_lJSON.t,"-02", sep=""), format="%Y-%m-%d"), imp_lJSON.cc 
                       , imp_lJSON.cn, imp_lJSON.hic, imp_lJSON.hid
                       , as.numeric(imp_lJSON.avm),as.numeric(imp_lJSON.avy))
  # put in dataframe
  names(imp_df) <- c("year_month", "country_code", "country_name", "high_tech_code", "high_tech_desc"
                     , "monthly_import_value", "ytd_import_value") 
  
  # name the vars in the data frame
  return(imp_df)
  
}

month_imports <- get_imports(census_key, year_month, import_vars, ctry_code)
View(month_imports)

# Function to extract exports data
get_exports <- function(census_key, year_month, export_vars, ctry_code) {
  
  exp_resURL <- paste("https://api.census.gov/data/timeseries/intltrade/exports/",
                      "hitech?get=",export_vars,"&time=",year_month,
                      "&CTY_CODE=",ctry_code,"&key=",census_key,sep="")
  
  exp_lJSON <- fromJSON(exp_resURL) # convert JSON content to R objects
  
  exp_lJSON <- exp_lJSON[2:length(exp_lJSON)]         # keep everything but the 1st element (var names) in lJSON
  exp_lJSON.cc <- sapply(exp_lJSON,function(x) x[1])  # extract country code
  exp_lJSON.cn <- sapply(exp_lJSON,function(x) x[2])  # extract country name
  exp_lJSON.hic <- sapply(exp_lJSON,function(x) x[3]) # extract high tech product code
  exp_lJSON.hid <- sapply(exp_lJSON,function(x) x[4]) # extract high tech description
  exp_lJSON.avm <- sapply(exp_lJSON,function(x) x[5]) # extract all value month
  exp_lJSON.avy <- sapply(exp_lJSON,function(x) x[6]) # extract all value year
  exp_lJSON.t <- sapply(exp_lJSON,function(x) x[7])   # extract time
  
  exp_df <- data.frame(as.Date(paste(exp_lJSON.t,"-02", sep=""), format="%Y-%m-%d"), exp_lJSON.cc
                       , exp_lJSON.cn, exp_lJSON.hic, exp_lJSON.hid
                       , as.numeric(exp_lJSON.avm),as.numeric(exp_lJSON.avy))
  # put in dataframe
  names(exp_df) <- c("year_month", "country_code", "country_name", "high_tech_code", "high_tech_desc"
                     , "monthly_export_value", "ytd_export_value") 
  # name the vars in the data frame
  return(exp_df)
}

month_exports <- get_exports(census_key, year_month, export_vars, ctry_code)
View(month_exports)

###############################################################################################################################

#### MERGE DATA AND CREATE LAGS ####

# Merge import and export datasets
trade_data <- merge(month_imports, month_exports, by = c("year_month", "country_code", "country_name", "high_tech_code", "high_tech_desc"), all = TRUE)

# Drop "Total" and "Not Advanced" values
trade_data <- subset(trade_data, (high_tech_code != "00" & high_tech_code != "-"))
View(trade_data)

# Proper case high tech field names
trade_data$high_tech_desc <- tolower(trade_data$high_tech_desc)
trade_data$high_tech_desc <- toTitleCase(trade_data$high_tech_desc)

# Create quarterly date-time variable in trade data
trade_data$yq <- as.yearqtr(trade_data$year_month, format = "%Y-%m-%d")
format(trade_data$yq, format = "%y 0%q")

# Create trade balance and lagged variable by high tech code

# YTD data
ytd_data <- sqldf("select high_tech_code, high_tech_desc, year_month, ytd_import_value, ytd_export_value from trade_data group by 1, 2, 3")
ytd_data$ytd_ex_less_im <- ytd_data$ytd_export_value - ytd_data$ytd_import_value

ytd_data <- 
  ytd_data %>%
  group_by(high_tech_code) %>%
  mutate(lag.ytd_import_value = dplyr::lag(ytd_import_value, n = 12, order_by = high_tech_code, default = NA))

ytd_data <- 
  ytd_data %>%
  group_by(high_tech_code) %>%
  mutate(lag.ytd_export_value = dplyr::lag(ytd_export_value, n = 12, order_by = high_tech_code, default = NA))

ytd_data <- 
  ytd_data %>%
  group_by(high_tech_code) %>%
  mutate(lag.ytd_ex_less_im = dplyr::lag(ytd_ex_less_im, n = 12, order_by = high_tech_code, default = NA))

# Quarterly data
quarterly_data <- sqldf("select yq, high_tech_code, high_tech_desc, sum(monthly_import_value) as yq_import_value, sum(monthly_export_value) as yq_export_value from trade_data group by 1, 2, 3")
quarterly_data <- sqldf("select high_tech_code, high_tech_desc, yq, yq_import_value, yq_export_value from quarterly_data group by 1, 2, 3")
quarterly_data$yq_ex_less_im <- quarterly_data$yq_export_value - quarterly_data$yq_import_value

quarterly_data <- 
  quarterly_data %>%
  group_by(high_tech_code) %>%
  mutate(lag.yq_import_value = dplyr::lag(yq_import_value, n = 4, order_by = high_tech_code, default = NA))

quarterly_data <- 
  quarterly_data %>%
  group_by(high_tech_code) %>%
  mutate(lag.yq_export_value = dplyr::lag(yq_export_value, n = 4, order_by = high_tech_code, default = NA))

quarterly_data <- 
  quarterly_data %>%
  group_by(high_tech_code) %>%
  mutate(lag.yq_ex_less_im = dplyr::lag(yq_ex_less_im, n = 4, order_by = high_tech_code, default = NA))

#### PERCENTAGE CHANGE VALUES ####

pct_change <- function(new, old){(new - old)/old}

quarterly_data$yq_change_m_yoy <-  pct_change(quarterly_data$yq_import_value, quarterly_data$lag.yq_import_value)
quarterly_data$yq_change_x_yoy <-  pct_change(quarterly_data$yq_export_value, quarterly_data$lag.yq_export_value)
quarterly_data$yq_change_xm_yoy <- pct_change(quarterly_data$yq_ex_less_im, quarterly_data$lag.yq_ex_less_im)

ytd_data$ytd_change_m_yoy <-  pct_change(ytd_data$ytd_import_value, ytd_data$lag.ytd_import_value)
ytd_data$ytd_change_x_yoy <-  pct_change(ytd_data$ytd_export_value, ytd_data$lag.ytd_export_value)
ytd_data$ytd_change_xm_yoy <- pct_change(ytd_data$ytd_ex_less_im, ytd_data$lag.ytd_ex_less_im)

#### SUBSET TO CURRENT QUARTER and YTD VALUES  ####

quarter_subset <- subset(quarterly_data, yq == current_quarter, select = c(yq, high_tech_desc, yq_export_value, yq_import_value, yq_ex_less_im, lag.yq_ex_less_im, yq_change_xm_yoy))
ytd_subset <- subset(ytd_data, year_month == current_month, select = c(year_month, high_tech_desc, ytd_export_value, ytd_import_value, ytd_ex_less_im, lag.ytd_ex_less_im, ytd_change_xm_yoy))

#### SUBSET FOR CURRENT QUARTER - 1 ####

last_quarter_month <- if(month(current_month)>3) (month(current_month)-3) else 12
last_quarter_year <- if(month(current_month)>3) (year(current_month)) else (year(current_month)-1)
last_quarter_date <- as.Date(paste(last_quarter_year,"-",last_quarter_month,"-02",sep=""), format = "%Y-%m-%d")
last_quarter <- as.yearqtr(last_quarter_date)
format(last_quarter, format = "20%y Q%q")
quarter_subset_1 <- subset(quarterly_data, yq == last_quarter, select = c(yq, high_tech_desc, yq_export_value, yq_import_value, yq_ex_less_im, lag.yq_ex_less_im, yq_change_xm_yoy))
ytd_subset_1 <- subset(ytd_data, year_month == last_quarter_date, select = c(year_month, high_tech_desc, ytd_export_value, ytd_import_value, ytd_ex_less_im, lag.ytd_ex_less_im, ytd_change_xm_yoy))

#### RESHAPE FOR CHARTS ####

View(quarterly_data)
quarterly_data$high_tech_desc2 <- trimws(gsub("[[0-9]+]", "", quarterly_data$high_tech_desc))
quarterly_charts_gross <- quarterly_data[, c(13, 3:6)]
names(quarterly_charts_gross) <- c("high_tech_desc2", "yq", "U.S. Imports", "U.S. Exports", "U.S. Exports Less Imports")
quarterly_charts_gross2 <- melt(quarterly_charts_gross, id.vars = c("high_tech_desc2", "yq"), variable.name = "trade_type", value.name = "gross_value")

###############################################################################################################################

#### CHARTS ####

# Set theme
theme_set(theme_bw())

quarterly_gross_charts <- function(df){
  
  # Create list of industries
  tech_list <- unique(df$high_tech_desc2)
  
  # Produce by-industry plots
  for(i in seq_along(tech_list)){
    
    # Create plots
    plot <- 
      ggplot(subset(df, high_tech_desc2 == tech_list[i]), aes(x = yq, y = gross_value, color = trade_type)) +
      geom_line() +
      # ylab("Value ($)") +
      # xlab("Period") +
      scale_x_continuous("Period") +
      scale_y_continuous("Value ($)", labels = scales::dollar) +
      ggtitle(paste("U.S.-Germany Trade in ", tech_list[i], " Products, 2013 Q1 to 2018 Q4", sep = "")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(legend.spacing.x = unit(0.25, 'cm')) +
      labs(caption = "Source: U.S. Census, U.S. Trade in Advanced Technology Products.")
    
    # Save plots
    ggsave(plot, file = paste(analysis, tech_list[i], " Gross Chart.pdf", sep = ""))
    
    # Print to screen
    print(plot)
    
  }
}

quarterly_gross_charts(quarterly_charts_gross2)


