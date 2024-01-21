rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(car)
library(openxlsx)
library(readxl)
library(dplyr)
library(stringr)
library(tcltk)

# Set options
options(scipen=999)

# Open file dialog to select multiple files
file_paths <- tk_choose.files(default="", caption="Select Files", multi=TRUE, filters=NULL)

# Read the data from the selected files
data_list <- lapply(file_paths, function(file) {
  read.xlsx(file, 1, startRow = 5)
})

# Combine the data from different sources
combined_data <- do.call(rbind, data_list)

# Check and modify data types
glimpse(combined_data)
combined_data$SALE.DATE <- openxlsx::convertToDateTime(combined_data$SALE.DATE)

# Investigate and drop empty columns
combined_data$`EASE-MENT`
combined_data = subset(combined_data, select = -c(`EASE-MENT`) )
glimpse(combined_data)

# Transfer information from APART.MENT.NUMBER to ADDRESS
combined_data$APART.MENT.NUMBER
combined_data$ADDRESS <- paste(combined_data$ADDRESS, combined_data$APART.MENT.NUMBER)
combined_data$ADDRESS
combined_data = subset(combined_data, select = -c(APART.MENT.NUMBER) )

# Update borough names for better understanding
borough_names <- c("1" = "1. MANHATTAN", "2" = "2. BRONX", "3" = "3. BROOKLYN", "4" = "4. QUEENS", "5" = "5. STATEN ISLAND")
combined_data$BOROUGH <- borough_names[combined_data$BOROUGH]
combined_data$BOROUGH

# Handle missing values where 0 is inexplicable 
cols_with_na <- c("YEAR.BUILT", "LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "SALE.PRICE", "ZIP.CODE")
combined_data[cols_with_na][combined_data[cols_with_na] == 0] <- NA

# Discard invalid entries
combined_data <- combined_data[-c(which(combined_data$SALE.PRICE < 10000)), ]
combined_data <- combined_data[-c(which(combined_data$LAND.SQUARE.FEET < 70)), ]
combined_data <- combined_data[-c(which(combined_data$GROSS.SQUARE.FEET < 70)), ]
combined_data <- combined_data[-c(which(combined_data$TOTAL.UNITS == 0)), ]

# Outlier detection
summary(combined_data)
combined_data <- combined_data[-c(which(combined_data$RESIDENTIAL.UNITS > 1500)), ]

# Plot histograms for various variables
hist(combined_data$RESIDENTIAL.UNITS, xlab = "Residential Units", main = "Residential Units Histogram")
hist(combined_data$COMMERCIAL.UNITS, xlab = "Commercial Units", main = "Commercial Units Histogram")
hist(combined_data$TOTAL.UNITS, xlab = "Total Units", main = "Total Units Histogram")
hist(combined_data$LAND.SQUARE.FEET, xlab = "Land Sq. Feet", main = "Land Sq. Feet Histogram")
hist(combined_data$GROSS.SQUARE.FEET, xlab = "Gross Sq. Feet", main = "Gross Sq. Feet Histogram")
hist(combined_data$YEAR.BUILT, xlab = "Year Built", main = "Year Built Histogram") 
hist(combined_data$SALE.PRICE, xlab = "Sale Price", main = "Sale Price Histogram")

# Plot boxplots for various variables
boxplot(combined_data$RESIDENTIAL.UNITS, xlab = "Residential Units", main = "Residential Units Boxplot")
boxplot(combined_data$COMMERCIAL.UNITS, xlab = "Commercial Units", main = "Commercial Units Boxplot")
boxplot(combined_data$TOTAL.UNITS, xlab = "Total Units", main = "Total Units Boxplot")
boxplot(combined_data$LAND.SQUARE.FEET, xlab = "Land Sq. Feet", main = "Land Sq. Feet Boxplot")
boxplot(combined_data$GROSS.SQUARE.FEET, xlab = "Gross Sq. Feet", main = "Gross Sq. Feet Boxplot")
boxplot(combined_data$YEAR.BUILT, xlab = "Year Built", main = "Year Built Boxplot") 
boxplot(combined_data$SALE.PRICE, xlab = "Sale Price", main = "Sale Price Boxplot")

# Check type of BUILDING.CLASS.CATEGORY and its unique values
typeof(combined_data$BUILDING.CLASS.CATEGORY)
unique(combined_data$BUILDING.CLASS.CATEGORY)

# Filter data for specific building class categories
building_class_plot <- combined_data%>%filter(str_detect(combined_data$BUILDING.CLASS.CATEGORY, 
                                                         "01  ONE FAMILY HOMES                        |
                  02  TWO FAMILY HOMES                        |
                  03  THREE FAMILY HOMES                      |
                  13  CONDOS - ELEVATOR APARTMENTS            |
                  12  CONDOS - WALKUP APARTMENTS              |
                  11A CONDO-RENTALS                           |
                  09  COOPS - WALKUP APARTMENTS               |
                  10  COOPS - ELEVATOR APARTMENTS             |
                  15  CONDOS - 2-10 UNIT RESIDENTIAL          |
                  16  CONDOS - 2-10 UNIT WITH COMMERCIAL UNIT |
                  17  CONDOPS
            "))
                                                 
# Check class and summary of the filtered data
class(building_class_plot)
summary(building_class_plot)
                                                         
# Plot year built vs sale price
plot(building_class_plot$YEAR.BUILT, building_class_plot$SALE.PRICE, main="Year Built vs Sale Price",
xlab="Year Built", ylab="Sale Price", pch=20)
                                                         
# Plot year built vs gross sq.feet
plot(building_class_plot$YEAR.BUILT, building_class_plot$GROSS.SQUARE.FEET, main="Year Built vs Gross Sq.Feet",
xlab="Year Built", ylab="Gross Sq Feet", pch=20)                                                                 

