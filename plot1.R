# Plot 1 (script)

rm(list = ls())
graphics.off()

# Load R packages
packages <- c("dplyr", "ggplot2") # list of packages to load

package.check <- lapply( # load or install & load list of packages
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
) 
rm(packages, package.check)


# Get data

## Create data folder if necessary
if(!dir.exists("data")){ 
  dir.create(path = "data")  
}
## Download file (if not already downloaded)
if(!file.exists("./data/exdata_data_NEI_data.zip")){
  download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", 
                destfile = "./data/exdata_data_NEI_data.zip")
}
## Unzip .txt file (if not already unzipped)
if(!file.exists("./data/Source_Classification_Code.rds") | 
   !file.exists("./data/summarySCC_PM25.rds")){
  unzip(zipfile = "./data/exdata_data_NEI_data.zip", 
        exdir = "data")
}

## Import data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")


# Data wrangling

## Merge tables - observations & mapping labels
NEI <- NEI %>% 
  left_join(x = .,
            y = SCC,
            by = "SCC")

## Create aggregation - total emissions
NEI.tot <- NEI %>% 
  group_by(year) %>% 
  summarise(emission_tot = sum(Emissions)) %>% 
  ungroup()

# Create plot & save it to .png
png(filename = "plot1.png", width = 800, height = 600, units = "px")
barplot(names = NEI.tot$year, 
        height = NEI.tot$emission_tot / 1000,
        main = "US emissions over the years",
        xlab = "Year", 
        ylab = "Total emissions in kilotons (1000 X tons)",
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)

dev.off()
