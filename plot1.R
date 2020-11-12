# Plot 1 (script)

rm(list = ls())
graphics.off()

# Load R packages
packages <- c("dplyr", "ggplot2", "lubridate") # list of packages to load

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

## Convert variables & create new variables
NEI <- NEI %>% 
  mutate(Emissions.log = log10(Emissions)) # logarithm (10) of emissions

# Create plot & save it to .png
png(filename = "plot1.png", width = 800, height = 600, units = "px")
boxplot(Emissions.log ~ year, 
        data = NEI,
        main = "US emissions over the years",
        xlab = "Year",
        ylab = "Total emissions (PM2.5)",
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
dev.off()
