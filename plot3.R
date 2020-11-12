# Plot 3 (script)

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

## Convert variables & create new variables 
NEI <- NEI %>% 
  mutate(Emissions.log = log10(Emissions),  # logarithm (10) of emissions
         type = factor(type, levels = c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD"))) # convert type to factor and set levels manually

## Filter rows (Data for Baltimore)
NEI.Baltimore <- NEI %>% filter(fips == "24510")

# Create plot & save it to .png
NEI.Baltimore %>% 
  ggplot(aes(x = year, y = Emissions.log, color = type)) +
  geom_point(alpha = 1/2, size = 1) +
  stat_summary(fun = median, geom = "line", size = 0.9) +
  scale_x_continuous(breaks = NEI.Baltimore %>% pull(year) %>% unique() %>% sort()) +
  facet_grid(. ~ type) +
  scale_color_viridis_d() +
  ggtitle("Baltimore City emissions over the years (break down by type) - added yearly median values (lines)") +
  xlab("Year") +
  ylab ("Total emissions (PM2.5) - log10 scale") +
  theme(text = element_text(size = 8),
        legend.position = "none")

ggsave(filename = "plot3.png", plot = last_plot(), 
       device = "png", width = 20, height = 12, dpi = 250, units = "cm")
