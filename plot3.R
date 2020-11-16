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

## Convert variables & create new factor variables 
NEI <- NEI %>% 
  mutate(type = factor(type, levels = c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD"))) # convert type to factor and set levels manually

## Filter rows (Data for Baltimore)
NEI.Baltimore <- NEI %>% filter(fips == "24510")

## Create aggregation - total emissions
NEI.Baltimore.Type.tot <- NEI.Baltimore %>% 
  group_by(fips, year, type) %>% 
  summarise(emission_tot = sum(Emissions)) %>% 
  ungroup()


# Create plot & save it to .png
NEI.Baltimore.Type.tot %>% 
  ggplot(aes(x = year, y = emission_tot, fill = type)) +
  geom_col(position = "dodge", color = "black") +
  scale_x_continuous(breaks = NEI.Baltimore %>% pull(year) %>% unique() %>% sort()) +
  facet_grid(. ~ type) +
  scale_color_viridis_d() +
  ggtitle("Baltimore City emissions over the years (break down by type)") +
  xlab("Year") +
  ylab("Total emissions in tons") +
  theme(text = element_text(size = 8),
        legend.position = "none")
ggsave(filename = "plot3.png", plot = last_plot(), 
       device = "png", width = 20, height = 12, dpi = 250, units = "cm")
