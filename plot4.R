# Plot 4 (script)

rm(list = ls())
graphics.off()

# Load R packages
packages <- c("dplyr", "ggplot2", "stringr") # list of packages to load

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

## Try to find - coal combustion-related sources (using source classification columns)
SCC %>% head()
SCC %>% pull(Short.Name) %>% tolower() %>% str_subset(string = ., pattern = "coal") %>% unique()
SCC %>% pull(Short.Name) %>% tolower() %>% str_subset(string = ., pattern = "combustion") %>% unique()
SCC %>% pull(Short.Name) %>% tolower() %>% str_subset(string = ., pattern = "coal") %>% unique() %>% str_subset(string = ., pattern = "combustion")

SCC %>% pull(EI.Sector) %>% tolower() %>% str_subset(string = ., pattern = "coal") %>% unique()


## Add flags for "coal" to NEI table
NEI <- NEI %>% 
  mutate(ei.sector = tolower(EI.Sector),
         coal = str_detect(string = ei.sector, pattern = "coal"),
         target_rows = coal)

## Check how many rows target rows
NEI %>% filter(target_rows == T) %>% nrow()
NEI %>% count(target_rows, EI.Sector)
NEI %>% filter(target_rows == T) %>% count(fips)
NEI %>% filter(target_rows == T) %>% count(year)

## Filter coal - coal combustion-related sources
NEI.coal <- NEI %>% filter(target_rows == T)

## Create aggregation - total emissions
NEI.coal.tot <- NEI.coal %>% 
  group_by(year, target_rows) %>% 
  summarise(emission_tot = sum(Emissions)) %>% 
  ungroup()

# Create plot & save it to .png
NEI.coal.tot %>% 
  ggplot(aes(x = as.factor(year), y = emission_tot/1000)) +
  geom_col(color = "black", fill = "gray67") +
  ggtitle("US coal combustion-related sources emissions over the years") +
  xlab("Year") +
  ylab ("Total emissions in kilotons (1000 X tons)") +
  theme(text = element_text(size = 8),
        legend.position = "none")
ggsave(filename = "plot4.png", plot = last_plot(), 
       device = "png", width = 20, height = 12, dpi = 250, units = "cm")
