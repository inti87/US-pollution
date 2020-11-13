# Plot 5 (script)

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

## Convert variables & create new variables 
NEI <- NEI %>% 
  mutate(Emissions.log = log10(Emissions),  # logarithm (10) of emissions
         type = factor(type, levels = c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD"))) # convert type to factor and set levels manually

## Try to find - motor vehicle sources (using source classification columns)
SCC %>% head()
SCC %>% pull(Short.Name) %>% tolower() %>% str_subset(string = ., pattern = "motor") %>% unique()
SCC %>% pull(Short.Name) %>% tolower() %>% str_subset(string = ., pattern = "vehicle") %>% unique()
SCC %>% pull(Short.Name) %>% tolower() %>% str_subset(string = ., pattern = "motor") %>% unique() %>% str_subset(string = ., pattern = "vehicle")
SCC %>% pull(Short.Name) %>% tolower() %>% str_subset(string = ., pattern = "motor vehicle") %>% unique()
SCC %>% pull(Short.Name) %>% tolower() %>% str_subset(string = ., pattern = "(motor vehicle|motorcycle)") %>% unique()
SCC %>% pull(EI.Sector) %>% tolower() %>% str_subset(string = ., pattern = "vehicle") %>% unique()


## Add flags for "motor" & vehicle to NEI table
NEI <- NEI %>% 
  mutate(short.name = tolower(Short.Name),
         motor = str_detect(string = short.name, pattern = "(motor vehicle|motorcycle)"),
         target_rows = motor)

## Check how many rows target rows
NEI %>% filter(target_rows == T) %>% nrow()
NEI %>% filter(target_rows == T) %>%  count(Short.Name, target_rows)
NEI %>% filter(target_rows == T) %>% count(fips)
NEI %>% filter(target_rows == T) %>% count(year)
NEI %>% filter(target_rows == T & fips == "24510") %>% count(year)

## Filter Baltimore & motor vehicle sources 
NEI.Baltimore.motor <- NEI %>% filter(target_rows == T & fips == "24510")


# Create plot & save it to .png
NEI.Baltimore.motor %>% 
  ggplot(aes(x = as.factor(year), y = Emissions.log, )) +
  geom_boxplot() +
  ggtitle("Baltimore City motor vehicle sources related emissions over the years") +
  xlab("Year") +
  ylab ("Total emissions (PM2.5) - log10 scale") +
  theme(text = element_text(size = 8),
        legend.position = "none")

ggsave(filename = "plot5.png", plot = last_plot(), 
       device = "png", width = 20, height = 12, dpi = 250, units = "cm")
