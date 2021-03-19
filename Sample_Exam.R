# Jack Verity

# Load packages I'm going to use
library(tidyverse)
if (!require("here")) install.packages("here")
library(here)

# Read in us-counties.csv
us_counties <- read_csv(here("covid-19-data","us-counties.csv"))

# Filter out non-PA counties

pa_counties <- us_counties %>% filter(state=="Pennsylvania")

# Filter to keep only NEPA counties

NEPA <- c("Bradford", "Carbon", "Columbia", "Lackawanna", "Luzerne", "Monroe", "Montour",
          "Northumberland", "Pike", "Schuylkill", "Sullivan", "Susquehanna", "Wayne", "Wyoming")
NEPA_counties <- pa_counties[pa_counties$county %in% NEPA,]

# Filter to get only latest rows

NEPA_latest <- NEPA_counties[NEPA_counties$date == max(NEPA_counties$date),]

# View(pa_counties)

# Make graph

p <- ggplot(data = NEPA_latest,
            mapping = aes(x = county, y=cases,fill = county))
p + geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

