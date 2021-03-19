#Jack Verity
#load packages 
library(tidyverse)
if (!require("here")) install.packages("here")
library(here)

# Read in us-counties.csv
aDF <- read_csv(here("AAPL.csv"))

aDF$changeAdjC <-  c(-diff(aDF$Close)/aDF$Close[-1] *  100, NA)

aDF$updown <- ifelse(aDF$changeAdjC >= 0, 'UP','DOWN')

view(aDF)

p <- ggplot(data = aDF,
            mapping = aes(x = Date, y=changeAdjC, color=updown))

p <- p + geom_point()

p + labs(title='Changes in AAPL Daily Prices over Last 5 Years',
         x='03/19/2020 through 03/18/2021', y='Change in Adjusted Closing Price',
         subtitle = "Jack Verity")


