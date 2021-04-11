# Load packages

library(tidyverse)
if(!require("here")) install.packages("here")
library(here)

#Read in csv file
us_counties <- read_csv(here("covid-19-data","us-counties.csv"))

#Filter non-Pennsylvania counties
pa_counties <- us_counties %>% filter(state=="Pennsylvania")

#Filter rows
pa_latest <- pa_counties[pa_counties$date == max(pa_counties$date),]

library(ggrepel)

View(elections_historic)
elections_historic %>% select(2:7)

p_title <- paste("COVID-19 Deaths vs Cases for PA as of", as.character(max(pa_counties$date))," ")
x_label <- "Cases"
y_label <- "Deaths"

p <- ggplot(pa_latest, aes(x= cases, y= deaths,
                           label = county))

p+ geom_point() +
  geom_text_repel()+
  geom_smooth(se=FALSE,method="lm") +
  labs(x=x_label, y=y_label,title=p_title)

