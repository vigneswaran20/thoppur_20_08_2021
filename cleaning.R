library(tidyverse)
library(readxl)
library(lubridate)
library(RSocrata)


#Stitch multiple sheets into a single sheet. Note: Make sure all the sheets have the same variable name -------------------------------> step - 2

thoppur_accidents <- excel_sheets("E:\\cleaned_sheet.xlsx") %>% map_df(~read_xlsx("E:\\cleaned_sheet.xlsx",.))
View(thoppur_accidents)
write.csv(thoppur_accidents,'E:\\single_sheet.csv')

thoppur_accidents_cleaned <- read.csv("E:\\single_sheet.csv")
variable.names(thoppur_accidents_cleaned)
View(thoppur_accidents_cleaned)


#Time Series

thoppur_timeseriesplot <- thoppur_accidents_cleaned %>% select(date,total_cases)


#Expanding the frequency
thoppur_timeseriesplot <- data.frame(thoppur_timeseriesplot)

dat <-  thoppur_timeseriesplot
work_data <- dat[rep(1:nrow(dat), dat[["total_cases"]]), ]

#Assigning value 1 
work_data$total_cases[work_data$total_cases>0] <- 1

#Creating row id's
library("dplyr")

rownames(work_data) <- NULL
work_data <- work_data %>% select(date, total_cases)

variable.names(work_data)
dim(work_data)




##---------------------

library(extrafont)
library(ggplot2)
library(viridis)
library(hrbrthemes)
install.packages("viridis")
loadfonts(device = "win")


write.csv(work_data,'E:\\timeseries_thoppur.csv')

##Load and convert through r 

work_data <- timeseries_thoppur
# load fonts - every session
loadfonts(device = "win", quiet = TRUE)
str(work_data)
work_data %>%
  mutate(date = floor_date(date, unit = "year")) %>%
  count(date, total_cases) %>%
  filter(
    date != last(date),
    date != first(date)
  ) %>%
  ggplot(aes(date, n, color = total_cases)) +
  geom_line(size = 1.5, alpha = 0.7, color= "red" ) +
  geom_point(aes(size = n)) + 
  scale_y_continuous(limits = (c(0, NA))) +
  labs(
    x = "Year",
    y = "Number of of people involved in the accident",
    title = "Number of people involved in the accident over the past 10 years",
  ) + theme_minimal()   + theme(legend.position = "none") +  geom_point(shape = 21,
                                                                       colour = "#0081C8",
                                                                        fill = "#00A651",
                                                                        size = 4) + theme(plot.title = element_text(hjust = 0.5),text = element_text( size = 12, family = "Open Sans"))

warnings()


library(tidyverse)
library(extrafont)

library(cowplot)
font_import()
y
