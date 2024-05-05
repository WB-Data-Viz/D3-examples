
rm(list=ls())
# Load libraries
rm(list=ls())
library(tidyverse)
library(zoo)
library(ggtext)
library(ggforce)
library(showtext)
font_add_google("Poppins", "Poppins")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()


## Read and organize data
states = read_csv("hw1-data.csv")

states.clean = states %>% mutate(fips = ifelse(state == "New England", 67, fips))

states.clean = states.clean %>% 
  mutate(cases = as.numeric(cases),
         deaths = as.numeric(deaths),
         date = as.Date(date)) ## default Date arguments need to be year, month, and day for this function to work.



##*1.How many unique values are in the state variable?*
##  There are 57 unique values in the state variable

length(unique(states$state)) ## 57 

##*2.List the unique values in the state variable.*
#  Includes territories and USA (Virgin Islands, Northern Mariana Islands, Guam, etc.)
unique(sort(states$state))


## 3.Make a coronavirus chart for the United States and California
states_averages = states.clean %>% 
  group_by(state) %>%
  arrange(date) %>% 
  mutate(daily_cases = cases - lag(cases), #calculate daily rather than cumulative cases
         roll_avg_7 = rollmean(x=daily_cases, k = 7, align = "right", na.pad = T),  #add a layer for rolling 7 day averages to reduce noise
         first_vaccine = ifelse(date == "2020-12-14", 1, 0),
         date = as.Date(date)
  )


make_graphs = function(df, state_name, flag) {
  df %>% 
    filter(state == state_name) %>%
    ggplot(aes(x=date, y = roll_avg_7)) +
    geom_bar(stat = "identity", aes(x=date, daily_cases), alpha = 0.2, fill = "maroon") + 
    ## alpha de-emphasizes the noise 
    geom_line(color = "maroon", size = 1) + 
    theme_minimal() +
    labs(y= "Daily Case Counts", 
         x = "",
         caption = "Hospitalization data from the U.S. Department of Health and Human Services",
         title = paste("Daily COVID19 cases in <span style = 'color:maroon'><b>",state_name,"</b></span>", "<img src =", flag, "flag width = '25'>")) + 
    theme(plot.title = element_markdown(family = "Poppins", size = 12),
          plot.caption = element_text(family = "Poppins", size = 8, color = "gray20"),
          axis.text.x = element_text(family = "Poppins", size = 8),
          axis.text.y = element_text(family = "Poppins", size = 8)) +
    ggforce::geom_mark_circle(aes(filter = first_vaccine == "1"), 
                              description = "Vaccine rollout begins", 
                              color = "maroon",
                              label.fontsize = 9,
                              label.buffer = unit(20, "mm"),
                              label.margin = margin(2,2,2,2, "mm"),
                              label.minwidth = unit(30, "mm")) + 
    scale_y_continuous(labels = scales::comma) + 
    scale_x_date(date_labels = "%b '%y", date_breaks = "3 months")
}
pdf("C:/Users/yinin/Documents/GitHub/dataviz-class/Week1_CovidData/CovidCases.pdf", height = 5, width = 7)

make_graphs(states_averages, "California", 'ca_flag_cropped.png')
make_graphs(states_averages, "USA", 'usa_flag.png')


dev.off() ## tell R to stop saving everything i print
