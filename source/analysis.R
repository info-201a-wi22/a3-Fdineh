library(areaplot)
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)
library("plotly")
library("dplyr")
library("tidyr")
library("stringr")
# install.packages("plotrix")
# install.packages("maps")
library(maps)
library(tidyverse)
# install.packages("mapproj")
library(mapproj)

#
incarceration_trends <- read.csv(
  "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
  stringsAsFactors = FALSE
)
View(incarceration_trends)


#Table
female_prison <- incarceration_trends %>% 
  filter(state == "WA", na.omit= TRUE) %>% 
  group_by(county_name) %>% 
  select(state, year, county_name, white_female_prison_pop, black_female_prison_pop)
female_prison <- na.omit(female_prison)
View(female_prison)

female_prison <- mutate(female_prison, 
                      total_black_white = black_female_prison_pop + white_female_prison_pop) %>% 
  group_by(county_name)


# Summary table
# What is the total prison population from 1988 to 2016?
total_pop <- female_prison %>% 
group_by(state) %>% 
select(year, total_black_white) %>% 
summarise(total = sum(total_black_white)) 
View(total_pop)

# What is the difference of the black and white prison population?
total_black <- female_prison %>% 
  group_by(state) %>% 
  select(year, black_female_prison_pop) %>% 
summarise(total = sum(black_female_prison_pop)) 
total_black

total_white <- female_prison %>% 
  group_by(state) %>% 
  select(year, white_female_prison_pop) %>% 
  summarise(total = sum(white_female_prison_pop)) 
total_white

difference_black_white <- total_white$total - total_black$total
difference_black_white

#What county has the highest prison population?
total_county <- female_prison %>% 
group_by(county_name) %>% 
select(total_black_white) %>% 
  summarise(total = sum(total_black_white)) %>% 
  arrange(desc(total)) %>% 
  slice(1)
View(total_county)


#Trend
x<- plot_ly(data = female_prison,
         x = ~year,
         y = ~total_black_white,
         color = ~year,
         type = "bar"
        ) %>% 
 layout(title = "Black and white females in prison (1988-2018)", 
       xaxis = list(title ="Year"),
       yaxis = list(title ="Black and white females"))
x
#Chart
y <- plot_ly(data = female_prison,
        x = ~white_female_prison_pop,
        y = ~black_female_prison_pop,
        color = ~year,
        type = "bar"
) %>%
  layout(title = "Comparing Black and White Female Prison Population",
         xaxis = list(title ="White females prison"),
         yaxis = list(title ="Black females prison"))
y











#Map

map_table <- incarceration_trends %>% 
select(state, year, county_name, fips, white_female_prison_pop, black_female_prison_pop)
View(map_table)

wa_map <- map_data("county") %>% 
unite(polyname, region, subregion, sep = ",")
View(wa_map)

join_wa <- left_join(wa_map, county.fips)
View(join_wa)

fd_wa <- left_join(map_table, join_wa) %>% 
  filter(state == "WA" & year == "2016")
View(fd_wa)


blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()      
  )

yd_wa <- ggplot(data= fd_wa) +
  geom_polygon(mapping = aes(x= long, y=lat, group = group, fill = white_female_prison_pop)) + 
  blank_theme + 
  coord_map() +
  scale_fill_continuous(limits = c(0, max(fd_wa$white_female_prison_pop)), high = "#FC766AFF", low = "#B0B8B4FF", na.value = "#184A45FF")+
  ggtitle("White female prison population in 2016") 
yd_wa


ja_wa <- ggplot(data= fd_wa) +
  geom_polygon(mapping = aes(x= long, y=lat, group = group, fill = black_female_prison_pop)) + 
  blank_theme + 
  coord_map() +
  scale_fill_continuous(limits = c(0, max(fd_wa$black_female_prison_pop)), high = "#FC766AFF", low = "#B0B8B4FF", na.value = "#184A45FF") +
  ggtitle("Black female prison population in 2016")
ja_wa
