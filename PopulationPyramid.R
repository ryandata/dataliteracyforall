library(idbr)
library(ggplot2)
library(animation)
library(dplyr)
library(ggthemes)

setwd("/home/ryan/Desktop/R_for_DataLiteracy/github")
# idb_api_key("Your Census API key goes here")
# get your key at
# http://api.census.gov/data/key_signup.html

idb_api_key('your key here')

countrycode<-"MG"
countryname<-"MG"
startyear=2010
endyear=2050

male <- idb1(countrycode, startyear:endyear, sex = 'male') %>%
  mutate(POP = POP * -1,
         SEX = 'Male')

female <- idb1(countrycode, startyear:endyear, sex = 'female') %>%
  mutate(SEX = 'Female')

countrydata <- rbind(male, female) %>%
  mutate(abs_pop = abs(POP))

# Animate it with a for loop

saveGIF({

  for (i in startyear:endyear) {

    title <- as.character(i)

    year_data <- filter(countrydata, time == i)

    g1 <- ggplot(year_data, aes(x = AGE, y = POP, fill = SEX, width = 1)) +
      coord_fixed() +
      coord_flip() +
      annotate('text', x = 98, y = -800000,
               label = 'Data: US Census Bureau IDB; idbr R package', size = 3) +
      geom_bar(data = subset(year_data, SEX == "Female"), stat = "identity") +
      geom_bar(data = subset(year_data, SEX == "Male"), stat = "identity") +
      scale_y_continuous(breaks = seq(-1000000, 1000000, 500000),
                         labels = paste0(as.character(c(seq(1, 0, -0.5), c(0.5, 1))), "m"),
                         limits = c(min(countrydata$POP), max(countrydata$POP))) +
      theme_economist(base_size = 14) +
      scale_fill_manual(values = c('#ff9896', '#d62728')) +
      ggtitle(paste0('Population structure of ',countryname, title)) +
      ylab('Population') +
      xlab('Age') +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(fill = guide_legend(reverse = TRUE))

    print(g1)

  }

}, movie.name = 'pyramid.gif', interval = 0.1, ani.width = 700, ani.height = 600)

