###############
# Evaluating Population
# using the idbr package
# developed for Rutgers Future Scholars
# by Ryan Womack
# 2017-07-12, updated 2018-05-29
# associated files at
# https://libguides.rutgers.edu/datafuturescholars
###############

# see
# https://www.census.gov/data/developers/data-sets/international-database.html
# for information of variables used by idb
#
# look up FIPS country codes here
# https://en.wikipedia.org/wiki/List_of_FIPS_country_codes
#
# FIPS are standardized two-letter country codes and are
# the easiest way to input country information for idbr


#####
# install required packages
#####

# You should have installed these from the earlier R_for_Data_Literacy.R file

#####
# load required packages
#####

library(idbr)
library(ggplot2)
library(animation)
library(dplyr)
library(ggthemes)

######
# setup
# you must request an Census API key for this package to work
# get your key at
# http://api.census.gov/data/key_signup.html

idb_api_key('you must put your Census API key in here for this to work')

######

# fertility

# function to compare fertility rates
# input two country codes, start year, end year
# put quotes around country codes

fertility <- function(a,b,c,d)
{
  country_one<-a
  country_two<-b
  start_year<-c
  end_year<-d

  ssr_df <- idb5(c(country_one,country_two), year=c:d, variable='TFR' ,country_name = TRUE)

  ggplot(ssr_df, aes(x = time, y=TFR, color=NAME)) +
    geom_line(size = 1) +
    ylab('Total Fertility Rate') +
    xlab('Year') +
    theme(legend.title = element_blank(),
          legend.position = "bottom")
}

######

# death rate

# function to compare death rates
# input two country codes, start year, end year
# put quotes around country codes

death <- function(a,b,c,d)
{
  country_one<-a
  country_two<-b
  start_year<-c
  end_year<-d

  ssr_df <- idb5(c(country_one,country_two), year=c:d, variable='CDR' ,country_name = TRUE)

  ggplot(ssr_df, aes(x = time, y=CDR, color=NAME)) +
    geom_line(size = 1) +
    ylab('Crude Death Rate') +
    xlab('Year') +
    theme(legend.title = element_blank(),
          legend.position = "bottom")
}

######

# growth rate

# function to compare growth rates
# input two country codes, start year, end year
# put quotes around country codes

growth <- function(a,b,c,d)
{
  country_one<-a
  country_two<-b
  start_year<-c
  end_year<-d

  ssr_df <- idb5(c(country_one,country_two), year=c:d, variable='GR' ,country_name = TRUE)

  ggplot(ssr_df, aes(x = time, y=GR, color=NAME)) +
    geom_line(size = 1) +
    ylab('Growth Rate') +
    xlab('Year') +
    theme(legend.title = element_blank(),
          legend.position = "bottom")
}


######

# population

# function to compare total population
# input two country codes, start year, end year
# put quotes around country codes

population <- function(a,b,c,d)
{
  country_one<-a
  country_two<-b
  start_year<-c
  end_year<-d

  ssr_df <- idb5(c(country_one,country_two), year=c:d, variable='POP' ,country_name = TRUE)

  ggplot(ssr_df, aes(x = time, y=POP, color=NAME)) +
    geom_line(size = 1) +
    ylab('Total Population') +
    xlab('Year') +
    theme(legend.title = element_blank(),
          legend.position = "bottom")
}


#####
# Note we can also retrieve sets of concepts like this
ssr_df <- idb5(c('RS','US'), year=1990:2015, concept='Components of population growth' ,country_name = TRUE)
ssr_df <- idb5(c('RS'), year=1990:2015, concept='Fertility rates' ,country_name = TRUE)



##############################
# Population Pyramid
#
# Create a population pyramid!
##############################

pyramid2<-function(countrycode,countryname,startyear,endyear)
{

# countrycode<-c('UK')
# countryname<-"UK"
# startyear=2010
# endyear=2050



male <- idb1(countrycode, startyear:endyear, sex = 'male') %>%
  mutate(POP = POP * -1,
         SEX = 'Male')

female <- idb1(countrycode, startyear:endyear, sex = 'female') %>%
  mutate(SEX = 'Female')

countrydata <- rbind(male, female) %>%
  mutate(abs_pop = abs(POP))

# Animate it with a for loop

saveHTML({

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

}, htmlfile="index.html")


}
