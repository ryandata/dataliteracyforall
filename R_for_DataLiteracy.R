###############################
# Data Literacy for All, with R
# IASSIST Workshop
# Ryan Womack
# created 2017-05-23
# revised 2018-05-29
###############################

#setup

# obtain a recent version of R from http://r-project.org
# Windows users will want to download Rtools from the site as well.
# and a recent version of RStudio from https://rstudio.com
# or run on https://rstudio.cloud

# later Image Magick will be useful http://imagemagick.com (linux/mac users may already have it)


# install required packages for this session

install.packages("WDI", dependencies=TRUE)
install.packages("googleVis", dependencies=TRUE)
install.packages("lattice", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("ggvis", dependencies=TRUE)
install.packages("reshape2", dependencies=TRUE)
install.packages("tidyverse", dependencies=TRUE)
install.packages("devtools", dependencies=TRUE)
install.packages("animation", dependencies=TRUE)
install.packages("ggthemes", dependencies=TRUE)
install.packages("dplyr", dependencies=TRUE)
install.packages("gridExtra", dependencies=TRUE)
install.packages("idbr", dependencies=TRUE)
install.packages("mosaic", dependencies=TRUE)
install.packages("manipulate", dependencies=TRUE)


# replace with your directory name!
# or not, if you want to use default directory

setwd("/home/ryan/Desktop/dataliteracy/")

# now create a New Package using menus in RStudio

# explore some data with WDI package

library(WDI)
#google to find these codes :)
countries<-c("US","CN","GB","DE","MN","RU","CA","AU")
indicators<-c("NY.GDP.PCAP.KD","SP.URB.TOTL.IN.ZS","SP.RUR.TOTL.ZS","ST.INT.ARVL")
realnames<-c("iso2c","country","year","PerCapitaGDP_Constant","Urban_%","Rural_%","Tourism_Arrivals")
mydata<-WDI(country=countries, indicator=indicators, start=1996, end=2015)
names(mydata)<-realnames
library(lattice)
attach(mydata)
xyplot(PerCapitaGDP_Constant~year, groups=country, auto.key=list(space="right"))
library(ggplot2)
ggplot(data=mydata, aes(x=year, y=PerCapitaGDP_Constant,group=country, colour=country)) +
  geom_line()

# save the data ...
library(devtools)
use_data(mydata)

# alternatively, we could find a .csv file and import with
# mydata <- read.csv("myfile.csv")
# and save the data...

# functions

# a basic function
myfunction<-function(x,y)
{
  x+y+200
}

#save this as myfunction.R into the R directory of your package

library(ggplot2)
library(lattice)
data(diamonds)
attach(diamonds)

# comparison functions
CompareIt<-function(a,b)
{
  cat("Median ",median(a)," Mean ",mean(a))
  cat(" Median ",median(b)," Mean ",mean(b))
}

GraphIt<-function(a,b)
{
  plot_a<-bwplot(a)
  plot_b<-bwplot(b)
  plot_c<-densityplot(a)
  plot_d<-densityplot(b)
  print(plot_a, split = c(1, 1, 2, 2), more = TRUE)
  print(plot_b, split = c(2, 1, 2, 2), more = TRUE)
  print(plot_c, split = c(1, 2, 2, 2), more = TRUE)
  print(plot_d, split = c(2, 2, 2, 2), more = FALSE)  # more = FALSE is redundant

}

# ggvis
library(ggvis)

diamonds %>% ggvis(~carat, ~price, fill=~clarity)

diamonds %>%
  ggvis(~carat) %>%
  layer_densities(
    adjust = input_slider(.1, 2, value = 1, step = .1, label = "Bandwidth adjustment"),
    kernel = input_select(
      c("Gaussian" = "gaussian",
        "Epanechnikov" = "epanechnikov",
        "Rectangular" = "rectangular",
        "Triangular" = "triangular",
        "Biweight" = "biweight",
        "Cosine" = "cosine",
        "Optcosine" = "optcosine"),
      label = "Kernel")
  )

# use shinyapp.R file
# PopulationPyramid.R file and
# Population.R file
# for the remainder of the exercise
