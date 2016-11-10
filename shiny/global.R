library(shiny)
library(scales)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(plotly)
library(stringr)
library(lazyeval)
library(dplyr)


final.shiny <- readRDS('ShinyDatset.rds')
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")

# NumberFormatDollars
nfd <- function(x) { return(paste("$", format(x, digits=2, nsmall=0,  big.mark=","), sep="")) }