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

# NumberFormatDollars
nfd <- function(x) { return(paste("$", format(x, digits=2, nsmall=0,  big.mark=","), sep="")) }
nf <- function(x) { return(paste(format(x, digits=2, nsmall=0,  big.mark=","), sep="")) }
properCase <- function(x) { paste(toupper(substring(x, 1, 1)), 
                                  substring(x, 2),
                                  sep = "", 
                                  collapse = "") }