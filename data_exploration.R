library(data.table)
library(dplyr)
library(mgcv)
library(bit64)
library(ggplot2)

visas <- readRDS('H1BVisas.rds')

hist(visas[which(visas$normalized_wage < 250000),]$normalized_wage, breaks=500, freq=T)

hist(visas[which(visas$normalized_wage < 250000),]$normalized_wage, breaks=500, xlim=c(20000, 100000))
summary(visas[which(visas$normalized_wage < 250000),]$normalized_wage)

# saveRDS(visas, 'H1BVisas.rds')

hist(normalized_prevailing_wage, xlim=c(0, 300000), breaks=500)

visas[which(visas$normalized_wage > 500000), ]$normalized_wage <- NA

ggplot(visas, aes(factor(fy), wage_rate)) + geom_boxplot()

test <- visas[which(visas$normalized_wage < 40000)]

dim(test)

test <- test[which(test$normalized_wage > 36000)]

dim(test)

test1 <- melt(test, id.vars = 'fy')

wage <- select(visas, fy, normalized_wage) %>% 
  filter(!is.na(normalized_wage)) %>%
  filter(normalized_wage < 30200) %>%
  filter(normalized_wage > 30000)

ggplot(wage, aes(x = normalized_wage, group = fy))  + 
  geom_boxplot()

qplot(factor(fy), normalized_wage, data = visas, geom = "boxplot")

p <- ggplot(mtcars, aes(factor(cyl), mpg))

p + geom_boxplot()
 
unique(visas$file_name)


# Quick and dirty analysis
wage <- select(visas, normalized_prevailing_wage) %>% 
  filter(!is.na(normalized_prevailing_wage)) %>%
  filter(normalized_prevailing_wage > 1000) %>%
  filter(normalized_prevailing_wage < 500000)
years <- select(visas, ) %>% 
  filter(!is.na(normalized_prevailing_wage)) %>%
  filter(normalized_prevailing_wage > 1000) %>%
  filter(normalized_prevailing_wage < 500000)

hist(wage$normalized_prevailing_wage, xlim=c(0, 300000), breaks=500)

plot(visas$year, visas$normalized_prevailing_wage)
