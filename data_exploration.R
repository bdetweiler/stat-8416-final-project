# Use this for "scratch paper" - trying things out, jotting down ideas, exploring data.

library(data.table)
library(mgcv)
library(bit64)
library(ggplot2)
library(dplyr)

options(scipen=999)

visas <- readRDS('H1BVisas.rds')

str(visas)

visas.by.job <- select(visas, fy, 
                              file_name,
                              case_number,
                              visa_class,
                              total_workers,
                              status,
                              normalized_wage,
                              normalized_prevailing_wage,
                              dot_job_title,
                              naics_title)


saveRDS(visas.by.job, 'VisasByJob.rds')

# Once the above has been done, can start here
visas <- readRDS('VisasByJob.rds')

visas.by.status <- select(visas, fy, total_workers, status) %>%
    filter(!is.na(status)) %>%
    filter(status != "") %>%
    group_by(status, fy) %>%
    summarise(tot = sum(total_workers))


ggplot(visas.by.status, aes(x = status, y = tot))  +
  geom_bar(stat = "identity") +
  facet_grid(. ~ fy)
  coord_flip +

# Clearly, we can see a huge outlier in 2010 that is throwing the whole chart off. Let's clean some of the ridiculous ones.
sort(visas$total_workers, decreasing = T)
 
# Let's look at the statuses of the higher ones and if they were denied, we'll exclude them. 
 
outliers.by.status <- select(visas, fy, total_workers, status) %>%
    filter(!is.na(status)) %>%
    filter(total_workers > 1000)

# Everything over 1000 workers were either denied or withdrawn. We can ignore those.
   
visas.by.status <- select(visas, fy, total_workers, status) %>%
    filter(!is.na(status)) %>%
    filter(status != "") %>%
    filter(total_workers < 1000) %>%
    group_by(status, fy) %>%
    summarise(tot = sum(total_workers))
   
ggplot(visas.by.status, aes(x = status, y = tot))  +
  geom_bar(stat = "identity") +
  facet_grid(. ~ fy)

# Since this chart is hard to read, let's just look at the certified ones.

visas.by.status <- select(visas, fy, total_workers, status) %>%
    filter(!is.na(status)) %>%
    filter(status == "CERTIFIED") %>%
    filter(total_workers < 1000) %>%
    group_by(status, fy) %>%
    summarise(tot = sum(total_workers))
 
# This seems to match the job growth shown by the U.S. Bureau of Labor Statistics 
# https://www.whitehouse.gov/blog/2016/03/04/employment-situation-february
ggplot(visas.by.status, aes(x = fy, y = tot))  +
  geom_bar(stat = "identity") +
  xlab(unique(visas.by.status$fy))

# Let's look at the other statuses
unique(visas$status)

# We'll count the total number of applications that were denied (as opposed to the total workers requested)
visas.by.status <- select(visas, fy, total_workers, status) %>%
    filter(!is.na(status)) %>%
    filter(status == "DENIED") %>%
    filter(total_workers < 1000) 

ggplot(visas.by.status, aes(x = fy))  +
  geom_bar(stat = "bin")

# The iCERT system was implemented in 2009. It could be inferred that over the years, the system has gotten better
# possibly at form input validation

visas.by.status <- select(visas, fy, total_workers, status) %>%
    filter(!is.na(status)) %>%
    filter(status == "WITHDRAWN") %>%
    filter(total_workers < 1000) 

ggplot(visas.by.status, aes(x = fy))  +
  geom_bar(stat = "bin")

# 2008 and 2009 had a huge number of applications withdrawn, likely due to the financial crisis and bad economy
# However, it's interesting to see the uptick in the last few years

visas.by.status <- select(visas, fy, total_workers, status) %>%
    filter(!is.na(status)) %>%
    filter(status == "CERTIFIED-WITHDRAWN") %>%
    filter(total_workers < 1000) 

ggplot(visas.by.status, aes(x = fy))  +
  geom_bar(stat = "bin")

# This is not a terribly interesting status or statistic

visas.by.status <- select(visas, fy, total_workers, status) %>%
    filter(!is.na(status)) %>%
    filter(status %in% c("PENDING QUALITY AND COMPLIANCE REVIEW - UNASSIGNED", "INVALIDATED", "REJECTED")) %>%
    filter(total_workers < 1000) 

ggplot(visas.by.status, aes(x = fy))  +
  geom_bar(stat = "bin")

# only 15 and 2 for 2013 and 2014 only - we can probably ignore these guys

visas.by.status <- select(visas, fy, total_workers, status) %>%
    filter(!is.na(status)) %>%
    filter(status == "REJECTED") %>%
    filter(total_workers < 1000) 

ggplot(visas.by.status, aes(x = fy))  +
  geom_bar(stat = "bin")



hist(visas[which(visas$naics_title == 'Computer Systems Design Services'),]$normalized_wage, breaks=500, xlim = c(0, 500000))

hist(visas[which(visas$naics_title == 'Parole Offices and Probation Offices'),]$normalized_wage, breaks=500, xlim=c(0, 500000))

wage <- select(visas, normalized_wage, fy, case_number) %>% 
  filter(!is.na(normalized_wage)) %>%
  filter(normalized_wage > 500000)

case <- filter(visas, case_number == 'I-09177-5040915')


case2009 <- select(fy2009, case_number, wage_rate, wage_rate_from, wage_rate_to, wage_unit) %>%
  filter(wage_unit != 'YR')



wage <- select(visas, normalized_prevailing_wage, fy, case_number) %>% 
  filter(!is.na(normalized_prevailing_wage)) %>%
  filter(normalized_prevailing_wage > 500000)

summary(wage)

ggplot(wage, aes(x = normalized_wage)) +
  stat_bin()
  

findmode <- function(x, na.rm = TRUE) {
  
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

wage.mode <- findmode(visas$normalized_wage)
wage.mode
abline(v=wage.mode + 500, col="red")

hist(visas[which(visas$normalized_prevailing_wage < 250000),]$normalized_prevailing_wage, breaks=500)
pw.mode <- findmode(visas$normalized_prevailing_wage)
pw.mode
abline(v=wage.mode + 500, col="red")



hist(visas[which(visas$normalized_wage < 250000),]$normalized_wage, breaks=500, xlim=c(20000, 100000))
summary(visas[which(visas$normalized_wage < 250000),]$normalized_wage)


hist(normalized_prevailing_wage, xlim=c(0, 300000), breaks=500)

visas[which(visas$normalized_wage > 500000), ]$normalized_wage <- NA

# This hoses my computer. Don't run it
# ggplot(visas) + 
  # geom_boxplot(aes(normalized_wage)) +
  # facet_wrap(fy~normalized_wage)
  


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




# Maps example 1

crimes <-data.frame(state = tolower(rownames(USArrests)), USArrests)

states_map <-map_data("state")

ggplot(crimes, aes(map_id = state)) +
  geom_map(aes(fill = Murder), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) 


remove.territories = function(.df) {
  subset(.df, 
         .df$id != "AS" &
           .df$id != "MP" &
           .df$id != "GU" & 
           .df$id != "PR" &
           .df$id != "VI" 
  )
}

plain_theme = theme(axis.text=element_blank()) + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank())

no_ylab = ylab("") 
no_xlab = xlab("")


# Shiny plots

library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)


final.shiny <- readRDS('ShinyDatset.rds')

# Should also filter out outliers
choro <- select(final.shiny, employer_state, normalized_wage) %>%
  filter(!is.na(employer_state)) %>%
  filter(!is.na(normalized_wage)) %>%
  group_by(region = employer_state) %>%
  summarise(value = mean(normalized_wage))

state_choropleth(df = choro)





ggplot(final.shiny, aes(x = long, 
                       y = lat)) +
  geom_polygon(aes(group = group, 
                   fill = value), 
               color = "grey65") +
  scale_fill_gradient(low = "#ffffff", 
                      high = "#b50306", 
                      space = "Lab",
                      na.value = "grey50",
                      guide = FALSE) +
  facet_wrap(~variable)








# Interactive state map

library(plotly)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(scope = 'usa',
          projection = list(type = 'albers usa'),
          showlakes = TRUE,
          lakecolor = toRGB('white'))

plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(z = ~total.exports, 
            text = ~hover, 
            locations = ~code,
            color = ~total.exports, 
            colors = 'Purples') %>%
  colorbar(title = "Millions USD") %>%
  layout(title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
         geo = g)

nonstates <- final.shiny$employer_state[which(!final.shiny$employer_state %in% tolower(state.name))]
final.shiny$employer_state[which(!is.na(final.shiny$employer_state))]




final.shiny.map <- select(final.shiny, fy,
                                       visa_class,
                                       normalized_wage,
                                       normalized_prevailing_wage,
                                       employer_state) %>%
                   filter(!is.na(employer_state)) %>%
                   filter(!is.na(normalized_wage)) %>%
                   filter(normalized_wage > 0) %>%
                   group_by(employer_state) %>%           #  We'll also allow grouping by fy and visa_class
                   summarise(med = median(normalized_wage), 
                             mean = mean(normalized_wage), 
                             min = min(normalized_wage),
                             max = max(normalized_wage))

final.shiny.map$employer_state_abb <- final.shiny.map$employer_state
for (state in state.name) {
  final.shiny.map$employer_state_abb[which(tolower(final.shiny.map$employer_state) == tolower(state))]  <- 
    state.abb[tolower(state.name) %in% tolower(state)]
}

head(final.shiny.map$hover)
final.shiny.map$hover <- with(final.shiny.map, paste(employer_state, "<br>",
                                                     "Mean wage", mean, "<br>", 
                                                     "Median wage", med, "<br>",
                                                     "Minimum wage", min, "<br>",
                                                     "Maximum wage", max))

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(scope = 'usa',
          projection = list(type = 'albers usa'),
          showlakes = TRUE,
          lakecolor = toRGB('white'))

plot_geo(final.shiny.map, locationmode = 'USA-states') %>%
  add_trace(z = ~med, 
            text = ~hover, 
            locations = ~employer_state_abb,
            color = ~med, 
            colors = 'Purples') %>%
  colorbar(title = "Median wage USD") %>%
  layout(title = 'Foreign Worker Wages by State<br>(Hover for breakdown)',
         geo = g)

test1 <- "^__DUMMY1__$|^PERM$|^H-1B$|^H-1B1 Singapore$|^H-1B1 Chile$|^E-3 Australian$|^__DUMMY2__$"
test <- final.shiny %>% filter(grepl("^E-3 Australian$", visa_class))
table(test$visa_class)

visa_classifications <- c("H-1B", "PERM")
head(unlist(lapply(final.shiny$visa_class, '%in%', visa_classifications)))

select(final.shiny, visa_class, employer_state)
unique(final.shiny$employer_state)


final.shiny <- readRDS('ShinyDatset.rds')
setwd('/home/bdetweiler/src/Data_Science/stat-8416-final-project/shiny')











