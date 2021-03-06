# Use this for "scratch paper" - trying things out, jotting down ideas, exploring data.


library(data.table)
library(mgcv)
library(bit64)
library(ggplot2)
library(rjags)
library(dplyr)

#setwd('/home/bdetweiler/src/Data_Science/stat-8416-final-project/') 
source("DBDA2Eprograms/DBDA2E-utilities.R")

options(scipen=999)
visas <- readRDS('H1BVisas.rds')

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
    
visas.by.job %>% 
    filter(fy == 2015) %>%
    group_by(status) %>%
    summarise(n=n(), sum(total_workers))

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


final.shiny <- readRDS('shiny/ShinyDatset.rds')

######################## Presentation stuff

    # Input conditionals
    final.shiny.plot <- final.shiny %>%
                       dplyr::filter(normalized_wage > 0) %>%
                       dplyr::filter(normalized_wage < 500000) %>%
                       dplyr::filter(visa_class %in% 'H-1B')
   
    # Final aggregation 
    final.shiny.plot <- final.shiny.plot %>% 
                       group_by(employer_state, visa_class) %>%           #  We'll also allow grouping by fy and visa_class
                       summarise(med = median(normalized_wage), 
                                 mean = mean(normalized_wage), 
                                 min = min(normalized_wage),
                                 max = max(normalized_wage),
                                 n = n()) %>%
                       arrange(med)

    final.shiny.plot$employer_state <- 
      factor(final.shiny.plot$employer_state, levels = final.shiny.plot$employer_state)
   
    neb.med <- final.shiny.plot$med[which(final.shiny.plot$employer_state == 'nebraska')]
    
    ggplot(final.shiny.plot, aes(y = med, x = employer_state, col = n)) +
      geom_point(stat="identity") +
      scale_y_continuous(labels = scales::comma) +
      scale_colour_gradient(name="LCA Requests", labels = scales::comma, low = "#f7b7b7", high = "#8e0000") +
      geom_text(aes(x = "nebraska", y = neb.med - 10000, label = paste0("Nebraska median: $", neb.med))) +
      coord_flip() +
      labs(x = "State", y = "Median Wage")

##############################################################################
#    Analytics
##############################################################################

        
attorneys <- visas %>%
  filter(!is.na(agent_attorney_first_name)) %>%
  filter(agent_attorney_state == 'DC') %>%
  filter(status == 'CERTIFIED') %>%
  select(fy, 
         employer_state, 
         worksite_state, 
         total_workers, 
         status, 
         agent_attorney_first_name, 
         agent_attorney_last_name, 
         agent_attorney_city, 
         agent_attorney_state) %>%
    group_by(agent_attorney_last_name, agent_attorney_first_name, agent_attorney_city, agent_attorney_state, status) %>%
    summarise(n=n()) %>%
    arrange(desc(n))

# Only take top attorney
attorneys <- attorneys[1,] 

for (i in state.abb) {
  top.attorney <- visas %>%
    filter(!is.na(agent_attorney_first_name)) %>%
    filter(agent_attorney_state == i) %>%
    filter(status == 'CERTIFIED') %>%
    select(fy, 
           employer_state, 
           worksite_state, 
           total_workers, 
           status, 
           agent_attorney_first_name, 
           agent_attorney_last_name, 
           agent_attorney_city, 
           agent_attorney_state) %>%
      group_by(agent_attorney_last_name, agent_attorney_first_name, agent_attorney_city, agent_attorney_state, status) %>%
      summarise(n=n()) %>%
      arrange(desc(n))
  
  # Only take top attorney
  top.attorney <- top.attorney[1,] 
 
  attorneys <- rbind(attorneys, top.attorney)
}

attorneys$name <- paste(attorneys$agent_attorney_last_name, attorneys$agent_attorney_state, sep="-")
attorneys$name
attorneys$name <-  factor(attorneys$name, levels=attorneys$name[order(attorneys$n)])
ggplot(attorneys, aes(x=name, y=n)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Top Attorney in Each State (and D.C.)", x="Applications Certified", y="Attorney Name / State")
    
unique(attorneys$agent_attorney_state)
attorneys$agent_attorney_last_name <- gsub("\\W", " ", attorneys$agent_attorney_last_name)
unique(paste(toupper(attorneys$agent_attorney_first_name), toupper(attorneys$agent_attorney_last_name)))
table(attorneys$agent_attorney_last_name)

visas.samples <- visas[sample(nrow(visas), 2000), ]

visas.reduced <- visas %>%
  select(normalized_wage, normalized_prevailing_wage, fy) %>%
  filter(normalized_wage < 500000) %>%
  filter(normalized_wage > 0) %>%
  filter(!is.na(normalized_wage))
hist(visas.reduced)
saveRDS(visas.reduced, "visasreduced.rds")

##############################################################################
#    Bayesian Analysis
##############################################################################

visas.reduced <- readRDS("visasreduced.rds")
visas.reduced <- visas.reduced %>%
  filter(!is.na(normalized_prevailing_wage)) %>%
  filter(normalized_prevailing_wage > 0)

unique(visas.reduced$fy)

hist(visas.reduced, breaks = 100, freq=F)

visas.reduced.samples <- visas.reduced[sample(nrow(visas.reduced), 1000), ]
visas.reduced.normalized_wage <- visas.reduced.samples$normalized_wage
visas.reduced.normalized_prevailing_wage <- visas.reduced.samples$normalized_prevailing_wage


hist(log(visas.reduced$normalized_wage), 
     freq = FALSE, 
     xlim = c(9, 15), 
     breaks = 100, 
     col = rgb(0, 0, 1), 
     main = "Density of Normalized Wages (Population overlayed by Sample)",
     xlab = "Noramlized Wage")


hist(visas.reduced.normalized_wage, 
     freq = FALSE, 
     xlim = c(0, 250000), 
     breaks = 100, 
     col = rgb(1, 0, 0, 1/3), 
     add = TRUE)

hist(visas.reduced$normalized_prevailing_wage,
     freq = FALSE, 
     xlim = c(0, 250000), 
     breaks = 100, 
     col = rgb(0, 0, 1), 
     main = "Density of Normalized Wages (Population overlayed by Sample)",
     xlab = "Noramlized Wage")
hist(visas.reduced.normalized_prevailing_wage, 
     freq = FALSE, 
     xlim = c(0, 250000), 
     breaks = 100, 
     col = rgb(1, 0, 0, 1/3), 
     add = TRUE)

modelString ="
model {
  for (i in 1:N) {
    y[i] ~ dlnorm(muOfLogY, tauOfY) 
    x[i] ~ dlnorm(muOfLogX, tauOfX)
  }

  tauOfY <- 1 / pow(sigmaOfLogY, 2)
  tauOfX <- 1 / pow(sigmaOfLogX, 2)

  sigmaOfLogY ~ dunif(0.001 * sdOfLogY, 1000 * sdOfLogY)
  sigmaOfLogX ~ dunif(0.001 * sdOfLogX, 1000 * sdOfLogX)

  muOfLogY ~ dnorm(meanOfLogY, 0.001 * (1 / pow(sdOfLogY, 2)))
  muOfLogX ~ dnorm(meanOfLogX, 0.001 * (1 / pow(sdOfLogX, 2)))

  muOfY <- exp(muOfLogY + (pow(sigmaOfLogY, 2) / 2))
  muOfX <- exp(muOfLogX + (pow(sigmaOfLogX, 2) / 2))

  sigmaOfY <- sqrt(exp(2 * muOfLogY + pow(sigmaOfLogY, 2)) * (exp(pow(sigmaOfLogY, 2)) - 1))
  sigmaOfX <- sqrt(exp(2 * muOfLogX + pow(sigmaOfLogX, 2)) * (exp(pow(sigmaOfLogX, 2)) - 1))

  modeOfY <- exp(muOfLogY - pow(sigmaOfLogY, 2))
  modeOfX <- exp(muOfLogX - pow(sigmaOfLogX, 2))

  deltaMu <- muOfX - muOfY
}
"


writeLines(modelString, con="wages.jags")

trueMy <- mean(visas.reduced$normalized_wage)
trueSDy <- sd(visas.reduced$normalized_wage)
trueMx <- mean(visas.reduced$normalized_prevailing_wage)
trueSDx <- sd(visas.reduced$normalized_prevailing_wage)

log(visas.reduced.normalized_wage - mean(visas.reduced.normalized_wage) / 
      (sd(visas.reduced.normalized_wage) * trueSDy + trueMy))

LogY <- log((visas.reduced.normalized_wage - mean(visas.reduced.normalized_wage)) / 
              sd(visas.reduced.normalized_wage) * trueSDy + trueMy)

LogX <- log(visas.reduced.normalized_prevailing_wage - mean(visas.reduced.normalized_prevailing_wage) /
  (sd(visas.reduced.normalized_prevailing_wage) * trueSDx + trueMx))

y <- exp(LogY)
x <- exp(LogX)

dataList <- list(y = y,
                 x = x,
                 N = length(visas.reduced.normalized_wage),
                 meanOfLogY = mean(LogY),
                 sdOfLogY = sd(LogY),
                 meanOfLogX = mean(LogX),
                 sdOfLogX = sd(LogX))

wages.model = jags.model(file="wages.jags", 
                             data=dataList,
                             n.chains=4)

update(wages.model, n.iter=1000000)

wages.samples <- coda.samples(wages.model, 
                              n.iter=500000, 
                              variable.names=c("muOfY", 
                                               "modeOfY", 
                                               "sigmaOfY", 
                                               "muOfX",
                                               "modeOfX",
                                               "sigmaOfX",
                                               "deltaMu"), 
                              thin=5)

summary(wages.samples)
saveRDS(wages.samples, 'wages.samples.rds')
wages.samp.M <- as.matrix(wages.samples)

hist((-1 * wages.samp.M[,"deltaMu"]), breaks = 100, freq = F)
wage.minus.prevailing.CI <- quantile(x = (-1 * wages.samp.M[, "deltaMu"]), probs = c(0.025, 0.975))
wage.minus.prevailing.CI
abline(v=c(wage.minus.prevailing.CI[[1]], wage.minus.prevailing.CI[[2]]), col = "red")SD

hist(wages.samp.M[,"muOfX"], breaks = 100, freq = F)


head(wages.samp.M)

#diagMCMC(codaObject = wages.samples, parName="muOfY")

log(sd(visas.reduced$normalized_wage))
##### Checking samples vs. population
hist(visas.reduced$normalized_wage, 
     freq = FALSE, 
     xlim = c(0, max(visas.reduced$normalized_wage)), 
     breaks = 100, 
     col = rgb(0, 0, 1), 
     main = "Density of Normalized Wages (Population overlayed by Sample)",
     xlab = "Noramlized Wage")

newY <- rlnorm(n = length(wages.samp.M[, "muOfY"]), meanlog = log(mean(wages.samp.M[, "muOfY"]))-0.18, sdlog = (sd(LogY)-0.08 ))
#newY <- rlnorm(n = length(wages.samp.M[, "muOfY"]), meanlog = 11, sdlog = 1.34)
summary(newY)
hist(newY,
     freq = FALSE, 
     breaks = 100, 
     col = rgb(1, 0, 0, 1/3), 
     add = TRUE)

logMuOfY <- log(mean(wages.samp.M[,"muOfY"]))
exp(logMuOfY + .)

muCI <- quantile(wages.samp.M[, "muOfY"], probs =c(0.025, 0.975))

logSdOfY <- log(mean(wages.samp.M[,"sigmaOfY"]))
logSdOfY
logSdOfY <- log(mean(wages.samp.M[,"sigmaOfY"]))
logSdOfY

sample.vals.lower <- rlnorm(n=10000, meanlog = log(muCI[[1]]), sdlog = 1)
sample.vals.upper <- rlnorm(n=10000, meanlog = log(muCI[[2]]), sdlog = 1)

d <- density(sample.vals.lower,
        adjust = 1,
        kernel = c("epanechnikov"))

polygon(d, col=rgb(1, 0, 0, 1/3))

d <- density(sample.vals.upper,
        adjust = 1,
        kernel = c("epanechnikov"))

polygon(d, col=rgb(0, 1, 0, 1/3))

summary(visas.reduced$normalized_prevailing_wage)
hist(visas.reduced$normalized_prevailing_wage,
     freq = FALSE, 
     breaks = 100, 
     col = rgb(1, 0, 0, 1/4), 
     add = TRUE)

hist(log(visas.reduced$normalized_wage), 
     freq = FALSE, 
     xlim = c(9, 15), 
     breaks = 100, 
     col = rgb(0, 0, 1), 
     main = "Density of Normalized Wages (Population overlayed by Sample)",
     xlab = "Noramlized Wage")

log(sqrt(mean(wages.samp.M[,"sigmaOfY"])))

x <- seq(0, 15, length=1000)
hx <- dnorm(x, mean = 11.2, sd = .3)
            #meanlog = log(mean(wages.samp.M[,"muOfY"])), sdlog = log(mean(wages.samp.M[,"sigmaOfY"])))
hx
exp(11.2)
exp(.3)
summary(hx)
colors <- c("red")
lines(x, hx, type="l", col="red")




# Glassdoor Data
# <a href='https://www.glassdoor.com/index.htm'>powered by <img src='https://www.glassdoor.com/static/img/api/glassdoor_logo_80.png' title='Job Search' /></a>


