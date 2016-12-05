% TODO: Maybe run a PCA on the data to see what the most likely predictors of higher-than-median wage is
<<pca, fig.cap='Actual visa wages compared with the US median wage by occupation and state. The red line indicates where the visa wage equals the median wage.', fig.width=10, fig.height=5, fig.align='center', fig.pos='hbtp', out.width='.45\\linewidth', echo=FALSE, error=FALSE, warning=FALSE, message=FALSE>>=
fy2015 <- visas %>% 
  filter(fy == 2015)
fy2015$soc_code <- gsub("-", ".", fy2015$soc_code) 
fy2015$soc_code <- as.numeric(fy2015$soc_code)
fy2015$ST_OCC <- paste(fy2015$worksite_state, fy2015$soc_code)
fy2015$ST_OCC <- gsub("\\.[0-9][0-9]$", "", fy2015$ST_OCC)

fy2015 <- merge(fy2015, bls.fy2015, by.x="ST_OCC", by.y="ST_OCC", all.x=T, all.y=F)

fy2015 <- filter(fy2015, !is.na(A_MEDIAN)) %>%
  select(visa_class, total_workers, soc_code, normalized_wage, normalized_prevailing_wage, A_MEAN, A_MEDIAN)

fy2015$E3 <- 0
fy2015$H1B <- 0
fy2015$H1B1C <- 0
fy2015$H1B1S <- 0
fy2015$E3[fy2015$visa_class == 'E-3 Australian'] <- 1
fy2015$H1B[fy2015$visa_class == 'H-1B'] <- 1
fy2015$H1B1C[fy2015$visa_class == 'H-1B1 Chile'] <- 1
fy2015$H1B1S[fy2015$visa_class == 'H-1B1 Singapore'] <- 1

fy2015$A_MEAN <- gsub(",", "", fy2015$A_MEAN)
fy2015$A_MEAN <- as.numeric(fy2015$A_MEAN)
fy2015$A_MEDIAN <- gsub(",", "", fy2015$A_MEDIAN)
fy2015$A_MEDIAN <- as.numeric(fy2015$A_MEDIAN)
fy2015$right.skew <- as.numeric(fy2015$A_MEAN > fy2015$A_MEDIAN)
fy2015$pw.above.med <- as.numeric(fy2015$normalized_prevailing_wage > fy2015$A_MEDIAN)
fy2015$wage.above.med <- as.numeric(fy2015$normalized_wage > fy2015$A_MEDIAN)

# Remove visa_class
fy2015 <- fy2015 %>% 
  select(total_workers, soc_code, E3, H1B, H1B1C, H1B1S, right.skew, pw.above.med, wage.above.med) %>%
  filter(!is.na(right.skew)) %>%
  filter(!is.na(pw.above.med)) %>%
  filter(!is.na(wage.above.med))
  
# Remove normalized_wage, normalized_prevailing_wage, A_MEAN, and A_MEDIAN
str(fy2015)

fy2015.train <- fy2015[1:299612, ]
fy2015.test <- fy2015[299613:599225, ]


head(fy2015)

library(stats)
library(devtools)
install_github("vqv/ggbiplot")
pca <- princomp(na.omit(fy2015.train), cor = TRUE)
summary(pca)
train_reduced <- predict(pca, fy2015.train)
test_reduced <- predict(pca, fy2015.test)

str(train_reduced)
str(test_reduced)
summary(test_reduced)
@



