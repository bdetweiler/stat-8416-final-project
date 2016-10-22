library(readxl)
library(dplyr)

#setwd("C:/Users/pavi/Desktop/UNO/IntroDataScience/Project/Data/testdat")

# data frame with columns that arent there in 2008-2014 dataset
add_cols = data.frame('JOB_INFO_WORK_POSTAL_CODE'=NA, 'EMPLOYER_NUM_EMPLOYEES'=NA,	'EMPLOYER_YR_ESTAB'=NA,	'EMPLOYER_PHONE'=NA,	'AGENT_FIRM_NAME'=NA,	'AGENT_CITY'=NA,	'AGENT_STATE'=NA,	'PW_SOC_TITLE'=NA,	'JOB_INFO_JOB_TITLE'=NA,	'JOB_INFO_EDUCATION'=NA,	'JOB_INFO_MAJOR'=NA,	'JOB_INFO_FOREIGN_ED'=NA,	'RI_1ST_AD_NEWSPAPER_NAME'=NA,	'RI_2ND_AD_NEWSPAPER_NAME'=NA,	'FW_INFO_YR_REL_EDU_COMPLETED'=NA,	'FOREIGN_WORKER_INFO_INST'=NA, 'JOB_INFO_EXPERIENCE_NUM_MONTHS'=NA)


dataset <- function (ds, y) {
  
  if (y >= 2015) {
    ds = subset(ds,select=c(YEAR, CASE_NUMBER,	DECISION_DATE,	CASE_STATUS,	EMPLOYER_NAME,	EMPLOYER_ADDRESS_1,	EMPLOYER_ADDRESS_2,	
                            EMPLOYER_CITY,	EMPLOYER_STATE,	EMPLOYER_POSTAL_CODE,	NAICS_US_CODE,	NAICS_US_TITLE,	
                            PW_SOC_CODE,	PW_JOB_TITLE_9089,	PW_LEVEL_9089,	PW_AMOUNT_9089,	PW_UNIT_OF_PAY_9089,	
                            WAGE_OFFER_FROM_9089,	WAGE_OFFER_TO_9089,	WAGE_OFFER_UNIT_OF_PAY_9089,	
                            JOB_INFO_WORK_CITY, 	JOB_INFO_WORK_STATE,	COUNTRY_OF_CITIZENSHIP,	CLASS_OF_ADMISSION
                            
                            , JOB_INFO_WORK_POSTAL_CODE ,EMPLOYER_NUM_EMPLOYEES,	EMPLOYER_YR_ESTAB,	EMPLOYER_PHONE,	AGENT_FIRM_NAME,	AGENT_CITY,	
                            AGENT_STATE,	PW_SOC_TITLE,	JOB_INFO_JOB_TITLE,	JOB_INFO_EDUCATION,	JOB_INFO_MAJOR,	JOB_INFO_FOREIGN_ED,	
                            RI_1ST_AD_NEWSPAPER_NAME,	RI_2ND_AD_NEWSPAPER_NAME,	FW_INFO_YR_REL_EDU_COMPLETED,	
                            FOREIGN_WORKER_INFO_INST, JOB_INFO_EXPERIENCE_NUM_MONTHS
    ))
    return(ds)
  }
  else {
    ds = subset(ds,select=c(YEAR, CASE_NUMBER,	DECISION_DATE,	CASE_STATUS,	EMPLOYER_NAME,	EMPLOYER_ADDRESS_1,	EMPLOYER_ADDRESS_2,	
                            EMPLOYER_CITY,	EMPLOYER_STATE,	EMPLOYER_POSTAL_CODE,	NAICS_US_CODE,	NAICS_US_TITLE,	
                            PW_SOC_CODE,	PW_JOB_TITLE_9089,	PW_LEVEL_9089,	PW_AMOUNT_9089,	PW_UNIT_OF_PAY_9089,	
                            WAGE_OFFER_FROM_9089,	WAGE_OFFER_TO_9089,	WAGE_OFFER_UNIT_OF_PAY_9089,	
                            JOB_INFO_WORK_CITY,	JOB_INFO_WORK_STATE,	COUNTRY_OF_CITIZENSHIP,	CLASS_OF_ADMISSION
    ))
    return(data.frame(ds,add_cols))
  }
}

x =read_excel("PERM_FY2008.xlsx",col_names = TRUE,sheet = 1,na = "", col_types = rep('text',25))
names(x) <- gsub(" ", "_", names(x))
names(x) <- gsub("2007_","", names(x))
x=dplyr::rename(x,COUNTRY_OF_CITIZENSHIP = COUNTRY_OF_CITZENSHIP)
x=subset(x,select=-c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
y =read_excel("PERM_FY2008.xlsx",col_names = TRUE,sheet = 1,na = "")
names(y) <- gsub(" ", "_", names(y))
y=subset(y,select=c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
file=cbind(x,y, YEAR='2008',EMPLOYER_ADDRESS_2=NA)
file2008=dataset(file,2008)


x =read_excel("PERM_FY2009.xlsx",col_names = TRUE,sheet = 1,na = "", col_types = rep('text',25))
names(x) <- gsub(" ", "_", names(x))
names(x) <- gsub("2007_","", names(x))
x=dplyr::rename(x,COUNTRY_OF_CITIZENSHIP=COUNTRY_OF_CITZENSHIP)
x=subset(x,select=-c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))

y =read_excel("PERM_FY2009.xlsx",col_names = TRUE,sheet = 1,na = "")
names(y) <- gsub(" ", "_", names(y))
y=subset(y,select=c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
file=cbind(x,y, YEAR='2009')
file2009=dataset(file,2009)




x =read_excel("PERM_FY2010.xlsx",col_names = TRUE,sheet = 1,na = "", col_types = rep('text',27))
x=subset(x,select=-c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
names(x) <- gsub(" ", "_", names(x))
names(x) <- gsub("2007_","", names(x))
x=dplyr::rename(x,CASE_NUMBER=CASE_NO,COUNTRY_OF_CITIZENSHIP=COUNTRY_OF_CITZENSHIP)
y =read_excel("PERM_FY2010.xlsx",col_names = TRUE,sheet = 1,na = "")
y=subset(y,select=c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
file=cbind(x,y, YEAR='2010')
file2010=dataset(file,2010)


x =read_excel("PERM_FY2011.xlsx",col_names = TRUE,sheet = 1,na = "", col_types = rep('text',27))
x=subset(x,select=-c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
names(x) <- gsub(" ", "_", names(x))
names(x) <- gsub("2007_","", names(x))
x=dplyr::rename(x,CASE_NUMBER=CASE_NO,COUNTRY_OF_CITIZENSHIP=COUNTRY_OF_CITZENSHIP)
y =read_excel("PERM_FY2011.xlsx",col_names = TRUE,sheet = 1,na = "")
y=subset(y,select=c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
file=cbind(x,y, YEAR='2011')
file2011=dataset(file,2011)


x =read_excel("PERM_FY2012_Q4.xlsx",col_names = TRUE,sheet = 1,na = "", col_types = rep('text',26))
x=subset(x,select=-c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
names(x) <- gsub(" ", "_", names(x))
names(x) <- gsub("2007_","", names(x))
x=dplyr::rename(x,CASE_NUMBER=CASE_NO,COUNTRY_OF_CITIZENSHIP=COUNTRY_OF_CITZENSHIP)
y =read_excel("PERM_FY2012_Q4.xlsx",col_names = TRUE,sheet = 1,na = "")
y=subset(y,select=c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
file=cbind(x,y, YEAR='2012',EMPLOYER_ADDRESS_2=NA)
file2012=dataset(file,2012)


x =read_excel("PERM_FY2013.xlsx",col_names = TRUE,sheet = 1,na = "", col_types = rep('text',27))
names(x) <- gsub(" ", "_", names(x))
names(x) <- toupper(gsub("2007_","", names(x)))
x=dplyr::rename(x,CASE_NUMBER=CASE_NO,WAGE_OFFER_FROM_9089=WAGE_OFFERED_FROM_9089,WAGE_OFFER_TO_9089=WAGE_OFFERED_TO_9089)
x=subset(x,select=-c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
y =read_excel("PERM_FY2013.xlsx",col_names = TRUE,sheet = 1,na = "")
names(y) <- gsub(" ", "_", names(y))
names(y) <- toupper(gsub("2007_","", names(y)))
y=dplyr::rename(y,WAGE_OFFER_FROM_9089=WAGE_OFFERED_FROM_9089,WAGE_OFFER_TO_9089=WAGE_OFFERED_TO_9089)
y=subset(y,select=c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
file=cbind(x,y, YEAR='2013')
file$WAGE_OFFER_UNIT_OF_PAY_9089=file$PW_UNIT_OF_PAY_9089
file2013=dataset(file,2013)


x =read_excel("PERM_FY14_Q4.xlsx",col_names = TRUE,sheet = 1,na = "", col_types = rep('text',27))
names(x) <- gsub(" ", "_", names(x))
names(x) <- toupper(gsub("2007_","", names(x)))
x=dplyr::rename(x,CASE_NUMBER=CASE_NO,WAGE_OFFER_FROM_9089=WAGE_OFFERED_FROM_9089,WAGE_OFFER_TO_9089=WAGE_OFFERED_TO_9089,WAGE_OFFER_UNIT_OF_PAY_9089=WAGE_OFFERED_UNIT_OF_PAY_9089)
x=subset(x,select=-c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
y =read_excel("PERM_FY14_Q4.xlsx",col_names = TRUE,sheet = 1,na = "")
names(y) <- gsub(" ", "_", names(y))
names(y) <- toupper(gsub("2007_","", names(y)))
y=dplyr::rename(y,WAGE_OFFER_FROM_9089=WAGE_OFFERED_FROM_9089,WAGE_OFFER_TO_9089=WAGE_OFFERED_TO_9089)
y=subset(y,select=c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
file=cbind(x,y, YEAR='2014')
file2014=dataset(file,2014)



x =read_excel("PERM_Disclosure_Data_FY15_Q4.xlsx",col_names = TRUE,sheet = 1,na = "", col_types = rep('text',125))
names(x) <- toupper(names(x))
x=subset(x,select=-c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
y =read_excel("PERM_Disclosure_Data_FY15_Q4.xlsx",col_names = TRUE,sheet = 1,na = "")
y=subset(y,select=c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
file=cbind(x,y, YEAR='2015')
file2015=dataset(file,2015)


x =read_excel("PERM_Disclosure_Data_FY16.xlsx",col_names = TRUE,sheet = 1,na = "", col_types = rep('text',125))
x=subset(x,select=-c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089,WAGE_OFFER_TO_9089))
y =read_excel("PERM_Disclosure_Data_FY16.xlsx",col_names = TRUE,sheet = 1,na = "")
y=subset(y,select=c(DECISION_DATE,PW_AMOUNT_9089,WAGE_OFFER_FROM_9089,WAGE_OFFER_TO_9089))
file=cbind(x,y, YEAR='2016')
file2016=dataset(file,2016)


a = rbind(file2008,file2009,file2010,file2011,file2012,file2013,file2014,file2015,file2016)

saveRDS(a,"PermDataPrep.rds")

gc(verbose = T)
rm(list=ls(all=TRUE))





mysample = readRDS(file="PermDataPrep.rds")
# mysample <- mysample[sample(1:nrow(mysample), 500,replace=FALSE),]
mysample=transform(mysample,DECISION_DATE=as.Date(mysample$DECISION_DATE, format = '%m/%d/%Y'))
# write.csv(mysample, file = "sampleperm.csv")

# changing all character variables to upper case

permds <- mysample %>% mutate_if(is.factor,as.character)%>% mutate_if(is.character,toupper)


# converting time values to standard name format

permds$PW_UNIT_OF_PAY_9089[permds$PW_UNIT_OF_PAY_9089 == 'HOUR'] <- 'HR'
permds$PW_UNIT_OF_PAY_9089[permds$PW_UNIT_OF_PAY_9089 == 'WEEK'] <- 'WK'
permds$PW_UNIT_OF_PAY_9089[permds$PW_UNIT_OF_PAY_9089 == 'YEAR'] <- 'YR'
permds$PW_UNIT_OF_PAY_9089[permds$PW_UNIT_OF_PAY_9089 == 'MONTH'] <- 'MTH'
permds$PW_UNIT_OF_PAY_9089[permds$PW_UNIT_OF_PAY_9089 == 'BI'] <- 'BIWK'

permds$WAGE_OFFER_UNIT_OF_PAY_9089[permds$WAGE_OFFER_UNIT_OF_PAY_9089 == 'HOUR'] <- 'HR'
permds$WAGE_OFFER_UNIT_OF_PAY_9089[permds$WAGE_OFFER_UNIT_OF_PAY_9089 == 'WEEK'] <- 'WK'
permds$WAGE_OFFER_UNIT_OF_PAY_9089[permds$WAGE_OFFER_UNIT_OF_PAY_9089 == 'YEAR'] <- 'YR'
permds$WAGE_OFFER_UNIT_OF_PAY_9089[permds$WAGE_OFFER_UNIT_OF_PAY_9089 == 'MONTH'] <- 'MTH'
permds$WAGE_OFFER_UNIT_OF_PAY_9089[permds$WAGE_OFFER_UNIT_OF_PAY_9089 == 'BI'] <- 'BIWK'

# prevailing wage normalized and renamed

normalized_prevailing_wage <- permds$PW_UNIT_OF_PAY_9089
normalized_prevailing_wage[which(normalized_prevailing_wage == 'YR')] <- 1
normalized_prevailing_wage[which(normalized_prevailing_wage == 'HR')] <- 40 * 52
normalized_prevailing_wage[which(normalized_prevailing_wage == 'BIWK')] <- 52 / 2
normalized_prevailing_wage[which(normalized_prevailing_wage == 'WK')] <- 52
normalized_prevailing_wage[which(normalized_prevailing_wage == 'MTH')] <- 12
normalized_prevailing_wage[which(is.na(normalized_prevailing_wage))] <- 0

normalized_prevailing_wage <- as.numeric(normalized_prevailing_wage)
permds$PW_AMOUNT_9089 <- normalized_prevailing_wage * permds$PW_AMOUNT_9089


# wage from and maximum normalized and renamed

normalized_wage <- permds$WAGE_OFFER_UNIT_OF_PAY_9089
normalized_wage[which(normalized_wage == 'YR')] <- 1
normalized_wage[which(normalized_wage == 'HR')] <- 40 * 52
normalized_wage[which(normalized_wage == 'BIWK')] <- 52 / 2
normalized_wage[which(normalized_wage == 'WK')] <- 52
normalized_wage[which(normalized_wage == 'MTH')] <- 12
normalized_wage[which(is.na(normalized_wage))] <- 0

normalized_wage <- as.numeric(normalized_wage)
normalized_wage_max <- as.numeric(normalized_wage)

permds$WAGE_OFFER_FROM_9089[which(is.na(permds$WAGE_OFFER_FROM_9089))] <- 0
permds$WAGE_OFFER_TO_9089[which(is.na(permds$WAGE_OFFER_TO_9089))] <- 0
permds$normalized_wage <- normalized_wage * permds$WAGE_OFFER_FROM_9089
permds$normalized_wage_max <- normalized_wage_max * permds$WAGE_OFFER_TO_9089

# renaming job postal code to worksite zip code

permds$worksite_zip_code <- gsub(" ", "", permds$JOB_INFO_WORK_POSTAL_CODE)
permds$worksite_zip_code <- gsub("-[[:digit:]]*", "", permds$worksite_zip_code)

# renaming employer postal code to employer zip code

permds$employer_zip_code <- gsub(" ", "", permds$EMPLOYER_POSTAL_CODE)
permds$employer_zip_code <- gsub("-[[:digit:]]*", "", permds$employer_zip_code)

st.codes<-data.frame(
  statecode=as.character(c("AL",	"AK",	"AS",	"AZ",	"AR",	"CA",	"CO",	"CT",	"DE",	"DC",	"FM",	"FL",	"GA",	"GU",	"HI",	
                           "ID",	"IL",	"IN",	"IA",	"KS",	"KY",	"LA",	"ME",	"MH",	"MD",	"MA",	"MI",	"MN",	"MS",	"MO",	"MT",	"NE",	
                           "NV",	"NH",	"NJ",	"NM",	"NY",	"NC",	"ND",	"MP",	"OH",	"OK",	"OR",	"PW",	"PA",	"PR",	"RI",	"SC",	"SD",	
                           "TN",	"TX",	"UT",	"VT",	"VI",	"VA",	"WA",	"WV",	"WI",	"WY"
  )),
  statename=as.character(c("ALABAMA",	"ALASKA",	"AMERICAN SAMOA",	"ARIZONA",	"ARKANSAS",	"CALIFORNIA",	"COLORADO",	"CONNECTICUT",	
                           "DELAWARE",	"DISTRICT OF COLUMBIA",	"FEDERATED STATES OF MICRONESIA",	"FLORIDA",	"GEORGIA",	"GUAM",	"HAWAII",	"IDAHO",	
                           "ILLINOIS",	"INDIANA",	"IOWA",	"KANSAS",	"KENTUCKY",	"LOUISIANA",	"MAINE",	"MARSHALL ISLANDS",	"MARYLAND",	"MASSACHUSETTS",	
                           "MICHIGAN",	"MINNESOTA",	"MISSISSIPPI",	"MISSOURI",	"MONTANA",	"NEBRASKA",	"NEVADA",	"NEW HAMPSHIRE",	"NEW JERSEY",	"NEW MEXICO",	
                           "NEW YORK",	"NORTH CAROLINA",	"NORTH DAKOTA",	"NORTHERN MARIANA ISLANDS",	"OHIO",	"OKLAHOMA",	"OREGON",	"PALAU",	"PENNSYLVANIA",	
                           "PUERTO RICO",	"RHODE ISLAND",	"SOUTH CAROLINA",	"SOUTH DAKOTA",	"TENNESSEE",	"TEXAS",	"UTAH",	"VERMONT",	"VIRGIN ISLANDS",	
                           "VIRGINIA",	"WASHINGTON",	"WEST VIRGINIA",	"WISCONSIN",	"WYOMING"
  )), stringsAsFactors = FALSE
)

# merge state codes with data frame and those items with full statenames are replaced by employerstate
permds=left_join(permds, st.codes, by = c("EMPLOYER_STATE"="statecode"))
permds$statename[which(is.na(permds$statename))]<-permds$EMPLOYER_STATE[which(is.na(permds$statename))]
permds$EMPLOYER_STATE<-permds$statename
permds=subset(permds, select = -c(statename))

permds=left_join(permds, st.codes, by = c("JOB_INFO_WORK_STATE"="statecode"))
permds$statename[which(is.na(permds$statename))]<-permds$JOB_INFO_WORK_STATE[which(is.na(permds$statename))]
permds$JOB_INFO_WORK_STATE<-permds$statename
permds=subset(permds, select = -c(statename))



permds$ID  <- 1:nrow(permds)

empmapdat = permds %>%  group_by(EMPLOYER_NAME,EMPLOYER_ADDRESS_1,EMPLOYER_CITY,
                                 EMPLOYER_STATE,employer_zip_code) %>%  
  summarise(count = n(), mean_salary = mean(normalized_wage, na.rm = TRUE)) %>%  
  arrange(desc(count),desc(mean_salary)) 
empmapdat$ID  <- 1:nrow(empmapdat)

saveRDS(permds,"PermData.rds")
saveRDS(empmapdat,"PermEmpMapsdat.rds")

rm(list=ls(all=TRUE))


