library(data.table)
library(dplyr)
library(mgcv)
library(bit64)
library(ggplot2)

# For some reason, prevailing wage is printing in scientific notation, which is annoying to look at
options(scipen=999)

######################################################################################################
#                                        2008                                                        #
######################################################################################################

# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2008	H-1B_Case_Data_FY2008	CASE_NO	SUBMITTED_DATE	PROGRAM	NAME	ADDRESS1	ADDRESS2	CITY	STATE	POSTAL_CODE	CITY_1	STATE_1	NA	NBR_IMMIGRANTS	DOL_DECISION_DATE	BEGIN_DATE	END_DATE	CERTIFIED_BEGIN_DATE	CERTIFIED_END_DATE	JOB_TITLE	JOB_CODE	APPROVAL_STATUS	WITHDRAWN	NA	NA	OCCUPATIONAL_TITLE	WAGE_RATE_1	NA	NA	RATE_PER_1	PREVAILING_WAGE_1	WAGE_SOURCE_1	MAX_RATE_1	PART_TIME_1	NA	NA	NA	NA', split = '\t'))
colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

					
# Copied and pasted from second part of excel sheet
renameCols <- unlist(strsplit('submitted_date	case_number	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	total_workers	begin_date	end_date	job_title	dol_decision_date	certified_begin_date	certified_end_date	job_code	status	wage_rate	wage_unit	max_wage	part_time	worksite_city	worksite_state	prevailing_wage	prevailing_wage_unit	withdrawn	occupational_title', split = '\t'))
file_name <- 'H-1B_2008.csv'
fy2008 <- fread(file_name, 
                header = TRUE, 
                select = colsToRead,
                stringsAsFactors = FALSE,
                col.names = renameCols)

# Add two additional columns for fiscall year and file_name, as well as any missing columns
headers <- c(colnames(fy2008), 
             'fy', 
             'file_name',
             'worksite_zip_code',
             'soc_code',
             'soc_name',
             'wage_rate_from',
             'wage_rate_to',
             'agent_attorney_first_name',
             'agent_attorney_last_name',
             'agent_attorney_city',
             'agent_attorney_state')

# Add the year as a column
fy2008$fy <- rep(2008, dim(fy2008)[1])

# Add the filename as a column
fy2008$file_name <- rep(file_name, dim(fy2008)[1])

# Missing columns
fy2008$worksite_zip_code <- rep(NA, dim(fy2008)[1])
fy2008$soc_code <- rep(NA, dim(fy2008)[1])
fy2008$soc_name <- rep(NA, dim(fy2008)[1])
fy2008$wage_rate_from <- rep(NA, dim(fy2008)[1])
fy2008$wage_rate_to <- rep(NA, dim(fy2008)[1])
fy2008$agent_attorney_first_name <- rep(NA, dim(fy2008)[1])
fy2008$agent_attorney_last_name <- rep(NA, dim(fy2008)[1])
fy2008$agent_attorney_city <- rep(NA, dim(fy2008)[1])
fy2008$agent_attorney_state <- rep(NA, dim(fy2008)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2008) <- headers

fy2008$prevailing_wage <- as.numeric(fy2008$prevailing_wage)
fy2008$employer_zip_code <- as.character(fy2008$employer_zip_code)
fy2008$worksite_zip_code <- as.character(fy2008$worksite_zip_code)

fy2008$wage_rate <- as.numeric(fy2008$wage_rate)
fy2008$wage_rate_from <- fy2008$wage_rate
fy2008$wage_rate_to <- fy2008$max_wage

# If we have a max_wage, assign that to wage_rate_to
fy2008$wage_rate_to[which(is.na(fy2008$wage_rate_to))] <- fy2008$max_wage[which(is.na(fy2008$wage_rate_to))]
# Where there is still NAs in wage_rate_to, set equal to wage_rate_from
fy2008$wage_rate_to[which(is.na(fy2008$wage_rate_to))] <- fy2008$wage_rate_from[which(is.na(fy2008$wage_rate_to))]
fy2008$wage_rate_to[which(fy2008$wage_rate_to == 0)] <- fy2008$wage_rate_from[which(fy2008$wage_rate_to == 0)]

# FY 2008 column goes by "PART_TIME"
fy2008$part_time <- fy2008$part_time == "Y"

columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2008, columnOrder)

######################################################################################################
#                                        2009 non-iCert                                              #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2009	H-1B_Case_Data_FY2009	CASE_NO	SUBMITTED_DATE	PROGRAM_DESIGNATION	EMPLOYER_NAME	EMPLOYER_ADDRESS1	EMPLOYER_ADDRESS2	EMPLOYER_CITY	EMPLOYER_STATE	EMPLOYER_POSTAL_CODE	CITY_1	STATE_1	NA	NBR_IMMIGRANTS	DOL_DECISION_DATE	BEGIN_DATE	END_DATE	NA	NA	JOB_TITLE	OCCUPATIONAL_CODE	APPROVAL_STATUS	WITHDRAWN	NA	NA	OCCUPATIONAL_TITLE	WAGE_RATE_1	NA	NA	RATE_PER_1	PREVAILING_WAGE_1	NA	MAX_RATE_1	PART_TIME_1	NA	NA	NA	NA', split = '\t'))

# Missing columnscolsToRead <- unlist(strsplit('2009	H-1B_Case_Data_FY2009	CASE_NO	SUBMITTED_DATE	PROGRAM_DESIGNATION	EMPLOYER_NAME	EMPLOYER_ADDRESS1	EMPLOYER_ADDRESS2	EMPLOYER_CITY	EMPLOYER_STATE	EMPLOYER_POSTAL_CODE	CITY_1	STATE_1	NA	NBR_IMMIGRANTS	DOL_DECISION_DATE	BEGIN_DATE	END_DATE	NA	NA	JOB_TITLE	OCCUPATIONAL_CODE	APPROVAL_STATUS	WITHDRAWN	NA	NA	OCCUPATIONAL_TITLE	WAGE_RATE_1	NA	NA	RATE_PER_1	PREVAILING_WAGE_1	NA	MAX_RATE_1	PART_TIME_1	NA	NA	NA	NA', split = '\t')) 
colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

# Copied and pasted from second part of excel sheet
renameCols <- unlist(strsplit('submitted_date	case_number	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	total_workers	begin_date	end_date	job_title	dol_decision_date	job_code	occupational_title	status	wage_rate	wage_unit	max_wage	part_time	worksite_city	worksite_state	prevailing_wage	withdrawn', split = '\t'))

file_name <- 'H-1B_2009.csv'
fy2009 <- fread(file_name, 
                header = TRUE, 
                select = colsToRead,
                stringsAsFactors = FALSE,
                col.names = renameCols)

# Add two additional columns for fiscall year and file_name, as well as any missing columns
headers <- c(colnames(fy2009), 
             'fy', 
             'file_name',
             'prevailing_wage_unit',
             'certified_begin_date',
             'certified_end_date',
             'worksite_zip_code',
             'soc_code',
             'soc_name',
             'wage_rate_from',
             'wage_rate_to',
             'agent_attorney_first_name',
             'agent_attorney_last_name',
             'agent_attorney_city',
             'agent_attorney_state')

# Add the year as a column
fy2009$fy <- rep(2009, dim(fy2009)[1])

# Add the filename as a column
fy2009$file_name <- rep(file_name, dim(fy2009)[1])
fy2009$prevailing_wage_unit <- rep(NA, dim(fy2009)[1])
fy2009$certified_begin_date <- rep(NA, dim(fy2009)[1])
fy2009$certified_end_date <- rep(NA, dim(fy2009)[1])
fy2009$worksite_zip_code <- rep(NA, dim(fy2009)[1])
fy2009$soc_code <- rep(NA, dim(fy2009)[1])
fy2009$soc_name <- rep(NA, dim(fy2009)[1])
fy2009$wage_rate_from <- rep(NA, dim(fy2009)[1])
fy2009$wage_rate_to <- rep(NA, dim(fy2009)[1])
fy2009$agent_attorney_first_name <- rep(NA, dim(fy2009)[1])
fy2009$agent_attorney_last_name <- rep(NA, dim(fy2009)[1])
fy2009$agent_attorney_city <- rep(NA, dim(fy2009)[1])
fy2009$agent_attorney_state <- rep(NA, dim(fy2009)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2009) <- headers

fy2009$prevailing_wage <- as.numeric(fy2009$prevailing_wage)
fy2009$employer_zip_code <- as.character(fy2009$employer_zip_code)
fy2009$worksite_zip_code <- as.character(fy2009$worksite_zip_code)

# 2009 only listed a single wage-rate
fy2009$wage_rate <- as.numeric(fy2009$wage_rate)
fy2009$wage_rate_from <- fy2009$wage_rate
fy2009$wage_rate_to <- fy2009$wage_rate

# If we have a max_wage, assign that to wage_rate_to
fy2009$wage_rate_to[which(is.na(fy2009$wage_rate_to))] <- fy2009$max_wage[which(is.na(fy2009$wage_rate_to))]
# Where there is still NAs in wage_rate_to, set equal to wage_rate_from
fy2009$wage_rate_to[which(is.na(fy2009$wage_rate_to))] <- fy2009$wage_rate_from[which(is.na(fy2009$wage_rate_to))]
fy2009$wage_rate_to[which(fy2009$wage_rate_to == 0)] <- fy2009$wage_rate_from[which(fy2009$wage_rate_to == 0)]

# FY 2009 non-Icert column goes by "PART_TIME"
fy2009$part_time <- fy2009$part_time == "Y"

columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2009, columnOrder)


######################################################################################################
#                                        2009 iCert                                                  #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2009	Icert_LCA_FY2009	LCA_CASE_NUMBER	LCA_CASE_SUBMIT	VISA_CLASS	LCA_CASE_EMPLOYER_NAME	LCA_CASE_EMPLOYER_ADDRESS	NA	LCA_CASE_EMPLOYER_CITY	LCA_CASE_EMPLOYER_STATE	LCA_CASE_EMPLOYER_POSTAL_CODE	LCA_CASE_WORKLOC1_CITY	LCA_CASE_WORKLOC1_STATE	NA	TOTAL_WORKERS	Decision_Date	LCA_CASE_EMPLOYMENT_START_DATE	LCA_CASE_EMPLOYMENT_END_DATE	NA	NA	LCA_CASE_JOB_TITLE	LCA_CASE_NAICS_CODE	STATUS	NA	LCA_CASE_SOC_CODE	LCA_CASE_SOC_NAME	NA	NA	LCA_CASE_WAGE_RATE_FROM	LCA_CASE_WAGE_RATE_TO	LCA_CASE_WAGE_RATE_UNIT	PW_1	PW_UNIT_1	NA	FULL_TIME_POS	NA	NA	NA	NA', split = '\t')) 
colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

# Copied and pasted from second part of excel sheet, excluding the NAs
renameCols <- unlist(strsplit('case_number	status	submitted_date	dol_decision_date	visa_class	begin_date	end_date	employer_name	employer_address1	employer_city	employer_state	employer_zip_code	soc_code	soc_name	job_title	wage_rate_from	wage_rate_to	wage_unit	part_time	total_workers	worksite_city	worksite_state	prevailing_wage	prevailing_wage_unit	job_code', split = '\t'))
file_name <- 'H-1B_2009_icert.csv'
fy2009iCert <- fread(file_name, 
                     header = TRUE, 
                     select = colsToRead,
                     stringsAsFactors = FALSE,
                     col.names = renameCols)

# Add two additional columns for fiscall year and file_name, as well as any missing columns
headers <- c(colnames(fy2009iCert), 
             'fy', 
             'file_name',
             'employer_address2',
             'worksite_zip_code',
             'certified_begin_date',
             'certified_end_date',
             'occupational_title',
             'wage_rate',
             'max_wage',
             'withdrawn',
             'agent_attorney_first_name',
             'agent_attorney_last_name',
             'agent_attorney_city',
             'agent_attorney_state')

# Add the year as a column
fy2009iCert$fy <- rep(2009, dim(fy2009iCert)[1])

# Add the filename as a column
fy2009iCert$file_name <- rep(file_name, dim(fy2009iCert)[1])

# Missing columns
fy2009iCert$employer_address2 <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$worksite_zip_code <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$certified_begin_date <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$certified_end_date <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$occupational_title <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$wage_rate <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$max_wage <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$withdrawn <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$agent_attorney_first_name <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$agent_attorney_last_name <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$agent_attorney_city <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$agent_attorney_state <- rep(NA, dim(fy2009iCert)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2009iCert) <- headers

fy2009iCert$wage_rate_from <- as.numeric(fy2009iCert$wage_rate_from)
fy2009iCert$wage_rate_to <- as.numeric(fy2009iCert$wage_rate_to)
fy2009iCert$prevailing_wage <- as.numeric(fy2009iCert$prevailing_wage)
fy2009iCert$employer_zip_code <- as.character(fy2009iCert$employer_zip_code)
fy2009iCert$worksite_zip_code <- as.character(fy2009iCert$worksite_zip_code)

fy2009iCert$wage_rate_to[which(is.na(fy2009iCert$wage_rate_to))] <- fy2009iCert$wage_rate_from[which(is.na(fy2009iCert$wage_rate_to))]

# FY 2009 Icert column goes by "FULL_TIME_POS" so we need to negate
fy2009iCert$part_time <- fy2009iCert$part_time == "N"

columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2009iCert, columnOrder)


######################################################################################################
#                                        2010                                                        #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2010	H-1B_FY2010	LCA_CASE_NUMBER	LCA_CASE_SUBMIT	NA	LCA_CASE_EMPLOYER_NAME	LCA_CASE_EMPLOYER_ADDRESS1	LCA_CASE_EMPLOYER_ADDRESS2	LCA_CASE_EMPLOYER_CITY	LCA_CASE_EMPLOYER_STATE	LCA_CASE_EMPLOYER_POSTAL_CODE	WORK_LOCATION_CITY1	WORK_LOCATION_STATE1	NA	TOTAL_WORKERS	DECISION_DATE	LCA_CASE_EMPLOYMENT_START_DATE	LCA_CASE_EMPLOYMENT_END_DATE	NA	NA	LCA_CASE_JOB_TITLE	LCA_CASE_NAICS_CODE	STATUS	NA	LCA_CASE_SOC_CODE	LCA_CASE_SOC_NAME	NA	NA	LCA_CASE_WAGE_RATE_FROM	LCA_CASE_WAGE_RATE_TO	NA	PW_1	PW_UNIT_1	NA	NA	NA	NA	NA	NA', split = '\t'))
colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

# Copied and pasted from second part of excel sheet, excluding the NAs
renameCols <- unlist(strsplit('case_number	status	submitted_date	dol_decision_date	begin_date	end_date	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	soc_code	soc_name	job_title	wage_rate_from	wage_rate_to	total_workers	worksite_city	worksite_state	prevailing_wage	prevailing_wage_unit	job_code', split = '\t'))
file_name <- 'H-1B_2010.csv'
fy2010 <- fread(file_name, 
                header = TRUE, 
                select = colsToRead,
                stringsAsFactors = FALSE,
                col.names = renameCols)

# Add two additional columns for fiscall year and file_name, as well as any missing columns
headers <- c(colnames(fy2010), 
             'fy', 
             'file_name',
             'visa_class',
             'worksite_zip_code',
             'certified_begin_date',
             'certified_end_date',
             'wage_rate',
             'wage_unit',
             'occupational_title',
             'max_wage',
             'part_time',
             'withdrawn',
             'agent_attorney_first_name',
             'agent_attorney_last_name',
             'agent_attorney_city',
             'agent_attorney_state')

# Add the year as a column
fy2010$fy <- rep(2010, dim(fy2010)[1])

# Add the filename as a column
fy2010$file_name <- rep(file_name, dim(fy2010)[1])

# Missing columns
fy2010$visa_class <- rep(NA, dim(fy2010)[1])
fy2010$worksite_zip_code <- rep(NA, dim(fy2010)[1])
fy2010$certified_begin_date <- rep(NA, dim(fy2010)[1])
fy2010$certified_end_date <- rep(NA, dim(fy2010)[1])
fy2010$wage_rate <- rep(NA, dim(fy2010)[1])
fy2010$wage_unit <- rep(NA, dim(fy2010)[1])
fy2010$occupational_title <- rep(NA, dim(fy2010)[1])
fy2010$max_wage <- rep(NA, dim(fy2010)[1])
fy2010$part_time <- rep(NA, dim(fy2010)[1])
fy2010$withdrawn <- rep(NA, dim(fy2010)[1])
fy2010$agent_attorney_first_name <- rep(NA, dim(fy2010)[1])
fy2010$agent_attorney_last_name <- rep(NA, dim(fy2010)[1])
fy2010$agent_attorney_city <- rep(NA, dim(fy2010)[1])
fy2010$agent_attorney_state <- rep(NA, dim(fy2010)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2010) <- headers

fy2010$wage_rate_from <- as.numeric(fy2010$wage_rate_from)
fy2010$wage_rate_to <- as.numeric(fy2010$wage_rate_to)
fy2010$prevailing_wage <- as.numeric(fy2010$prevailing_wage)
fy2010$employer_zip_code <- as.character(fy2010$employer_zip_code)
fy2010$worksite_zip_code <- as.character(fy2010$worksite_zip_code)

fy2010$wage_rate_to[which(is.na(fy2010$wage_rate_to))] <- fy2010$wage_rate_from[which(is.na(fy2010$wage_rate_to))]

# DON'T CHANGE
columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2010, columnOrder)


######################################################################################################
#                                        2011                                                        #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2011	H-1B_iCert_LCA_FY2011	LCA_CASE_NUMBER	LCA_CASE_SUBMIT	VISA_CLASS	LCA_CASE_EMPLOYER_NAME	LCA_CASE_EMPLOYER_ADDRESS	NA	LCA_CASE_EMPLOYER_CITY	LCA_CASE_EMPLOYER_STATE	LCA_CASE_EMPLOYER_POSTAL_CODE	LCA_CASE_WORKLOC1_CITY	LCA_CASE_WORKLOC1_STATE	NA	TOTAL_WORKERS	DECISION_DATE	LCA_CASE_EMPLOYMENT_START_DATE	LCA_CASE_EMPLOYMENT_END_DATE	NA	NA	LCA_CASE_JOB_TITLE	LCA_CASE_NAICS_CODE	STATUS	NA	LCA_CASE_SOC_CODE	LCA_CASE_SOC_NAME	NA	NA	LCA_CASE_WAGE_RATE_FROM	LCA_CASE_WAGE_RATE_TO	LCA_CASE_WAGE_RATE_UNIT	PW_1	PW_UNIT_1	NA	FULL_TIME_POS	NA	NA	NA	NA', split = '\t'))
colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

# Copied and pasted from second part of excel sheet, excluding the NAs
renameCols <- unlist(strsplit('case_number	status	submitted_date	dol_decision_date	visa_class	begin_date	end_date	employer_name	employer_address1	employer_city	employer_state	employer_zip_code	soc_code	soc_name	job_title	wage_rate_from	wage_rate_to	wage_unit	part_time	total_workers	worksite_city	worksite_state	prevailing_wage	prevailing_wage_unit	job_code', split = '\t'))
file_name <- 'H-1B_2011.csv'
fy2011 <- fread(file_name, 
                header = TRUE, 
                select = colsToRead,
                stringsAsFactors = FALSE,
                col.names = renameCols)


# Add two additional columns for fiscall year and file_name, as well as any missing columns
headers <- c(colnames(fy2011), 
             'fy', 
             'file_name',
             'employer_address2',
             'max_wage',
             'worksite_zip_code',
             'certified_begin_date',
             'certified_end_date',
             'occupational_title',
             'wage_rate',
             'withdrawn',
             'agent_attorney_first_name',
             'agent_attorney_last_name',
             'agent_attorney_city',
             'agent_attorney_state')

# Add the year as a column
fy2011$fy <- rep(2011, dim(fy2011)[1])

# Add the filename as a column
fy2011$file_name <- rep(file_name, dim(fy2011)[1])

# Missing columns
fy2011$employer_address2 <- rep(NA, dim(fy2011)[1])
fy2011$max_wage <- rep(NA, dim(fy2011)[1])
fy2011$worksite_zip_code <- rep(NA, dim(fy2011)[1])
fy2011$certified_begin_date <- rep(NA, dim(fy2011)[1])
fy2011$certified_end_date <- rep(NA, dim(fy2011)[1])
fy2011$occupational_title <- rep(NA, dim(fy2011)[1])
fy2011$wage_rate <- rep(NA, dim(fy2011)[1])
fy2011$withdrawn <- rep(NA, dim(fy2011)[1])
fy2011$agent_attorney_first_name <- rep(NA, dim(fy2011)[1])
fy2011$agent_attorney_last_name <- rep(NA, dim(fy2011)[1])
fy2011$agent_attorney_city <- rep(NA, dim(fy2011)[1])
fy2011$agent_attorney_state <- rep(NA, dim(fy2011)[1])


# Name the columns as they appear in the spreadsheet
colnames(fy2011) <- headers

fy2011$wage_rate_from <- as.numeric(fy2011$wage_rate_from)
fy2011$wage_rate_to <- as.numeric(fy2011$wage_rate_to)
fy2011$prevailing_wage <- as.numeric(fy2011$prevailing_wage)
fy2011$employer_zip_code <- as.character(fy2011$employer_zip_code)
fy2011$worksite_zip_code <- as.character(fy2011$worksite_zip_code)

fy2011$wage_rate_to[which(is.na(fy2011$wage_rate_to))] <- fy2011$wage_rate_from[which(is.na(fy2011$wage_rate_to))]

# FY 2011  column goes by "FULL_TIME_POS" so we need to negate
fy2011$part_time <- fy2011$part_time == "N"


# DON'T CHANGE
columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2011, columnOrder)


######################################################################################################
#                                        2012                                                        #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2012	LCA_FY2012_Q4	LCA_CASE_NUMBER	LCA_CASE_SUBMIT	VISA_CLASS	LCA_CASE_EMPLOYER_NAME	LCA_CASE_EMPLOYER_ADDRESS	NA	LCA_CASE_EMPLOYER_CITY	LCA_CASE_EMPLOYER_STATE	LCA_CASE_EMPLOYER_POSTAL_CODE	LCA_CASE_WORKLOC1_CITY	LCA_CASE_WORKLOC1_STATE	NA	TOTAL_WORKERS	DECISION_DATE	LCA_CASE_EMPLOYMENT_START_DATE	LCA_CASE_EMPLOYMENT_END_DATE	NA	NA	LCA_CASE_JOB_TITLE	LCA_CASE_NAICS_CODE	STATUS	NA	LCA_CASE_SOC_CODE	LCA_CASE_SOC_NAME	NA	NA	LCA_CASE_WAGE_RATE_FROM	LCA_CASE_WAGE_RATE_TO	LCA_CASE_WAGE_RATE_UNIT	PW_1	PW_UNIT_1	NA	FULL_TIME_POS	NA	NA	NA	NA', split = '\t'))
colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

# Copied and pasted from second part of excel sheet, excluding the NAs
renameCols <- unlist(strsplit('case_number	status	submitted_date	dol_decision_date	visa_class	begin_date	end_date	employer_name	employer_address1	employer_city	employer_state	employer_zip_code	soc_code	soc_name	job_title	wage_rate_from	wage_rate_to	wage_unit	part_time	total_workers	worksite_city	worksite_state	prevailing_wage	prevailing_wage_unit	job_code', split = '\t'))
file_name <- 'H-1B_2012.csv'
fy2012 <- fread(file_name, 
                header = TRUE, 
                select = colsToRead,
                stringsAsFactors = FALSE,
                col.names = renameCols)


# Add two additional columns for fiscall year and file_name, as well as any missing columns
headers <- c(colnames(fy2012), 
             'fy', 
             'file_name',
             'employer_address2',
             'max_wage',
             'worksite_zip_code',
             'certified_begin_date',
             'certified_end_date',
             'occupational_title',
             'wage_rate',
             'withdrawn',
             'agent_attorney_first_name',
             'agent_attorney_last_name',
             'agent_attorney_city',
             'agent_attorney_state')

# Add the year as a column
fy2012$fy <- rep(2012, dim(fy2012)[1])

# Add the filename as a column
fy2012$file_name <- rep(file_name, dim(fy2012)[1])

# Missing columns
fy2012$employer_address2 <- rep(NA, dim(fy2012)[1])
fy2012$max_wage <- rep(NA, dim(fy2012)[1])
fy2012$worksite_zip_code <- rep(NA, dim(fy2012)[1])
fy2012$certified_begin_date <- rep(NA, dim(fy2012)[1])
fy2012$certified_end_date <- rep(NA, dim(fy2012)[1])
fy2012$occupational_title <- rep(NA, dim(fy2012)[1])
fy2012$wage_rate <- rep(NA, dim(fy2012)[1])
fy2012$withdrawn <- rep(NA, dim(fy2012)[1])
fy2012$agent_attorney_first_name <- rep(NA, dim(fy2012)[1])
fy2012$agent_attorney_last_name <- rep(NA, dim(fy2012)[1])
fy2012$agent_attorney_city <- rep(NA, dim(fy2012)[1])
fy2012$agent_attorney_state <- rep(NA, dim(fy2012)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2012) <- headers

fy2012$wage_rate_to[which(is.na(fy2012$wage_rate_to))] <- fy2012$wage_rate_from[which(is.na(fy2012$wage_rate_to))]

fy2012$wage_rate_from <- as.numeric(fy2012$wage_rate_from)
fy2012$wage_rate_to <- as.numeric(fy2012$wage_rate_to)
fy2012$prevailing_wage <- as.numeric(fy2012$prevailing_wage)
fy2012$employer_zip_code <- as.character(fy2012$employer_zip_code)
fy2012$worksite_zip_code <- as.character(fy2012$worksite_zip_code)

# FY 2012  column goes by "FULL_TIME_POS" so we need to negate
fy2012$part_time <- fy2012$part_time == "N"

# DON'T CHANGE
columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2012, columnOrder)


######################################################################################################
#                                        2013                                                        #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2013	LCA_FY2013	LCA_CASE_NUMBER	LCA_CASE_SUBMIT	VISA_CLASS	LCA_CASE_EMPLOYER_NAME	LCA_CASE_EMPLOYER_ADDRESS	NA	LCA_CASE_EMPLOYER_CITY	LCA_CASE_EMPLOYER_STATE	LCA_CASE_EMPLOYER_POSTAL_CODE	LCA_CASE_WORKLOC1_CITY	LCA_CASE_WORKLOC1_STATE	NA	TOTAL_WORKERS	Decision_Date	LCA_CASE_EMPLOYMENT_START_DATE	LCA_CASE_EMPLOYMENT_END_DATE	NA	NA	LCA_CASE_JOB_TITLE	LCA_CASE_NAICS_CODE	STATUS	NA	LCA_CASE_SOC_CODE	LCA_CASE_SOC_NAME	NA	NA	LCA_CASE_WAGE_RATE_FROM	LCA_CASE_WAGE_RATE_TO	LCA_CASE_WAGE_RATE_UNIT	PW_1	PW_UNIT_1	NA	FULL_TIME_POS	NA	NA	NA	NA', split = '\t'))
colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

# Copied and pasted from second part of excel sheet, excluding the NAs
renameCols <- unlist(strsplit('case_number	status	submitted_date	dol_decision_date	visa_class	begin_date	end_date	employer_name	employer_address1	employer_city	employer_state	employer_zip_code	soc_code	soc_name	job_title	wage_rate_from	wage_rate_to	wage_unit	part_time	total_workers	worksite_city	worksite_state	prevailing_wage	prevailing_wage_unit	job_code', split = '\t'))
file_name <- 'H-1B_2013.csv'
fy2013 <- fread(file_name, 
                header = TRUE, 
                select = colsToRead,
                stringsAsFactors = FALSE,
                col.names = renameCols)


# Add two additional columns for fiscall year and file_name, as well as any missing columns
headers <- c(colnames(fy2013), 
             'fy', 
             'file_name',
             'employer_address2',
             'max_wage',
             'worksite_zip_code',
             'certified_begin_date',
             'certified_end_date',
             'occupational_title',
             'wage_rate',
             'withdrawn',
             'agent_attorney_first_name',
             'agent_attorney_last_name',
             'agent_attorney_city',
             'agent_attorney_state')

# Add the year as a column
fy2013$fy <- rep(2013, dim(fy2013)[1])

# Add the filename as a column
fy2013$file_name <- rep(file_name, dim(fy2013)[1])

# Missing columns
fy2013$employer_address2 <- rep(NA, dim(fy2013)[1])
fy2013$max_wage <- rep(NA, dim(fy2013)[1])
fy2013$worksite_zip_code <- rep(NA, dim(fy2013)[1])
fy2013$certified_begin_date <- rep(NA, dim(fy2013)[1])
fy2013$certified_end_date <- rep(NA, dim(fy2013)[1])
fy2013$occupational_title <- rep(NA, dim(fy2013)[1])
fy2013$wage_rate <- rep(NA, dim(fy2013)[1])
fy2013$withdrawn <- rep(NA, dim(fy2013)[1])
fy2013$agent_attorney_first_name <- rep(NA, dim(fy2013)[1])
fy2013$agent_attorney_last_name <- rep(NA, dim(fy2013)[1])
fy2013$agent_attorney_city <- rep(NA, dim(fy2013)[1])
fy2013$agent_attorney_state <- rep(NA, dim(fy2013)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2013) <- headers

fy2013$wage_rate_to[which(is.na(fy2013$wage_rate_to))] <- fy2013$wage_rate_from[which(is.na(fy2013$wage_rate_to))]

fy2013$wage_rate_from <- as.numeric(fy2013$wage_rate_from)
fy2013$wage_rate_to <- as.numeric(fy2013$wage_rate_to)
fy2013$prevailing_wage <- as.numeric(fy2013$prevailing_wage)
fy2013$employer_zip_code <- as.character(fy2013$employer_zip_code)
fy2013$worksite_zip_code <- as.character(fy2013$worksite_zip_code)

# FY 2013  column goes by "FULL_TIME_POS" so we need to negate
fy2013$part_time <- fy2013$part_time == "N"

# DON'T CHANGE
columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2013, columnOrder)


######################################################################################################
#                                        2014                                                        #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2014	H-1B_FY14_Q4	LCA_CASE_NUMBER	LCA_CASE_SUBMIT	VISA_CLASS	LCA_CASE_EMPLOYER_NAME	LCA_CASE_EMPLOYER_ADDRESS	NA	LCA_CASE_EMPLOYER_CITY	LCA_CASE_EMPLOYER_STATE	LCA_CASE_EMPLOYER_POSTAL_CODE	LCA_CASE_WORKLOC1_CITY	LCA_CASE_WORKLOC1_STATE	NA	TOTAL_WORKERS	DECISION_DATE	LCA_CASE_EMPLOYMENT_START_DATE	LCA_CASE_EMPLOYMENT_END_DATE	NA	NA	LCA_CASE_JOB_TITLE	LCA_CASE_NAICS_CODE	STATUS	NA	LCA_CASE_SOC_CODE	LCA_CASE_SOC_NAME	NA	NA	LCA_CASE_WAGE_RATE_FROM	LCA_CASE_WAGE_RATE_TO	LCA_CASE_WAGE_RATE_UNIT	PW_1	PW_UNIT_1	NA	FULL_TIME_POS	NA	NA	NA	NA', split = '\t'))

colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

# Copied and pasted from second part of excel sheet, excluding the NAs
renameCols <- unlist(strsplit('case_number	status	submitted_date	dol_decision_date	visa_class	begin_date	end_date	employer_name	employer_address1	employer_city	employer_state	employer_zip_code	soc_code	soc_name	job_title	wage_rate_from	wage_rate_to	wage_unit	part_time	total_workers	worksite_city	worksite_state	prevailing_wage	prevailing_wage_unit	job_code', split = '\t'))
file_name <- 'H-1B_2014.csv'
fy2014 <- fread(file_name, 
                header = TRUE, 
                select = colsToRead,
                stringsAsFactors = FALSE,
                col.names = renameCols)


# Add two additional columns for fiscall year and file_name, as well as any missing columns
headers <- c(colnames(fy2014), 
             'fy', 
             'file_name',
             'employer_address2',
             'max_wage',
             'worksite_zip_code',
             'certified_begin_date',
             'certified_end_date',
             'occupational_title',
             'wage_rate',
             'withdrawn',
             'agent_attorney_first_name',
             'agent_attorney_last_name',
             'agent_attorney_city',
             'agent_attorney_state')

# Add the year as a column
fy2014$fy <- rep(2014, dim(fy2014)[1])

# Add the filename as a column
fy2014$file_name <- rep(file_name, dim(fy2014)[1])

# Missing columns
fy2014$employer_address2 <- rep(NA, dim(fy2014)[1])
fy2014$max_wage <- rep(NA, dim(fy2014)[1])
fy2014$worksite_zip_code <- rep(NA, dim(fy2014)[1])
fy2014$certified_begin_date <- rep(NA, dim(fy2014)[1])
fy2014$certified_end_date <- rep(NA, dim(fy2014)[1])
fy2014$occupational_title <- rep(NA, dim(fy2014)[1])
fy2014$wage_rate <- rep(NA, dim(fy2014)[1])
fy2014$withdrawn <- rep(NA, dim(fy2014)[1])
fy2014$agent_attorney_first_name <- rep(NA, dim(fy2014)[1])
fy2014$agent_attorney_last_name <- rep(NA, dim(fy2014)[1])
fy2014$agent_attorney_city <- rep(NA, dim(fy2014)[1])
fy2014$agent_attorney_state <- rep(NA, dim(fy2014)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2014) <- headers

fy2014$wage_rate_to[which(is.na(fy2014$wage_rate_to))] <- fy2014$wage_rate_from[which(is.na(fy2014$wage_rate_to))]

fy2014$wage_rate_from <- as.numeric(fy2014$wage_rate_from)
fy2014$wage_rate_to <- as.numeric(fy2014$wage_rate_to)
fy2014$prevailing_wage <- as.numeric(fy2014$prevailing_wage)
fy2014$employer_zip_code <- as.character(fy2014$employer_zip_code)
fy2014$worksite_zip_code <- as.character(fy2014$worksite_zip_code)

# FY 2014  column goes by "FULL_TIME_POS" so we need to negate
fy2014$part_time <- fy2014$part_time == "N"

# DON'T CHANGE
columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2014, columnOrder)



######################################################################################################
#                                        2015                                                        #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2015	H-1B_Disclosure_Data_FY15_Q4	CASE_NUMBER	CASE_SUBMITTED	VISA_CLASS	EMPLOYER_NAME	EMPLOYER_ADDRESS1	EMPLOYER_ADDRESS2	EMPLOYER_CITY	EMPLOYER_STATE	EMPLOYER_POSTAL_CODE	WORKSITE_CITY	WORKSITE_STATE	WORKSITE_POSTAL_CODE	TOTAL WORKERS	DECISION_DATE	EMPLOYMENT_START_DATE	EMPLOYMENT_END_DATE	NA	NA	JOB_TITLE	NAIC_CODE	CASE_STATUS	NA	SOC_CODE	SOC_NAME	NA	WAGE_RATE_OF_PAY	NA	NA	WAGE_UNIT_OF_PAY	PREVAILING_WAGE	PW_UNIT_OF_PAY	NA	FULL_TIME_POSITION	AGENT_ATTORNEY_NAME	AGENT_ATTORNEY_CITY	AGENT_ATTORNEY_STATE', split = '\t'))
colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

# Copied and pasted from second part of excel sheet, excluding the NAs
renameCols <- unlist(strsplit('case_number	status	submitted_date	dol_decision_date	visa_class	begin_date	end_date	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	agent_attorney_first_name	agent_attorney_city	agent_attorney_state	job_title	soc_code	soc_name	job_code	total_workers	part_time	prevailing_wage	prevailing_wage_unit	wage_rate	wage_unit	worksite_city	worksite_state	worksite_zip_code', split = '\t'))
file_name <- 'H-1B_2015.csv'
fy2015 <- fread(file_name, 
                header = TRUE, 
                select = colsToRead,
                stringsAsFactors = FALSE,
                col.names = renameCols)


# Add two additional columns for fiscall year and file_name, as well as any missing columns
headers <- c(colnames(fy2015), 
             'fy', 
             'file_name',
             'wage_rate_from',
             'wage_rate_to',
             'withdrawn',
             'agent_attorney_last_name',
             'certified_begin_date',
             'certified_end_date',
             'max_wage',
             'occupational_title')
# Add the year as a column
fy2015$fy <- rep(2015, dim(fy2015)[1])

# Add the filename as a column
fy2015$file_name <- rep(file_name, dim(fy2015)[1])

# Missing columns
fy2015$wage_rate_from <- rep(NA, dim(fy2015)[1])
fy2015$wage_rate_to <- rep(NA, dim(fy2015)[1])
fy2015$max_wage <- rep(NA, dim(fy2015)[1])
fy2015$withdrawn <- rep(NA, dim(fy2015)[1])
fy2015$agent_attorney_last_name <- rep(NA, dim(fy2015)[1])
fy2015$certified_begin_date <- rep(NA, dim(fy2015)[1])
fy2015$certified_end_date <- rep(NA, dim(fy2015)[1])
fy2015$occupational_title <- rep(NA, dim(fy2015)[1])
fy2015$employer_zip_code <- rep(NA, dim(fy2015)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2015) <- headers


fy2015$prevailing_wage <- as.numeric(fy2015$prevailing_wage)
fy2015$employer_zip_code <- as.character(fy2015$employer_zip_code)
fy2015$worksite_zip_code <- as.character(fy2015$worksite_zip_code)

wage_rate_tmp <- gsub("\\$", "", fy2015$wage_rate)
wage_rate_tmp <- gsub(" ", "", wage_rate_tmp)
wage_rate_from <- gsub("-.*", "", wage_rate_tmp)
wage_rate_to <- gsub(".*-", "", wage_rate_tmp)

fy2015$wage_rate_from <- as.numeric(wage_rate_from)
fy2015$wage_rate_to <- as.numeric(wage_rate_to)

fy2015$wage_rate_to[which(is.na(fy2015$wage_rate_to))] <- fy2015$wage_rate_from[which(is.na(fy2015$wage_rate_to))]

# FY 2015  column goes by "FULL_TIME_POS" so we need to negate
fy2015$part_time <- fy2015$part_time == "N"

# DON'T CHANGE
columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2015, columnOrder)

fy2015$wage_rate_from

######################################################################################################
#                                        2016                                                        #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2016	H-1B_Disclosure_Data_FY16	CASE_NUMBER	CASE_SUBMITTED	VISA_CLASS	EMPLOYER_NAME	EMPLOYER_ADDRESS	NA	EMPLOYER_CITY	EMPLOYER_STATE	EMPLOYER_POSTAL_CODE	WORKSITE_CITY	WORKSITE_STATE	WORKSITE_POSTAL_CODE	TOTAL_WORKERS	DECISION_DATE	EMPLOYMENT_START_DATE	EMPLOYMENT_END_DATE	NA	NA	JOB_TITLE	NAIC_CODE	CASE_STATUS	NA	SOC_CODE	SOC_NAME	NA	NA	WAGE_RATE_OF_PAY_FROM	WAGE_RATE_OF_PAY_TO	WAGE_UNIT_OF_PAY	PREVAILING_WAGE	PW_UNIT_OF_PAY	NA	FULL_TIME_POSITION	AGENT_ATTORNEY_NAME	AGENT_ATTORNEY_CITY	AGENT_ATTORNEY_STATE', split = '\t'))
colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

# Copied and pasted from second part of excel sheet, excluding the NAs
renameCols <- unlist(strsplit('case_number	status	submitted_date	dol_decision_date	visa_class	begin_date	end_date	employer_name	employer_address1	employer_city	employer_state	employer_zip_code	agent_attorney_first_name	agent_attorney_city	agent_attorney_state	job_title	soc_code	soc_name	job_code	total_workers	part_time	prevailing_wage	prevailing_wage_unit	wage_rate_from	wage_rate_to	wage_unit	worksite_city	worksite_state	worksite_zip_code', split = '\t'))
file_name <- 'H-1B_2016.csv'
fy2016 <- fread(file_name, 
                header = TRUE, 
                select = colsToRead,
                stringsAsFactors = FALSE,
                col.names = renameCols)


# Add two additional columns for fiscall year and file_name, as well as any missing columns
headers <- c(colnames(fy2016), 
             'fy', 
             'file_name',
             'withdrawn',
             'agent_attorney_last_name',
             'certified_begin_date',
             'certified_end_date',
             'max_wage',
             'occupational_title',
             'wage_rate',
             'employer_address2')
# Add the year as a column
fy2016$fy <- rep(2016, dim(fy2016)[1])

# Add the filename as a column
fy2016$file_name <- rep(file_name, dim(fy2016)[1])

# Missing columns
fy2016$withdrawn <- rep(NA, dim(fy2016)[1])
fy2016$agent_attorney_last_name <- rep(NA, dim(fy2016)[1])
fy2016$certified_begin_date <- rep(NA, dim(fy2016)[1])
fy2016$certified_end_date <- rep(NA, dim(fy2016)[1])
fy2016$max_wage <- rep(NA, dim(fy2016)[1])
fy2016$occupational_title <- rep(NA, dim(fy2016)[1])
fy2016$wage_rate <- rep(NA, dim(fy2016)[1])
fy2016$employer_address2 <- rep(NA, dim(fy2016)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2016) <- headers

fy2016$prevailing_wage <- as.character(fy2016$prevailing_wage)
fy2016$employer_zip_code <- as.character(fy2016$employer_zip_code)
fy2016$worksite_zip_code <- as.character(fy2016$worksite_zip_code)

# FY 2016 column goes by "FULL_TIME_POS" so we need to negate
fy2016$part_time <- fy2016$part_time == "N"

fy2016$wage_rate_from <- gsub("\\$", "", fy2016$wage_rate_from)
fy2016$wage_rate_to <- gsub("\\$", "", fy2016$wage_rate_to)
fy2016$wage_rate_from <- gsub(",", "", fy2016$wage_rate_from)
fy2016$wage_rate_to <- gsub(",", "", fy2016$wage_rate_to)

fy2016$wage_rate_from <- as.numeric(fy2016$wage_rate_from)
fy2016$wage_rate_to <- as.numeric(fy2016$wage_rate_to)

fy2016$wage_rate_to[which(is.na(fy2016$wage_rate_to))] <- fy2016$wage_rate_from[which(is.na(fy2016$wage_rate_to))]
fy2016$wage_rate_to[which(fy2016$wage_rate_to == 0)] <- fy2016$wage_rate_from[which(fy2016$wage_rate_to == 0)]

# DON'T CHANGE
columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2016, columnOrder)


visas <- rbind(fy2008, fy2009, fy2009iCert, fy2010, fy2011, fy2012, fy2013, fy2014, fy2015, fy2016)

# Post processing

visas$submitted_date <- as.Date(visas$submitted_date, format = '%m/%d/%Y')
visas$dol_decision_date <- as.Date(visas$dol_decision_date, format = '%m/%d/%Y')
visas$begin_date <- as.Date(visas$begin_date, format = '%m/%d/%Y')
visas$end_date <- as.Date(visas$end_date, format = '%m/%d/%Y')
visas$certified_begin_date <- as.Date(visas$certified_begin_date, format = '%m/%d/%Y')
visas$certified_end_date <- as.Date(visas$certified_end_date, format = '%m/%d/%Y')

# Prior years used codes to represent visa classes
visas$visa_class[which(visas$visa_class == 'R')] <- 'H-1B'
visas$visa_class[which(visas$visa_class == 'C')] <- 'H-1B1 Chile'
visas$visa_class[which(visas$visa_class == 'A')] <- 'E-3 Australian'
visas$visa_class[which(visas$visa_class == 'S')] <- 'H-1B1 Singapore'
visas$visa_class[which(visas$visa_class == '')] <- NA
# Assuming this is an error from a web input form
visas$visa_class[which(visas$visa_class == 'Select Visa Classification')] <- NA

visas$wage_rate <- as.numeric(visas$wage_rate)
visas$prevailing_wage <- as.numeric(visas$prevailing_wage)
# Get rid of spaces
visas$worksite_zip_code <- gsub(" ", "", visas$worksite_zip_code)
visas$employer_zip_code <- gsub(" ", "", visas$employer_zip_code)
# Remove the sub-zip code. Just keep the 5 digit zips
visas$worksite_zip_code <- gsub("-[[:digit:]]*", "", visas$worksite_zip_code)
visas$employer_zip_code <- gsub("-[[:digit:]]*", "", visas$employer_zip_code)
# Remove any other punctuation (there are some periods in there)
visas$employer_zip_code <- gsub("[[:punct:]]*", "", visas$employer_zip_code)
visas$worksite_zip_code <- gsub("[[:punct:]]*", "", visas$worksite_zip_code)
# Remove any alpha characters (Canada is the only place with alpha charcters in their zips)
visas$employer_zip_code <- gsub(".*[[:alpha:]].*", "", visas$employer_zip_code)
visas$worksite_zip_code <- gsub(".*[[:alpha:]].*", "", visas$worksite_zip_code)
# Remove any other non-digit characters (Unicode, etc)
visas$employer_zip_code <- gsub("[^[:digit:]]", "", visas$employer_zip_code)
visas$worksite_zip_code <- gsub("[^[:digit:]]", "", visas$worksite_zip_code)

# Withdrawn is the actual status
visas$status[which(visas$withdrawn == 'Y')] <- 'WITHDRAWN'

# Get all units into the same code
visas$wage_unit[visas$wage_unit == 'yr'] <- 'YR'
visas$wage_unit[visas$wage_unit == 'hr'] <- 'HR'
visas$wage_unit[visas$wage_unit == 'mth'] <- 'MTH'
visas$wage_unit[visas$wage_unit == 'wk'] <- 'WK'
visas$wage_unit[visas$wage_unit == 'hour'] <- 'HR'
visas$wage_unit[visas$wage_unit == 'Week'] <- 'WK'
visas$wage_unit[visas$wage_unit == 'Month'] <- 'MTH'
visas$wage_unit[visas$wage_unit == 'bi'] <- 'BIWK'
visas$wage_unit[visas$wage_unit == 'BI'] <- 'BIWK'
visas$wage_unit[visas$wage_unit == 'Bi-Weekly'] <- 'BIWK'
visas$wage_unit[visas$wage_unit == 'Select Pay Range'] <- NA
visas$wage_unit[visas$wage_unit == 'Year'] <- 'YR'
visas$wage_unit[visas$wage_unit == 'Hour'] <- 'HR'
visas$wage_unit[visas$wage_unit == ''] <- NA

# Get all units into the same code
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'yr'] <- 'YR'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'hr'] <- 'HR'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'mth'] <- 'MTH'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'wk'] <- 'WK'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'hour'] <- 'HR'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'week'] <- 'WK'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'month'] <- 'MTH'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'year'] <- 'YR'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'bi'] <- 'BIWK'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'BI'] <- 'BIWK'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'Bi-Weekly'] <- 'BIWK'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'Hour'] <- 'HR'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'Week'] <- 'WK'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'Month'] <- 'MTH'
visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'Year'] <- 'YR'

visas$prevailing_wage_unit[visas$prevailing_wage_unit == 'Select Pay Range'] <- NA
visas$prevailing_wage_unit[visas$prevailing_wage_unit == ''] <- NA


visas$worksite_state[visas$worksite_state == 'MH'] <- 'MI'
visas$worksite_state[visas$worksite_state == 'MP'] <- 'MI'
visas$worksite_state[visas$worksite_state == 'FM'] <- 'FL'
visas$worksite_state[visas$worksite_state == 'PW'] <- 'PA'
visas$worksite_state[visas$worksite_state == 'PR'] <- 'Puerto Rico'
visas$worksite_state[visas$worksite_state == 'GU'] <- 'Guam'

# Some were for AZ, some were for AK, and some were just like, 'asdasd' 
visas$worksite_state[visas$worksite_state == 'AS'] <- NA

# 2008 and 2009 data is missing prevailing wage unit, so we'll assume it's the same as wage_unit

visas$prevailing_wage_unit[which(is.na(visas$prevailing_wage_unit))] <- visas$wage_rate[which(is.na(visas$prevailing_wage_unit))]

normalized_wage <- visas$wage_unit
normalized_wage[which(normalized_wage == 'YR')] <- 1
normalized_wage[which(normalized_wage == 'HR')] <- 40 * 52
normalized_wage[which(normalized_wage == 'BIWK')] <- 52 / 2
normalized_wage[which(normalized_wage == 'WK')] <- 52
normalized_wage[which(normalized_wage == 'MTH')] <- 12
normalized_wage[which(is.na(normalized_wage))] <- 0

normalized_wage <- as.numeric(normalized_wage)

visas$normalized_wage <- ((normalized_wage * visas$wage_rate_from) + (normalized_wage * visas$wage_rate_to)) / 2

normalized_prevailing_wage <- visas$prevailing_wage_unit
normalized_prevailing_wage[which(normalized_prevailing_wage == 'YR')] <- 1
normalized_prevailing_wage[which(normalized_prevailing_wage == 'HR')] <- 40 * 52
normalized_prevailing_wage[which(normalized_prevailing_wage == 'BIWK')] <- 52 / 2
normalized_prevailing_wage[which(normalized_prevailing_wage == 'WK')] <- 52
normalized_prevailing_wage[which(normalized_prevailing_wage == 'MTH')] <- 12
normalized_prevailing_wage[which(is.na(normalized_prevailing_wage))] <- 0

normalized_prevailing_wage <- as.numeric(normalized_prevailing_wage)

visas$normalized_prevailing_wage <- normalized_prevailing_wage * visas$prevailing_wage
visas$normalized_prevailing_wage[which(visas$normalized_prevailing_wage == 0)] <- NA

# There were some major problems with the 2008 and 2009 wage rate data so we have to remove some outliers
# We'll assume anything over $2,000,000 is an outlier
visas$normalized_prevailing_wage[which(visas$normalized_prevailing_wage > 2000000)] <- NA
visas$normalized_wage[which(visas$normalized_wage > 2000000)] <- NA

visas$agent_attorney_first_name[which(visas$agent_attorney_first_name == '')] <- NA
visas$agent_attorney_last_name <- visas$agent_attorney_first_name
visas$agent_attorney_last_name <- sub(',.*', '', visas$agent_attorney_first_name)
visas$agent_attorney_first_name <- sub('ESQ., ', '', sub('\\w+, ', '', visas$agent_attorney_first_name))


# Save post processing
saveRDS(visas, 'H1BVisas.rds')

visas <- readRDS('H1BVisas.rds')

unique(visas$job_code)

# 2008 and 2009 used these Dictionary of Occupational Titles (DOT) codes found here:
# https://www.uscis.gov/files/form/m-746.pdf
file_name <- 'dot_job_codes.csv'
dot_job_codes <- fread(file_name,  header = TRUE)

visas <- merge(visas, dot_job_codes, by.x="job_code", by.y="dot_job_code", all.x=T, all.y=F)
unique(visas$fy[which(!is.na(visas$dot_job_title))])

# Only 2008 and 2009-non-iCERT uses the DOT codes
visas$dot_job_title[which(visas$fy > 2009)] <- NA
visas$dot_job_title[which(visas$file_name == 'H-1B_2009_icert.csv')] <- NA

colsToRead <- c('2007 NAICS US Code', '2007 NAICS US Title')
renameCols <- c('naics_code', 'naics_title')
file_name <- '2-digit_2007_naics_codes.csv'
naics_2007 <- fread(file_name, 
                    header = TRUE, 
                    select = colsToRead,
                    stringsAsFactors = FALSE,
                    col.names = renameCols)

# Blank line
naics_2007[1]$naics_code <- NA
naics_2007[1]$naics_title <- NA

# Of course they use ranges. Of course.
naics_2007$naics_code[naics_2007$naics_code == '44-45'] <- '44'
naics_2007 <- rbind(naics_2007, as.list(c(45, 'Retail Trade')))

naics_2007$naics_code[naics_2007$naics_code == '48-49'] <- '48'
naics_2007 <- rbind(naics_2007, as.list(c(49, 'Transportation and Warehousing')))

naics_2007$naics_code[naics_2007$naics_code == '31-33'] <- '31'
naics_2007 <- rbind(naics_2007, as.list(c(32, 'Manufacturing')))
naics_2007 <- rbind(naics_2007, as.list(c(33, 'Manufacturing')))

naics_2007$naics_code <- as.numeric(naics_2007$naics_code)

visas <- merge(visas, naics_2007, by.x="job_code", by.y="naics_code", all.x=T, all.y=F, allow.cartesian = T)

#colsToRead <- c('2012 NAICS US   Code', '2012 NAICS US Title')
#renameCols <- c('naics_code', 'naics_title')
#file_name <- '2-digit_2012_naics_codes.csv'
#naics_2012 <- fread(file_name, 
                    #header = TRUE,
                    #select = colsToRead,
                    #stringsAsFactors = FALSE,
                    #col.names = renameCols)
#head(naics_2012)

# Save post processing
saveRDS(visas, 'H1BVisas.rds')

addy <- cbind(visas$employer_name, visas$employer_address1, visas$employer_address2, visas$employer_city, visas$employer_state,  visas$employer_zip_code)
colnames(addy) <- c('employer_name', 'employer_address1', 'employer_address2', 'employer_city', 'employer_state', 'employer_zipcode')
uaddy <- unique(addy)
saveRDS(uaddy, 'H1BUniqueAddresses.rds')


# visas <- readRDS('H1BVisas.rds')

