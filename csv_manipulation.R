library(data.table)
library(dplyr)
library(mgcv)
library(bit64)

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

fy2008$prevailing_wage <- as.character(fy2008$prevailing_wage)
fy2008$employer_zip_code <- as.character(fy2008$employer_zip_code)
fy2008$worksite_zip_code <- as.character(fy2008$worksite_zip_code)

columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))


# Order the columns
setcolorder(fy2008, columnOrder)

######################################################################################################
#                                        2009 non-iCert                                              #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2009	H-1B_Case_Data_FY2009	CASE_NO	SUBMITTED_DATE	PROGRAM_DESIGNATION	EMPLOYER_NAME	EMPLOYER_ADDRESS1	EMPLOYER_ADDRESS2	EMPLOYER_CITY	EMPLOYER_STATE	EMPLOYER_POSTAL_CODE	CITY_1	STATE_1	NA	NBR_IMMIGRANTS	DOL_DECISION_DATE	BEGIN_DATE	END_DATE	NA	NA	JOB_TITLE	OCCUPATIONAL_CODE	APPROVAL_STATUS	WITHDRAWN	NA	NA	OCCUPATIONAL_TITLE	WAGE_RATE_1	NA	NA	RATE_PER_1	PREVAILING_WAGE_1	NA	MAX_RATE_1	PART_TIME_1	NA	NA	NA	NA', split = '\t')) 
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

# Missing columns
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

fy2009$prevailing_wage <- as.character(fy2009$prevailing_wage)
fy2009$employer_zip_code <- as.character(fy2009$employer_zip_code)
fy2009$worksite_zip_code <- as.character(fy2009$worksite_zip_code)


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
fy2009iCert$wage_rate_from <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$wage_rate_to <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$max_wage <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$withdrawn <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$agent_attorney_first_name <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$agent_attorney_last_name <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$agent_attorney_city <- rep(NA, dim(fy2009iCert)[1])
fy2009iCert$agent_attorney_state <- rep(NA, dim(fy2009iCert)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2009iCert) <- headers

fy2009iCert$prevailing_wage <- as.character(fy2009iCert$prevailing_wage)
fy2009iCert$employer_zip_code <- as.character(fy2009iCert$employer_zip_code)
fy2009iCert$worksite_zip_code <- as.character(fy2009iCert$worksite_zip_code)

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
             'max_wage',
             'part_time',
             'visa_class',
             'worksite_zip_code',
             'certified_begin_date',
             'certified_end_date',
             'occupational_title',
             'wage_rate',
             'wage_unit',
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
fy2010$employer_address2 <- rep(NA, dim(fy2010)[1])
fy2010$max_wage <- rep(NA, dim(fy2010)[1])
fy2010$part_time <- rep(NA, dim(fy2010)[1])
fy2010$visa_class <- rep(NA, dim(fy2010)[1])
fy2010$worksite_zip_code <- rep(NA, dim(fy2010)[1])
fy2010$certified_begin_date <- rep(NA, dim(fy2010)[1])
fy2010$certified_end_date <- rep(NA, dim(fy2010)[1])
fy2010$occupational_title <- rep(NA, dim(fy2010)[1])
fy2010$wage_rate <- rep(NA, dim(fy2010)[1])
fy2010$wage_unit <- rep(NA, dim(fy2010)[1])
fy2010$withdrawn <- rep(NA, dim(fy2010)[1])
fy2010$agent_attorney_first_name <- rep(NA, dim(fy2010)[1])
fy2010$agent_attorney_last_name <- rep(NA, dim(fy2010)[1])
fy2010$agent_attorney_city <- rep(NA, dim(fy2010)[1])
fy2010$agent_attorney_state <- rep(NA, dim(fy2010)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2010) <- headers

fy2010$prevailing_wage <- as.character(fy2010$prevailing_wage)
fy2010$employer_zip_code <- as.character(fy2010$employer_zip_code)
fy2010$worksite_zip_code <- as.character(fy2010$worksite_zip_code)

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
fy2011$fy <- rep(2010, dim(fy2011)[1])

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

fy2011$prevailing_wage <- as.character(fy2011$prevailing_wage)
fy2011$employer_zip_code <- as.character(fy2011$employer_zip_code)
fy2011$worksite_zip_code <- as.character(fy2011$worksite_zip_code)

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
fy2012$fy <- rep(2010, dim(fy2012)[1])

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

fy2012$prevailing_wage <- as.character(fy2012$prevailing_wage)
fy2012$employer_zip_code <- as.character(fy2012$employer_zip_code)
fy2012$worksite_zip_code <- as.character(fy2012$worksite_zip_code)

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
fy2013$fy <- rep(2010, dim(fy2013)[1])

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

fy2013$prevailing_wage <- as.character(fy2013$prevailing_wage)
fy2013$employer_zip_code <- as.character(fy2013$employer_zip_code)
fy2013$worksite_zip_code <- as.character(fy2013$worksite_zip_code)

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
fy2014$fy <- rep(2010, dim(fy2014)[1])

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

fy2014$prevailing_wage <- as.character(fy2014$prevailing_wage)
fy2014$employer_zip_code <- as.character(fy2014$employer_zip_code)
fy2014$worksite_zip_code <- as.character(fy2014$worksite_zip_code)

# DON'T CHANGE
columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2014, columnOrder)



######################################################################################################
#                                        2015                                                        #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2015	H-1B_Disclosure_Data_FY15_Q4	CASE_NUMBER	CASE_SUBMITTED	VISA_CLASS	EMPLOYER_NAME	EMPLOYER_ADDRESS2	EMPLOYER_ADDRESS1	EMPLOYER_CITY	EMPLOYER_STATE	EMPLOYER_POSTAL_CODE	WORKSITE_CITY	WORKSITE_STATE	WORKSITE_POSTAL_CODE	TOTAL WORKERS	DECISION_DATE	EMPLOYMENT_START_DATE	EMPLOYMENT_END_DATE	NA	NA	JOB_TITLE	NAIC_CODE	CASE_STATUS	NA	SOC_CODE	SOC_NAME	NA	NA	WAGE_RATE_OF_PAY_FROM	WAGE_RATE_OF_PAY_TO	WAGE_UNIT_OF_PAY	PREVAILING_WAGE	PW_UNIT_OF_PAY	NA	FULL_TIME_POSITION	AGENT_ATTORNEY_NAME	AGENT_ATTORNEY_CITY	AGENT_ATTORNEY_STATE', split = '\t'))
colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

# Copied and pasted from second part of excel sheet, excluding the NAs
renameCols <- unlist(strsplit('case_number	status	submitted_date	dol_decision_date	visa_class	begin_date	end_date	employer_name	employer_address1	employer_address2	employer_city	employer_state	agent_attorney_first_name	agent_attorney_city	agent_attorney_state	job_title	soc_code	soc_name	job_code	total_workers	part_time	prevailing_wage	prevailing_wage_unit	wage_rate	wage_unit	worksite_city	worksite_state	worksite_zip_code', split = '\t'))
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
             'max_wage',
             'withdrawn',
             'agent_attorney_last_name',
             'certified_begin_date',
             'certified_end_date',
             'occupational_title',
             'employer_zip_code')
# Add the year as a column
fy2015$fy <- rep(2010, dim(fy2015)[1])

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

fy2015$prevailing_wage <- as.character(fy2015$prevailing_wage)
fy2015$employer_zip_code <- as.character(fy2015$employer_zip_code)
fy2015$worksite_zip_code <- as.character(fy2015$worksite_zip_code)

# DON'T CHANGE
columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2015, columnOrder)


######################################################################################################
#                                        2016                                                        #
######################################################################################################
# Copied and pasted from first part of Excel sheet - These are the columns we want to read
# These are not in the order that will be in the CSV
colsToRead <- unlist(strsplit('2016	H-1B_Disclosure_Data_FY16_Q4	CASE_NUMBER	CASE_SUBMITTED	VISA_CLASS	EMPLOYER_NAME	EMPLOYER_ADDRESS	EMPLOYER_CITY	EMPLOYER_STATE	EMPLOYER_POSTAL_CODE	WORKSITE_CITY	WORKSITE_STATE	WORKSITE_POSTAL_CODE	TOTAL WORKERS	DECISION_DATE	EMPLOYMENT_START_DATE	EMPLOYMENT_END_DATE	NA	NA	JOB_TITLE	NAIC_CODE	CASE_STATUS	NA	SOC_CODE	SOC_NAME	NA	NA	WAGE_RATE_OF_PAY_FROM	WAGE_RATE_OF_PAY_TO	WAGE_UNIT_OF_PAY	PREVAILING_WAGE	PW_UNIT_OF_PAY	NA	FULL_TIME_POSITION	AGENT_ATTORNEY_NAME	AGENT_ATTORNEY_CITY	AGENT_ATTORNEY_STATE', split = '\t'))
colsToRead <- colsToRead[!colsToRead %in% 'NA']
# Remove the year and filename
colsToRead <- colsToRead[3:length(colsToRead)]

# Copied and pasted from second part of excel sheet, excluding the NAs
renameCols <- unlist(strsplit('case_number	status	submitted_date	dol_decision_date	visa_class	begin_date	end_date	employer_name	employer_address1	employer_city	employer_state	employer_zip_code	agent_attorney_first_name	agent_attorney_city	agent_attorney_state	job_title	soc_code	soc_name	job_code	total_workers	part_time	prevailing_wage	prevailing_wage_unit	wage_rate	wage_unit	worksite_city	worksite_state	worksite_zip_code', split = '\t'))
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
             'wage_rate_from',
             'wage_rate_to',
             'max_wage',
             'withdrawn',
             'agent_attorney_last_name',
             'certified_begin_date',
             'certified_end_date',
             'occupational_title',
             'employer_address2')
# Add the year as a column
fy2016$fy <- rep(2010, dim(fy2016)[1])

# Add the filename as a column
fy2016$file_name <- rep(file_name, dim(fy2016)[1])

# Missing columns
fy2016$wage_rate_from <- rep(NA, dim(fy2016)[1])
fy2016$wage_rate_to <- rep(NA, dim(fy2016)[1])
fy2016$max_wage <- rep(NA, dim(fy2016)[1])
fy2016$withdrawn <- rep(NA, dim(fy2016)[1])
fy2016$agent_attorney_last_name <- rep(NA, dim(fy2016)[1])
fy2016$certified_begin_date <- rep(NA, dim(fy2016)[1])
fy2016$certified_end_date <- rep(NA, dim(fy2016)[1])
fy2016$occupational_title <- rep(NA, dim(fy2016)[1])
fy2016$employer_address2 <- rep(NA, dim(fy2016)[1])

# Name the columns as they appear in the spreadsheet
colnames(fy2016) <- headers

fy2016$prevailing_wage <- as.character(fy2016$prevailing_wage)
fy2016$employer_zip_code <- as.character(fy2016$employer_zip_code)
fy2016$worksite_zip_code <- as.character(fy2016$worksite_zip_code)

# DON'T CHANGE
columnOrder <- unlist(strsplit('fy	file_name	case_number	submitted_date	visa_class	employer_name	employer_address1	employer_address2	employer_city	employer_state	employer_zip_code	worksite_city	worksite_state	worksite_zip_code	total_workers	dol_decision_date	begin_date	end_date	certified_begin_date	certified_end_date	job_title	job_code	status	withdrawn	soc_code	soc_name	occupational_title	wage_rate	wage_rate_from	wage_rate_to	wage_unit	prevailing_wage	prevailing_wage_unit	max_wage	part_time	agent_attorney_first_name	agent_attorney_last_name	agent_attorney_city	agent_attorney_state', split = '\t'))

# Order the columns
setcolorder(fy2016, columnOrder)



# finalDF <- rbind(fy2008, fy2009, fill = TRUE)
finalDF <- rbind(fy2008, fy2009, fy2009iCert, fy2010, fy2011, fy2012, fy2013, fy2014, fy2015, fy2016)

saveRDS(finalDF, 'H1BVisas.rds')


# Post processing

visas <- readRDS('H1BVisas.rds')

visas$submitted_date <- as.Date(visas$submitted_date, format = '%m/%d/%Y')
visas$dol_decision_date <- as.Date(visas$dol_decision_date, format = '%m/%d/%Y')
visas$begin_date <- as.Date(visas$begin_date, format = '%m/%d/%Y')
visas$end_date <- as.Date(visas$end_date, format = '%m/%d/%Y')

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
unique(visas$prevailing_wage_unit)
which(visas$prevailing_wage_unit == 'hr')


unique(visas$agent_attorney_first_name)
