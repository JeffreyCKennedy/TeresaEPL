# Add alternative data importation commands for files in 
# the data folder that should not be imported using standard data importation rules

library(car) # to use recode function later

CarPref <- sjlabelled::read_spss("./data/Career Preferences Survey_October 2, 2017_1707_17.37.sav")


# Warning on reading SPSS file --------------------------------------------

# Following warning is given: "Unrecognized record type 7, subtype 18
# encountered in system file" In a news group, Jon Peck gave following
# explanation: "Record 7 is an extension record for the SPSS sav file format.
# It is used for new features in order to preserve the ability of older versions
# of SPSS to read newer files.  I don't recall what subtype 18 is, but in most
# cases, an unrecognized subtype does not affect the data."


# Qualtrics provided a unique ID (ResponseId), while for the manually completed surveys, Teresa entered
# a questionnaire number (Qstnr_number). Need to create an ID to cover all cases, which can be used
# for tracking original source of any anomalous data. Also useful for creating scales.
CarPref$ID <- c(1:nrow(CarPref))

# Create a list of variables to drop from the working dataframe:
dropvars <- c("StartDate","EndDate","Status","IPAddress","Progress","Duration__in_seconds_",
              "Finished","RecordedDate","RecipientLastName","RecipientFirstName",
              "RecipientEmail","ExternalReference","LocationLatitude",
              "LocationLongitude","DistributionChannel","UserLanguage","EMAIL_ADRESS",
              "CONSENT_TO_PARTICIPATE","Qualification_text","Occupational_group_text")

CarPref <- CarPref %>% 
    select(-one_of(dropvars)) %>% 
    filter(Country == 123)  # Drop non-New Zealand responses
    
# Replace missing values in these two vars so can then combine years and months
# to get total tenure. Without this step, blank (zero) values are treated
# as NA. See dplyr for the coalesce function.
CarPref$Years_organisation <- coalesce(as.integer(CarPref$Years_organisation), 0L)
CarPref$Months_organisation <- coalesce(as.integer(CarPref$Months_organisation), 0L)
CarPref <- CarPref %>% 
    mutate(tenure_yrs = ((12*Years_organisation)+Months_organisation)/12)

# Reverse code items (uses recode function from car package)
CarPref$MOT5_LAI <- car::recode(CarPref$MOT5_LAI_REVERSE,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1 ; else=NA")
CarPref$MOT11_LNCL <- car::recode(CarPref$MOT11_LNCL_REVERSE,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
CarPref$MOT26_LSN <- car::recode(CarPref$MOT26_LSN_REVERSE,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
CarPref$JS_2 <- car::recode(CarPref$JS_2_REVERSE,"1=7 ; 2=6 ; 3=5 ; 4=4 ; 5=3; 6=2; 7=1; else=NA")

# Missing value not coded properly in original spss file.
CarPref$Age[CarPref$Age==98] <- NA

# Used following to check if any other variables had problems with missing var value
# datacheck <- subset(CarPref, select = c("Age","Gender","Qualification","Occupational_group",
#                                         "Job_status","Job_status_other","Years_in_organisation","Years_organisation",
#                                         "Months_organisation","Hours_perweek_work"))
# summary(datacheck)
# 
# save(CarPref, file = "./data/CarPref.Rdata")