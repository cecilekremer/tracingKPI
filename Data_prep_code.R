
###################################################################################
######                                                                      #######
###### CONTROLENTOREN PROJECT UA/UH    --   DATA PREPARATION                #######
######                                                                      #######
###### AUTHORS: Cécile Kremer (UH) & Lander Willem (UA)                     #######
###### LAST UPDATE: 27/10/2022                                              #######
######                                                                      #######
###################################################################################


# dependencies
library(data.table) # data.table format
library(lubridate)  # date operations
library(plyr)
library(dplyr)

###################################
#### DATA CLEANING            #####
###################################

### LOAD DATA  ----

data_all <- read.csv('20220519_covid19_controle_toren_detail_hashed.csv',sep=";")
data_INDEX <- data_all
rm(data_all)
data_HR <- read.csv('20220519_covid19_controle_toren_vlg_detail_hr_hashed.csv',sep=";")
data_LR <- read.csv('20220519_covid19_controle_toren_vlg_detail_lr_hashed.csv', sep=";")
data_COLL <- read.csv('~/CONTROLETOREN/20220927_covid19_controle_toren_vlg_detail_collectivity.csv', sep=";")

INDEX <- data_INDEX
HRcontacts <- data_HR
LRcontacts <- data_LR
rm(data_INDEX); rm(data_HR); rm(data_LR)
gc()

## Select index cases for which call ok
INDEX <- INDEX[INDEX$CALL_STATUS=="CALL_OK",]

INDEX <- data.table(INDEX)
HRcontacts <- data.table(HRcontacts)
LRcontacts <- data.table(LRcontacts)

#### REFORMAT & PRE-PROCESS ----

# reformat: dates
INDEX[,EVENT_START_DT := as.Date(EVENT_START_DT)]
INDEX[,TEST_COLLECTION_DT := as.Date(TEST_COLLECTION_DT)]
INDEX[TEST_COLLECTION_DT == "1900-01-01",TEST_COLLECTION_DT := NA]
INDEX[,TEST_RETURN_DT := as.Date(TEST_RETURN_DT)]
INDEX[TEST_RETURN_DT == "1900-01-01",TEST_RETURN_DT := NA]
INDEX[,CALL_DATETIME := as.Date(CALL_DATETIME)]
INDEX[,BIRTH_DT := as.Date(BIRTH_DT)]
INDEX[BIRTH_DT == "1900-01-01",BIRTH_DT := NA]
INDEX[,SYMPTOMS_START_DT := as.Date(SYMPTOMS_START_DT, format="%Y-%m-%d")]

HRcontacts[,SCIENSANO_REQUEST_TICKET_DT := as.Date(SCIENSANO_REQUEST_TICKET_DT)]
HRcontacts[,LAST_CONTACT_DT := as.Date(LAST_CONTACT_DT, format="%Y-%m-%d")]
HRcontacts[LAST_CONTACT_DT == "1900-01-01",LAST_CONTACT_DT := NA]
HRcontacts[,BIRTH_DT := as.Date(BIRTH_DT, format="%Y-%m-%d")]
HRcontacts[BIRTH_DT == "1900-01-01",BIRTH_DT := NA]

LRcontacts[,SCIENSANO_REQUEST_TICKET_DT := as.Date(SCIENSANO_REQUEST_TICKET_DT)]
LRcontacts[,LAST_CONTACT_DT := as.Date(LAST_CONTACT_DT, format="%Y-%m-%d")]
LRcontacts[LAST_CONTACT_DT == "1900-01-01",LAST_CONTACT_DT := NA]
LRcontacts[,BIRTH_DT := as.Date(BIRTH_DT, format="%Y-%m-%d")]
LRcontacts[BIRTH_DT == "1900-01-01",BIRTH_DT := NA]

# ADD AGE; set to NA for impossible dates
INDEX[BIRTH_DT>"2021-10-27" | BIRTH_DT<"1909-05-07", BIRTH_DT := NA]
HRcontacts[BIRTH_DT>"2021-10-27" | BIRTH_DT<"1909-05-07", BIRTH_DT := NA]
LRcontacts[BIRTH_DT>"2021-10-27" | BIRTH_DT<"1909-05-07", BIRTH_DT := NA]

INDEX[,AGE := time_length(difftime(EVENT_START_DT,BIRTH_DT),"years")]
INDEX[AGE<0, AGE := NA]
HRcontacts[,AGE := time_length(difftime(SCIENSANO_REQUEST_TICKET_DT,BIRTH_DT),"years")]
LRcontacts[,AGE := time_length(difftime(SCIENSANO_REQUEST_TICKET_DT,BIRTH_DT),"years")]
HRcontacts[AGE<0, AGE := NA]
LRcontacts[AGE<0, AGE := NA]

# Set symptoms to missing if symptoms after call date
INDEX[SYMPTOMS_START_DT > CALL_DATETIME, SYMPTOMS_START_DT := NA]
INDEX[SYMPTOMS_START_DT < "2020-05-01", SYMPTOMS_START_DT := NA]

# Save raw data
save.image("RawData060922.RData")

summary(INDEX$TEST_RETURN_DT)
dim(INDEX)
INDEX <- INDEX[INDEX$TEST_RETURN_DT < '2022-01-01',]

#################################################################################
# REMOVE DUPLICATES: 
#
# - duplicate RRN: could be reinfections? --> only remove if same ticket nr
# 
#################################################################################

load("G:/My Drive/UHasselt/PhD/COVID outbreaks/CONTROLETOREN/UPDATE OKT 21/UPDATE 2022/RawData190522.RData")

setwd("G:/My Drive/UHasselt/PhD/COVID outbreaks/CONTROLETOREN/UPDATE OKT 21/UPDATE 2022/FINAL MANUSCRIPT CODE/")

## Only use laboratory-confirmed cases & unique entries
INDEX2 = INDEX[INDEX$TEST_TYPE!=""] # no test collection / return date

INDEX2[GENDER=="", GENDER := NA]
INDEX2[POSTAL_CD=="", POSTAL_CD := NA]

# keep distinct entries
library(tidyverse)
test.df = INDEX2 %>%
  distinct(SCIENSANO_REQUEST_TICKET_NBR, NATIONAL_INSURANCE_NBR, .keep_all=T)
detach("package:tidyverse", unload=T)
dim(test.df) == dim(INDEX2)
# Keep only those with test date available
INDEX2 = test.df[!is.na(test.df$TEST_RETURN_DT),]
rm(test.df)

INDEX.UNIQUE = data.table(INDEX2) # should contain only unique ticket numbers; duplicated RRN are re-infections?
dim(INDEX.UNIQUE)
length(unique(INDEX.UNIQUE$SCIENSANO_REQUEST_TICKET_NBR))
length(unique(INDEX.UNIQUE$NATIONAL_INSURANCE_NBR))
rm(INDEX2)

### Add sequencing variable and variant if sequenced
### STRAIN_CD_DETECTED_MUTATION == "SDropout" since 10/12/2021 --> Omicron?
INDEX.UNIQUE$SEQUENCED = ifelse(INDEX.UNIQUE$STRAIN_CREATION_DT!="",1,0)
INDEX.UNIQUE[, VARIANT := NA]
INDEX.UNIQUE$VARIANT=ifelse(grepl("501Y.V3",INDEX.UNIQUE$STRAIN_CD_TEST_RESULT,fixed=T), "Brazil", INDEX.UNIQUE$VARIANT)
INDEX.UNIQUE$VARIANT=ifelse(grepl("501Y.V1",INDEX.UNIQUE$STRAIN_CD_TEST_RESULT,fixed=T), "UK", INDEX.UNIQUE$VARIANT)
INDEX.UNIQUE$VARIANT=ifelse(grepl("501Y.V2",INDEX.UNIQUE$STRAIN_CD_TEST_RESULT,fixed=T), "ZA", INDEX.UNIQUE$VARIANT)
INDEX.UNIQUE$VARIANT=ifelse(grepl("Niet",INDEX.UNIQUE$STRAIN_CD_TEST_RESULT,fixed=T),"None", INDEX.UNIQUE$VARIANT)
INDEX.UNIQUE$VARIANT=ifelse(grepl("niet",INDEX.UNIQUE$STRAIN_CD_TEST_RESULT,fixed=T),"None", INDEX.UNIQUE$VARIANT)
INDEX.UNIQUE$VARIANT=ifelse(grepl("Geen",INDEX.UNIQUE$STRAIN_CD_TEST_RESULT,fixed=T),"None", INDEX.UNIQUE$VARIANT)
INDEX.UNIQUE$VARIANT=ifelse(grepl("Andere",INDEX.UNIQUE$STRAIN_CD_TEST_RESULT,fixed=T)|grepl("TRVOC",INDEX.UNIQUE$STRAIN_CD_TEST_RESULT,fixed=T),
                            "Other", INDEX.UNIQUE$VARIANT)
INDEX.UNIQUE$VARIANT=ifelse(is.na(INDEX.UNIQUE$VARIANT) & INDEX.UNIQUE$SEQUENCED==1, "None", INDEX.UNIQUE$VARIANT)

## Delta zit niet in STRAIN_CD_TEST_RESULT
INDEX.UNIQUE$VARIANT=ifelse((grepl("452R", INDEX.UNIQUE$STRAIN_CD_DETECTED_MUTATION,fixed=T) | 
                               grepl("478K", INDEX.UNIQUE$STRAIN_CD_DETECTED_MUTATION,fixed=T) |
                               grepl("681R", INDEX.UNIQUE$STRAIN_CD_DETECTED_MUTATION,fixed=T)),
                            "India", INDEX.UNIQUE$VARIANT)

################################
## HR contacts with missing RRN 

HRcontactsNoRRN <- HRcontacts[HRcontacts$NATIONAL_INSURANCE_NBR=="",]
dim(HRcontactsNoRRN)[1]/dim(HRcontacts)[1]
summary(HRcontactsNoRRN$BIRTH_DT)


##################
## Index case traced as contact before positive test?

INDEX.UNIQUE$traced = ifelse((INDEX.UNIQUE$NATIONAL_INSURANCE_NBR %in% HRcontacts$NATIONAL_INSURANCE_NBR) |  
                               (INDEX.UNIQUE$NATIONAL_INSURANCE_NBR %in% LRcontacts$NATIONAL_INSURANCE_NBR),
                             1, 0)

INDEX.UNIQUE$LR.traced = ifelse(INDEX.UNIQUE$NATIONAL_INSURANCE_NBR %in% LRcontacts$NATIONAL_INSURANCE_NBR, 1, 0)
INDEX.UNIQUE$HR.traced = ifelse(INDEX.UNIQUE$NATIONAL_INSURANCE_NBR %in% HRcontacts$NATIONAL_INSURANCE_NBR, 1, 0)
table(INDEX.UNIQUE$traced)/dim(INDEX.UNIQUE)[1]

###
## Account for test date and contact call date

## Fix test collection date
INDEX.UNIQUE$TEST_COLLECTION_DT = ifelse(INDEX.UNIQUE$TEST_COLLECTION_DT > "2022-01-01", NA, as.Date(INDEX.UNIQUE$TEST_COLLECTION_DT, format = "%Y-%m-%d"))
INDEX.UNIQUE$TEST_COLLECTION_DT = as.Date(INDEX.UNIQUE$TEST_COLLECTION_DT, origin = "1970-01-01")

## Join index and contact datasets (to get call dates as HR and/or LR)
INDEX.UNIQUE2 = left_join(INDEX.UNIQUE, HRcontacts[ ,c('NATIONAL_INSURANCE_NBR','SCIENSANO_REQUEST_TICKET_DT')], by = "NATIONAL_INSURANCE_NBR")
INDEX.UNIQUE2 <- INDEX.UNIQUE2 %>%
  distinct(SCIENSANO_REQUEST_TICKET_NBR, NATIONAL_INSURANCE_NBR, .keep_all = T)

INDEX.UNIQUE3 = left_join(INDEX.UNIQUE2, LRcontacts[ ,c('NATIONAL_INSURANCE_NBR','SCIENSANO_REQUEST_TICKET_DT')], by = "NATIONAL_INSURANCE_NBR")
INDEX.UNIQUE3 <- INDEX.UNIQUE3 %>%
  distinct(SCIENSANO_REQUEST_TICKET_NBR, NATIONAL_INSURANCE_NBR, .keep_all = T)

INDEX.UNIQUE3 <- INDEX.UNIQUE3 %>%
  dplyr::rename(HRcallDate = SCIENSANO_REQUEST_TICKET_DT.y,
                LRcallDate = SCIENSANO_REQUEST_TICKET_DT)

INDEX.UNIQUE = INDEX.UNIQUE3
rm(INDEX.UNIQUE2, INDEX.UNIQUE3)
rm(INDEX)
gc()

INDEX.UNIQUE$SCIENSANO_REQUEST_TICKET_DT = INDEX.UNIQUE$SCIENSANO_REQUEST_TICKET_DT.x
INDEX.UNIQUE = INDEX.UNIQUE[, c(1:26, 28:86)]

## 'Known index' only if traced before positive test !!
# LR traced before test
INDEX.UNIQUE$LR.traced2 = NA
INDEX.UNIQUE$LR.traced2 = ifelse(INDEX.UNIQUE$LRcallDate < INDEX.UNIQUE$TEST_COLLECTION_DT, 1, 0)
INDEX.UNIQUE$LR.traced2 = ifelse(is.na(INDEX.UNIQUE$LR.traced2), 0, INDEX.UNIQUE$LR.traced2)
# HR traced before test
INDEX.UNIQUE$HR.traced2 = NA
INDEX.UNIQUE$HR.traced2 = ifelse(INDEX.UNIQUE$HRcallDate < INDEX.UNIQUE$TEST_COLLECTION_DT, 1, 0)
INDEX.UNIQUE$HR.traced2 = ifelse(is.na(INDEX.UNIQUE$HR.traced2), 0, INDEX.UNIQUE$HR.traced2)
# overall traced before test
INDEX.UNIQUE$traced2 = ifelse(INDEX.UNIQUE$LR.traced2 == 1 | INDEX.UNIQUE$HR.traced2 == 1, 1, 0)
table(INDEX.UNIQUE$traced2)/dim(INDEX.UNIQUE)[1]

######
save(INDEX.UNIQUE, file="INDEXUNIQUEv1_060922.RData")


#################################
# LINK CONTACTS TO THEIR INDEX
#################################

setkey(INDEX.UNIQUE,"SCIENSANO_REQUEST_TICKET_NBR") # data sorted according to ID (ascending)

# contact tested positive if they return in INDEX.UNIQUE dataset 
HRcontacts[, test.pos := 0]
LRcontacts[, test.pos := 0]
LRcontacts <- mutate(LRcontacts, test.pos = LRcontacts$NATIONAL_INSURANCE_NBR %in% INDEX.UNIQUE$NATIONAL_INSURANCE_NBR)
HRcontacts <- mutate(HRcontacts, test.pos = HRcontacts$NATIONAL_INSURANCE_NBR %in% INDEX.UNIQUE$NATIONAL_INSURANCE_NBR)
LRcontacts$test.pos = as.numeric(LRcontacts$test.pos)
HRcontacts$test.pos = as.numeric(HRcontacts$test.pos)

## Remove contacts with missing RRN (can't link them to index dataset, ie no info on conversion)
LRcontacts = data.table(LRcontacts); HRcontacts = data.table(HRcontacts)
LRcontacts[NATIONAL_INSURANCE_NBR=="", NATIONAL_INSURANCE_NBR := NA]
HRcontacts[NATIONAL_INSURANCE_NBR=="", NATIONAL_INSURANCE_NBR := NA]

# low risk
LRnomissRRN = LRcontacts[!is.na(NATIONAL_INSURANCE_NBR),]
# high risk
HRnomissRRN = HRcontacts[!is.na(NATIONAL_INSURANCE_NBR),]

### REMOVE DUPLICATES IF THEY ARE LINKED MORE THAN ONCE TO SAME INDEX CASE

# LR contacts 
LR.contacts = LRnomissRRN %>% distinct(NATIONAL_INSURANCE_NBR, INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all = T)

# HR contacts 
HR.contacts = HRnomissRRN %>% distinct(NATIONAL_INSURANCE_NBR, INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all = T)

# remove those that can't be linked to index case
LR.contacts = LR.contacts[LR.contacts$INDEX_SCIENSANO_REQUEST_TICKET_NBR!="",]
HR.contacts = HR.contacts[HR.contacts$INDEX_SCIENSANO_REQUEST_TICKET_NBR!="",]

rm(HRcontacts); rm(LRcontacts); rm(HRnomissRRN); rm(LRnomissRRN)
gc()

###########################
### Define number of traced contacts for each index case

## Assign number of total contacts, HR contacts, LR contacts + number of secondary cases for each index

## low risk
lr.index = LR.contacts$INDEX_SCIENSANO_REQUEST_TICKET_NBR
index.in.lr = INDEX.UNIQUE[INDEX.UNIQUE$SCIENSANO_REQUEST_TICKET_NBR%in%lr.index,]
index.out.lr = INDEX.UNIQUE[!INDEX.UNIQUE$SCIENSANO_REQUEST_TICKET_NBR%in%lr.index,]
tdf <- LR.contacts %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.LR.contacts")
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.LR.contacts")]
tdf4 <- tdf3[which(tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR%in%INDEX.UNIQUE$SCIENSANO_REQUEST_TICKET_NBR),]
tdf4$SCIENSANO_REQUEST_TICKET_NBR = tdf4$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf4 = tdf4[,c(2,3)]
index.in.lr2 <- merge(index.in.lr, tdf4, by="SCIENSANO_REQUEST_TICKET_NBR")
index.out.lr[, n.LR.contacts :=0]
INDEX.UNIQUE2 = rbind(index.in.lr2, index.out.lr)

## high risk
hr.index = HR.contacts$INDEX_SCIENSANO_REQUEST_TICKET_NBR
index.in.hr = INDEX.UNIQUE2[INDEX.UNIQUE2$SCIENSANO_REQUEST_TICKET_NBR%in%hr.index,]
index.out.hr = INDEX.UNIQUE2[!INDEX.UNIQUE2$SCIENSANO_REQUEST_TICKET_NBR%in%hr.index,]
tdf <- HR.contacts %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.HR.contacts")
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.HR.contacts")]
tdf4 <- tdf3[which(tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR%in%INDEX.UNIQUE$SCIENSANO_REQUEST_TICKET_NBR),]
tdf4$SCIENSANO_REQUEST_TICKET_NBR = tdf4$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf4 = tdf4[,c(2,3)]
index.in.hr2 <- merge(index.in.hr, tdf4, by="SCIENSANO_REQUEST_TICKET_NBR")
index.out.hr[, n.HR.contacts :=0]
INDEX.UNIQUE3 = rbind(index.in.hr2, index.out.hr)

INDEX.UNIQUE3$n.contacts = INDEX.UNIQUE3$n.LR.contacts+INDEX.UNIQUE3$n.HR.contacts
INDEX.UNIQUE = INDEX.UNIQUE3

rm(INDEX.UNIQUE2); rm(INDEX.UNIQUE3)
rm(index.in.hr); rm(index.in.hr2); rm(index.in.lr); rm(index.in.lr2)
rm(index.out.hr); rm(index.out.lr)
rm(tdf); rm(tdf2); rm(tdf3); rm(tdf4)
rm(hr.index); rm(lr.index)
gc()

## POSITIVE CONTACTS

setkey(INDEX.UNIQUE,"SCIENSANO_REQUEST_TICKET_NBR") # data sorted according to ID (ascending)

# high risk
hr.index = HR.contacts$INDEX_SCIENSANO_REQUEST_TICKET_NBR[HR.contacts$test.pos==1]
index.in.hr = INDEX.UNIQUE[INDEX.UNIQUE$SCIENSANO_REQUEST_TICKET_NBR%in%hr.index,]
index.out.hr = INDEX.UNIQUE[!INDEX.UNIQUE$SCIENSANO_REQUEST_TICKET_NBR%in%hr.index,]
hr.pos = HR.contacts[HR.contacts$test.pos==1,]
tdf <- hr.pos %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.pos.HR.contacts")
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.pos.HR.contacts")]
tdf4 <- tdf3[which(tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR%in%INDEX.UNIQUE$SCIENSANO_REQUEST_TICKET_NBR),]
tdf4$SCIENSANO_REQUEST_TICKET_NBR = tdf4$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf4 = tdf4[,c(2,3)]
index.in.hr2 <- merge(index.in.hr, tdf4, by="SCIENSANO_REQUEST_TICKET_NBR")
index.out.hr[, n.pos.HR.contacts :=0]
INDEX.UNIQUE2 = rbind(index.in.hr2, index.out.hr)

## low risk
lr.index = LR.contacts$INDEX_SCIENSANO_REQUEST_TICKET_NBR[LR.contacts$test.pos==1]
index.in.lr = INDEX.UNIQUE2[INDEX.UNIQUE2$SCIENSANO_REQUEST_TICKET_NBR%in%lr.index,]
index.out.lr = INDEX.UNIQUE2[!INDEX.UNIQUE2$SCIENSANO_REQUEST_TICKET_NBR%in%lr.index,]
lr.pos = LR.contacts[LR.contacts$test.pos==1,]
tdf <- lr.pos %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.pos.LR.contacts")
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.pos.LR.contacts")]
tdf4 <- tdf3[which(tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR%in%INDEX.UNIQUE2$SCIENSANO_REQUEST_TICKET_NBR),]
tdf4$SCIENSANO_REQUEST_TICKET_NBR = tdf4$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf4 = tdf4[,c(2,3)]
index.in.lr2 <- merge(index.in.lr, tdf4, by="SCIENSANO_REQUEST_TICKET_NBR")
index.out.lr[, n.pos.LR.contacts :=0]
INDEX.UNIQUE3 = rbind(index.in.lr2, index.out.lr)

INDEX.UNIQUE3$n.pos.contacts = INDEX.UNIQUE3$n.pos.LR.contacts+INDEX.UNIQUE3$n.pos.HR.contacts

INDEX.UNIQUE = INDEX.UNIQUE3

rm(cnt3,cnt4); rm(hr.pos, index.in.hr, index.in.hr2, index.in.lr, index.in.lr2, index.out.hr, index.out.lr)
rm(INDEX.UNIQUE2, INDEX.UNIQUE3, lr.pos, tdf, tdf2, tdf3, tdf4)
rm(hr.index, lr.index)
gc()

#######################
### FINAL INDEX DATASET

save(INDEX.UNIQUE, file="INDEX_UNIQUE_0609.RData")

#############################################################
#### CONTACTS: keep only those linked to available index case

ticket.nrs = unique(INDEX.UNIQUE$SCIENSANO_REQUEST_TICKET_NBR)
contacts.HR = HR.contacts[HR.contacts$INDEX_SCIENSANO_REQUEST_TICKET_NBR%in%ticket.nrs]
contacts.LR = LR.contacts[LR.contacts$INDEX_SCIENSANO_REQUEST_TICKET_NBR%in%ticket.nrs]
dim(contacts.LR); dim(contacts.HR)

################################
## CREATE CONTACT LINE LIST
################################

contacts.LR[GENDER=="", GENDER := NA]
contacts.LR[PRIMARY_ADDRESS_STREET=="", PRIMARY_ADDRESS_STREET := NA]
contacts.LR[PRIMARY_ADDRESS_HOUSE_NBR=="", PRIMARY_ADDRESS_HOUSE_NBR := NA]
contacts.LR[PRIMARY_ADDRESS_POSTAL_CD=="", PRIMARY_ADDRESS_POSTAL_CD := NA]
## keep only unique combination of ticket nr - index case - rrn
library(tidyverse)
test.df = contacts.LR %>%
  distinct(SCIENSANO_REQUEST_TICKET_NBR, INDEX_SCIENSANO_REQUEST_TICKET_NBR, NATIONAL_INSURANCE_NBR, .keep_all=T)
detach("package:tidyverse", unload=T)
contacts.LR = data.table(test.df)
rm(test.df)

contacts.HR[GENDER=="", GENDER := NA]
contacts.HR[PRIMARY_ADDRESS_STREET=="", PRIMARY_ADDRESS_STREET := NA]
contacts.HR[PRIMARY_ADDRESS_HOUSE_NBR=="", PRIMARY_ADDRESS_HOUSE_NBR := NA]
contacts.HR[PRIMARY_ADDRESS_POSTAL_CD=="", PRIMARY_ADDRESS_POSTAL_CD := NA]
## keep only unqiue combination of ticket nr - index case - rrn
library(tidyverse)
test.df = contacts.HR %>%
  distinct(SCIENSANO_REQUEST_TICKET_NBR, INDEX_SCIENSANO_REQUEST_TICKET_NBR, NATIONAL_INSURANCE_NBR, .keep_all=T)
detach("package:tidyverse", unload=T)
contacts.HR = data.table(test.df)
rm(test.df)

gc()

## Variables: contact id, index id, contact age, index age, contact gender, index gender, contact positive, contact type (LR, HR), last contact date, address
contacts.LR$type = 1
data.LR = contacts.LR[,c("NATIONAL_INSURANCE_NBR","INDEX_SCIENSANO_REQUEST_TICKET_NBR","AGE","GENDER","test.pos","type","LAST_CONTACT_DT","SCIENSANO_REQUEST_TICKET_NBR",
                         "SCIENSANO_REQUEST_TICKET_DT",
                         "PRIMARY_ADDRESS_POSTAL_CD","PRIMARY_ADDRESS_STREET","PRIMARY_ADDRESS_HOUSE_NBR")]
data.LR$CONTACT_TICKET_NBR = data.LR$SCIENSANO_REQUEST_TICKET_NBR
contacts.HR$type = 2
data.HR = contacts.HR[,c("NATIONAL_INSURANCE_NBR","INDEX_SCIENSANO_REQUEST_TICKET_NBR","AGE","GENDER","test.pos","type","LAST_CONTACT_DT","SCIENSANO_REQUEST_TICKET_NBR",
                         "SCIENSANO_REQUEST_TICKET_DT",
                         "PRIMARY_ADDRESS_POSTAL_CD","PRIMARY_ADDRESS_STREET","PRIMARY_ADDRESS_HOUSE_NBR")]
data.HR$CONTACT_TICKET_NBR = data.HR$SCIENSANO_REQUEST_TICKET_NBR

## Add age and gender index + symptom onset & date pos test & address
test.data = left_join(data.LR, INDEX.UNIQUE, by=c("INDEX_SCIENSANO_REQUEST_TICKET_NBR" = "SCIENSANO_REQUEST_TICKET_NBR"), suffix = c("_CLR","_I"))
data.LR = test.data[,c("NATIONAL_INSURANCE_NBR_CLR","NATIONAL_INSURANCE_NBR_I","INDEX_SCIENSANO_REQUEST_TICKET_NBR","AGE_CLR","GENDER_CLR","GENDER_I","AGE_I","type",
                       "CALL_DATETIME", "SCIENSANO_REQUEST_TICKET_DT_I", # Index call date??
                       "TEST_RETURN_DT","LAST_CONTACT_DT","CONTACT_TICKET_NBR", 
                       "SCIENSANO_REQUEST_TICKET_DT_CLR","SYMPTOMS_START_DT","test.pos",
                       "EVENT_START_DT","TEST_CREATION_DT","TEST_COLLECTION_DT",
                       "PRIMARY_ADDRESS_STREET","PRIMARY_ADDRESS_POSTAL_CD", "PRIMARY_ADDRESS_HOUSE_NBR",
                       "STREET","HOUSE_NBR","BOX","POSTAL_CD","VARIANT","SEQUENCED","traced2")]
rm(test.data)
test.data = left_join(data.HR, INDEX.UNIQUE, by=c("INDEX_SCIENSANO_REQUEST_TICKET_NBR" = "SCIENSANO_REQUEST_TICKET_NBR"), suffix = c("_CHR","_I"))
data.HR = test.data[,c("NATIONAL_INSURANCE_NBR_CHR","NATIONAL_INSURANCE_NBR_I","INDEX_SCIENSANO_REQUEST_TICKET_NBR","AGE_CHR","GENDER_CHR","GENDER_I","AGE_I","type",
                       "CALL_DATETIME","SCIENSANO_REQUEST_TICKET_DT_I","TEST_RETURN_DT","LAST_CONTACT_DT","CONTACT_TICKET_NBR",
                       "SCIENSANO_REQUEST_TICKET_DT_CHR","SYMPTOMS_START_DT","test.pos",
                       "EVENT_START_DT","TEST_CREATION_DT","TEST_COLLECTION_DT",
                       "PRIMARY_ADDRESS_STREET","PRIMARY_ADDRESS_POSTAL_CD", "PRIMARY_ADDRESS_HOUSE_NBR",
                       "STREET","HOUSE_NBR","BOX","POSTAL_CD","VARIANT","SEQUENCED","traced2")]
rm(test.data)

# rename variables
data.LR$C.AGE = data.LR$AGE_CLR; data.LR$I.AGE = data.LR$AGE_I; data.LR$C.GENDER = data.LR$GENDER_CLR; data.LR$I.GENDER = data.LR$GENDER_I; data.LR$I.TEST = data.LR$TEST_RETURN_DT
data.LR$C.CALLDT = data.LR$SCIENSANO_REQUEST_TICKET_DT_CLR; data.LR$I.CALLDT = data.LR$CALL_DATETIME; data.LR$I.CALLDT2 = data.LR$SCIENSANO_REQUEST_TICKET_DT_I; 
data.LR$I.SYMPTOMS = data.LR$SYMPTOMS_START_DT; 
data.LR$C.RRN = data.LR$NATIONAL_INSURANCE_NBR_CLR
data.LR$C.POS = data.LR$test.pos; data.LR$I.RRN = data.LR$NATIONAL_INSURANCE_NBR_I
data.LR$C.STREET = data.LR$PRIMARY_ADDRESS_STREET; data.LR$C.HOUSENR = data.LR$PRIMARY_ADDRESS_HOUSE_NBR; data.LR$C.POSTCODE = data.LR$PRIMARY_ADDRESS_POSTAL_CD
data.LR$I.STREET = data.LR$STREET; data.LR$I.HOUSENR = data.LR$HOUSE_NBR; data.LR$I.POSTCODE = data.LR$POSTAL_CD; data.LR$I.BOX = data.LR$BOX
data.LR$I.SEQUENCED = data.LR$SEQUENCED; data.LR$I.VARIANT = data.LR$VARIANT
data.LR$I.traced = data.LR$traced2

data.HR$C.AGE = data.HR$AGE_CHR; data.HR$I.AGE = data.HR$AGE_I; data.HR$C.GENDER = data.HR$GENDER_CHR; data.HR$I.GENDER = data.HR$GENDER_I; data.HR$I.TEST = data.HR$TEST_RETURN_DT
data.HR$C.CALLDT = data.HR$SCIENSANO_REQUEST_TICKET_DT_CHR; data.HR$I.CALLDT = data.HR$CALL_DATETIME; data.HR$I.CALLDT2 = data.HR$SCIENSANO_REQUEST_TICKET_DT_I; 
data.HR$I.SYMPTOMS = data.HR$SYMPTOMS_START_DT; data.HR$C.RRN = data.HR$NATIONAL_INSURANCE_NBR_CHR
data.HR$C.POS = data.HR$test.pos; data.HR$I.RRN = data.HR$NATIONAL_INSURANCE_NBR_I
data.HR$C.STREET = data.HR$PRIMARY_ADDRESS_STREET; data.HR$C.HOUSENR = data.HR$PRIMARY_ADDRESS_HOUSE_NBR; data.HR$C.POSTCODE = data.HR$PRIMARY_ADDRESS_POSTAL_CD
data.HR$I.STREET = data.HR$STREET; data.HR$I.HOUSENR = data.HR$HOUSE_NBR; data.HR$I.POSTCODE = data.HR$POSTAL_CD; data.HR$I.BOX = data.HR$BOX
data.HR$I.SEQUENCED = data.HR$SEQUENCED; data.HR$I.VARIANT = data.HR$VARIANT
data.HR$I.traced = data.HR$traced2

# select data to keep in contact line list
data.LR = data.LR[,c(3,8,9,12,13,30:51)]
names(data.LR)

data.HR = data.HR[,c(3,8,9,12,13,30:51)]
names(data.HR)

## Combine high (type == 2) and low (type == 1) risk contacts
contact.line.list = rbind(data.LR,data.HR)

# remove entries where index == contact
contact.line.list = contact.line.list[which(as.character(contact.line.list$C.RRN) != as.character(contact.line.list$I.RRN)),]
dim(contact.line.list)

## Take contact gender from index data if test pos + symptom onset of positive contacts
test.data = left_join(contact.line.list, INDEX.UNIQUE, by=c("C.RRN" = "NATIONAL_INSURANCE_NBR"), suffix=c("_C","_I"))
names(test.data)

test.data$C.GENDER = if_else(is.na(test.data$C.GENDER), test.data$GENDER, test.data$C.GENDER)
test.data$C.SYMPTOMS = test.data$SYMPTOMS_START_DT
test.data$C.TEST = test.data$TEST_RETURN_DT
test.data$C.VARIANT = test.data$VARIANT; test.data$C.SEQUENCED = test.data$SEQUENCED

#########################
## FINAL CONTACT LINELIST

rm(ticket.nrs, contacts.HR, contacts.LR, data.HR, data.LR)

contact.line.list = test.data[,c(1:27, 121:124)]
rm(test.data)

## Set to negative (not infected by that index) for entries where test contact not within -21 to 21 days of test index 
contact.line.list$time_between = as.numeric(difftime(contact.line.list$C.TEST, contact.line.list$I.TEST, units="days"))
contact.line.list$timetocall = as.numeric(difftime(contact.line.list$C.CALLDT, contact.line.list$I.CALLDT2, units="days"))

contact.line.list$C.POS = if_else(contact.line.list$time_between<(-21)|contact.line.list$time_between>21, 0, contact.line.list$C.POS)
contact.line.list$C.POS[is.na(contact.line.list$C.POS)] = 0

## Set to negative if both are sequenced but different variant found
contact.line.list$C.POS = ifelse((contact.line.list$I.VARIANT==contact.line.list$C.VARIANT)|is.na(contact.line.list$I.VARIANT)|is.na(contact.line.list$C.VARIANT),
                                 contact.line.list$C.POS, 0)

contact.line.list$C.POS2 = if_else(contact.line.list$time_between<(-14)|contact.line.list$time_between>14, 0, contact.line.list$C.POS)
contact.line.list$C.POS2[is.na(contact.line.list$C.POS2)] = 0
table(contact.line.list$C.POS2)

## Serial interval
contact.line.list$SI <- as.numeric(contact.line.list$C.SYMPTOMS - contact.line.list$I.SYMPTOMS)
data.pos <- contact.line.list[contact.line.list$C.POS == 1,]
summary(data.pos$SI); sum(!is.na(data.pos$SI))/dim(data.pos)[1]
sum(data.pos$SI < -5 | data.pos$SI > 21, na.rm = T)
dim(data.pos)

#######################
## SAVE CLEAN DATASET
#######################

save(contact.line.list,file="CONTACT_LINELIST_0609.RData")

#########################################################
### UPDATE NUMBER OF POSITIVE CONTACTS IN INDEX DATA

tdf <- contact.line.list[contact.line.list$C.POS==1 & contact.line.list$type==1,] %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.pos.LR.contacts")
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.pos.LR.contacts")]
tdf3$SCIENSANO_REQUEST_TICKET_NBR = tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf3 = tdf3[,c(2,3)]
test.data = left_join(INDEX.UNIQUE, tdf3, by = c("SCIENSANO_REQUEST_TICKET_NBR"))
test.data$n.pos.LR.contacts = test.data$n.pos.LR.contacts.y
test.data$n.pos.LR.contacts = ifelse(is.na(test.data$n.pos.LR.contacts), 0, test.data$n.pos.LR.contacts)
INDEX.UNIQUE = test.data[, c(1:92, 94, 96)]
rm(test.data)

tdf <- contact.line.list[contact.line.list$C.POS==1 & contact.line.list$type==2,] %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.pos.HR.contacts")
summary(tdf$n.pos.HR.contacts)
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
dim(tdf)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.pos.HR.contacts")]
tdf3$SCIENSANO_REQUEST_TICKET_NBR = tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf3 = tdf3[,c(2,3)]
test.data = left_join(INDEX.UNIQUE, tdf3, by = c("SCIENSANO_REQUEST_TICKET_NBR"))
test.data$n.pos.HR.contacts = test.data$n.pos.HR.contacts.y
test.data$n.pos.HR.contacts = ifelse(is.na(test.data$n.pos.HR.contacts), 0, test.data$n.pos.HR.contacts)
INDEX.UNIQUE = test.data[, c(1:91, 93,94,96)]

rm(test.data)
rm(tdf, tdf2, tdf3)

#####################################################################
### UPDATE NUMBER OF POSITIVE CONTACTS IN INDEX DATA BASED ON 14 DAYS

tdf <- contact.line.list[contact.line.list$C.POS2==1 & contact.line.list$type==1,] %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.pos.LR.contacts2")
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.pos.LR.contacts2")]
tdf3$SCIENSANO_REQUEST_TICKET_NBR = tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf3 = tdf3[,c(2,3)]
test.data = left_join(INDEX.UNIQUE, tdf3, by = c("SCIENSANO_REQUEST_TICKET_NBR"))
test.data$n.pos.LR.contacts2 = test.data$n.pos.LR.contacts2
test.data$n.pos.LR.contacts2 = ifelse(is.na(test.data$n.pos.LR.contacts2), 0, test.data$n.pos.LR.contacts2)
INDEX.UNIQUE = test.data
rm(test.data)

tdf <- contact.line.list[contact.line.list$C.POS2==1 & contact.line.list$type==2,] %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.pos.HR.contacts2")
summary(tdf$n.pos.HR.contacts2)
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
dim(tdf)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.pos.HR.contacts2")]
tdf3$SCIENSANO_REQUEST_TICKET_NBR = tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf3 = tdf3[,c(2,3)]
test.data = left_join(INDEX.UNIQUE, tdf3, by = c("SCIENSANO_REQUEST_TICKET_NBR"))
test.data$n.pos.HR.contacts2 = test.data$n.pos.HR.contacts2
test.data$n.pos.HR.contacts2 = ifelse(is.na(test.data$n.pos.HR.contacts2), 0, test.data$n.pos.HR.contacts2)
INDEX.UNIQUE = test.data

rm(test.data)
rm(tdf, tdf2, tdf3)

##### FINAL INDEX.UNIQUE DATA
INDEX.UNIQUE$n.pos.contacts = INDEX.UNIQUE$n.pos.HR.contacts + INDEX.UNIQUE$n.pos.LR.contacts
INDEX.UNIQUE$n.pos.contacts2 = INDEX.UNIQUE$n.pos.HR.contacts2 + INDEX.UNIQUE$n.pos.LR.contacts2

save(INDEX.UNIQUE, file="INDEX_UNIQUE_0609.RData")

##########################
## Time from index test date to tracing of contact?

contact.line.list$time_to_trace = as.numeric(difftime(contact.line.list$C.CALLDT, contact.line.list$I.TEST, units="days"))

########################################
#### CONTACT LINE LIST: HH & REGION ####
########################################

# load("CONTACT_LINELIST_2710.RData")

## Create HH variable
contact.line.list$I.ADRESS = paste(contact.line.list$I.STREET, contact.line.list$I.HOUSENR, contact.line.list$I.POSTCODE, sep="")
contact.line.list$C.ADRESS = paste(contact.line.list$C.STREET, contact.line.list$C.HOUSENR, contact.line.list$C.POSTCODE, sep="")

contact.line.list[, HH.member := 0]
# setkey(contact.line.list, C.RRN)
# for(i in 1:dim(contact.line.list)[1]){
#   if(!is.na(contact.line.list$I.STREET[i]) & !is.na(contact.line.list$I.HOUSENR[i]) & !is.na(contact.line.list$I.POSTCODE[i]) &
#      !is.na(contact.line.list$C.STREET[i]) & !is.na(contact.line.list$C.HOUSENR[i]) & !is.na(contact.line.list$C.POSTCODE[i])){
#     if(contact.line.list$C.ADRESS[i]==contact.line.list$I.ADRESS[i]){
#       contact.line.list[i, HH.member := 1]
#     }
#   }else{contact.line.list[i, HH.member := NA]}
# }
# rm(i)
contact.line.list$HH.member = ifelse(grepl("H", contact.line.list$CONTACT_TICKET_NBR), 1, contact.line.list$HH.member)

contact.line.list$I.AGE = as.integer(contact.line.list$I.AGE)
contact.line.list$C.AGE = as.integer(contact.line.list$C.AGE)

## REGION
contact.line.list$postcode.i = as.numeric(as.character(contact.line.list$I.POSTCODE))
contact.line.list[, I.REGION := NULL]
contact.line.list$I.REGION = if_else(contact.line.list$postcode.i>=2000 & contact.line.list$postcode.i<3000, "AW", contact.line.list$I.REGION)
contact.line.list$I.REGION = if_else(contact.line.list$postcode.i>=3500 & contact.line.list$postcode.i<4000, "LI", contact.line.list$I.REGION)
contact.line.list$I.REGION = if_else(contact.line.list$postcode.i>=1500 & contact.line.list$postcode.i<2000, "VB", contact.line.list$I.REGION)
contact.line.list$I.REGION = if_else(contact.line.list$postcode.i>=3000 & contact.line.list$postcode.i<3500, "VB", contact.line.list$I.REGION)
contact.line.list$I.REGION = if_else(contact.line.list$postcode.i>=1000 & contact.line.list$postcode.i<1300, "BXL", contact.line.list$I.REGION)
contact.line.list$I.REGION = if_else(contact.line.list$postcode.i>=8000 & contact.line.list$postcode.i<9000, "WV", contact.line.list$I.REGION)
contact.line.list$I.REGION = if_else(contact.line.list$postcode.i>=9000 & contact.line.list$postcode.i<10000, "OV", contact.line.list$I.REGION)

contact.line.list$postcode.c = as.numeric(as.character(contact.line.list$C.POSTCODE))
contact.line.list[, C.REGION := NULL]
contact.line.list$C.REGION = if_else(contact.line.list$postcode.c>=2000 & contact.line.list$postcode.c<3000, "AW", contact.line.list$C.REGION)
contact.line.list$C.REGION = if_else(contact.line.list$postcode.c>=3500 & contact.line.list$postcode.c<4000, "LI", contact.line.list$C.REGION)
contact.line.list$C.REGION = if_else(contact.line.list$postcode.c>=1500 & contact.line.list$postcode.c<2000, "VB", contact.line.list$C.REGION)
contact.line.list$C.REGION = if_else(contact.line.list$postcode.c>=3000 & contact.line.list$postcode.c<3500, "VB", contact.line.list$C.REGION)
contact.line.list$C.REGION = if_else(contact.line.list$postcode.c>=1000 & contact.line.list$postcode.c<1300, "BXL", contact.line.list$C.REGION)
contact.line.list$C.REGION = if_else(contact.line.list$postcode.c>=8000 & contact.line.list$postcode.c<9000, "WV", contact.line.list$C.REGION)
contact.line.list$C.REGION = if_else(contact.line.list$postcode.c>=9000 & contact.line.list$postcode.c<10000, "OV", contact.line.list$C.REGION)

##
## Time of onset & positive test
minDate = "2020-03-08"
contact.line.list$C.DOO = as.integer(time_length(difftime(contact.line.list$C.SYMPTOMS, minDate),"days")+1)
contact.line.list$I.DOO = as.integer(time_length(difftime(contact.line.list$I.SYMPTOMS, minDate),"days")+1)
contact.line.list$C.time.test = as.integer(time_length(difftime(contact.line.list$C.TEST, minDate),"days")+1)
contact.line.list$I.time.test = as.integer(time_length(difftime(contact.line.list$I.TEST, minDate),"days")+1)

calldate = as.integer(time_length(difftime(contact.line.list$C.CALLDT, minDate),"days")+1) # contact calldate
contact.line.list$LAST_CONTACT_DT = ifelse(contact.line.list$LAST_CONTACT_DT>"2021-12-31" | contact.line.list$LAST_CONTACT_DT<minDate, NA, contact.line.list$LAST_CONTACT_DT)
contact.line.list$LAST_CONTACT_DT = as.Date(contact.line.list$LAST_CONTACT_DT, origin = "1970-01-01")
contact.line.list$last.contact = as.integer(time_length(difftime(contact.line.list$LAST_CONTACT_DT, minDate),"days")+1)
# contact.line.list$last.contact = ifelse(contact.line.list$last.contact<(contact.line.list$I.time.test-21), NA, contact.line.list$last.contact)
contact.line.list$last.contact = ifelse(contact.line.list$last.contact<(contact.line.list$I.time.test-14), NA, contact.line.list$last.contact)
contact.line.list$last.contact = ifelse(contact.line.list$last.contact>calldate, NA, contact.line.list$last.contact)

## add period
contact.line.list$period = NULL
P1 = as.integer(time_length(difftime("2020-09-01", minDate),"days")+1)
P2 = as.integer(time_length(difftime("2020-12-01", minDate),"days")+1)
P3 = as.integer(time_length(difftime("2021-03-01", minDate),"days")+1)
P4 = as.integer(time_length(difftime("2021-05-01", minDate),"days")+1)
P5 = as.integer(time_length(difftime("2021-07-01", minDate),"days")+1)
P6 = as.integer(time_length(difftime("2021-09-01", minDate),"days")+1)
P7 = as.integer(time_length(difftime("2021-11-01", minDate),"days")+1)

contact.line.list$period = if_else(contact.line.list$I.time.test<P1, 1, contact.line.list$period)
contact.line.list$period = if_else(contact.line.list$I.time.test>=P1 & contact.line.list$I.time.test<P2, 2, contact.line.list$period)
contact.line.list$period = if_else(contact.line.list$I.time.test>=P2 & contact.line.list$I.time.test<P3, 3, contact.line.list$period)
contact.line.list$period = if_else(contact.line.list$I.time.test>=P3 & contact.line.list$I.time.test<P4, 4, contact.line.list$period)
contact.line.list$period = if_else(contact.line.list$I.time.test>=P4 & contact.line.list$I.time.test<P5, 5, contact.line.list$period)
contact.line.list$period = if_else(contact.line.list$I.time.test>=P5 & contact.line.list$I.time.test<P6, 6, contact.line.list$period)
contact.line.list$period = if_else(contact.line.list$I.time.test>=P6 & contact.line.list$I.time.test<P7, 7, contact.line.list$period)
contact.line.list$period = if_else(contact.line.list$I.time.test>=P7, 8, contact.line.list$period)

############################
### UPDATED CONTACT LINELIST
save(contact.line.list, file="CONTACT_LINELISTv2_0609.RData")

## ADD NUMBER OF HH CONTACTS TO INDEX DATA
contact.line.list$HH.certain = ifelse(grepl('H', contact.line.list$CONTACT_TICKET_NBR), 1, 0)
table(contact.line.list$HH.certain, contact.line.list$type)

# all HH vs nonHH contacts
tdf <- contact.line.list[contact.line.list$HH.certain == 1,] %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.HH.contacts")
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.HH.contacts")]
tdf3$SCIENSANO_REQUEST_TICKET_NBR = tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf3 = tdf3[,c(2,3)]
test.data = left_join(INDEX.UNIQUE, tdf3, by = c("SCIENSANO_REQUEST_TICKET_NBR"))
test.data$n.HH.contacts = test.data$n.HH.contacts
test.data$n.HH.contacts = ifelse(is.na(test.data$n.HH.contacts), 0, test.data$n.HH.contacts)
INDEX.UNIQUE = test.data
rm(test.data)

tdf <- contact.line.list[contact.line.list$HH.certain == 0,] %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.nonHH.contacts")
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.nonHH.contacts")]
tdf3$SCIENSANO_REQUEST_TICKET_NBR = tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf3 = tdf3[,c(2,3)]
test.data = left_join(INDEX.UNIQUE, tdf3, by = c("SCIENSANO_REQUEST_TICKET_NBR"))
test.data$n.nonHH.contacts = test.data$n.nonHH.contacts
test.data$n.nonHH.contacts = ifelse(is.na(test.data$n.nonHH.contacts), 0, test.data$n.nonHH.contacts)
INDEX.UNIQUE = test.data
rm(test.data)

# positive HH vs nonHH contacts
tdf <- contact.line.list[contact.line.list$C.POS==1 & contact.line.list$HH.certain == 1,] %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.pos.HH.contacts")
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.pos.HH.contacts")]
tdf3$SCIENSANO_REQUEST_TICKET_NBR = tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf3 = tdf3[,c(2,3)]
test.data = left_join(INDEX.UNIQUE, tdf3, by = c("SCIENSANO_REQUEST_TICKET_NBR"))
test.data$n.pos.HH.contacts = test.data$n.pos.HH.contacts
test.data$n.pos.HH.contacts = ifelse(is.na(test.data$n.pos.HH.contacts), 0, test.data$n.pos.HH.contacts)
INDEX.UNIQUE = test.data
rm(test.data)

tdf <- contact.line.list[contact.line.list$C.POS==1 & contact.line.list$HH.certain == 0,] %>%
  add_count(INDEX_SCIENSANO_REQUEST_TICKET_NBR, name="n.pos.nonHH.contacts")
tdf2 <- tdf %>%
  distinct(INDEX_SCIENSANO_REQUEST_TICKET_NBR, .keep_all=T)
dim(tdf)
tdf3 <- tdf2[,c("INDEX_SCIENSANO_REQUEST_TICKET_NBR","n.pos.nonHH.contacts")]
tdf3$SCIENSANO_REQUEST_TICKET_NBR = tdf3$INDEX_SCIENSANO_REQUEST_TICKET_NBR
tdf3 = tdf3[,c(2,3)]
test.data = left_join(INDEX.UNIQUE, tdf3, by = c("SCIENSANO_REQUEST_TICKET_NBR"))
test.data$n.pos.nonHH.contacts = test.data$n.pos.nonHH.contacts
test.data$n.pos.nonHH.contacts = ifelse(is.na(test.data$n.pos.nonHH.contacts), 0, test.data$n.pos.nonHH.contacts)
INDEX.UNIQUE = test.data

rm(test.data)
rm(tdf, tdf2, tdf3)

save(INDEX.UNIQUE, file="INDEX_UNIQUE_2807.RData")

