
###################################################################################
######                                                                      #######
###### CONTROLENTOREN PROJECT UA/UH    --   REPORT                          #######
######                                                                      #######
###### AUTHORS: Cecile Kremer (UH) & Lander Willem (UA)                     #######
###### LAST UPDATE: 21/09/2023                                              #######
######                                                                      #######
###################################################################################


# dependencies
library(data.table) # data.table format
library(lubridate)  # date operations
library(plyr)
library(dplyr)

########################
##### DESCRIPTIVES #####
########################

### INDEX CASES

load("INDEX_UNIQUE_2807.RData")

INDEX.UNIQUE = INDEX.UNIQUE[INDEX.UNIQUE$TEST_RETURN_DT>="2020-09-01" & INDEX.UNIQUE$TEST_RETURN_DT<"2022-01-01",]
dim(INDEX.UNIQUE)
table(INDEX.UNIQUE$CALL_WRAPUP)
table(INDEX.UNIQUE$CALL_WRAPUP[INDEX.UNIQUE$SHARED_CONTACT_AMOUNT==0])

sum(INDEX.UNIQUE$n.contacts!=0)/dim(INDEX.UNIQUE)[1]

INDEX.UNIQUE$traced.type <- ifelse(INDEX.UNIQUE$HR.traced2==1, 2, 1) # 1 = LR, 2 = HR
INDEX.UNIQUE$traced.type <- ifelse(INDEX.UNIQUE$traced2==0, NA, INDEX.UNIQUE$traced.type)
table(INDEX.UNIQUE$traced.type)/sum(table(INDEX.UNIQUE$traced.type))

## add period
minDate = "2020-03-08"
INDEX.UNIQUE$period = NULL
INDEX.UNIQUE$period = if_else(INDEX.UNIQUE$TEST_RETURN_DT<"2020-09-01", 1, INDEX.UNIQUE$period)
INDEX.UNIQUE$period = if_else(INDEX.UNIQUE$TEST_RETURN_DT>="2020-09-01" & INDEX.UNIQUE$TEST_RETURN_DT<"2020-12-01", 2, INDEX.UNIQUE$period)
INDEX.UNIQUE$period = if_else(INDEX.UNIQUE$TEST_RETURN_DT>="2020-12-01" & INDEX.UNIQUE$TEST_RETURN_DT<"2021-03-01", 3, INDEX.UNIQUE$period)
INDEX.UNIQUE$period = if_else(INDEX.UNIQUE$TEST_RETURN_DT>="2021-03-01" & INDEX.UNIQUE$TEST_RETURN_DT<"2021-05-01", 4, INDEX.UNIQUE$period)
INDEX.UNIQUE$period = if_else(INDEX.UNIQUE$TEST_RETURN_DT>="2021-05-01" & INDEX.UNIQUE$TEST_RETURN_DT<"2021-07-01", 5, INDEX.UNIQUE$period)
INDEX.UNIQUE$period = if_else(INDEX.UNIQUE$TEST_RETURN_DT>="2021-07-01" & INDEX.UNIQUE$TEST_RETURN_DT<"2021-09-01", 6, INDEX.UNIQUE$period)
INDEX.UNIQUE$period = if_else(INDEX.UNIQUE$TEST_RETURN_DT>="2021-09-01" & INDEX.UNIQUE$TEST_RETURN_DT<"2021-11-01", 7, INDEX.UNIQUE$period)
INDEX.UNIQUE$period = if_else(INDEX.UNIQUE$TEST_RETURN_DT>="2021-11-01", 8, INDEX.UNIQUE$period)
table(INDEX.UNIQUE$period)

### CONTACTS

load("CONTACT_LINELISTv2_0609.RData")
contact.line.list = contact.line.list[contact.line.list$I.TEST>="2020-09-01" & contact.line.list$I.TEST<"2022-01-01",]
dim(contact.line.list)
table(contact.line.list$type)/sum(table(contact.line.list$type))
# only HRC
contact.line.list <- contact.line.list[contact.line.list$type == 2, ]

contact.line.list$C.POS3 = if_else(contact.line.list$time_between<(-14)|contact.line.list$time_between>20, 0, contact.line.list$C.POS)
contact.line.list$C.POS3[is.na(contact.line.list$C.POS3)] = 0
table(contact.line.list$C.POS3)/sum(table(contact.line.list$C.POS3))
table(contact.line.list$C.POS2)/sum(table(contact.line.list$C.POS2))
table(contact.line.list$C.POS)/sum(table(contact.line.list$C.POS))

################################
### DELAY DISTRIBUTIONS      ###
################################


## Index symptom onset to test collection
INDEX.UNIQUE$time.to.test <- as.numeric(INDEX.UNIQUE$TEST_COLLECTION_DT - INDEX.UNIQUE$SYMPTOMS_START_DT)
summary(INDEX.UNIQUE$time.to.test)
quantile(INDEX.UNIQUE$time.to.test, 0.99, na.rm = T)
sum(INDEX.UNIQUE$time.to.test < 0, na.rm = T)
sum(INDEX.UNIQUE$time.to.test < 0, na.rm = T)/sum(!is.na(INDEX.UNIQUE$time.to.test))*100
summary(INDEX.UNIQUE$time.to.test[INDEX.UNIQUE$time.to.test>0])


delay1 = aggregate(INDEX.UNIQUE$time.to.test, by = list(INDEX.UNIQUE$TEST_RETURN_DT), FUN = mean, na.rm = T)
dim(delay1)[1] == as.numeric(as.Date('2021-12-31') - as.Date('2020-09-01')) + 1
av7days1 = zoo::rollmean(delay1$x, k = 31)

## Index symptom onset to test result
INDEX.UNIQUE$time.to.result <- as.numeric(INDEX.UNIQUE$TEST_RETURN_DT - INDEX.UNIQUE$SYMPTOMS_START_DT)
summary(INDEX.UNIQUE$time.to.result)
quantile(INDEX.UNIQUE$time.to.result, 0.9, na.rm = T)
sum(INDEX.UNIQUE$time.to.result < 0, na.rm = T)
summary(INDEX.UNIQUE$time.to.result[INDEX.UNIQUE$time.to.result>0])


delay2 = aggregate(INDEX.UNIQUE$time.to.result, by = list(INDEX.UNIQUE$TEST_RETURN_DT), FUN = mean, na.rm = T)
dim(delay2)[1] == as.numeric(as.Date('2021-12-31') - as.Date('2020-09-01')) + 1
av7days2 = zoo::rollmean(delay2$x, k = 31)

## Positive test index to call CT
INDEX.UNIQUE$time.to.call <- as.numeric(INDEX.UNIQUE$CALL_DATETIME - INDEX.UNIQUE$TEST_RETURN_DT)

summary(INDEX.UNIQUE$time.to.call)
quantile(INDEX.UNIQUE$time.to.call, 0.9, na.rm = T)
sum(INDEX.UNIQUE$time.to.call < 0, na.rm = T)

delay3 = aggregate(INDEX.UNIQUE$time.to.call, by = list(INDEX.UNIQUE$TEST_RETURN_DT), FUN = mean, na.rm = T)
dim(delay3)[1] == as.numeric(as.Date('2021-12-31') - as.Date('2020-09-01')) + 1
av7days3 = zoo::rollmean(delay3$x, k = 31)

## Positive test index to call HRC
contact.line.list$time.to.trace <- as.numeric(contact.line.list$C.CALLDT - as.Date(contact.line.list$I.CALLDT2))
summary(contact.line.list$time.to.trace)
quantile(contact.line.list$time.to.trace, 0.9, na.rm = T)

delay4 = aggregate(contact.line.list$time_to_trace, by = list(contact.line.list$I.TEST), FUN = mean, na.rm = T)
dim(delay4)[1] == as.numeric(as.Date('2021-12-31') - as.Date('2020-09-01')) + 1
av7days4 = zoo::rollmean(delay4$x, k = 31)

## Plot 7-day moving average for delays
tiff("Fig2.tif", width = 19.05, height = 15, units = 'cm', res = 300, compression = 'lzw')
par(mar = c(5.1,5.1,4.1,2.1))
plot(seq(1,length(av7days1)), av7days1, type = "l", xlab = "Time", ylab = "Delay in days (monthly moving average)",
     cex.axis = 1.5, cex.lab = 1.5,
     xaxt = "n", lwd = 2,
     ylim = c(0,5))
lines(seq(1,length(av7days2)), av7days2, col = 2, lwd = 2)
lines(seq(1,length(av7days3)), av7days3, col = 3, lwd = 2)
# lines(seq(1,length(av7days4)), av7days4, col = 4, lwd = 2)
axis(side=1, at=c(1,30,61,91,122,153,181,212,242,273,303,334,365,395,426,456), 
     labels=c("1/9/20","1/10/20","1/11/20","1/12/20","1/1/21","1/2/21","1/3/21",
              "1/4/21",'1/5/21','1/6/21','1/7/21','1/8/21','1/9/21',
              '1/10/21','1/11/21','1/12/21'), cex.axis = 1.5)

legend('topright', bty = 'n', cex = 1.3,
       c('Index SO to test collection', 'Index SO to test result', 'Positive test index to call CT'),
       col = c(1,2,3), lty = c(1,1,1), lwd = c(2,2,2))
dev.off()

###################################
### REPORTED VS TRACED CONTACTS ###
###################################

# only for those with 'given' contacts
INDEX.UNIQUE.GIVEN <- INDEX.UNIQUE[INDEX.UNIQUE$GIVEN_HR_CONTACTS_AMOUNT > 0, ]
dim(INDEX.UNIQUE.GIVEN)
summary(INDEX.UNIQUE.GIVEN$GIVEN_HR_CONTACTS_AMOUNT)

INDEX.UNIQUE.GIVEN$prop.traced = NULL
INDEX.UNIQUE.GIVEN$prop.traced = ifelse(INDEX.UNIQUE.GIVEN$GIVEN_HR_CONTACTS_AMOUNT < INDEX.UNIQUE.GIVEN$n.HR.contacts,
                                        1,
                                        INDEX.UNIQUE.GIVEN$n.HR.contacts / INDEX.UNIQUE.GIVEN$GIVEN_HR_CONTACTS_AMOUNT)

summary(INDEX.UNIQUE.GIVEN$prop.traced)
sum(!is.na(INDEX.UNIQUE.GIVEN$prop.traced))

summary(INDEX.UNIQUE$n.HR.contacts[INDEX.UNIQUE$n.HR.contacts>0])

ptrace = aggregate(INDEX.UNIQUE.GIVEN$prop.traced, by = list(INDEX.UNIQUE.GIVEN$TEST_RETURN_DT), FUN = mean, na.rm = T)
dim(ptrace)[1] == as.numeric(as.Date('2021-12-31') - as.Date('2020-11-23')) + 1
av.ptrace = zoo::rollmean(ptrace$x, k = 31)
tiff("S2_Fig.tif", width = 19.05, height = 15, units = 'cm', res = 300, compression = 'lzw')
par(mar = c(6.1,5.1,4.1,2.1))
plot(seq(1,length(av.ptrace)), av.ptrace, type = "l", ann = F,
     cex.axis = 1.5, cex.lab = 1.5,
     xaxt = "n", lwd = 2,
     ylim = c(0,1))
mtext(side = 2, text = "Proportion of registered HRC effectively traced", line = 3.5, cex = 1.5)
mtext(side = 2, text = "(monthly moving average)", line = 2.5, cex = 1.5)
mtext(side = 1, text = 'Time', line = 3, cex = 1.5)
axis(side=1, at=c(39,98,159,220,282,343), labels=c("1/1/20",'1/3/21','1/5/21','1/7/21','1/9/21','1/11/21'), cex.axis = 1.5)
dev.off()

####################################
### KEY PERFORMANCE INDICATORS   ###
####################################

### Proportion of known index cases
summary(contact.line.list$I.TEST) # index cases until end of December 2021

table(INDEX.UNIQUE$traced2)/sum(table(INDEX.UNIQUE$traced2))
table(INDEX.UNIQUE$HR.traced2[INDEX.UNIQUE$traced2==1])/sum(table(INDEX.UNIQUE$HR.traced2[INDEX.UNIQUE$traced2==1]))

library(sciplot)

# by age group
library(Hmisc)
age_labels = c("0-17","18-29","30-39","40-49","50-64","65+")
INDEX.UNIQUE = INDEX.UNIQUE %>% 
  dplyr::mutate(AGEGR = cut2(AGE, c(18,30,40,50,65)),
                AGEGR = factor(AGEGR, labels=age_labels))
table(INDEX.UNIQUE$AGEGR)/sum(table(INDEX.UNIQUE$AGEGR))

tiff("S3_Fig.tif", width = 60, height = 20, units = 'cm', res = 300, compression = 'lzw')
par(mfrow=c(1,2))
par(mar = c(5.1,5.1,4.1,2.1))
lineplot.CI(data = INDEX.UNIQUE, response = traced2, x.factor = AGEGR, type="b",# group=AGEGR,
            xlab="Age group", ylab="Proportion 'known' index cases", err.width = 0,
            x.leg = 3.5, y.leg = 0.2, cex.axis=1.5, cex.lab=1.5, cex=1.5,
            # fun = function(x) mean(x, na.rm=TRUE),
            ci.fun= function(x) c(mean(x, na.rm=T)-1.96*se(x), mean(x, na.rm=T)+1.96*se(x))
)
lineplot.CI(data = INDEX.UNIQUE, response = traced2, x.factor = period, type="b", group=AGEGR,
            xlab="Period", ylab="", err.width = 0,
            x.leg = 3.5, y.leg = 0.17, cex.axis=1.5, cex.lab=1.5, cex=1.5, cex.leg=1.2,
            ci.fun= function(x) c(mean(x, na.rm=T)-1.96*se(x), mean(x, na.rm=T)+1.96*se(x)),
            xaxt = "n")
axis(side=1, at=c(1,2,3,4,5,6,7), labels=c("Sep-Nov","Dec-Feb","Mar-Apr","May-Jun","Jul-Aug","Sep-Oct","Nov-Dec"), cex.axis=1.5)
dev.off()

## SECONDARY ATTACK RATE
setkey(contact.line.list,"C.RRN") 

# Select random index case for each contact
sar.data <- contact.line.list %>%
  group_by(C.RRN) %>%
  slice_sample(n=1)
dim(sar.data)

length(unique(contact.line.list$I.RRN))
length(unique(contact.line.list$C.RRN))

binom.test(length(sar.data$C.RRN[sar.data$C.POS==1]),
           length(unique(sar.data$C.RRN)))
binom.test(length(sar.data$C.RRN[sar.data$C.POS==1 & sar.data$type==2]),
           length(unique(sar.data$C.RRN[sar.data$type==2])))

## HH contacts
binom.test(length(sar.data$C.RRN[sar.data$C.POS==1 & sar.data$HH.member==1]),
           length(unique(sar.data$C.RRN[sar.data$HH.member==1])))
binom.test(length(sar.data$C.RRN[sar.data$C.POS==1 & sar.data$HH.member==0 & sar.data$type==2]),
           length(unique(sar.data$C.RRN[sar.data$HH.member==0 & sar.data$type==2])))

## Known index
binom.test(length(sar.data$C.RRN[sar.data$C.POS==1 & sar.data$I.traced==1 & sar.data$type==2]),
           length(unique(sar.data$C.RRN[sar.data$I.traced==1 & sar.data$type==2])))

binom.test(length(sar.data$C.RRN[sar.data$C.POS==1 & sar.data$HH.member==1 & sar.data$I.traced==1]),
           length(unique(sar.data$C.RRN[sar.data$HH.member==1 & sar.data$I.traced==1])))

binom.test(length(sar.data$C.RRN[sar.data$C.POS==1 & sar.data$type==2 & sar.data$I.traced==1 & sar.data$HH.member==0]),
           length(unique(sar.data$C.RRN[sar.data$type==2 & sar.data$I.traced==1 & sar.data$HH.member==0])))

## New index
binom.test(length(sar.data$C.RRN[sar.data$C.POS==1 & sar.data$HH.member==1 & sar.data$I.traced==0]),
           length(unique(sar.data$C.RRN[sar.data$HH.member==1 & sar.data$I.traced==0])))

binom.test(length(sar.data$C.RRN[sar.data$C.POS==1 & sar.data$type==2 & sar.data$I.traced==0 & sar.data$HH.member==0]),
           length(unique(sar.data$C.RRN[sar.data$type==2 & sar.data$I.traced==0 & sar.data$HH.member==0])))

# ## Sensitivity analysis: remove duplicated contacts
# dup.rrn = contact.line.list$C.RRN[duplicated(contact.line.list$C.RRN)]
# sens.sar.data = sar.data[which(!(sar.data$C.RRN%in%dup.rrn)), ]
# length(unique(sens.sar.data$C.RRN))
# 
# binom.test(length(sens.sar.data$C.RRN[sens.sar.data$C.POS==1]),
#            length(unique(sens.sar.data$C.RRN)))
# binom.test(length(sens.sar.data$C.RRN[sens.sar.data$C.POS==1 & sens.sar.data$type==2]),
#            length(unique(sens.sar.data$C.RRN[sens.sar.data$type==2])))
# binom.test(length(sens.sar.data$C.RRN[sens.sar.data$C.POS==1 & sens.sar.data$type==1]),
#            length(unique(sens.sar.data$C.RRN[sens.sar.data$type==1])))
# ## HH contacts
# binom.test(length(sens.sar.data$C.RRN[sens.sar.data$C.POS==1 & sens.sar.data$HH.member==1]),
#            length(unique(sens.sar.data$C.RRN[sens.sar.data$HH.member==1])))

##### FULL PLOT
tiff("Fig1.tif", width = 45, height = 30, units = 'cm', res = 300, compression = 'lzw')
par(mfrow=c(2,2))
par(mar = c(5.1,5.1,4.1,2.1))
# prop known
lineplot.CI(data = INDEX.UNIQUE, response = traced2, x.factor = period, type="b",
            xlab="", ylab="Proportion 'known' index cases", main = 'a', err.width = 0,
            x.leg = 3.5, xaxt = "n", cex=1.5, cex.axis=1.5, cex.lab=1.5,
            ci.fun= function(x) c(mean(x, na.rm=T)-1.96*se(x), mean(x, na.rm=T)+1.96*se(x))
)
axis(side=1, at=c(1,2,3,4,5,6,7), labels=c("Sep-Nov","Dec-Feb","Mar-Apr","May-Jun","Jul-Aug","Sep-Oct","Nov-Dec"), cex.axis = 1.5)
# traced HRC
lineplot.CI(data=INDEX.UNIQUE, response=n.HR.contacts, x.factor=period, type="b", group=traced2,
            xlab="",ylab="Average number of traced HRC", main = 'b', err.width = 0,
            leg.lab = c("'New' index","'Known' index"),
            x.leg=1, y.leg = 1.4, xaxt = "n",
            cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.leg=1.2,
            ci.fun= function(x) c(mean(x, na.rm=T)-1.96*se(x), mean(x, na.rm=T)+1.96*se(x))
)
axis(side=1, at=c(1,2,3,4,5,6,7), labels=c("Sep-Nov","Dec-Feb","Mar-Apr","May-Jun","Jul-Aug","Sep-Oct","Nov-Dec"), cex.axis = 1.5)
# secondary cases
lineplot.CI(data=INDEX.UNIQUE, response=n.pos.HR.contacts, x.factor=period, type="b", group=traced2,
            xlab="Period",ylab="Average number of secondary cases", main = 'c', err.width = 0,
            legend = F,
            xaxt = "n",
            cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.leg=1.2,
            ci.fun= function(x) c(mean(x, na.rm=T)-1.96*se(x), mean(x, na.rm=T)+1.96*se(x))
)#, pch=16, las=2)
axis(side=1, at=c(1,2,3,4,5,6,7), labels=c("Sep-Nov","Dec-Feb","Mar-Apr","May-Jun","Jul-Aug","Sep-Oct","Nov-Dec"), cex.axis = 1.5)
# SAR
lineplot.CI(data=sar.data[sar.data$type==2 & sar.data$period<9,], response=C.POS, x.factor=period, type="b",group=I.traced, 
            xlab="Period",ylab="SAR among traced HRC", main = 'd', err.width = 0, 
            xaxt = "n", legend = F,
            cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.leg=1.2,
            ci.fun= function(x) c(mean(x, na.rm=T)-1.96*se(x), mean(x, na.rm=T)+1.96*se(x))
)
axis(side=1, at=c(1,2,3,4,5,6,7), labels=c("Sep-Nov","Dec-Feb","Mar-Apr","May-Jun","Jul-Aug","Sep-Oct","Nov-Dec"), cex.axis = 1.5)
dev.off()

##################################
#### BASELINE CHARACTERISTICS ####
##################################

INDEX.UNIQUE$postcode.c = as.numeric(as.character(INDEX.UNIQUE$POSTAL_CD))
INDEX.UNIQUE$REGION = "UNK"
INDEX.UNIQUE$REGION = if_else(INDEX.UNIQUE$postcode.c>=2000 & INDEX.UNIQUE$postcode.c<3000, "AW", INDEX.UNIQUE$REGION)
INDEX.UNIQUE$REGION = if_else(INDEX.UNIQUE$postcode.c>=3500 & INDEX.UNIQUE$postcode.c<4000, "LI", INDEX.UNIQUE$REGION)
INDEX.UNIQUE$REGION = if_else(INDEX.UNIQUE$postcode.c>=1500 & INDEX.UNIQUE$postcode.c<2000, "VB", INDEX.UNIQUE$REGION)
INDEX.UNIQUE$REGION = if_else(INDEX.UNIQUE$postcode.c>=3000 & INDEX.UNIQUE$postcode.c<3500, "VB", INDEX.UNIQUE$REGION)
INDEX.UNIQUE$REGION = if_else(INDEX.UNIQUE$postcode.c>=8000 & INDEX.UNIQUE$postcode.c<9000, "WV", INDEX.UNIQUE$REGION)
INDEX.UNIQUE$REGION = if_else(INDEX.UNIQUE$postcode.c>=9000 & INDEX.UNIQUE$postcode.c<10000, "OV", INDEX.UNIQUE$REGION)
INDEX.UNIQUE$REGION = if_else(INDEX.UNIQUE$postcode.c>=1000 & INDEX.UNIQUE$postcode.c<2000, "BXL", INDEX.UNIQUE$REGION)
INDEX.UNIQUE$REGION = if_else(is.na(INDEX.UNIQUE$REGION), "UNK", INDEX.UNIQUE$REGION)
table(INDEX.UNIQUE$REGION)/sum(table(INDEX.UNIQUE$REGION))*100
sum(is.na(INDEX.UNIQUE$REGION)); sum(!is.na(INDEX.UNIQUE$REGION))

table(INDEX.UNIQUE$AGEGR)
INDEX.UNIQUE$AGEGR = ifelse(is.na(INDEX.UNIQUE$AGEGR), "UNK", INDEX.UNIQUE$AGEGR)
sum(is.na(INDEX.UNIQUE$AGEGR))

round(table(INDEX.UNIQUE$REGION, INDEX.UNIQUE$AGEGR)/sum(table(INDEX.UNIQUE$REGION, INDEX.UNIQUE$AGEGR)), 3)

###########################
#### QUANTITATIVE KPIs ####
###########################

table(INDEX.UNIQUE$period, INDEX.UNIQUE$traced2)

p1 <- lineplot.CI(data = INDEX.UNIQUE, response = traced2, x.factor = period, type="b",
                  xlab="", ylab="Proportion 'known' index cases", main = 'a', err.width = 0,
                  x.leg = 3.5, xaxt = "n", cex=1.5, cex.axis=1.5, cex.lab=1.5,
                  ci.fun= function(x) c(mean(x, na.rm=T)-1.96*se(x), mean(x, na.rm=T)+1.96*se(x)))
p1$vals; p1$CI

p2 <- lineplot.CI(data=INDEX.UNIQUE[INDEX.UNIQUE$traced2==0], response=n.HR.contacts, x.factor=period, type="b", #group=traced2,
                  xlab="",ylab="Average number of traced HRC", main = 'b', err.width = 0,
                  cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.leg=1.2,
                  ci.fun= function(x) c(mean(x, na.rm=T)-1.96*se(x), mean(x, na.rm=T)+1.96*se(x)))
p2$vals; p2$CI

p3 <- lineplot.CI(data=INDEX.UNIQUE[INDEX.UNIQUE$traced2==0], response=n.pos.HR.contacts, x.factor=period, type="b", #group=traced2,
                  xlab="Period",ylab="Average number of secondary cases", main = 'c', err.width = 0,
                  legend = F,
                  xaxt = "n",
                  cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.leg=1.2,
                  ci.fun= function(x) c(mean(x, na.rm=T)-1.96*se(x), mean(x, na.rm=T)+1.96*se(x)))
p3$vals; p3$CI

p4 <- lineplot.CI(data=sar.data[sar.data$type==2 & sar.data$period<9 & sar.data$I.traced==0,], response=C.POS, x.factor=period, type="b", #group=I.traced, 
                  xlab="Period",ylab="SAR among traced HRC", main = 'd', err.width = 0, 
                  xaxt = "n", legend = F,
                  cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.leg=1.2,
                  ci.fun= function(x) c(mean(x, na.rm=T)-1.96*se(x), mean(x, na.rm=T)+1.96*se(x)))
p4$vals; p4$CI

## overall
summary(INDEX.UNIQUE$n.HR.contacts[INDEX.UNIQUE$traced2==1])
summary(INDEX.UNIQUE$n.HR.contacts[INDEX.UNIQUE$traced2==0])

summary(INDEX.UNIQUE$n.pos.HR.contacts[INDEX.UNIQUE$traced2==1])
summary(INDEX.UNIQUE$n.pos.HR.contacts[INDEX.UNIQUE$traced2==0])

summary(sar.data$C.POS[sar.data$type==2 & sar.data$period>2 & sar.data$period<7 & sar.data$I.traced==1])
summary(sar.data$C.POS[sar.data$type==2 & sar.data$period>2 & sar.data$period<7 & sar.data$I.traced==0])

#################################
#### ZOOM IN OP SEP-NOV 2020 ####
#################################

INDEXES = INDEX.UNIQUE[INDEX.UNIQUE$TEST_RETURN_DT<"2020-12-01",]
table(INDEXES$traced2)/sum(table(INDEXES$traced2))

## 2-week time periods
INDEXES$period2 = NULL
INDEXES$period2 = if_else(INDEXES$TEST_RETURN_DT<as.Date("2020-09-01")+14, 1, INDEXES$period2)
INDEXES$period2 = if_else(INDEXES$TEST_RETURN_DT>=as.Date("2020-09-01")+14 & INDEXES$TEST_RETURN_DT<as.Date("2020-09-01")+28, 2, INDEXES$period2)
INDEXES$period2 = if_else(INDEXES$TEST_RETURN_DT>=as.Date("2020-09-01")+28 & INDEXES$TEST_RETURN_DT<as.Date("2020-09-01")+42, 3, INDEXES$period2)
INDEXES$period2 = if_else(INDEXES$TEST_RETURN_DT>=as.Date("2020-09-01")+42 & INDEXES$TEST_RETURN_DT<as.Date("2020-09-01")+56, 4, INDEXES$period2)
INDEXES$period2 = if_else(INDEXES$TEST_RETURN_DT>=as.Date("2020-09-01")+56 & INDEXES$TEST_RETURN_DT<as.Date("2020-09-01")+70, 5, INDEXES$period2)
INDEXES$period2 = if_else(INDEXES$TEST_RETURN_DT>=as.Date("2020-09-01")+70 & INDEXES$TEST_RETURN_DT<as.Date("2020-09-01")+84, 6, INDEXES$period2)
INDEXES$period2 = if_else(INDEXES$TEST_RETURN_DT>=as.Date("2020-09-01")+84, 7, INDEXES$period2)
table(INDEXES$period2)

## Aantal index cases al gekend
table(INDEXES$traced2)/sum(table(INDEXES$traced2))
table(INDEXES$HR.traced2[INDEXES$traced2==1])/sum(table(INDEXES$HR.traced2[INDEXES$traced2==1]))

tiff("S5_Fig.tif", width = 40, height = 30, units = 'cm', res = 300, compression = 'lzw')
par(mfrow = c(2,2))
par(mar = c(5.1,5.1,4.1,2.1))

## Plot 7-day moving average 
daily.data.traced = aggregate(INDEXES$traced2, by = list(INDEXES$TEST_RETURN_DT), FUN = sum)
av7days = zoo::rollsum(daily.data.traced$x, k = 7)
daily.data.all = as.data.frame(table(INDEXES$TEST_RETURN_DT))
av7daysall = zoo::rollsum(daily.data.all$Freq, k = 7)

plot(seq(1,length(av7days)), av7days/av7daysall, type = "l", xlab = "Time", ylab = "Proportion 'known' index cases",main='a',
     cex.axis = 1.5, cex.lab = 1.5,
     xaxt = "n", lwd = 2)
axis(side=1, at=c(1,27,57,85), labels=c("4/9/20","1/10/20", "31/10/20","27/11/20"), cex.axis = 1.5)

# 7-daags gemiddelde
daily.data.traced = aggregate(INDEXES$n.HR.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = mean)
av7days = zoo::rollmean(daily.data.traced$x, k = 7)
daily.data.other = aggregate(INDEXES$n.HR.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = mean)
av7daysall = zoo::rollmean(daily.data.other$x, k = 7)
plot(seq(1,length(av7days)), av7days, type = "l", xlab = "Time", ylab = "Average number of traced HRC",main='b',
     cex.axis = 1.5, cex.lab = 1.5, ylim = c(0,1.5),
     xaxt = "n", lwd = 2)
lines(seq(1, length(av7daysall)), av7daysall, lty = 2, lwd = 2)
legend("topright", c("New index","Known index"), col = c(1,1), lwd = c(2,2), lty = c(2,1), bty = "n")
axis(side=1, at=c(1,27,57,85), labels=c("4/9/20","1/10/20", "31/10/20","27/11/20"), cex.axis = 1.5)

daily.data.traced = aggregate(INDEXES$n.pos.HR.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = mean)
av7days = zoo::rollmean(daily.data.traced$x, k = 7)
daily.data.other = aggregate(INDEXES$n.pos.HR.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = mean)
av7daysall = zoo::rollmean(daily.data.other$x, k = 7)
plot(seq(1,length(av7days)), av7days, type = "l", xlab = "Time", ylab = "Average number of secondary cases",main='c',
     cex.axis = 1.5, cex.lab = 1.5, ylim = c(0,0.3),
     xaxt = "n", lwd = 2)
lines(seq(1, length(av7daysall)), av7daysall, lty = 2, lwd = 2)
legend("topright", c("New index","Known index"), col = c(1,1), lwd = c(2,2), lty = c(2,1), bty = "n")
axis(side=1, at=c(1,27,57,85), labels=c("4/9/20","1/10/20", "31/10/20","27/11/20"), cex.axis = 1.5)

sar.data2 = sar.data[which(sar.data$I.RRN%in%INDEXES$NATIONAL_INSURANCE_NBR),]
dim(sar.data2)

## 2-week time periods
sar.data2$period2 = NULL
sar.data2$period2 = if_else(sar.data2$I.TEST<as.Date("2020-09-01")+14, 1, sar.data2$period2)
sar.data2$period2 = if_else(sar.data2$I.TEST>=as.Date("2020-09-01")+14 & sar.data2$I.TEST<as.Date("2020-09-01")+28, 2, sar.data2$period2)
sar.data2$period2 = if_else(sar.data2$I.TEST>=as.Date("2020-09-01")+28 & sar.data2$I.TEST<as.Date("2020-09-01")+42, 3, sar.data2$period2)
sar.data2$period2 = if_else(sar.data2$I.TEST>=as.Date("2020-09-01")+42 & sar.data2$I.TEST<as.Date("2020-09-01")+56, 4, sar.data2$period2)
sar.data2$period2 = if_else(sar.data2$I.TEST>=as.Date("2020-09-01")+56 & sar.data2$I.TEST<as.Date("2020-09-01")+70, 5, sar.data2$period2)
sar.data2$period2 = if_else(sar.data2$I.TEST>=as.Date("2020-09-01")+70 & sar.data2$I.TEST<as.Date("2020-09-01")+84, 6, sar.data2$period2)
sar.data2$period2 = if_else(sar.data2$I.TEST>=as.Date("2020-09-01")+84, 7, sar.data2$period2)

# 7-daags gemiddelde
daily.data.traced = aggregate(INDEXES$n.pos.HR.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = sum)
av7days = zoo::rollsum(daily.data.traced$x, k = 7)
daily.data.all = aggregate(INDEXES$n.HR.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = sum)
av7daysall = zoo::rollsum(daily.data.all$x, k = 7)
av7days/av7daysall
plot(seq(1,length(av7days)), av7days/av7daysall, type = "l", xlab = "Time", ylab = "SAR among traced HRC",main='d',
     cex.axis = 1.5, cex.lab = 1.5, ylim = c(0,0.35),
     xaxt = "n", lwd = 2)

daily.data.traced = aggregate(INDEXES$n.pos.HR.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = sum)
av7days = zoo::rollsum(daily.data.traced$x, k = 7)
daily.data.all = aggregate(INDEXES$n.HR.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = sum)
av7daysall = zoo::rollsum(daily.data.all$x, k = 7)
av7days/av7daysall

lines(seq(1,length(av7days)), av7days/av7daysall, lwd = 2, lty = 2)
legend("topleft", c("New index","Known index"), col = c(1,1), lwd = c(2,2), lty = c(2,1), bty = "n")
axis(side=1, at=c(1,27,57,85), labels=c("4/9/20","1/10/20", "31/10/20","27/11/20"), cex.axis = 1.5)

dev.off()

######################################
#### ZOOM IN OP JUN-AUG 2021 (HH) ####

INDEXES = INDEX.UNIQUE[INDEX.UNIQUE$TEST_RETURN_DT>="2021-06-01" & INDEX.UNIQUE$TEST_RETURN_DT<"2021-12-31",]
dim(INDEXES)
CONTACTS = contact.line.list[contact.line.list$I.TEST>='2021-06-01' & contact.line.list$I.TEST<'2021-12-31',]
dim(CONTACTS)

# 7-daags gemiddelde
tiff("S4_Fig.tif", width = 40, height = 30, units = 'cm', res = 300, compression = 'lzw')

daily.HH.other = aggregate(INDEXES$n.HH.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = mean)
av7daysHH = zoo::rollmean(daily.HH.other$x, k = 7)
daily.HH.known = aggregate(INDEXES$n.HH.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = mean)
av7daysHH2 = zoo::rollmean(daily.HH.known$x, k = 7)
daily.nonHH.other = aggregate(INDEXES$n.nonHH.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = mean)
av7daysnonHH = zoo::rollmean(daily.nonHH.other$x, k = 7)
daily.nonHH.known = aggregate(INDEXES$n.nonHH.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = mean)
av7daysnonHH2 = zoo::rollmean(daily.nonHH.known$x, k = 7)

par(mfrow=c(2,2))
par(mar = c(5.1,5.1,4.1,2.1))

plot(seq(1,length(av7daysHH)), av7daysHH, type = "l", xlab = "Time", ylab = "Average number of traced HRC",
     cex.axis = 1.5, cex.lab = 1.5, ylim = c(0,2), main='a',
     xaxt = "n", lwd = 2, lty = 2, col = 2)
lines(seq(1, length(av7daysHH2)), av7daysHH2, lty = 1, lwd = 2, col = 2)
lines(seq(1, length(av7daysnonHH)), av7daysnonHH, lty = 2, lwd = 2, col = 4)
lines(seq(1, length(av7daysnonHH2)), av7daysnonHH2, lty = 1, lwd = 2, col = 4)

legend("topright", c("New index","Known index","Household","Non-household"), 
       col = c(1,1,2,4), lwd = c(2,2,2,2), lty = c(2,1,NA,NA), pch=c(NA,NA,20,20), bty = "n")
# min(daily.data.traced$Group.1) + 3
axis(side=1, at=c(1,92,153), labels=c("1/6/21","1/9/21",'1/11/21'), cex.axis = 1.5)

daily.HH.traced = aggregate(INDEXES$n.pos.HH.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = mean)
av7daysHH = zoo::rollmean(daily.HH.traced$x, k = 7)
daily.HH.other = aggregate(INDEXES$n.pos.HH.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = mean)
av7daysHH2 = zoo::rollmean(daily.HH.other$x, k = 7)

daily.nonHH.traced = aggregate(INDEXES$n.pos.nonHH.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = mean)
av7daysnonHH = zoo::rollmean(daily.nonHH.traced$x, k = 7)
daily.nonHH.other = aggregate(INDEXES$n.pos.nonHH.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = mean)
av7daysnonHH2 = zoo::rollmean(daily.nonHH.other$x, k = 7)

plot(seq(1,length(av7daysHH)), av7daysHH, type = "l", xlab = "Time", ylab = "Average number of secondary cases",
     cex.axis = 1.5, cex.lab = 1.5, ylim = c(0,0.4), main = 'b',
     xaxt = "n", lwd = 2, lty = 1, col = 2)
lines(seq(1, length(av7daysHH2)), av7daysHH2, lty = 2, lwd = 2, col = 2)
lines(seq(1, length(av7daysnonHH)), av7daysnonHH, lty = 1, lwd = 2, col = 4)
lines(seq(1, length(av7daysnonHH2)), av7daysnonHH2, lty = 2, lwd = 2, col = 4)
axis(side=1, at=c(1,92,153), labels=c("1/6/21","1/9/21",'1/11/21'), cex.axis = 1.5)

### SAR

daily.data.other = aggregate(INDEXES$n.pos.HH.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = sum)
daily.data.all = aggregate(INDEXES$n.HH.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = sum)
av7days.new = zoo::rollmean(daily.data.other$x/daily.data.all$x, k = 7)

plot(seq(1,length(av7days.new)), av7days.new, type = "l", xlab = "Time", ylab = "Secondary attack rate",
     cex.axis = 1.5, cex.lab = 1.5, ylim = c(0.05,0.5), main = 'c',
     xaxt = "n", lwd = 2, lty = 2, col = 2)
axis(side=1, at=c(1,92,153), labels=c("1/6/21","1/9/21",'1/11/21'), cex.axis = 1.5)

daily.data.other = aggregate(INDEXES$n.pos.HH.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = sum)
daily.data.all = aggregate(INDEXES$n.HH.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = sum)
av7days.known = zoo::rollmean(daily.data.other$x/daily.data.all$x, k = 7)
lines(seq(1,length(av7days.known)), av7days.known, lwd = 2, lty = 1, col = 2)

daily.data.other = aggregate(INDEXES$n.pos.nonHH.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = sum)
daily.data.all = aggregate(INDEXES$n.nonHH.contacts[INDEXES$traced2==1], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==1]), FUN = sum)
av7days.known = zoo::rollmean(daily.data.other$x/daily.data.all$x, k = 7)
lines(seq(1,length(av7days.known)), av7days.known, lwd = 2, lty = 1, col = 4)

daily.data.other = aggregate(INDEXES$n.pos.nonHH.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = sum)
daily.data.all = aggregate(INDEXES$n.nonHH.contacts[INDEXES$traced2==0], by = list(INDEXES$TEST_RETURN_DT[INDEXES$traced2==0]), FUN = sum)
av7days.new = zoo::rollmean(daily.data.other$x/daily.data.all$x, k = 7)
lines(seq(1,length(av7days.new)), av7days.new, lwd = 2, lty = 2, col = 4)

dev.off()

########################################
##### HOUSEHOLD STATUS             #####
########################################


data.HH <- INDEX.UNIQUE[,c(100,88,95,97)]
data.HH$type <- 'Household'
names(data.HH) <- c('Period','Traced','n.contacts','n.pos.contacts','type')
data.nonHH <- INDEX.UNIQUE[,c(100,88,96,98)]
data.nonHH$type <- 'Non-household'
names(data.nonHH) <- c('Period','Traced','n.contacts','n.pos.contacts','type')
data.HR <- INDEX.UNIQUE[,c(100,88,90,94)]
data.HR$type <- 'High-risk (all)'
names(data.HR) <- c('Period','Traced','n.contacts','n.pos.contacts','type')
data.plot <- rbind(data.HH, data.nonHH, data.HR)
data.plot$attack.rate <- data.plot$n.pos.contacts/data.plot$n.contacts

lineplot.CI(data=data.plot[data.plot$type=='Household' & data.plot$n.contacts>0], response=n.contacts, x.factor=Period, type="b", group=Traced,
            xlab="Period",ylab="Mean number of contacts", err.width = 0,
            leg.lab = c("Nieuwe index","Gekende index"),
            # x.leg=1, y.leg = 1.4, xaxt = "n",
            cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.leg=1.2)#, pch=16, las=2)


#####
## Secondary cases
jpeg(filename = "known_nsec.jpg", width = 20, height = 15, res = 300, unit = "cm")
par(mfrow=c(1,1))
lineplot.CI(data=INDEX.UNIQUE, response=n.pos.contacts, x.factor=period, type="b", group=traced2,
            xlab="Tijd",ylab="Gem. aantal secundaire gevallen", err.width = 0,
            leg.lab = c("Nieuwe index","Gekende index"),
            x.leg=3.5, y.leg = 0.25,
            xaxt = "n",
            cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.leg=1.2)#, pch=16, las=2)
axis(side=1, at=c(1,2,3,4,5), labels=c("Sep-Nov","Dec-Feb","Mar-Apr","Mei-Jun","Jul-Aug"), cex.axis = 1.5)
dev.off()

## SAR
jpeg(filename = "knownSAR.jpg", width = 20, height = 15, res = 300, unit = "cm")
lineplot.CI(data=sar.data[sar.data$type==2 & sar.data$period<7,], response=C.POS, x.factor=period, type="b",group=I.traced, 
            xlab="Tijd",ylab="SAR onder hoogrisico contacten", err.width = 0, 
            leg.lab=c("Nieuwe index","Gekende index"), x.leg = 2, y.leg = 0.2,
            # pch=16, 
            xaxt = "n",
            cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.leg=1.2)
axis(side=1, at=c(1,2,3,4,5), labels=c("Sep-Nov","Dec-Feb","Mar-Apr","Mei-Jun","Jul-Aug"), cex.axis = 1.5)
dev.off()

###############################
#### CONTACT MATRICES      ####
###############################

library(multcomp)
library(CIplot)
library(ggeffects)
library(ggplot2)
library(Hmisc)

data = contact.line.list

## 10-year age groups
age_labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
data = data %>% 
  dplyr::mutate(I.AGEGR2 = cut2(I.AGE, c(10,20,30,40,50,60,70,80,90)),
                I.AGEGR2 = factor(I.AGEGR2, labels=age_labels))
data = data %>% 
  dplyr::mutate(C.AGEGR2 = cut2(C.AGE, c(10,20,30,40,50,60,70,80,90)),
                C.AGEGR2 = factor(C.AGEGR2, labels=age_labels))

###########---------------######################
### Montly contact matrices


# jpeg(filename = "Fig4.jpg", width = 24, height = 40, res = 300, unit = "cm")
tiff("Fig3.tif", width = 24, height = 40, units = 'cm', res = 300, compression = 'lzw')

data = data[!is.na(data$I.AGEGR2) & !is.na(data$C.AGEGR2)]
data = data[data$I.TEST < '2021-09-01',]

par(mfcol=c(4,2))

for(i in c(12,4,7)){ # numeric month
  
  data.contact.matrix2 = data[month(data$I.TEST) == i, c("I.AGEGR2","C.AGEGR2")]
  dim(data.contact.matrix2)
  # remove pairs with missing age
  data.contact.events = data.contact.matrix2 %>% dplyr::filter(!is.na(C.AGEGR2) | !is.na(I.AGEGR2))
  dim(data.contact.events)
  
  age_pairs_contact <- data.frame(xtabs(~I.AGEGR2 + C.AGEGR2, data.contact.events))

  contact.mat = matrix(age_pairs_contact$Freq, ncol=10, nrow=10) # row = index, col = contact
  rownames(contact.mat) = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
  colnames(contact.mat) = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
  contact.mat = apply(t(contact.mat),2,rev) # row = contact, col = index
  
  tmp = data %>%
    distinct(I.RRN, .keep_all = T)
  dim(tmp)
  N.agegroup = (plyr::count(tmp$I.AGEGR2[month(tmp$I.TEST) == i & !is.na(tmp$C.AGEGR2)]))
  N.agegroup = N.agegroup[!is.na(N.agegroup$x),]
  N.mat = matrix(rep(N.agegroup$freq,10), nrow=10, ncol=10, byrow=T)

  mean.contact.mat = contact.mat / N.mat
  
  year = ifelse(i>8, "20", "21")
  name = paste0(month.abb[i]," ", year)
  
  library(plot.matrix)
  library(RColorBrewer)
  par(mar=c(5.1, 5.1, 4.1, 4.1))
  plot(mean.contact.mat, xlab="Index age", ylab="Contact age", main=name, col=colorRampPalette(brewer.pal(5, "GnBu")),
       fmt.key="%.0f", border=NA, 
       # asp=T, 
       cex.axis=0.7, cex.lab=1.5, 
       axis.col=list(side=1, las=2),
       axis.row=list(side=2, las=2),
       fmt.cell="%.2f", text.cell=list(cex=0.7), key=NULL,#  key=list(tick=F, at=c(0,2,4,6), labels=c(0,2,4,6)), 
       breaks=seq(0,1.35,0.001))
  
}

data.omicron = contact.line.list[contact.line.list$I.TEST > '2021-09-01',]
## 10-year age groups
age_labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
data.omicron = data.omicron %>% 
  dplyr::mutate(I.AGEGR2 = cut2(I.AGE, c(10,20,30,40,50,60,70,80,90)),
                I.AGEGR2 = factor(I.AGEGR2, labels=age_labels))
data.omicron = data.omicron %>% 
  dplyr::mutate(C.AGEGR2 = cut2(C.AGE, c(10,20,30,40,50,60,70,80,90)),
                C.AGEGR2 = factor(C.AGEGR2, labels=age_labels))
data.omicron = data.omicron[!is.na(data.omicron$I.AGEGR2) & !is.na(data.omicron$C.AGEGR2)]

data.contact.matrix2 = data.omicron[month(data.omicron$I.TEST) == 12, c("I.AGEGR2","C.AGEGR2")]
dim(data.contact.matrix2)
# remove pairs with missing age
data.contact.events = data.contact.matrix2 %>% dplyr::filter(!is.na(C.AGEGR2) | !is.na(I.AGEGR2))
dim(data.contact.events)

age_pairs_contact <- data.frame(xtabs(~I.AGEGR2 + C.AGEGR2, data.contact.events))

contact.mat = matrix(age_pairs_contact$Freq, ncol=10, nrow=10) # row = index, col = contact
rownames(contact.mat) = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
colnames(contact.mat) = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
contact.mat = apply(t(contact.mat),2,rev) # row = contact, col = index

tmp = data.omicron %>%
  distinct(I.RRN, .keep_all = T)
dim(tmp)
N.agegroup = (plyr::count(tmp$I.AGEGR2[month(tmp$I.TEST) == 12 & !is.na(tmp$C.AGEGR2)]))
N.agegroup = N.agegroup[!is.na(N.agegroup$x),]
N.mat = matrix(rep(N.agegroup$freq,10), nrow=10, ncol=10, byrow=T)

mean.contact.mat = contact.mat / N.mat

year = "21"
name = paste0(month.abb[12]," ", year)

library(plot.matrix)
library(RColorBrewer)
par(mar=c(5.1, 5.1, 4.1, 4.1))
plot(mean.contact.mat, xlab="Index age", ylab="Contact age", main=name, col=colorRampPalette(brewer.pal(5, "GnBu")),
     fmt.key="%.0f", border=NA, 
     # asp=T, 
     cex.axis=0.7, cex.lab=1.5, 
     axis.col=list(side=1, las=2),
     axis.row=list(side=2, las=2),
     fmt.cell="%.2f", text.cell=list(cex=0.7), key=NULL,#  key=list(tick=F, at=c(0,2,4,6), labels=c(0,2,4,6)), 
     breaks=seq(0,1.35,0.001))

###########---------------######################
### Montly transmission matrices

for(i in c(12,4,7)){ # numeric month
  
  data.contact.matrix2 = data[month(data$I.TEST) == i & data$C.POS == 1, c("I.AGEGR2","C.AGEGR2")]
  dim(data.contact.matrix2)
  # remove pairs with missing age
  data.contact.events = data.contact.matrix2 %>% dplyr::filter(!is.na(C.AGEGR2) | !is.na(I.AGEGR2))
  dim(data.contact.events)
  
  age_pairs_contact <- data.frame(xtabs(~I.AGEGR2 + C.AGEGR2, data.contact.events))

  contact.mat = matrix(age_pairs_contact$Freq, ncol=10, nrow=10) # row = index, col = contact
  rownames(contact.mat) = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
  colnames(contact.mat) = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
  contact.mat = apply(t(contact.mat),2,rev) # row = contact, col = index
  
  N.agegroup = apply(contact.mat,1,sum) # total in INDEX age groups
  N.mat = matrix(rep(N.agegroup,10), nrow=10, ncol=10)#, byrow=T)
  
  mean.contact.mat = contact.mat / N.mat
  
  year = ifelse(i>8, "20", "21")
  name = paste0(month.abb[i]," ", year)
  
  library(plot.matrix)
  library(RColorBrewer)
  par(mar=c(5.1, 5.1, 4.1, 4.1))
  plot(mean.contact.mat, xlab="Index age", ylab="Contact age", main=name, col=colorRampPalette(brewer.pal(5, "GnBu")),
       fmt.key="%.0f", border=NA, 
       # asp=T, 
       cex.axis=0.7, cex.lab=1.5, 
       axis.col=list(side=1, las=2), axis.row=list(side=2, las=2),
       fmt.cell="%.2f", text.cell=list(cex=0.7), key=NULL,#  key=list(tick=F, at=c(0,2,4,6), labels=c(0,2,4,6)), 
       breaks=seq(0,0.7,0.001))
  
}

data.contact.matrix2 = data.omicron[month(data.omicron$I.TEST) == 12 & data.omicron$C.POS == 1, c("I.AGEGR2","C.AGEGR2")]
dim(data.contact.matrix2)
# remove pairs with missing age
data.contact.events = data.contact.matrix2 %>% dplyr::filter(!is.na(C.AGEGR2) | !is.na(I.AGEGR2))
dim(data.contact.events)

age_pairs_contact <- data.frame(xtabs(~I.AGEGR2 + C.AGEGR2, data.contact.events))

contact.mat = matrix(age_pairs_contact$Freq, ncol=10, nrow=10) # row = index, col = contact
rownames(contact.mat) = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
colnames(contact.mat) = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
contact.mat = apply(t(contact.mat),2,rev) # row = contact, col = index

N.agegroup = apply(contact.mat,1,sum) # total in INDEX age groups
N.mat = matrix(rep(N.agegroup,10), nrow=10, ncol=10)#, byrow=T)

mean.contact.mat = contact.mat / N.mat

year = '21'
name = paste0(month.abb[12]," ", year)

library(plot.matrix)
library(RColorBrewer)
par(mar=c(5.1, 5.1, 4.1, 4.1))
plot(mean.contact.mat, xlab="Index age", ylab="Contact age", main=name, col=colorRampPalette(brewer.pal(5, "GnBu")),
     fmt.key="%.0f", border=NA, 
     # asp=T, 
     cex.axis=0.7, cex.lab=1.5, 
     axis.col=list(side=1, las=2), axis.row=list(side=2, las=2),
     fmt.cell="%.2f", text.cell=list(cex=0.7), key=NULL,#  key=list(tick=F, at=c(0,2,4,6), labels=c(0,2,4,6)), 
     breaks=seq(0,0.7,0.001))

dev.off()


###############################
#### SERIEEL INTERVAL      ####
###############################

dim(contact.line.list)
data.si = contact.line.list[contact.line.list$C.POS==1 & !is.na(contact.line.list$I.SYMPTOMS) & !is.na(contact.line.list$C.SYMPTOMS) ,]
dim(data.si)
data.si$emp.si = as.numeric(data.si$C.SYMPTOMS - data.si$I.SYMPTOMS)
data.si = data.si[data.si$emp.si>=(-5) & data.si$emp.si<=21, ]
dim(data.si)
summary(data.si$emp.si)

# random index for each contact
data.si2 <- data.si %>%
  group_by(C.RRN) %>%
  slice_sample(n=1)
dim(data.si2)

tiff("Fig4.tif", width = 30, height = 20, units = 'cm', res = 300, compression = 'lzw')
si.obs = aggregate(data.si2$emp.si, by = list(data.si2$I.TEST), FUN = mean)
dim(si.obs)[1] == as.numeric(as.Date('2021-12-31') - as.Date('2020-10-31')) + 1
av7days = zoo::rollmean(si.obs$x, k = 31)
par(mar = c(5.1,5.1,4.1,2.1))
plot(seq(1,length(av7days)), av7days, type = "l", xlab = "Index test date", ylab = "Average serial interval",
     cex.axis = 1.5, cex.lab = 1.5, ylim = c(1,4.5),
     xaxt = "n", lwd = 3, col = 2)
axis(side=1, at=c(1,62,121,182,243,305,366), labels=c("31/10/20","1/1/21","1/3/21","1/5/21","1/7/21","1/9/21","1/11/21"), cex.axis = 1.5)

##############################
#### TRACED VS UNTRACED INDEX

data.si.traced = data.si2[data.si2$I.traced == 1,]
dim(data.si.traced)

data.si.not.traced = data.si2[data.si2$I.traced == 0,]
dim(data.si.not.traced)

si.traced = aggregate(data.si.traced$emp.si, by = list(data.si.traced$I.TEST), FUN = mean)
dim(si.traced)[1] == as.numeric(as.Date('2021-12-31') - as.Date('2020-10-31')) + 1
minD = as.Date('2020-10-31')
dates = c()
obs = c()
for(i in 1:427){
  if((minD + (i-1)) %in% si.traced$Group.1){
    dates[i] <- si.traced$Group.1[si.traced$Group.1 == (minD + (i-1))]
    obs[i] <- si.traced$x[si.traced$Group.1 == (minD + (i-1))]
  }else{
    dates[i] <- minD + (i-1)
    obs[i] <- NA
  }
}
si.traced2 <- data.frame(Date = as.Date(dates, origin = '1970-01-01'), SI = obs)
dim(si.traced2)[1] == as.numeric(as.Date('2021-12-31') - as.Date('2020-10-31')) + 1
av7days.traced = zoo::rollmean(si.traced2$SI, k = 31)

si.not.traced = aggregate(data.si.not.traced$emp.si, by = list(data.si.not.traced$I.TEST), FUN = mean)
dim(si.not.traced)[1] == as.numeric(as.Date('2021-12-31') - as.Date('2020-10-31')) + 1
av7days.not.traced = zoo::rollmean(si.not.traced$x, k = 31)

lines(seq(1,length(av7days.traced)), av7days.traced, type = "l", xlab = "Index test date", ylab = "Average serial interval",
      cex.axis = 1.5, cex.lab = 1.5,# ylim = c(0,1.5),
      xaxt = "n", lwd = 2, lty = 1, col = 1)
lines(seq(1, length(av7days.not.traced)), av7days.not.traced, col = 1, lwd = 2, lty = 2)
legend('topright', c('All','Known index','New index'), col = c(2,1,1), lwd = c(3,2,2), lty = c(1,1,2), bty = 'n', cex = 1.5)
dev.off()

