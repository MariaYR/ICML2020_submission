#Full data set
#scaled from min_depth_dist.R

library(data.table)
mydata <-fread("CF2017v2.tab", header = TRUE, sep = "\t", quote = "")

na_count <-sapply(mydata, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

library(tidyverse)


#create time difference variables
#number of days between report date and investigation date 
rptdt_test <- as.Date(mydata$RptDt, format="%Y-%m-%d", tz="UTC")
invdate_test <- as.Date(mydata$InvDate, format="%Y-%m-%d", tz="UTC")
mydata$rpt_to_inv <- difftime(as.POSIXct(invdate_test), as.POSIXct(rptdt_test, tz="UTC"), units="days")
table(mydata$rpt_to_inv)

#convert original to date as well 
mydata$RptDt <- as.Date(mydata$RptDt, format="%Y-%m-%d", tz="UTC")
mydata$InvDate <- as.Date(mydata$InvDate, format="%Y-%m-%d", tz="UTC")

#save.image("/Volumes/MariasDrive/ncands/220Data/TextFiles/full_dataset.RData")

#one variable between investigation date and disposition date 
rptdisdt_test <- as.Date(mydata$RpDispDt, format="%Y-%m-%d", tz="UTC")
mydata$inv_to_dis <- difftime(as.POSIXct(rptdisdt_test), as.POSIXct(invdate_test, tz="UTC"), units="days")
table(mydata$inv_to_dis)
#convert original to date as well 
mydata$RpDispDt <- as.Date(mydata$RpDispDt, format="%Y-%m-%d", tz="UTC")

#number of days between investigation date and service date
srvdt_test <- as.Date(mydata$ServDate, format="%Y-%m-%d", tz="UTC")
mydata$inv_to_srv <- difftime(as.POSIXct(invdate_test), as.POSIXct(srvdt_test, tz="UTC"), units="days")
table(mydata$inv_to_srv)
#convert original to date as well 
mydata$ServDate <- as.Date(mydata$ServDate, format="%Y-%m-%d", tz="UTC")

#number of days between report date and removal date
rmdt_test <- as.Date(mydata$RmvDate, format="%Y-%m-%d", tz="UTC")
mydata$rptdt_to_rmdt <- difftime(as.POSIXct(rptdt_test),as.POSIXct(rmdt_test, tz="UTC"), units="days")
table(mydata$rptdt_to_rmdt)
#convert original to date as well 
mydata$RmvDate <- as.Date(mydata$RmvDate, format="%Y-%m-%d", tz="UTC")

#number of days betwen report date and petition date 
#The month, day, and year that the juvenile court petition was filed.
petdt_test <- as.Date(mydata$PetDate, format="%Y-%m-%d", tz="UTC")
mydata$rpt_to_pet <- difftime(as.POSIXct(rptdt_test),as.POSIXct(petdt_test,  tz="UTC"), units="days")
table(mydata$rpt_to_pet)
#convert original to date as well 
mydata$PetDate <- as.Date(mydata$PetDate, format="%Y-%m-%d", tz="UTC")

str(mydata, list.len=ncol(mydata))

mydata_clean <- mydata
#remove unneccessary vars
library(data.table)
setDT(mydata_clean)

#drop factors
drop <- c("RptID","ChID", "Per1ID","Per2ID","Per3ID","AFCARSID","FCDchDt","StFCID")
mydata_clean = mydata[,.SD,.SDcols = !(names(mydata) %in% drop)]
str(mydata_clean, list.len=ncol(mydata))

drop_idvars <- c("StaTerr", "RptID", "ChID", "RptTm", "InvStrTm", "Per1ID", "Per2ID", "Per3ID","AFCARSID","FCDchDt","StFCID")
mydata_clean = mydata_clean[,.SD,.SDcols = !(names(mydata_clean) %in% drop_idvars)]
str(mydata_clean, list.len=ncol(mydata_clean))

drop_idvars2 <- c("subyr", "RptCnty", "RptFIPS")
mydata_clean = mydata_clean[,.SD,.SDcols = !(names(mydata_clean) %in% drop_idvars2)]
str(mydata_clean, list.len=ncol(mydata_clean))

#convert all integer to numeric
#index by column 
mydata_clean[,3:4] <- lapply(mydata_clean[,3:4], as.numeric)
#check
str(mydata_clean, list.len=ncol(mydata))

#probably easier to do this with a for loop. *shrug*
mydata_clean[,6:49] <- lapply(mydata_clean[,6:49], as.numeric)
str(mydata_clean, list.len=ncol(mydata))

mydata_clean[,51:53] <- lapply(mydata_clean[,51:53], as.numeric)
str(mydata_clean, list.len=ncol(mydata))

mydata_clean[,55] <- lapply(mydata_clean[,55], as.numeric)
str(mydata_clean, list.len=ncol(mydata))

#easier way? 
#install.packages("taRifx")
#library( taRifx )
#mydata_clean <- japply( mydata_clean, which(sapply(mydata_clean, class)=="integer"), as.numeric )
#Error: vector memory exhausted (limit reached?)

mydata_clean[,57] <- lapply(mydata_clean[,57], as.numeric)
str(mydata_clean, list.len=ncol(mydata))

mydata_clean[,59:135] <- lapply(mydata_clean[,59:135], as.numeric)
str(mydata_clean, list.len=ncol(mydata_clean))

#remove columns with too many NAs
#threshold 400,000, 
#exception child living arrangement - imput
na_count
library(tidyverse)
drop.cols <- c('RptTm',
               'InvStrTm',
               'chmil',
               'chmal2', 
               'mal2lev',
               'chmal3',
               'mal3lev', 
               'chmal4',
               'mal4lev',
               'MalDeath',
               'per2rel', 
               'per2prnt', 
               'per2cr', 
               'Per2Age', 
               'per2sex', 
               'P2RacAI', 
               'P2RacAs',   
               'P2RacBl',
               'P2RacNH', 
               'P2RacWh',
               'p2racud',
               'Per2Ethn', 
               'per2mil' ,  
               'per2pior',    
               'per2mal1',    
               'per2mal2',   
               'per2mal3',  
               'per2mal4',
               'per3rel',      
               'per3prnt',     
               'per3cr',       
               'Per3Age',      
               'per3sex',     
               'P3RacAI',      
               'P3RacAs',      
               'P3RacBl',      
               'P3RacNH',      
               'P3RacWh',      
               'p3racud',      
               'Per3Ethn',     
               'per3mil',      
               'per3pior',     
               'per3mal1',    
               'per3mal2',  
               'per3mal3',    
               'per3mal4') 

mydata_cleaner <- mydata_clean %>% select(-one_of(drop.cols))
str(mydata_cleaner, list.len=ncol(mydata_cleaner))
#mydata_clean<-mydata_clean %>% drop_na(per2rel:per3mal4)
str(mydata_clean, list.len=ncol(mydata_clean))

#new data
#mydata_cleaner <- mydata_clean
#str(mydata_cleaner, list.len=ncol(mydata_cleaner))

#count nas left
na_count2 <-sapply(mydata_cleaner, function(y) sum(length(which(is.na(y)))))
na_count2 <- data.frame(na_count2)
na_count2

#a few left
drop.cols2 <- c('RmvDate', 
                'PetDate', 
                'per1rel',        
                'per1prnt',       
                'per1cr',         
                'Per1Age',        
                'per1sex',        
                'P1RacAI',        
                'P1RacAs',        
                'P1RacBl',        
                'P1RacNH',        
                'P1RacWh',        
                'p1racud',        
                'Per1Ethn',       
                'per1mil',        
                'per1pior',       
                'per1mal1',       
                'per1mal2',       
                'per1mal3',       
                'per1mal4')       

mydata_cleaner <- mydata_cleaner %>% select(-one_of(drop.cols2))

na_count3 <-sapply(mydata_cleaner, function(y) sum(length(which(is.na(y)))))
na_count3 <- data.frame(na_count3)
na_count3

                
#remove date vars
#drop.cols3 <- c('RptDt', 'InvDate','RpDispDt', 'ServDate')
#mydata_cleaner <- mydata_cleaner %>% select(-one_of(drop.cols3))
#str(mydata_cleaner)

#convert 'difftime' vars to numeric 
#mydata_cleaner[,70:71] <- lapply(mydata_cleaner[,70:71], as.numeric)
#str(mydata_cleaner)

#convert response variable to factor
mydata_cleaner$RptDisp <- as.factor(mydata_cleaner$RptDisp)
str(mydata_cleaner)

table(mydata_cleaner$RptDisp)

str(mydata_cleaner, list.len=ncol(mydata_cleaner))

#Combine 5 &6 
#merge 88 & 99
library(plyr); library(dplyr)

mydata_cleaner$RptDisp <- mapvalues(mydata_cleaner$RptDisp, from = c("6"), to = c("5"))
table(mydata_cleaner$RptDisp)

mydata_cleaner$RptDisp <- mapvalues(mydata_cleaner$RptDisp, from = c("99"), to = c("88"))
table(mydata_cleaner$RptDisp)

#Current label definitions
#1 substantiated
#2 indicated or reason to suspect
#3 alternative response disposition-victim
#4 alternative response disposition-not a victim
#5 unsubstantiated or unsubstantiated due to intentionally false reporting
#7 closed-no finding
#88 other or Unknown or Missing

#re-code rptdisp to 3 levels: 
#1 = substantiated,indicated, or alt response (victim disposition) 
#2 = unsubstantiated, unsubstantiated due to intentionally false reporting
#3 = alternative response (not victim)

mydata_cleaner$RptDisp <- mapvalues(mydata_cleaner$RptDisp, from = c("2"), to = c("1"))
table(mydata_cleaner$RptDisp)

mydata_cleaner$RptDisp <- mapvalues(mydata_cleaner$RptDisp, from = c("3"), to = c("1"))
table(mydata_cleaner$RptDisp)

mydata_cleaner$RptDisp <- mapvalues(mydata_cleaner$RptDisp, from = c("4"), to = c("3"))
table(mydata_cleaner$RptDisp)

mydata_cleaner$RptDisp <- mapvalues(mydata_cleaner$RptDisp, from = c("5"), to = c("2"))
table(mydata_cleaner$RptDisp)

#get rid of others or missing (88) and closed no finding (7)
list_of_values <- c("1", "2", "3")
mydata_cleaner <- filter(mydata_cleaner, RptDisp %in% list_of_values)
table(mydata_cleaner$RptDisp)

#replace unknowns with na 
#install.packages("naniar")
library(naniar)
library(data.table)
#unkowns coded as 9
vars_rename <- c('ChAge', 'ChRacAI', 'ChRacAs', 
                 'ChRacBl', 'ChRacNH', 'ChRacWh', 'ChRacUD', 'CEthn',
                 'chmil', 'chprior', 'chmal1', 'MalDeath', 'cdalc', 'cddrug', 
                 'cdrtrd', 'cdemotnl', 'cdvisual', 'cdlearn', 'cdphys', 'cdbehav', 
                 'cdmedicl', 'fcalc', 'fcdrug', 'fcrtrd', 'fcemotnl', 'fcvisual', 'fclearn', 
                 'fcphys', 'fcmedicl', 'fcviol', 'fchouse', 'fcmoney', 'fcpublic', 'postserv', 
                 'famsup', 'fampres', 'fostercr', 'juvpet', 'cochrep', 'adopt', 'casemang', 
                 'counsel', 'daycare', 'educatn', 'employ', 'famplan', 'health', 'homebase',
                 'housing', 'transliv', 'inforef', 'legal', 'menthlth', 'pregpar', 'respite', 
                 'ssdisabl', 'ssdelinq', 'subabuse', 'transprt', 'othersv')

#using data table to speed up replacement 
mydata_cleaner <- as.data.table(mydata_cleaner)

#replace 9s
for (names in vars_rename) {
  set(mydata_cleaner,i = which(mydata_cleaner[[names]]==9),j = names,value = NA)  
}

#check
table(mydata_cleaner$ChRacAI)

#replace unable to determine for race vars (coded as 3) to na as well 
vars_race_nas <- c('ChRacAI', 'ChRacAs', 'ChRacBl', 'ChRacNH',
                   'ChRacWh','CEthn', 'ChRacUD')
for (names in vars_race_nas) {
  set(mydata_cleaner,i = which(mydata_cleaner[[names]]==3),j = names,value = NA)  
}

#check
table(mydata_cleaner$ChRacAI)

#replace unkowns coded as 99
vars_other_nas <- c('rptsrc', 'notifs','chlvng', 'mal1lev')
for (names in vars_other_nas) {
  set(mydata_cleaner,i = which(mydata_cleaner[[names]]==99),j = names,value = NA)  
}

na_count3 <-sapply(mydata_cleaner, function(y) sum(length(which(is.na(y)))))
na_count3 <- data.frame(na_count3)
na_count3

#remove subyr and RptCnty
#drops <- c("subyr","RptCnty")
#mydata_cleaner <- mydata_cleaner %>% select(-one_of(drops))
#names(mydata_cleaner)

#recode NOs (2) to 0 
vars_nos <- c('ChRacAI', 'ChRacAs', 
              'ChRacBl', 'ChRacNH', 'ChRacWh', 'ChRacUD', 'CEthn',
              'chmil', 'chprior', 'chmal1', 'MalDeath', 'cdalc', 'cddrug', 
              'cdrtrd', 'cdemotnl', 'cdvisual', 'cdlearn', 'cdphys', 'cdbehav', 
              'cdmedicl', 'fcalc', 'fcdrug', 'fcrtrd', 'fcemotnl', 'fcvisual', 'fclearn', 
              'fcphys', 'fcmedicl', 'fcviol', 'fchouse', 'fcmoney', 'fcpublic', 'postserv', 
              'famsup', 'fampres', 'fostercr', 'juvpet', 'cochrep', 'adopt', 'casemang', 
              'counsel', 'daycare', 'educatn', 'employ', 'famplan', 'health', 'homebase',
              'housing', 'transliv', 'inforef', 'legal', 'menthlth', 'pregpar', 'respite', 
              'ssdisabl', 'ssdelinq', 'subabuse', 'transprt', 'othersv')

for (names in vars_nos) {
  set(mydata_cleaner,i = which(mydata_cleaner[[names]]==2),j = names,value = 0)  
}

vars_age <- c('ChAge')
for (names in vars_age) {
  set(mydata_cleaner,i = which(mydata_cleaner[[names]]==99),j = names,value = NA)  
}

#set unborn to 0 
for (names in vars_age) {
  set(mydata_cleaner,i = which(mydata_cleaner[[names]]==77),j = names,value = 0)  
}

table(mydata_cleaner$ChAge)
#check
table(mydata_cleaner$ChRacUD)
table(mydata_cleaner$RptDisp)

#drop empty classes
mydata_cleaner$RptDisp = droplevels(mydata_cleaner$RptDisp)
table(mydata_cleaner$RptDisp)

#relevel RptDisp so that unsubstantiated is 1st
mydata_cleaner$RptDisp <- relevel(mydata_cleaner$RptDisp, "2")
table(mydata_cleaner$RptDisp)

#missed some numeric conversions
library(magrittr)
mydata_cleaner %<>% mutate_if(is.integer,as.numeric)
str(mydata_cleaner)

#special for difftime
mydata_cleaner$inv_to_dis<-as.numeric(mydata_cleaner$inv_to_dis, units="days")
mydata_cleaner$inv_to_srv<-as.numeric(mydata_cleaner$inv_to_srv, units="days")
mydata_cleaner$rpt_to_pet<-as.numeric(mydata_cleaner$rpt_to_pet, units="days")
mydata_cleaner$rptdt_to_rmdt<-as.numeric(mydata_cleaner$rptdt_to_rmdt, units="days")

str(mydata_cleaner)

table(mydata_cleaner$inv_to_dis)
table(mydata_cleaner$inv_to_srv)
table(mydata_cleaner$rpt_to_pet)
table(mydata$rptdt_to_rmdt)

#check values for vars of particular importance
table(mydata_cleaner$RptDisp)

#drop date vars for imputation
library(tidyverse)
drop_dates <- c("RptDt", "InvDate", "RpDispDt", "ServDate")
mydata_cleaner <- mydata_cleaner %>% select(-one_of(drop_dates))
names(mydata_cleaner)

###
#Descriptive stats
####

#table age distribution of children in data file 
table(mydata_cleaner$ChAge)
# labels outside bars
library(ggplot2)
library(scales)
library(gameofthrones)
options(scipen=10)
g <- ggplot(mydata_cleaner, aes(ChAge))
g + geom_histogram(bins = 18, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(ChAge)), linetype = "dashed", size = 0.6) +
  scale_y_continuous(label=comma) +
  ylab("Count") + xlab ("Child Ages") 
  

#table report sources
mydata_cleaner$RptSrc <- factor(mydata_cleaner$RptSrc,
                    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,88,99),
                    labels = c("Social Service", "Medical","Mental Health", "Legal/Law/CJ", 
                               "Education", "Child Care Provider", "Substitute Care Provider", 
                               "Alleged Victim", "Parent", "Other Relative", "Friend/Neighbor", "Alleged Prep",
                               "Anonymous", "Other", "Unknown")) 

h <- ggplot(mydata_cleaner, aes(RptSrc))
h + geom_bar(color = "black", fill = "gray") +
  coord_flip() +
  ylab("Count") + xlab ("Report Source") 

#table race

mydata_cleaner$race_total <- names(mydata_cleaner[6:11])[max.col(mydata_cleaner[6:11])]

r <- ggplot(mydata_cleaner, aes(race_total))
r + geom_bar(color = "black", fill = "gray") +
  coord_flip() +
  ylab("Count") + xlab ("Reported Race") 


#table case dispositions

#1 = substantiated,indicated, or alt response (victim disposition) 
#2 = unsubstantiated, unsubstantiated due to intentionally false reporting
#3 = alternative response (not victim)

mydata_cleaner$RptDisp <- factor(mydata_cleaner$RptDisp,
                                levels = c(1,2,3),
                                labels = c("Substantiated/Indicated/Alt Response", 
                                           "Unsubstantiated/False Reporting", 
                                           "alternative response (not victim)"))

i <- ggplot(mydata_cleaner, aes(RptDisp))

i + geom_bar(color = "black", fill = "gray") + 
  coord_flip() +
  ylab("Count") + xlab ("Report Disposition") 

#map report counties, examine for hotspots? 
devtools::install_github('UrbanInstitute/urbnmapr')

library(tidyverse)
library(urbnmapr)

ggplot() + 
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = 'grey', color = 'white') +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

#merge RptFIPS to mydatacleaner for mapping
merge(x = DF1, y = DF2[ , c("Client", "LO")], by = "Client", all.x=TRUE)

###stopped here
#need to use mydata to create a new data set for this bit. Move to a new script file 
#https://stackoverflow.com/questions/24191497/left-join-only-selected-columns-in-r-with-the-merge-function
#https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2

cw_reporting_data <- left_join(mydata, counties, by = "RptFIPS") 

household_data %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Median Household Income")


################
#random forest on full
################
#drop rptvictim and mal1lev - these occur as a result of the disposition
#this drop accuracy dramatically
drops2 <- c("RptVictim","mal1lev")
mydata_cleaner <- mydata_cleaner %>% select(-one_of(drops2))

install.packages("randomForest")
library(randomForest)
summary(mydata_cleaner)
str(mydata_cleaner)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(1234)

mydata_cleaner <- na.roughfix(mydata_cleaner)
train_full <- sample(nrow(mydata_cleaner), 0.7*nrow(mydata_cleaner), replace = FALSE)
TrainSet_full <- mydata_cleaner[train_full,]
ValidSet_full <- mydata_cleaner[-train_full,]
summary(TrainSet_full)
summary(ValidSet_full)

# Create a Random Forest model with default parameters
#options(stringsAsFactors = FALSE)
#model1_full <- randomForest(RptDisp ~ ., data = TrainSet_full, importance = TRUE)
#Error in randomForest.default(m, y, ...) : 
#  long vectors (argument 24) are not supported in .C

#mtry default = 12
#norm.votes	
#If TRUE (default), the final result of votes are expressed as fractions. 
#If FALSE, raw vote counts are returned (useful for combining results from different runs). Ignored for regression.

#do.trace	
#If set to TRUE, give a more verbose output as randomForest is run. If set to some integer, then running output 
#is printed for every do.trace trees.

#must reduce size of training set to avoid long vector .C error 
rf1 <- randomForest(RptDisp ~ ., TrainSet_full[1:600000,], ntree=100, norm.votes=FALSE, do.trace=10,importance=TRUE)
rf2 <- randomForest(RptDisp ~ ., TrainSet_full[600001:1200000,], ntree=100, norm.votes=FALSE, do.trace=10,importance=TRUE)
rf3 <- randomForest(RptDisp ~ ., TrainSet_full[1200001:1800000,], ntree=100, norm.votes=FALSE, do.trace=10,importance=TRUE)
rf4 <- randomForest(RptDisp ~ ., TrainSet_full[1800001:2400000,], ntree=100, norm.votes=FALSE, do.trace=10,importance=TRUE)
rf5 <- randomForest(RptDisp ~ ., TrainSet_full[2400001:2930623,], ntree=100, norm.votes=FALSE, do.trace=10,importance=TRUE)

#rf5 is of unequal size, use this solution 
#https://stackoverflow.com/questions/19170130/combining-random-forests-built-with-different-training-sets-in-r

my_combine <- function (...) 
{
  pad0 <- function(x, len) c(x, rep(0, len - length(x)))
  padm0 <- function(x, len) rbind(x, matrix(0, nrow = len - 
                                              nrow(x), ncol = ncol(x)))
  rflist <- list(...)
  areForest <- sapply(rflist, function(x) inherits(x, "randomForest"))
  if (any(!areForest)) 
    stop("Argument must be a list of randomForest objects")
  rf <- rflist[[1]]
  classRF <- rf$type == "classification"
  trees <- sapply(rflist, function(x) x$ntree)
  ntree <- sum(trees)
  rf$ntree <- ntree
  nforest <- length(rflist)
  haveTest <- !any(sapply(rflist, function(x) is.null(x$test)))
  vlist <- lapply(rflist, function(x) rownames(importance(x)))
  numvars <- sapply(vlist, length)
  if (!all(numvars[1] == numvars[-1])) 
    stop("Unequal number of predictor variables in the randomForest objects.")
  for (i in seq_along(vlist)) {
    if (!all(vlist[[i]] == vlist[[1]])) 
      stop("Predictor variables are different in the randomForest objects.")
  }
  haveForest <- sapply(rflist, function(x) !is.null(x$forest))
  if (all(haveForest)) {
    nrnodes <- max(sapply(rflist, function(x) x$forest$nrnodes))
    rf$forest$nrnodes <- nrnodes
    rf$forest$ndbigtree <- unlist(sapply(rflist, function(x) x$forest$ndbigtree))
    rf$forest$nodestatus <- do.call("cbind", lapply(rflist, 
                                                    function(x) padm0(x$forest$nodestatus, nrnodes)))
    rf$forest$bestvar <- do.call("cbind", lapply(rflist, 
                                                 function(x) padm0(x$forest$bestvar, nrnodes)))
    rf$forest$xbestsplit <- do.call("cbind", lapply(rflist, 
                                                    function(x) padm0(x$forest$xbestsplit, nrnodes)))
    rf$forest$nodepred <- do.call("cbind", lapply(rflist, 
                                                  function(x) padm0(x$forest$nodepred, nrnodes)))
    tree.dim <- dim(rf$forest$treemap)
    if (classRF) {
      rf$forest$treemap <- array(unlist(lapply(rflist, 
                                               function(x) apply(x$forest$treemap, 2:3, pad0, 
                                                                 nrnodes))), c(nrnodes, 2, ntree))
    }
    else {
      rf$forest$leftDaughter <- do.call("cbind", lapply(rflist, 
                                                        function(x) padm0(x$forest$leftDaughter, nrnodes)))
      rf$forest$rightDaughter <- do.call("cbind", lapply(rflist, 
                                                         function(x) padm0(x$forest$rightDaughter, nrnodes)))
    }
    rf$forest$ntree <- ntree
    if (classRF) 
      rf$forest$cutoff <- rflist[[1]]$forest$cutoff
  }
  else {
    rf$forest <- NULL
  }
  #
  #Tons of stuff removed here...
  #
  if (classRF) {
    rf$confusion <- NULL
    rf$err.rate <- NULL
    if (haveTest) {
      rf$test$confusion <- NULL
      rf$err.rate <- NULL
    }
  }
  else {
    rf$mse <- rf$rsq <- NULL
    if (haveTest) 
      rf$test$mse <- rf$test$rsq <- NULL
  }
  rf
}

#rf.combined <- combine(rf1,rf2,rf3,rf4,rf5)

#then
rf.all <- my_combine(rf1,rf2,rf3,rf4,rf5)

#save
save.image("~/Dropbox/TextFiles/acm_fat2020_submission/acm_fat2020.RData")

# Predicting on training set
predTrain_full <-predict(rf.all,TrainSet_full, type = "class")
#predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain_full, TrainSet_full$RptDisp)

#predTrain_full       2       1       3
#2 1771078  325499  119026
#1   40700  273231    6918
#3   40154    8986  345031

# Predicting on Validation set
predValid_full <- predict(rf.all, ValidSet_full, type = "class")
# Checking classification accuracy
mean(predValid_full == ValidSet_full$RptDisp) 
#[1] 0.8039423

# To check important variables
importance(rf.all)   

varImpPlot(rf.all, main = "Variable Importance: Final Model") 

# identify the right mtry for model
a=c()
i=5
for (i in 3:20) {
  model5 <- randomForest(rptdisp ~ ., data = TrainSet, ntree = 750, mtry = i, importance = TRUE)
  predValid <- predict(model5, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$rptdisp)
}

a

plot(3:20,a, main = "Model Accuracy for mtry = 3:20")
#mtry best at about 12, model 2

#random forest exaplainer
#install.packages("randomForestExplainer")
library(randomForestExplainer)
#https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html

#Current model 
rf.all

#best model? 

#distribution of minimal depth 
library(forcats)
min_depth_frame <- min_depth_distribution(rf.all)
#Minimal depth for a variable in a tree equals to the depth of the node 
#which splits on that variable and is the closest to the root of the tree. 
#If it is low than a lot of observations are divided into groups on the 
#basis of this variable
save(min_depth_frame, file = "min_depth_frame.rda")
load("min_depth_frame.rda")
head(min_depth_frame, n = 10)

# plot_min_depth_distribution(forest) 
plot_min_depth_distribution(min_depth_frame)
# y -axis = top ten variables 
#The x-axis ranges from zero trees to the maximum number of trees in which 
#any variable was used for splitting (B^) which is in this case equal to 500 and
#is reached by all variables plotted.

#The default option, "top_trees", penalizes missing values and this 
#penalization makes the interpretation of the values less obvious – 
#to address that we can calculate the mean minimal depth only using 
#non-missing observations

plot_min_depth_distribution(min_depth_frame, 
                            mean_sample = "relevant_trees",
                            min_no_of_trees = 12,
                            main = "Distribution of Minimal Depth & Mean: Top 10 Variables")
#relevant_trees ignores missing values, min_no_of_trees sets threshold for 
#number of trees that varibale must be included in for plotting 

plot_min_depth_distribution(min_depth_frame, 
                            mean_sample = "relevant_trees",
                            k = 15,
                            min_no_of_trees = 20,
                            main = "Distribution of Minimal Depth & Mean: Top 15 Variables")
#same variables as above, used as final plot 


#variable importance measures
importance_frame <- measure_importance(rf.all)
save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
importance_frame
#68 rows

plot_multi_way_importance(importance_frame, 
                          size_measure = "no_of_nodes",
                          min_no_of_trees = 20)
#default x-measure = times_a_root
#default y_measure = mean_min_depth
# negative relation between times_a_root and mean_min_depth

plot_multi_way_importance(importance_frame, 
                          size_measure = "p_value",
                          min_no_of_trees = 20)

plot_multi_way_importance(importance_frame, 
                          x_measure = "accuracy_decrease", 
                          y_measure = "gini_decrease", 
                          size_measure = "p_value") 
#min_no_of_trees = 20,
#no_of_labels = 5)
#plot importance of pairs
plot_importance_ggpairs(importance_frame)

#replot using these three measures: 
#no_nodes & accuracy_decrease
#times_a_root & no_nodes

#final importance plots
plot_multi_way_importance(importance_frame, 
                          x_measure = "accuracy_decrease", 
                          y_measure = "gini_decrease", 
                          size_measure = "no_of_nodes") 

plot_multi_way_importance(importance_frame, 
                          x_measure = "accuracy_decrease", 
                          y_measure = "gini_decrease", 
                          size_measure = "times_a_root") 

#To extract the names of 5 most important variables according to both 
#the mean minimal depth and number of trees in which a variable appeared, 
#pass importance_frame to the function important_variables 

# (vars <- important_variables(forest, k = 5, measures = c("mean_min_depth", "no_of_trees"))) # gives the same result as below but takes longer
(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))
#[1] "inv_to_dis" "chmal1"     "RptSrc"     "fostercr"   "juvpet"     

#obtain a data frame containing information on mean conditional 
#minimal depth of variables with respect to each element of vars
interactions_frame <- min_depth_interactions(rf.all, vars)
save(interactions_frame, file = "interactions_frame.rda")
load("interactions_frame.rda")
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

# plot_min_depth_interactions(model2) # calculates the interactions_frame for default settings so may give different results than the function below depending on our settings and takes more time
plot_min_depth_interactions(interactions_frame)
#interactions are ordered by decreasing number of occurrences
#Using the default mean_sample = "top_trees" penalizes interactions 
#that occur less frequently than the most frequent one.

#plot highest interactions
plot_predict_interaction(rf.all, mydata_cleaner, "RptSrc", "ChRacBl")
plot_predict_interaction(rf.all, mydata_cleaner, "chmal1", "ChRacBl")
# plot_predict_interaction(rf.all, mydata_cleaner, "fostercr", "chprior")
# plot_predict_interaction(rf.all, mydata_cleaner, "fostercr", "ChRacWh")
# 
# plot_predict_interaction(rf.all, mydata_cleaner, "rpt_to_inv", "ChRacBl")
# plot_predict_interaction(rf.all, mydata_cleaner, "inv_to_srv", "ChRacBl")

table(mydata_cleaner$fostercr)

#interactions of interest
plot_predict_interaction(rf.all, mydata_cleaner, "rptdisp", "ChRacBl")

#random forest explainer full 
explain_forest(rf.all, interactions = TRUE, data = mydata_cleaner)

#visualize a tree
# options(repos='http://cran.rstudio.org')
# have.packages <- installed.packages()
# cran.packages <- c('devtools','plotrix','randomForest','tree')
# to.install <- setdiff(cran.packages, have.packages[,1])
# if(length(to.install)>0) install.packages(to.install)
# 
# library(devtools)
# if(!('reprtree' %in% installed.packages())){
#   install_github('araastat/reprtree')
# }
# for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))
# 
# library(randomForest)
# library(reprtree)

#reprtree:::plot.getTree(rf.all)
# 
# tree <- getTree(rf.all, k=1, labelVar=TRUE)
# realtree <- reprtree:::as.tree(tree, rf.all)


