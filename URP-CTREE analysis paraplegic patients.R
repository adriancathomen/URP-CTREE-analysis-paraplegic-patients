#### >>>>>>>> URP-CTREE anaylsis <<<<<<<< ####
## contact: Adrian Cathomen, adrian.cathomen@balgrist.ch

rm(list=ls())

library(party)     ## for recursive partitioning

setwd("/Users/Measurements/Desktop/EMSCI_data")

imageDirectory<- "/Users/Measurements/Desktop/EMSCI_data/Graphiken/para"
saveInImageDirectory<-function(filename, width, height){imageFile <- file.path(imageDirectory, filename); ggsave(imageFile, width = width, height = height)}

#### Load data & structure ####
## Load:
data_EMSCI <- read.csv("data_EMSCI.csv", header=T, sep=",")

## Structure:
str(data_EMSCI)
# ID as factor:
data_EMSCI$PatientNumber <- factor(data_EMSCI$PatientNumber) 
# Date of injury as date:
data_EMSCI$DOI <- as.Date(data_EMSCI$DOI, "%d/%m/%Y")
# code for cause as factor:
data_EMSCI$code_Cause <- factor(data_EMSCI$code_Cause)
# code for exam stage as factor:
data_EMSCI$code_ExamStage <- factor(data_EMSCI$code_ExamStage)
# ISNSCI test date as date:
data_EMSCI$ISNCSCI_TestDate <- as.Date(data_EMSCI$ISNCSCI_TestDate, "%d/%m/%Y")
# code AIS as factor:
data_EMSCI$code_AIS <- factor(data_EMSCI$code_AIS)
# code NLI as factor:
data_EMSCI$code_NLI <- factor(data_EMSCI$code_NLI)
# code PT (Para/Tetra) as factor:
data_EMSCI$code_PT <- factor(data_EMSCI$code_PT)
# SCIM23 test date as date:
data_EMSCI$SCIM23_TestDate <- as.Date(data_EMSCI$SCIM23_TestDate, "%d/%m/%Y")
# 6mWT test date as date:
data_EMSCI$X6min_TestDate <- as.Date(data_EMSCI$X6min_TestDate, "%d/%m/%Y")
data_EMSCI$X10m_TestDate <- as.Date(data_EMSCI$X10m_TestDate, "%d/%m/%Y")
data_EMSCI$WISCI_TestDate <- as.Date(data_EMSCI$WISCI_TestDate, "%d/%m/%Y")



str(data_EMSCI) # NLI has 29 levels, because nobody is assigned to S4/S5. 

#remove columns which are not needed:
data_EMSCI$Center <- NULL #encoded in Country
data_EMSCI$Cause <- NULL #encoded in code_cause
data_EMSCI$ExamStage <- NULL #encoded in code_ExamStage
data_EMSCI$AIS <- NULL #encoded in code_AIS
data_EMSCI$NLI <- NULL #encoded in code_NLI
data_EMSCI$RUEMS <- NULL # not needed for analysis
data_EMSCI$LUEMS <- NULL 
data_EMSCI$RLEMS <- NULL
data_EMSCI$LLEMS <- NULL
data_EMSCI$RMS <- NULL
data_EMSCI$LMS <- NULL
data_EMSCI$RPP <- NULL
data_EMSCI$LPP <- NULL
data_EMSCI$RLT <- NULL
data_EMSCI$LLT <- NULL
data_EMSCI[,c(21:26, 31:36 )] <- NULL

#assign reasonable names to codes and columns 
levels(data_EMSCI$code_AIS) <- c("A", "B", "C", "D", "E")
levels(data_EMSCI$code_AIS)
levels(data_EMSCI$code_NLI) <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "T1", "T2", "T3", "T4","T5", "T6", "T7", "T8", "T9","T10", "T11", "T12", "L1", "L2", "L3", "L4", "L5", "S1", "S2", "S3", "Int")
levels(data_EMSCI$code_NLI)
levels(data_EMSCI$code_PT) <- c("Tetra", "Para", "Int")
levels(data_EMSCI$code_PT)
colnames(data_EMSCI)[25] <- "min6_TestDate"
colnames(data_EMSCI)[26] <- "min6"
colnames(data_EMSCI)[27] <- "m10_TestDate"
colnames(data_EMSCI)[28] <- "m10"

# change PatientNumber to ID
names(data_EMSCI)[1] <- "ID"

str(data_EMSCI)

#### >>>> Subsetting ####
names(data_EMSCI)
data.complete <- data_EMSCI[,c(1,3,6,7,9,11,12,13,15,17,18,21,22,23,24,26,30,31,32,38,39,40,41,42,48,49,50,51,52)]
names(data.complete) <- c("ID", "Sex", "Age", "Cause", "ExamStage", "AIS", "NLI", "PT", "LEMS", "PP", "LT", "SCIM_12", "SCIM_13", "SCIM_14", "SCIM_Mob", "min6", "WISCI", "speed6min", "speed10m", "RMS_L2", "RMS_L3", "RMS_L4", "RMS_L5", "RMS_S1", "LMS_L2", "LMS_L3", "LMS_L4", "LMS_L5", "LMS_S1")

# reshape to wide format
data.complete_wide <- reshape(data.complete, v.names = c("AIS", "NLI", "PT", "LEMS", "PP", "LT", "SCIM_12", "SCIM_13", "SCIM_14", "SCIM_Mob", "min6", "WISCI", "speed6min", "speed10m", "RMS_L2", "RMS_L3", "RMS_L4", "RMS_L5", "RMS_S1", "LMS_L2", "LMS_L3", "LMS_L4", "LMS_L5", "LMS_S1"), timevar="ExamStage", idvar="ID", direction = "wide")   ## reshape to wide format

## excluding measure @12months
names(data.complete_wide)


m.complete_wide <- data.complete_wide[,c("ID", "Sex", "Age", "Cause", "AIS.1","NLI.1","PT.1","LEMS.1","PP.1", "LT.1","RMS_L2.1", "RMS_L3.1", "RMS_L4.1", "RMS_L5.1", "RMS_S1.1", "LMS_L2.1", "LMS_L3.1", "LMS_L4.1", "LMS_L5.1", "LMS_S1.1",
                                         "AIS.4", "LEMS.4", "PP.4", "LT.4", "SCIM_12.4", "SCIM_13.4", "SCIM_14.4", "SCIM_Mob.4", "min6.4", "WISCI.4", "speed6min.4","speed10m.4","RMS_L2.4", "RMS_L3.4", "RMS_L4.4", "RMS_L5.4", "RMS_S1.4", "LMS_L2.4", "LMS_L3.4", "LMS_L4.4", "LMS_L5.4", "LMS_S1.4")]

names(m.complete_wide)

names(m.complete_wide) <- c("ID", "Sex", "Age", "Cause", "AIS_va", "NLI_va", "PT_va", "LEMS_va", "PP_va", "LT_va", "RMS_L2_va", "RMS_L3_va", "RMS_L4_va", "RMS_L5_va", "RMS_S1_va", "LMS_L2_va", "LMS_L3_va", "LMS_L4_va", "LMS_L5_va", "LMS_S1_va",
                            "AIS_6months", "LEMS_6months", "PP_6months", "LT_6months", "SCIM_12_6months", "SCIM_13_6months", "SCIM_14_6months","SCIM_Mob_6months", "min6_6months", "WISCI_6months", "speed6min_6months", "speed10m_6months", "RMS_L2_6months", "RMS_L3_6months", "RMS_L4_6months", "RMS_L5_6months", "RMS_S1_6months", "LMS_L2_6months", "LMS_L3_6months", "LMS_L4_6months", "LMS_L5_6months", "LMS_S1_6months")


#### Patient Selection: Step #1 ####
# Age 18-70y
m.para.data_va.6 <- subset(m.complete_wide, Age >= 18 & Age<=70)


#### Patient Selection: Step #2 ####
# traumatic cause

m.para.data_va.6 <- subset(m.para.data_va.6, Cause == 1 & !is.na(Cause))


#### Patient Selection: Step #3 ####
# delete incomplete data in predictors

m.para.data_va.6 <- subset(m.para.data_va.6, !is.na(AIS_va))
m.para.data_va.6 <- subset(m.para.data_va.6, !is.na(NLI_va))
m.para.data_va.6 <- subset(m.para.data_va.6, !is.na(LEMS_va))
m.para.data_va.6 <- subset(m.para.data_va.6, !is.na(LT_va))
m.para.data_va.6 <- subset(m.para.data_va.6, !is.na(PP_va))


#### Patient Selection: Step #4 ####
# paraplegic patients
m.para.data_va.6 <- subset(m.para.data_va.6, PT_va == "Para" & !is.na(PT_va))

table(m.para.data_va.6$PT_va)


### Reference group ###
reference_group <- subset(m.para.data_va.6, is.na(m.para.data_va.6$AIS_6months) | is.na(m.para.data_va.6$LEMS_6months) | is.na(m.para.data_va.6$PP_6months) | is.na(m.para.data_va.6$LT_6months) | is.na(m.para.data_va.6$min6_6months) | is.na(m.para.data_va.6$speed10m_6months) | is.na(m.para.data_va.6$SCIM_Mob_6months) | is.na(m.para.data_va.6$WISCI_6months))

reference_group <- subset(reference_group, !ID==90018)
#1 patient deleted because of implausible values (see supplemental material)

### Study group ###
m.para.data_va.6 <- na.omit(m.para.data_va.6)

## assign rownames to default 
rownames(m.para.data_va.6) <- NULL

str(m.para.data_va.6)

#### Patient Selection: Step #5 ####
## exclude patients with SCIM>9 and speed6min/speed10m ==0
m.para.data_va.6$speed6min_6months <- ifelse(m.para.data_va.6$SCIM_Mob_6months > 9 & m.para.data_va.6$speed6min_6months ==0 , NA, m.para.data_va.6$speed6min_6months)
m.para.data_va.6$speed10m_6months <- ifelse(m.para.data_va.6$SCIM_Mob_6months > 9 & m.para.data_va.6$speed10m_6months ==0 , NA, m.para.data_va.6$speed10m_6months)
m.para.data_va.6$speed6min_6months <- ifelse(m.para.data_va.6$speed6min_6months ==0 & m.para.data_va.6$speed10m_6months >0, NA, m.para.data_va.6$speed6min_6months)

which(is.na(m.para.data_va.6$speed6min_6months))
which(is.na(m.para.data_va.6$speed10m_6months))

patients.excluded <- subset(m.para.data_va.6, is.na(m.para.data_va.6$speed6min_6months) | is.na(m.para.data_va.6$speed10m_6months))

study_group <- na.omit(m.para.data_va.6)
study_group <- subset(study_group, !ID==620006)
#1 patient deleted because of implausible values (see supplemental material)



#### recursive partitioning 6MWT ####
## outcome 6MWT at 6 months, predictors LEMS, UEMS, LT, PP, age, and NLI  

tree_para_va.6_6MWT<-ctree(min6_6months ~ Age + NLI_va + LEMS_va + LT_va + PP_va , data= study_group, controls=ctree_control(maxdepth = 5))

# change N in title according to dataset
plot(tree_para_va.6_6MWT, terminal_panel= node_boxplot(tree_para_va.6_6MWT), main = "para: 6MWT @ 6 months, predictors @ acute, N=361")


# inspect nodes
study_group$node6MWT<-where(tree_para_va.6_6MWT) ## create column with node info in separate column 


para_va.6_6MWT_Node4<-subset(study_group, node6MWT==4)
write.csv(para_va.6_6MWT_Node4, "6MWT_URPtreeN361_va.6_Node4.csv", row.names = F)

para_va.6_6MWT_Node5<-subset(study_group, node6MWT==5)
write.csv(para_va.6_6MWT_Node5, "6MWT_URPtreeN361_va.6_Node5.csv", row.names = F)

para_va.6_6MWT_Node6<-subset(study_group, node6MWT==6)
write.csv(para_va.6_6MWT_Node6, "6MWT_URPtreeN361_va.6_Node6.csv", row.names = F)

para_va.6_6MWT_Node10<-subset(study_group, node6MWT==10)
write.csv(para_va.6_6MWT_Node10, "6MWT_URPtreeN361_va.6_Node10.csv", row.names = F)

para_va.6_6MWT_Node11<-subset(study_group, node6MWT==11)
write.csv(para_va.6_6MWT_Node11, "6MWT_URPtreeN361_va.6_Node11.csv", row.names = F)

para_va.6_6MWT_Node12<-subset(study_group, node6MWT==12)
write.csv(para_va.6_6MWT_Node12, "6MWT_URPtreeN361_va.6_Node12.csv", row.names = F)

para_va.6_6MWT_Node14<-subset(study_group, node6MWT==14)
write.csv(para_va.6_6MWT_Node14, "6MWT_URPtreeN361_va.6_Node14.csv", row.names = F)

para_va.6_6MWT_Node15<-subset(study_group, node6MWT==15)
write.csv(para_va.6_6MWT_Node15, "6MWT_URPtreeN361_va.6_Node15.csv", row.names = F)
