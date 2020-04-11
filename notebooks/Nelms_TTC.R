library(dplyr)
getwd()
setwd("C:/Users/patle/Documents/Work/Nelms")

TTC <- read.csv("lri_kroes.csv") #read dataset processed through the Kroes workflow

levels(TTC$Kroes.TTC.decision.tree) #check to identify any substances that were not processed

TTC = TTC[,-c(4,5)] # remove column that just had NAs

TTC[!complete.cases(TTC),] #to identify the NAs


Exclusions <- subset(TTC, TTC$Kroes.TTC.decision.tree == "Risk assessment requires compound-specific toxicity data")
Q1Y <- subset(Exclusions, Exclusions$Kroes.TTC.decision.tree.explanation == "Q1Y")
#Q1Y <- Q1Y[, 1:3]
HighPotCarc <- Exclusions[grep("Q2Y,Q3Y", Exclusions$Kroes.TTC.decision.tree.explanation), ]

write.csv(Q1Y, "Q1Y_lri.csv")

TTC_Q1Y <- read.csv("OPs_lri.csv") #Processed through new OP filter
OPs <- TTC_Q1Y[grep("Default Class 1", TTC_Q1Y$X..Aa.ad.epa.gov.ord.RTP.Users.E.J.GPatlewi.Net.MyDocuments.Read.across.ECHA.New.folder.TTC.OPs.tml),] 

GenToxTTC <- TTC[grep("Negli", TTC$Kroes.TTC.decision.tree),] # Pulls out Genetox that require a TTC of 0.15 ug/day 

write.csv(GenToxTTC, "GenToxTTC_lri.csv") # process through Steroids and Carbamates filters respectively

Steroids <- read.csv("steroids_lri.csv")
Carbamates <- read.csv("carbamates_lri.csv")

Carbamates <- Carbamates[grep("Default Class 1", Carbamates$New.decision.tree),]
Steroids <- Steroids[grep("Default Class 1", Steroids$New.decision.tree),]


GenToxTTC_1 <- anti_join(GenToxTTC,Steroids, by = "DSSTox_Substance_ID")
GenToxTTC_tot <- anti_join(GenToxTTC_1,Carbamates, by = "DSSTox_Substance_ID")
Q1Y_tot <- anti_join(Q1Y, OPs, by = "DSSTox_Substance_ID" )
Q1Y_tot <- full_join(Q1Y_tot, Steroids, by = "DSSTox_Substance_ID") #final set of exclusions
NA_4_TTC <- full_join(Q1Y_tot, HighPotCarc, by = "DSSTox_Substance_ID")

TTC_1 <- anti_join(TTC, Exclusions, by = "DSSTox_Substance_ID")
TTC_2 <- anti_join(TTC_1, GenToxTTC, by = "DSSTox_Substance_ID")
#TTC_2 <- TTC_2[, c(1,3)] #creating a data.frame once excluded substances and TTC for genetox substances have been removed. Limiting the columns to make it easier to create a csv for processing in KNIME to create a sdf
write.csv(TTC_2, "TTC_2_lri.csv")

TTC_Cramer <- read.csv('TTC2_Cramer_lri.csv')
ClassIII <- TTC_Cramer[grep("High", TTC_Cramer$Cramer.rules),]
ClassII <- TTC_Cramer[grep("Intermediate", TTC_Cramer$Cramer.rules),]
ClassI <- TTC_Cramer[grep("Low", TTC_Cramer$Cramer.rules),]
ACH_1 <- merge.data.frame(Carbamates, OPs, all = TRUE)

# writing out files as csv as needed
write.csv(ClassIII, "ClassIII_lri.csv")
write.csv(ClassII, "ClassII_lri.csv")
write.csv(ClassI, "ClassI_lri.csv")
write.csv(GenToxTTC_tot, "GenToxTTC_lri.csv")
write.csv(Exclusions, "Excluded_from_TTC_lri.csv")
write.csv(NA_4_TTC, "NA for TTC_lri.csv")
write.csv(HighPotCarc, "HighPotencycarcs_lri.csv")
write.csv(Carbamates, "Carbamates_lri.csv")
write.csv(OPs, "OPs_lri.csv")
write.csv(ACH_1, "ACH_1_lri.csv")
