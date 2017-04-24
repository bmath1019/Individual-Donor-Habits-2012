# Introduction -------------------------------------------
# 
# 
# Google Cloud Platform was used for data storage and analysis.
# Specs:
# Linux Ubuntu (16.04)
# R (3.3.3 "Another Canoe")
# R-Studio (1.0.136)
#
# The code is written explicitly for the purpose of annotation.
# 
#
###### Load Contribution data 2004, 2006, 2008, 2010, and 2012
# Soure:
# Database on Ideology, Money in Politics, and Elections (DIME)
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/O5PX0B
#
# Requires a repository of compressed DIME contribution datafiles:
# contribDB_2004.csv.gz (file size: 990 MB compressed / 4.3 GB uncompressed)
# contribDB_2006.csv.gz (file size: 1.1 GB compressed / 4.6 GB uncompressed)
# contribDB_2008.csv.gz (file size: 1.5 GB compressed / 6.3 GB uncompressed)
# contribDB_2010.csv.gz (file size: 1.4 GB compressed / 6.1 GB uncompressed)
# contribDB_2012.csv.gz (file size: 2.3 GB compressed / 12.4 GB uncompressed)
# 
# 


# Setup --------------------------------------------------

install.packages("dplyr")
install.packages("data.table")
install.packages("tidyr")
install.packages("broom")
install.packages("ggplot2")
install.packages("MatchIt")
install.packages("optmatch")
install.packages("lmtest")
install.packages("AER")
install.packages("cem")
install.packages("Matching")

library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(broom)
library(lmtest)
library(AER)
options(scipen=10000) #removing scientific notation from View in R


# 2004 contribution data --------------------------------------------------

# Load Contributions
cont <- fread("/home/bmath/cont2014/data/contribDB_2004.csv",colClasses=c(bonica.cid="character"))
cont <- select(cont, -V1)

# filter for individual contributors, to federal candidates and committees, remove corporations, remove transactions (candidate loans, )
cont04 <- cont %>% filter(contributor.type == "I") # select only individuals
cont04 <- cont04 %>% filter(grepl("federal",seat)) # select only federal candidate recipients
cont04 <- cont04 %>% filter(is.corp != "corp") # remove corporate contributions
cont04 <- cont04 %>% filter(efec.form.type !="SA17D",efec.form.type !="SA17d",efec.form.type !="SA11D",efec.form.type != "SA11d",efec.form.type != "SA13A",efec.form.type != "SA13a") # remove non-applicable form-types
cont04 <- cont04 %>% filter(transaction.type != "15C",transaction.type != "16C") #remove contributions/loans from the candidate
cont04 <- select(cont04,-transaction.id,-contributor.lname,-contributor.fname,-contributor.mname,-contributor.suffix, -contributor.title,-contributor.ffname,-is.corp,-latitude,-longitude,-contributor.type,-gis.confidence,-contributor.district.90s,-contributor.district.00s,-censustract,-efec.memo,-efec.memo2,-efec.transaction.id.orig,-bk.ref.transaction.id,-efec.org.orig,-efec.comid.orig,-excluded.from.scaling)

# Load the DIME committee list
dimeComms <- fread("/home/bmath/cont2014/data/dime_comms.csv")
dimeComms <- unique(select(filter(dimeComms,cycle == 2004,grepl("federal",seat)),bonica.rid,name,FEC.ID,comtype,seat,district,Incum.Chall,recipient.cfscore))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("senate",seat), "S", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("house",seat), "H", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("president",seat), "P", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("527",seat), "527", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("party",name), "Y", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("C9",FEC.ID), "I", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "", "Y", comtype))
dimeComms <- dimeComms %>% filter(FEC.ID != "C00272732",name != "jones, ben l", name != "robinson, tommy", name != "alliance for the west", name != "freedom works non-federal pac")
dimeComms <- select(dimeComms,-seat,-FEC.ID)
dimeComms <- unique(dimeComms)
dimeComms$bonica.rid <- as.character(dimeComms$bonica.rid)

# merge committee types with contributor/recipient data
nrow(cont04)
cont04 <- left_join(cont04, dimeComms, by="bonica.rid")
nrow(cont04)
cont04 <- cont04 %>% filter(comtype != "527")
cont04 <- cont04 %>% mutate(recipient.party2 = ifelse(recipient.party == "100","D",ifelse(recipient.party =="200","R",recipient.party)))

# get close election data merge with cont04
results04 <- read.csv("/home/bmath/cont2014/data/final/close_races_2004.csv")
results04 <- select(results04,-X)
results04$district <- as.character(results04$district)
results04$recipient.party <- as.character(results04$recipient.party)
results04$closeP <- as.character(results04$closeP)
results04$closeG <- as.character(results04$closeG)

cont04_P <- cont04 %>% filter(election.type == "P")
cont04_P <- left_join(cont04_P,select(results04,district,recipient.party,closeP),by=c("district","recipient.party2" = "recipient.party"))
colnames(cont04_P)[31] <- "close.race"
cont04_G <- cont04 %>% filter(election.type == "G")
cont04_G <- left_join(cont04_G,select(results04,district,recipient.party,closeG),by=c("district","recipient.party2" = "recipient.party"))
colnames(cont04_G)[31] <- "close.race"
cont04_O <- cont04 %>% filter(election.type != "P" & election.type != "G")
cont04_O$close.race <- NA
cont04 <- bind_rows(cont04_P,cont04_G,cont04_O)
rm(cont04_P,cont04_G,cont04_O,results04,dimeComms)

#total # of contributions form each contributor
individuals <- cont04 %>% group_by(bonica.cid) %>% summarize(contCount = n())
individuals$bonica.cid <- as.character(individuals$bonica.cid )

#cf for contrinbutor
individuals <- left_join(individuals,unique(select(cont04,bonica.cid,contributor.cfscore)),by='bonica.cid')
colnames(individuals) <- c('bonica.cid','contCount','contCF')

# average cf of contributor's recipeint committees
individuals <- left_join(individuals,(unique(select(filter(cont04,!is.na(recipient.cfscore)),bonica.cid,recipient.cfscore))) %>% group_by(bonica.cid) %>% summarize(recipMeanCF = mean(recipient.cfscore)),by='bonica.cid')

# variance cf of contributor's recipeint committees
individuals <- left_join(individuals,(unique(select(filter(cont04,!is.na(recipient.cfscore)),bonica.cid,recipient.cfscore))) %>% group_by(bonica.cid) %>% summarize(recipVarCF = var(recipient.cfscore)),by='bonica.cid')

# Difference Contributor CF, Recipient CF
individuals <- left_join(individuals,select(mutate(individuals, diffCF = contCF - recipMeanCF),bonica.cid,diffCF), by='bonica.cid')

# CF Distance from 0
individuals <- individuals %>% mutate(fromZeroCF = abs(0 - contCF))

#total $ contributions form each contributor
individuals <- left_join(individuals,cont04 %>% group_by(bonica.cid) %>% summarize(contTotal = sum(amount)))
individuals <- individuals %>% filter(contTotal > 0)

#max $ of contribution from individual
individuals <- left_join(individuals,cont04 %>% group_by(bonica.cid) %>% summarize(maxCont = max(amount)))

#average $ of contributions from invidual
individuals <- left_join(individuals,cont04 %>% group_by(bonica.cid) %>% summarize(avgCont = mean(amount)))

#unique # of commmittees
individuals <-  left_join(individuals,((unique(select(cont04,bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.rid))) %>% summarize(commCount = sum(count))),by="bonica.cid")

#unique # of candidates
individuals <-  left_join(individuals,((unique(select(filter(cont04,comtype == "H" | comtype == "S" | comtype == "P"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.rid))) %>% group_by(bonica.cid) %>% summarize(candCount = sum(count))),by="bonica.cid")

#total $ of candidate contributions
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(contCandTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# candidate % of total contributions
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contCandPerc = contCandTotal/ contTotal)

#max $ of cand contribution from individual
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(maxCandCont = max(amount))))

#average $ cand contributions from invidual
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(avgCandCont = mean(amount))))

#average $ cand contributions from invidual per candidate
individuals$avgCandContPerCand <- 0
individuals$avgCandContPerCand <- ifelse(individuals$candCount != 0, individuals$contCandTotal / individuals$candCount, individuals$avgCandContPerCand)

# amount and % of contributions to Incumbent Candidates
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I") %>% group_by(bonica.cid) %>% summarize(contIncTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contIncPerc = contIncTotal/ contCandTotal)

# amount and % of contributions to Incumbent Candidates in Close Races
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contIncCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Incumbent Candidates in Not Close Races
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contIncNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Challenger Candidates
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C") %>% group_by(bonica.cid) %>% summarize(contChalTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contChalPerc = contChalTotal/ contCandTotal)

# amount and % of contributions to Challenger Candidates in Close Races
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contChalCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Challenger Candidates in Close Races
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contChalNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O") %>% group_by(bonica.cid) %>% summarize(contOpenTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contOpenPerc = contOpenTotal/ contCandTotal)
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates in Close Races
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contOpenCloseTotal =  sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates in Not Close Races
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contOpenNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#unique # of IE Comms
individuals <-  left_join(individuals,((unique(select(filter(cont04,comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(ieCount = sum(count))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#total $ of IE contributions

individuals <- left_join(individuals,(cont04 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(contIETotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contIEPerc = contIETotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of IE contribution from individual
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(maxIECont = max(amount))))

#average $ of IE contributions from invidual
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(avgIECont = mean(amount))))

#average $ IE contributions from invidual per IE comm
individuals$avgIEContPerIE <- 0
individuals$avgIEContPerIE <- ifelse(individuals$ieCount != 0, individuals$contIETotal / individuals$ieCount, individuals$avgIEContPerIE)

#unique # of PAC Comms
individuals <-  left_join(individuals,((unique(select(filter(cont04,comtype == "Q"| comtype == "N"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(pacCount = sum(count))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#total $ of PAC contributions

individuals <- left_join(individuals,(cont04 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(contPACTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contPACPerc = contPACTotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of PAC contribution from individual
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(maxPACCont = max(amount))))

#average $ PAC contributions from individual
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(avgPACCont = mean(amount))))

#average $ PAC contributions from individual per PAC
individuals$avgPACContPerPAC <- 0
individuals$avgPACContPerPAC <- ifelse(individuals$pacCount != 0, individuals$contPACTotal / individuals$pacCount, individuals$avgPACContPerPAC)

#unique # of pty Comms
individuals <-  left_join(individuals,((unique(select(filter(cont04,comtype == "X"| comtype == "Y"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(ptyCount = sum(count))),by="bonica.cid")

#total $ of Pty contributions

individuals <- left_join(individuals,(cont04 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(contPtyTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contPtyPerc = contPtyTotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of pty contribution from individual
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(maxPtyCont = max(amount))))

#average $ pty contributions from invidual
individuals <- left_join(individuals,(cont04 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(avgPtyCont = mean(amount))))
individuals[is.na(individuals)] <- 0

#average $ pty contributions from invidual per pty
individuals$avgPtyContPerPty <- 0
individuals$avgPtyContPerPty <- ifelse(individuals$ptyCount != 0, individuals$contPtyTotal / individuals$ptyCount, individuals$avgPtyContPerPty)

write.csv(individuals,"/home/bmath/cont2014/data/final/individuals04.csv")

# 2006 contribution data --------------------------------------------------

# Load Contributions
cont <- fread("/home/bmath/cont2014/data/contribDB_2006.csv",colClasses=c(bonica.cid="character"))
cont <- select(cont, -V1)

# filter for individual contributors, to federal candidates and committees, remove corporations, remove transactions (candidate loans, )
cont06 <- cont %>% filter(contributor.type == "I") # select only individuals
cont06 <- cont06 %>% filter(grepl("federal",seat)) # select only federal candidate recipients
cont06 <- cont06 %>% filter(is.corp != "corp") # remove corporate contributions
cont06 <- cont06 %>% filter(efec.form.type !="SA17D",efec.form.type !="SA17d",efec.form.type !="SA11D",efec.form.type != "SA11d",efec.form.type != "SA13A",efec.form.type != "SA13a") # remove non-applicable form-types
cont06 <- cont06 %>% filter(transaction.type != "15C",transaction.type != "16C") #remove contributions/loans from the candidate
cont06 <- select(cont06,-transaction.id,-contributor.lname,-contributor.fname,-contributor.mname,-contributor.suffix, -contributor.title,-contributor.ffname,-is.corp,-latitude,-longitude,-contributor.type,-gis.confidence,-contributor.district.90s,-contributor.district.00s,-censustract,-efec.memo,-efec.memo2,-efec.transaction.id.orig,-bk.ref.transaction.id,-efec.org.orig,-efec.comid.orig,-excluded.from.scaling)

# Load the DIME committee list
dimeComms <- fread("/home/bmath/cont2014/data/dime_comms.csv")
dimeComms <- unique(select(filter(dimeComms,cycle == 2006,grepl("federal",seat)),bonica.rid,name,FEC.ID,comtype,seat,district,Incum.Chall,recipient.cfscore))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("senate",seat), "S", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("house",seat), "H", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("president",seat), "P", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("527",seat), "527", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "", "Y", comtype))
dimeComms <- dimeComms %>% filter(FEC.ID != "C00272732", FEC.ID != "C00305425",  FEC.ID != "C00392282",FEC.ID != "C00430512", name != "hilleary, w van", name != "warner, mark")
dimeComms <- select(dimeComms,-seat,-FEC.ID)
dimeComms <- unique(dimeComms)
dimeComms$bonica.rid <- as.character(dimeComms$bonica.rid)

# merge committee types with contributor/recipient data
nrow(cont06)
cont06 <- left_join(cont06, dimeComms, by="bonica.rid")
nrow(cont06)
cont06 <- cont06 %>% filter(comtype != "527")
cont06 <- cont06 %>% mutate(recipient.party2 = ifelse(recipient.party == "100","D",ifelse(recipient.party =="200","R",recipient.party)))

# get close election data merge with cont06
results06 <- read.csv("/home/bmath/cont2014/data/final/close_races_2006.csv")
results06 <- select(results06,-X)
results06$district <- as.character(results06$district)
results06$recipient.party <- as.character(results06$recipient.party)
results06$closeP <- as.character(results06$closeP)
results06$closeG <- as.character(results06$closeG)

cont06_P <- cont06 %>% filter(election.type == "P")
cont06_P <- left_join(cont06_P,select(results06,district,recipient.party,closeP),by=c("district","recipient.party2" = "recipient.party"))
colnames(cont06_P)[31] <- "close.race"
cont06_G <- cont06 %>% filter(election.type == "G")
cont06_G <- left_join(cont06_G,select(results06,district,recipient.party,closeG),by=c("district","recipient.party2" = "recipient.party"))
colnames(cont06_G)[31] <- "close.race"
cont06_O <- cont06 %>% filter(election.type != "P" & election.type != "G")
cont06_O$close.race <- NA
cont06 <- bind_rows(cont06_P,cont06_G,cont06_O)
rm(cont06_P,cont06_G,cont06_O,results06,dimeComms)

#total # of contributions form each contributor
individuals <- cont06 %>% group_by(bonica.cid) %>% summarize(contCount = n())
individuals$bonica.cid <- as.character(individuals$bonica.cid )

#cf for contrinbutor
individuals <- left_join(individuals,unique(select(cont06,bonica.cid,contributor.cfscore)),by='bonica.cid')
colnames(individuals) <- c('bonica.cid','contCount','contCF')

# average cf of contributor's recipeint committees
individuals <- left_join(individuals,(unique(select(filter(cont06,!is.na(recipient.cfscore)),bonica.cid,recipient.cfscore))) %>% group_by(bonica.cid) %>% summarize(recipMeanCF = mean(recipient.cfscore)),by='bonica.cid')

# variance cf of contributor's recipeint committees
individuals <- left_join(individuals,(unique(select(filter(cont06,!is.na(recipient.cfscore)),bonica.cid,recipient.cfscore))) %>% group_by(bonica.cid) %>% summarize(recipVarCF = var(recipient.cfscore)),by='bonica.cid')

# Difference Contributor CF, Recipient CF
individuals <- left_join(individuals,select(mutate(individuals, diffCF = contCF - recipMeanCF),bonica.cid,diffCF), by='bonica.cid')

# CF Distance from 0
individuals <- individuals %>% mutate(fromZeroCF = abs(0 - contCF))

#total $ contributions form each contributor
individuals <- left_join(individuals,cont06 %>% group_by(bonica.cid) %>% summarize(contTotal = sum(amount)))
individuals <- individuals %>% filter(contTotal > 0)

#max $ of contribution from individual
individuals <- left_join(individuals,cont06 %>% group_by(bonica.cid) %>% summarize(maxCont = max(amount)))

#average $ of contributions from invidual
individuals <- left_join(individuals,cont06 %>% group_by(bonica.cid) %>% summarize(avgCont = mean(amount)))

#unique # of commmittees
individuals <-  left_join(individuals,((unique(select(cont06,bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.rid))) %>% summarize(commCount = sum(count))),by="bonica.cid")

#unique # of candidates
individuals <-  left_join(individuals,((unique(select(filter(cont06,comtype == "H" | comtype == "S" | comtype == "P"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.rid))) %>% group_by(bonica.cid) %>% summarize(candCount = sum(count))),by="bonica.cid")

#total $ of candidate contributions
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(contCandTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# candidate % of total contributions
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contCandPerc = contCandTotal/ contTotal)

#max $ of cand contribution from individual
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(maxCandCont = max(amount))))

#average $ cand contributions from invidual
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(avgCandCont = mean(amount))))

#average $ cand contributions from invidual per candidate
individuals$avgCandContPerCand <- 0
individuals$avgCandContPerCand <- ifelse(individuals$candCount != 0, individuals$contCandTotal / individuals$candCount, individuals$avgCandContPerCand)

# amount and % of contributions to Incumbent Candidates
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I") %>% group_by(bonica.cid) %>% summarize(contIncTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contIncPerc = contIncTotal/ contCandTotal)

# amount and % of contributions to Incumbent Candidates in Close Races
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contIncCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Incumbent Candidates in Not Close Races
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contIncNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Challenger Candidates
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C") %>% group_by(bonica.cid) %>% summarize(contChalTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contChalPerc = contChalTotal/ contCandTotal)

# amount and % of contributions to Challenger Candidates in Close Races
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contChalCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Challenger Candidates in Close Races
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contChalNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O") %>% group_by(bonica.cid) %>% summarize(contOpenTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contOpenPerc = contOpenTotal/ contCandTotal)
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates in Close Races
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contOpenCloseTotal =  sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates in Not Close Races
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contOpenNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#unique # of IE Comms
individuals <-  left_join(individuals,((unique(select(filter(cont06,comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(ieCount = sum(count))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#total $ of IE contributions

individuals <- left_join(individuals,(cont06 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(contIETotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contIEPerc = contIETotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of IE contribution from individual
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(maxIECont = max(amount))))

#average $ of IE contributions from invidual
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(avgIECont = mean(amount))))

#average $ IE contributions from invidual per IE comm
individuals$avgIEContPerIE <- 0
individuals$avgIEContPerIE <- ifelse(individuals$ieCount != 0, individuals$contIETotal / individuals$ieCount, individuals$avgIEContPerIE)

#unique # of PAC Comms
individuals <-  left_join(individuals,((unique(select(filter(cont06,comtype == "Q"| comtype == "N"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(pacCount = sum(count))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#total $ of PAC contributions

individuals <- left_join(individuals,(cont06 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(contPACTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contPACPerc = contPACTotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of PAC contribution from individual
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(maxPACCont = max(amount))))

#average $ PAC contributions from individual
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(avgPACCont = mean(amount))))

#average $ PAC contributions from individual per PAC
individuals$avgPACContPerPAC <- 0
individuals$avgPACContPerPAC <- ifelse(individuals$pacCount != 0, individuals$contPACTotal / individuals$pacCount, individuals$avgPACContPerPAC)

#unique # of pty Comms
individuals <-  left_join(individuals,((unique(select(filter(cont06,comtype == "X"| comtype == "Y"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(ptyCount = sum(count))),by="bonica.cid")

#total $ of Pty contributions

individuals <- left_join(individuals,(cont06 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(contPtyTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contPtyPerc = contPtyTotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of pty contribution from individual
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(maxPtyCont = max(amount))))

#average $ pty contributions from invidual
individuals <- left_join(individuals,(cont06 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(avgPtyCont = mean(amount))))
individuals[is.na(individuals)] <- 0

#average $ pty contributions from invidual per pty
individuals$avgPtyContPerPty <- 0
individuals$avgPtyContPerPty <- ifelse(individuals$ptyCount != 0, individuals$contPtyTotal / individuals$ptyCount, individuals$avgPtyContPerPty)

write.csv(individuals,"/home/bmath/cont2014/data/final/individuals06.csv")

# 2008 contribution data --------------------------------------------------

# Load Contributions
cont <- read.csv("/home/bmath/cont2014/data/contribDB_2008.csv",colClasses=c(bonica.cid="character"))
cont <- select(cont, -V1)

# filter for individual contributors, to federal candidates and committees, remove corporations, remove transactions (candidate loans, )
cont08 <- cont %>% filter(contributor.type == "I") # select only individuals
cont08 <- cont08 %>% filter(grepl("federal",seat)) # select only federal candidate recipients
cont08 <- cont08 %>% filter(is.corp != "corp") # remove corporate contributions
cont08 <- cont08 %>% filter(efec.form.type !="SA17D",efec.form.type !="SA17d",efec.form.type !="SA11D",efec.form.type != "SA11d",efec.form.type != "SA13A",efec.form.type != "SA13a") # remove non-applicable form-types
cont08 <- cont08 %>% filter(transaction.type != "15C",transaction.type != "16C") #remove contributions/loans from the candidate
cont08 <- select(cont08,-transaction.id,-contributor.lname,-contributor.fname,-contributor.mname,-contributor.suffix, -contributor.title,-contributor.ffname,-is.corp,-latitude,-longitude,-contributor.type,-gis.confidence,-contributor.district.90s,-contributor.district.00s,-censustract,-efec.memo,-efec.memo2,-efec.transaction.id.orig,-bk.ref.transaction.id,-efec.org.orig,-efec.comid.orig,-excluded.from.scaling)

# Load the DIME committee list
dimeComms <- fread("/home/bmath/cont2014/data/dime_comms.csv")
dimeComms <- unique(select(filter(dimeComms,cycle == 2008,grepl("federal",seat)),bonica.rid,name,FEC.ID,comtype,seat,district,Incum.Chall,recipient.cfscore))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("senate",seat), "S", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("house",seat), "H", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("president",seat), "P", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("527",seat), "527", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("party",name), "Y", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("C9",FEC.ID), "I", comtype))
dimeComms <- dimeComms %>% filter(FEC.ID != "C00285866",FEC.ID != "C00481473", FEC.ID != "C00335521",name != "lummis for congress", name != "larocco, larry", name != "hilleary, w van", name != "andrews, camille spinello", name != "shows, ronnie", name != "friends of fred thompson",name != "richardson, bill",name != "slattery, jim",bonica.rid != "cand55903")
dimeComms <- dimeComms[-3401,]
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "", "Y", comtype))
dimeComms <- select(dimeComms,-seat,-FEC.ID)
dimeComms <- unique(dimeComms)
dimeComms$bonica.rid <- as.character(dimeComms$bonica.rid)

# merge committee types with contributor/recipient data
nrow(cont08)
cont08 <- left_join(cont08, dimeComms, by="bonica.rid")
nrow(cont08)
cont08 <- cont08 %>% filter(comtype != "527")
cont08 <- cont08 %>% mutate(recipient.party2 = ifelse(recipient.party == "100","D",ifelse(recipient.party =="200","R",recipient.party)))

# get close election data merge with cont08
results08 <- read.csv("/home/bmath/cont2014/data/final/close_races_2008.csv")
results08 <- select(results08,-X)
results08$district <- as.character(results08$district)
results08$recipient.party <- as.character(results08$recipient.party)
results08$closeP <- as.character(results08$closeP)
results08$closeG <- as.character(results08$closeG)

cont08_P <- cont08 %>% filter(election.type == "P")
cont08_P <- left_join(cont08_P,select(results08,district,recipient.party,closeP),by=c("district","recipient.party2" = "recipient.party"))
colnames(cont08_P)[31] <- "close.race"
cont08_G <- cont08 %>% filter(election.type == "G")
cont08_G <- left_join(cont08_G,select(results08,district,recipient.party,closeG),by=c("district","recipient.party2" = "recipient.party"))
colnames(cont08_G)[31] <- "close.race"
cont08_O <- cont08 %>% filter(election.type != "P" & election.type != "G")
cont08_O$close.race <- NA
cont08 <- bind_rows(cont08_P,cont08_G,cont08_O)
rm(cont08_P,cont08_G,cont08_O,results08,dimeComms)

#total # of contributions form each contributor
individuals <- cont08 %>% group_by(bonica.cid) %>% summarize(contCount = n())
individuals$bonica.cid <- as.character(individuals$bonica.cid )

#cf for contrinbutor
individuals <- left_join(individuals,unique(select(cont08,bonica.cid,contributor.cfscore)),by='bonica.cid')
colnames(individuals) <- c('bonica.cid','contCount','contCF')

# average cf of contributor's recipeint committees
individuals <- left_join(individuals,(unique(select(filter(cont08,!is.na(recipient.cfscore)),bonica.cid,recipient.cfscore))) %>% group_by(bonica.cid) %>% summarize(recipMeanCF = mean(recipient.cfscore)),by='bonica.cid')

# variance cf of contributor's recipeint committees
individuals <- left_join(individuals,(unique(select(filter(cont08,!is.na(recipient.cfscore)),bonica.cid,recipient.cfscore))) %>% group_by(bonica.cid) %>% summarize(recipVarCF = var(recipient.cfscore)),by='bonica.cid')

# Difference Contributor CF, Recipient CF
individuals <- left_join(individuals,select(mutate(individuals, diffCF = contCF - recipMeanCF),bonica.cid,diffCF), by='bonica.cid')

# CF Distance from 0
individuals <- individuals %>% mutate(fromZeroCF = abs(0 - contCF))

#total $ contributions form each contributor
individuals <- left_join(individuals,cont08 %>% group_by(bonica.cid) %>% summarize(contTotal = sum(amount)))
individuals <- individuals %>% filter(contTotal > 0)

#max $ of contribution from individual
individuals <- left_join(individuals,cont08 %>% group_by(bonica.cid) %>% summarize(maxCont = max(amount)))

#average $ of contributions from invidual
individuals <- left_join(individuals,cont08 %>% group_by(bonica.cid) %>% summarize(avgCont = mean(amount)))

#unique # of commmittees
individuals <-  left_join(individuals,((unique(select(cont08,bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.rid))) %>% summarize(commCount = sum(count))),by="bonica.cid")

#unique # of candidates
individuals <-  left_join(individuals,((unique(select(filter(cont08,comtype == "H" | comtype == "S" | comtype == "P"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.rid))) %>% group_by(bonica.cid) %>% summarize(candCount = sum(count))),by="bonica.cid")

#total $ of candidate contributions
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(contCandTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# candidate % of total contributions
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contCandPerc = contCandTotal/ contTotal)

#max $ of cand contribution from individual
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(maxCandCont = max(amount))))

#average $ cand contributions from invidual
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(avgCandCont = mean(amount))))

#average $ cand contributions from invidual per candidate
individuals$avgCandContPerCand <- 0
individuals$avgCandContPerCand <- ifelse(individuals$candCount != 0, individuals$contCandTotal / individuals$candCount, individuals$avgCandContPerCand)

# amount and % of contributions to Incumbent Candidates
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I") %>% group_by(bonica.cid) %>% summarize(contIncTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contIncPerc = contIncTotal/ contCandTotal)

# amount and % of contributions to Incumbent Candidates in Close Races
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contIncCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Incumbent Candidates in Not Close Races
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contIncNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Challenger Candidates
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C") %>% group_by(bonica.cid) %>% summarize(contChalTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contChalPerc = contChalTotal/ contCandTotal)

# amount and % of contributions to Challenger Candidates in Close Races
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contChalCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Challenger Candidates in Close Races
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contChalNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O") %>% group_by(bonica.cid) %>% summarize(contOpenTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contOpenPerc = contOpenTotal/ contCandTotal)
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates in Close Races
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contOpenCloseTotal =  sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates in Not Close Races
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contOpenNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#unique # of IE Comms
individuals <-  left_join(individuals,((unique(select(filter(cont08,comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(ieCount = sum(count))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#total $ of IE contributions

individuals <- left_join(individuals,(cont08 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(contIETotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contIEPerc = contIETotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of IE contribution from individual
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(maxIECont = max(amount))))

#average $ of IE contributions from invidual
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(avgIECont = mean(amount))))

#average $ IE contributions from invidual per IE comm
individuals$avgIEContPerIE <- 0
individuals$avgIEContPerIE <- ifelse(individuals$ieCount != 0, individuals$contIETotal / individuals$ieCount, individuals$avgIEContPerIE)

#unique # of PAC Comms
individuals <-  left_join(individuals,((unique(select(filter(cont08,comtype == "Q"| comtype == "N"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(pacCount = sum(count))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#total $ of PAC contributions

individuals <- left_join(individuals,(cont08 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(contPACTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contPACPerc = contPACTotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of PAC contribution from individual
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(maxPACCont = max(amount))))

#average $ PAC contributions from individual
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(avgPACCont = mean(amount))))

#average $ PAC contributions from individual per PAC
individuals$avgPACContPerPAC <- 0
individuals$avgPACContPerPAC <- ifelse(individuals$pacCount != 0, individuals$contPACTotal / individuals$pacCount, individuals$avgPACContPerPAC)

#unique # of pty Comms
individuals <-  left_join(individuals,((unique(select(filter(cont08,comtype == "X"| comtype == "Y"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(ptyCount = sum(count))),by="bonica.cid")

#total $ of Pty contributions

individuals <- left_join(individuals,(cont08 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(contPtyTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contPtyPerc = contPtyTotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of pty contribution from individual
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(maxPtyCont = max(amount))))

#average $ pty contributions from invidual
individuals <- left_join(individuals,(cont08 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(avgPtyCont = mean(amount))))
individuals[is.na(individuals)] <- 0

#average $ pty contributions from invidual per pty
individuals$avgPtyContPerPty <- 0
individuals$avgPtyContPerPty <- ifelse(individuals$ptyCount != 0, individuals$contPtyTotal / individuals$ptyCount, individuals$avgPtyContPerPty)

write.csv(individuals,"/home/bmath/cont2014/data/final/individuals08.csv")


# 2010 contribution data --------------------------------------------------

# Load Contributions
cont <- read.csv("/home/bmath/cont2014/data/contribDB_2010.csv",colClasses=c(bonica.cid="character"),fill=TRUE)
cont <- select(cont, -V1)

# filter for individual contributors, to federal candidates and committees, remove corporations, remove transactions (candidate loans, )
cont10 <- cont %>% filter(contributor.type == "I") # select only individuals
cont10 <- cont10 %>% filter(grepl("federal",seat)) # select only federal candidate recipients
cont10 <- cont10 %>% filter(is.corp != "corp") # remove corporate contributions
cont10 <- cont10 %>% filter(efec.form.type !="SA17D",efec.form.type !="SA17d",efec.form.type !="SA11D",efec.form.type != "SA11d",efec.form.type != "SA13A",efec.form.type != "SA13a") # remove non-applicable form-types
cont10 <- cont10 %>% filter(transaction.type != "15C",transaction.type != "16C") #remove contributions/loans from the candidate
cont10 <- select(cont10,-transaction.id,-contributor.lname,-contributor.fname,-contributor.mname,-contributor.suffix, -contributor.title,-contributor.ffname,-is.corp,-latitude,-longitude,-contributor.type,-gis.confidence,-contributor.district.90s,-contributor.district.00s,-censustract,-efec.memo,-efec.memo2,-efec.transaction.id.orig,-bk.ref.transaction.id,-efec.org.orig,-efec.comid.orig,-excluded.from.scaling)

# Load the DIME committee list
dimeComms <- fread("/home/bmath/cont2014/data/dime_comms.csv")
dimeComms <- unique(select(filter(dimeComms,cycle == 2010,grepl("federal",seat)),bonica.rid,name,FEC.ID,comtype,seat,district,Incum.Chall,recipient.cfscore))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("senate",seat), "S", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("house",seat), "H", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("president",seat), "P", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("527",seat), "527", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("party",name), "Y", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("C3",FEC.ID), "I", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "", "Y", comtype))
dimeComms <- dimeComms %>% filter(FEC.ID != "C00434795",FEC.ID != "C00381442", bonica.rid != "cand107189", bonica.rid != "cand1091", bonica.rid != "cand1107",bonica.rid != "cand1202", FEC.ID != "C00458646", name != "hilleary, w van", FEC.ID != "C00280206", FEC.ID != "C00479618", bonica.rid != "cand55903",  name != "richardson, bill", name != "slattery, jim", FEC.ID != "H0VA01185", name != "simmons, rob")
dimeComms <- select(dimeComms,-seat,-FEC.ID)
dimeComms <- unique(dimeComms)
dimeComms$bonica.rid <- as.character(dimeComms$bonica.rid)

# merge committee types with contributor/recipient data
nrow(cont10)
cont10 <- left_join(cont10, dimeComms, by="bonica.rid")
nrow(cont10)
cont10 <- cont10 %>% filter(comtype != "527")
cont10 <- cont10 %>% mutate(recipient.party2 = ifelse(recipient.party == "100","D",ifelse(recipient.party =="200","R",recipient.party)))

# get close election data merge with cont10
results10 <- read.csv("/home/bmath/cont2014/data/final/close_races_2010.csv")
results10 <- select(results10,-X)
results10$district <- as.character(results10$district)
results10$recipient.party <- as.character(results10$recipient.party)
results10$closeP <- as.character(results10$closeP)
results10$closeG <- as.character(results10$closeG)

cont10_P <- cont10 %>% filter(election.type == "P")
cont10_P <- left_join(cont10_P,select(results10,district,recipient.party,closeP),by=c("district","recipient.party2" = "recipient.party"))
colnames(cont10_P)[31] <- "close.race"
cont10_G <- cont10 %>% filter(election.type == "G")
cont10_G <- left_join(cont10_G,select(results10,district,recipient.party,closeG),by=c("district","recipient.party2" = "recipient.party"))
colnames(cont10_G)[31] <- "close.race"
cont10_O <- cont10 %>% filter(election.type != "P" & election.type != "G")
cont10_O$close.race <- NA
cont10 <- bind_rows(cont10_P,cont10_G,cont10_O)
rm(cont10_P,cont10_G,cont10_O,results10)

#total # of contributions form each contributor
individuals <- cont10 %>% group_by(bonica.cid) %>% summarize(contCount = n())
individuals$bonica.cid <- as.character(individuals$bonica.cid )

#cf for contrinbutor
individuals <- left_join(individuals,unique(select(cont10,bonica.cid,contributor.cfscore)),by='bonica.cid')
colnames(individuals) <- c('bonica.cid','contCount','contCF')

# average cf of contributor's recipeint committees
individuals <- left_join(individuals,(unique(select(filter(cont10,!is.na(recipient.cfscore)),bonica.cid,recipient.cfscore))) %>% group_by(bonica.cid) %>% summarize(recipMeanCF = mean(recipient.cfscore)),by='bonica.cid')

# variance cf of contributor's recipeint committees
individuals <- left_join(individuals,(unique(select(filter(cont10,!is.na(recipient.cfscore)),bonica.cid,recipient.cfscore))) %>% group_by(bonica.cid) %>% summarize(recipVarCF = var(recipient.cfscore)),by='bonica.cid')

# Difference Contributor CF, Recipient CF
individuals <- left_join(individuals,select(mutate(individuals, diffCF = contCF - recipMeanCF),bonica.cid,diffCF), by='bonica.cid')

# CF Distance from 0
individuals <- individuals %>% mutate(fromZeroCF = abs(0 - contCF))

#total $ contributions form each contributor
individuals <- left_join(individuals,cont10 %>% group_by(bonica.cid) %>% summarize(contTotal = sum(amount)))
individuals <- individuals %>% filter(contTotal > 0)

#max $ of contribution from individual
individuals <- left_join(individuals,cont10 %>% group_by(bonica.cid) %>% summarize(maxCont = max(amount)))

#average $ of contributions from invidual
individuals <- left_join(individuals,cont10 %>% group_by(bonica.cid) %>% summarize(avgCont = mean(amount)))

#unique # of commmittees
individuals <-  left_join(individuals,((unique(select(cont10,bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.rid))) %>% summarize(commCount = sum(count))),by="bonica.cid")

#unique # of candidates
individuals <-  left_join(individuals,((unique(select(filter(cont10,comtype == "H" | comtype == "S" | comtype == "P"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.rid))) %>% group_by(bonica.cid) %>% summarize(candCount = sum(count))),by="bonica.cid")

#total $ of candidate contributions
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(contCandTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# candidate % of total contributions
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contCandPerc = contCandTotal/ contTotal)

#max $ of cand contribution from individual
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(maxCandCont = max(amount))))

#average $ cand contributions from invidual
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(avgCandCont = mean(amount))))

#average $ cand contributions from invidual per candidate
individuals$avgCandContPerCand <- 0
individuals$avgCandContPerCand <- ifelse(individuals$candCount != 0, individuals$contCandTotal / individuals$candCount, individuals$avgCandContPerCand)

# amount and % of contributions to Incumbent Candidates
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I") %>% group_by(bonica.cid) %>% summarize(contIncTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contIncPerc = contIncTotal/ contCandTotal)

# amount and % of contributions to Incumbent Candidates in Close Races
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contIncCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Incumbent Candidates in Not Close Races
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contIncNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Challenger Candidates
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C") %>% group_by(bonica.cid) %>% summarize(contChalTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contChalPerc = contChalTotal/ contCandTotal)

# amount and % of contributions to Challenger Candidates in Close Races
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contChalCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Challenger Candidates in Close Races
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contChalNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O") %>% group_by(bonica.cid) %>% summarize(contOpenTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contOpenPerc = contOpenTotal/ contCandTotal)
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates in Close Races
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contOpenCloseTotal =  sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates in Not Close Races
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contOpenNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#unique # of IE Comms
individuals <-  left_join(individuals,((unique(select(filter(cont10,comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(ieCount = sum(count))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#total $ of IE contributions

individuals <- left_join(individuals,(cont10 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(contIETotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contIEPerc = contIETotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of IE contribution from individual
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(maxIECont = max(amount))))

#average $ of IE contributions from invidual
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(avgIECont = mean(amount))))

#average $ IE contributions from invidual per IE comm
individuals$avgIEContPerIE <- 0
individuals$avgIEContPerIE <- ifelse(individuals$ieCount != 0, individuals$contIETotal / individuals$ieCount, individuals$avgIEContPerIE)

#unique # of PAC Comms
individuals <-  left_join(individuals,((unique(select(filter(cont10,comtype == "Q"| comtype == "N"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(pacCount = sum(count))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#total $ of PAC contributions

individuals <- left_join(individuals,(cont10 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(contPACTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contPACPerc = contPACTotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of PAC contribution from individual
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(maxPACCont = max(amount))))

#average $ PAC contributions from individual
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(avgPACCont = mean(amount))))

#average $ PAC contributions from individual per PAC
individuals$avgPACContPerPAC <- 0
individuals$avgPACContPerPAC <- ifelse(individuals$pacCount != 0, individuals$contPACTotal / individuals$pacCount, individuals$avgPACContPerPAC)

#unique # of pty Comms
individuals <-  left_join(individuals,((unique(select(filter(cont10,comtype == "X"| comtype == "Y"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(ptyCount = sum(count))),by="bonica.cid")

#total $ of Pty contributions

individuals <- left_join(individuals,(cont10 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(contPtyTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contPtyPerc = contPtyTotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of pty contribution from individual
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(maxPtyCont = max(amount))))

#average $ pty contributions from invidual
individuals <- left_join(individuals,(cont10 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(avgPtyCont = mean(amount))))
individuals[is.na(individuals)] <- 0

#average $ pty contributions from invidual per pty
individuals$avgPtyContPerPty <- 0
individuals$avgPtyContPerPty <- ifelse(individuals$ptyCount != 0, individuals$contPtyTotal / individuals$ptyCount, individuals$avgPtyContPerPty)

write.csv(individuals,"/home/bmath/cont2014/data/final/individuals10.csv")


# 2012 contribution data --------------------------------------------------

# Load Contributions
cont <- read.csv("/home/bmath/cont2014/data/contribDB_2012.csv",colClasses=c(bonica.cid="character"))
cont <- select(cont, -V1)

# filter for individual contributors, to federal candidates and committees, remove corporations, remove transactions (candidate loans, )
cont12 <- cont %>% filter(contributor.type == "I") # select only individuals
cont12 <- cont12 %>% filter(grepl("federal",seat)) # select only federal candidate recipients
cont12 <- cont12 %>% filter(is.corp != "corp") # remove corporate contributions
cont12 <- cont12 %>% filter(efec.form.type !="SA17D",efec.form.type !="SA17d",efec.form.type !="SA11D",efec.form.type != "SA11d",efec.form.type != "SA13A",efec.form.type != "SA13a") # remove non-applicable form-types
cont12 <- cont12 %>% filter(transaction.type != "15C",transaction.type != "16C") #remove contributions/loans from the candidate
cont12 <- select(cont12,-transaction.id,-contributor.lname,-contributor.fname,-contributor.mname,-contributor.suffix, -contributor.title,-contributor.ffname,-is.corp,-latitude,-longitude,-contributor.type,-gis.confidence,-contributor.district.90s,-contributor.district.00s,-censustract,-efec.memo,-efec.memo2,-efec.transaction.id.orig,-bk.ref.transaction.id,-efec.org.orig,-efec.comid.orig,-excluded.from.scaling)

# Load the DIME committee list
dimeComms <- fread("/home/bmath/cont2014/data/dime_comms.csv")
dimeComms <- unique(select(filter(dimeComms,cycle == 2012,grepl("federal",seat)),bonica.rid,name,FEC.ID,comtype,seat,district,Incum.Chall,recipient.cfscore))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("senate",seat), "S", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("house",seat), "H", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("president",seat), "P", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("527",seat), "527", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("party",name), "Y", comtype))
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "" & grepl("C9",FEC.ID), "I", comtype))
dimeComms <- dimeComms %>% filter(bonica.rid != "comm68830412")
dimeComms <- dimeComms %>% mutate(comtype = ifelse(comtype == "", "Y", comtype))
dimeComms <- dimeComms %>% filter(bonica.rid != "cand103953", bonica.rid != "cand103893", bonica.rid != "cand104386",bonica.rid != "cand104502",bonica.rid != "cand104528", bonica.rid != "cand104809",bonica.rid != "cand104910",bonica.rid != "cand105037", name != "heinrich, martin trevor", bonica.rid != "cand107189",bonica.rid != "cand1091",bonica.rid != "cand1150", bonica.rid != "cand1161",bonica.rid !="cand1184",name != "santorum, richard j",FEC.ID != "C00458646",FEC.ID !="C00440289",name != "portman, rob", name != "hayworth, jd",bonica.rid != "cand136065", name != "stockman, steve", name != "cook, merrill a", bonica.rid != "cand23468",bonica.rid != "cand35387",bonica.rid != "cand47367", FEC.ID !="C00524637",name != "flake, jeffry lane",name != "simmons, robert r", name != "rehberg, dennis ray", FEC.ID !="C00499186", name != "murphy, christopher s", name !="donnelly, joseph s",bonica.rid != "cand98958", FEC.ID != "C00494229",bonica.rid != "cand9923")
dimeComms <- dimeComms[-14852,]
dimeComms <- dimeComms[-14853,]
dimeComms <- dimeComms[-14855,]
dimeComms <- dimeComms[-14855,]
dimeComms <- dimeComms[-14854,]
dimeComms <- dimeComms[-14853,]
dimeComms <- dimeComms[-14852,]
dimeComms <- select(dimeComms,-seat,-FEC.ID)
dimeComms <- unique(dimeComms)
dimeComms$bonica.rid <- as.character(dimeComms$bonica.rid)

# merge committee types with contributor/recipient data
nrow(cont12)
cont12 <- left_join(cont12, dimeComms, by="bonica.rid")
nrow(cont12)
cont12 <- cont12 %>% filter(comtype != "527")
cont12 <- cont12 %>% mutate(recipient.party2 = ifelse(recipient.party == "100","D",ifelse(recipient.party =="200","R",recipient.party)))

# get close election data merge with cont12
results12 <- read.csv("/home/bmath/cont2014/data/final/close_races_2012.csv")
results12 <- select(results12,-X)
results12$district <- as.character(results12$district)
results12$recipient.party <- as.character(results12$recipient.party)
results12$closeP <- as.character(results12$closeP)
results12$closeG <- as.character(results12$closeG)

cont12_P <- cont12 %>% filter(election.type == "P")
cont12_P <- left_join(cont12_P,select(results12,district,recipient.party,closeP),by=c("district","recipient.party2" = "recipient.party"))
colnames(cont12_P)[31] <- "close.race"
cont12_G <- cont12 %>% filter(election.type == "G")
cont12_G <- left_join(cont12_G,select(results12,district,recipient.party,closeG),by=c("district","recipient.party2" = "recipient.party"))
colnames(cont12_G)[31] <- "close.race"
cont12_O <- cont12 %>% filter(election.type != "P" & election.type != "G")
cont12_O$close.race <- NA
cont12 <- bind_rows(cont12_P,cont12_G,cont12_O)
rm(cont12_P,cont12_G,cont12_O)

#total # of contributions form each contributor
individuals <- cont12 %>% group_by(bonica.cid) %>% summarize(contCount = n())
individuals$bonica.cid <- as.character(individuals$bonica.cid )

#cf for contrinbutor
individuals <- left_join(individuals,unique(select(cont12,bonica.cid,contributor.cfscore)),by='bonica.cid')
colnames(individuals) <- c('bonica.cid','contCount','contCF')

# average cf of contributor's recipeint committees
individuals <- left_join(individuals,(unique(select(filter(cont12,!is.na(recipient.cfscore)),bonica.cid,recipient.cfscore))) %>% group_by(bonica.cid) %>% summarize(recipMeanCF = mean(recipient.cfscore)),by='bonica.cid')

# variance cf of contributor's recipeint committees
individuals <- left_join(individuals,(unique(select(filter(cont12,!is.na(recipient.cfscore)),bonica.cid,recipient.cfscore))) %>% group_by(bonica.cid) %>% summarize(recipVarCF = var(recipient.cfscore)),by='bonica.cid')

# Difference Contributor CF, Recipient CF
individuals <- left_join(individuals,select(mutate(individuals, diffCF = contCF - recipMeanCF),bonica.cid,diffCF), by='bonica.cid')

# CF Distance from 0
individuals <- individuals %>% mutate(fromZeroCF = abs(0 - contCF))

#total $ contributions form each contributor
individuals <- left_join(individuals,cont12 %>% group_by(bonica.cid) %>% summarize(contTotal = sum(amount)))
individuals <- individuals %>% filter(contTotal > 0)

#max $ of contribution from individual
individuals <- left_join(individuals,cont12 %>% group_by(bonica.cid) %>% summarize(maxCont = max(amount)))

#average $ of contributions from invidual
individuals <- left_join(individuals,cont12 %>% group_by(bonica.cid) %>% summarize(avgCont = mean(amount)))

#unique # of commmittees
individuals <-  left_join(individuals,((unique(select(cont12,bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.rid))) %>% summarize(commCount = sum(count))),by="bonica.cid")

#unique # of candidates
individuals <-  left_join(individuals,((unique(select(filter(cont12,comtype == "H" | comtype == "S" | comtype == "P"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.rid))) %>% group_by(bonica.cid) %>% summarize(candCount = sum(count))),by="bonica.cid")

#total $ of candidate contributions
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(contCandTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# candidate % of total contributions
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contCandPerc = contCandTotal/ contTotal)

#max $ of cand contribution from individual
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(maxCandCont = max(amount))))

#average $ cand contributions from invidual
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P") %>% group_by(bonica.cid) %>% summarize(avgCandCont = mean(amount))))

#average $ cand contributions from invidual per candidate
individuals$avgCandContPerCand <- 0
individuals$avgCandContPerCand <- ifelse(individuals$candCount != 0, individuals$contCandTotal / individuals$candCount, individuals$avgCandContPerCand)

# amount and % of contributions to Incumbent Candidates
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I") %>% group_by(bonica.cid) %>% summarize(contIncTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contIncPerc = contIncTotal/ contCandTotal)

# amount and % of contributions to Incumbent Candidates in Close Races
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contIncCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Incumbent Candidates in Not Close Races
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "I",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contIncNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Challenger Candidates
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C") %>% group_by(bonica.cid) %>% summarize(contChalTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contChalPerc = contChalTotal/ contCandTotal)

# amount and % of contributions to Challenger Candidates in Close Races
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contChalCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Challenger Candidates in Close Races
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "C",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contChalNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O") %>% group_by(bonica.cid) %>% summarize(contOpenTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contOpenPerc = contOpenTotal/ contCandTotal)
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates in Close Races
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O",close.race == "Y") %>% group_by(bonica.cid) %>% summarize(contOpenCloseTotal =  sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

# amount and % of contributions to Open Candidates in Not Close Races
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "H" | comtype == "S" | comtype == "P", Incum.Chall == "O",close.race == "N") %>% group_by(bonica.cid) %>% summarize(contOpenNotCloseTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#unique # of IE Comms
individuals <-  left_join(individuals,((unique(select(filter(cont12,comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(ieCount = sum(count))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#total $ of IE contributions

individuals <- left_join(individuals,(cont12 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(contIETotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contIEPerc = contIETotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of IE contribution from individual
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(maxIECont = max(amount))))

#average $ of IE contributions from invidual
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "I"| comtype == "U" | comtype == "O" |comtype == "W" | comtype == "V" | comtype == "E") %>% group_by(bonica.cid) %>% summarize(avgIECont = mean(amount))))

#average $ IE contributions from invidual per IE comm
individuals$avgIEContPerIE <- 0
individuals$avgIEContPerIE <- ifelse(individuals$ieCount != 0, individuals$contIETotal / individuals$ieCount, individuals$avgIEContPerIE)

#unique # of PAC Comms
individuals <-  left_join(individuals,((unique(select(filter(cont12,comtype == "Q"| comtype == "N"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(pacCount = sum(count))),by="bonica.cid")
individuals[is.na(individuals)] <- 0

#total $ of PAC contributions

individuals <- left_join(individuals,(cont12 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(contPACTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contPACPerc = contPACTotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of PAC contribution from individual
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(maxPACCont = max(amount))))

#average $ PAC contributions from individual
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "Q"| comtype == "N") %>% group_by(bonica.cid) %>% summarize(avgPACCont = mean(amount))))

#average $ PAC contributions from individual per PAC
individuals$avgPACContPerPAC <- 0
individuals$avgPACContPerPAC <- ifelse(individuals$pacCount != 0, individuals$contPACTotal / individuals$pacCount, individuals$avgPACContPerPAC)

#unique # of pty Comms
individuals <-  left_join(individuals,((unique(select(filter(cont12,comtype == "X"| comtype == "Y"),bonica.rid,bonica.cid)) %>% group_by(bonica.cid,bonica.rid) %>% summarize(count=n_distinct(bonica.cid))) %>% group_by(bonica.cid) %>% summarize(ptyCount = sum(count))),by="bonica.cid")

#total $ of Pty contributions

individuals <- left_join(individuals,(cont12 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(contPtyTotal = sum(amount))),by="bonica.cid")
individuals[is.na(individuals)] <- 0
individuals <- mutate(individuals,contPtyPerc = contPtyTotal/ contTotal)
individuals[is.na(individuals)] <- 0

#max $ of pty contribution from individual
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(maxPtyCont = max(amount))))

#average $ pty contributions from invidual
individuals <- left_join(individuals,(cont12 %>% filter(comtype == "X"| comtype == "Y") %>% group_by(bonica.cid) %>% summarize(avgPtyCont = mean(amount))))
individuals[is.na(individuals)] <- 0

#average $ pty contributions from invidual per pty
individuals$avgPtyContPerPty <- 0
individuals$avgPtyContPerPty <- ifelse(individuals$ptyCount != 0, individuals$contPtyTotal / individuals$ptyCount, individuals$avgPtyContPerPty)

write.csv(individuals,"/home/bmath/cont2014/data/final/individuals12.csv")

# Habit Analysis --------------------------------------------------
ind04 <- fread("/home/bmath/cont2014/data/final/individuals04.csv",colClasses=c(bonica.cid="character"))
ind04$cycle <- 2004
ind04 <- ind04 %>% filter(contCandPerc <= 1, contCandPerc >= 0)
ind06 <- fread("/home/bmath/cont2014/data/final/individuals06.csv",colClasses=c(bonica.cid="character"))
ind06$cycle <- 2006
ind06 <- ind06 %>% filter(contCandPerc <= 1, contCandPerc >= 0)
ind08 <- fread("/home/bmath/cont2014/data/final/individuals08.csv",colClasses=c(bonica.cid="character"))
ind08$cycle <- 2008
ind08 <- ind08 %>% filter(contCandPerc <= 1, contCandPerc >= 0)
ind10 <- fread("/home/bmath/cont2014/data/final/individuals10.csv",colClasses=c(bonica.cid="character"))
ind10$cycle <- 2010
ind10 <- ind10 %>% filter(contCandPerc <= 1, contCandPerc >= 0)
ind12 <- fread("/home/bmath/cont2014/data/final/individuals12.csv",colClasses=c(bonica.cid="character"))
ind12$cycle <- 2012
ind12 <- ind12 %>% filter(contCandPerc <= 1, contCandPerc >= 0)

# setup list of all individuals
ind <- bind_rows(select(ind04,bonica.cid),select(ind06,bonica.cid),select(ind08,bonica.cid),select(ind10,bonica.cid),select(ind12,bonica.cid))
ind <- unique(ind)

# Add Variables: name, cfscore, first activity, last activity
# cont <- fread("/home/bmath/cont2014/data/final/contributors.csv")
load("/home/bmath/cont2014/data/final/dime_contributors_1979_2014.rdata")
contribs$bonica.cid <- as.character(contribs$bonica.cid)
contribs <- contribs %>% select(bonica.cid,most.recent.contributor.name,contributor.gender,contributor.cfscore,num.distinct,first_cycle_active,last_cycle_active)
colnames(contribs) <- c("bonica.cid","name","gender","cfScore","numDistinctRec","firstActive","lastActive")
ind <- left_join(ind,contribs)
#ind <- subset(ind, !is.na(most.recent.contributor.name)) # Research to understand why there are so many NAs after this join, should be none.
rm(contribs)


####### Setup Data for frequent/infrequent donor variable
contAll <- select(ind,bonica.cid)

cont_04 <- select(ind04,bonica.cid)
cont_04$cont_04 <- 1
contAll <- left_join(contAll,cont_04)
cont_06 <- select(ind06,bonica.cid)
cont_06$cont_06 <- 1
contAll <- left_join(contAll,cont_06)
cont_08 <- select(ind08,bonica.cid)
cont_08$cont_08 <- 1
contAll <- left_join(contAll,cont_08)
cont_10 <- select(ind10,bonica.cid)
cont_10$cont_10 <- 1
contAll <- left_join(contAll,cont_10)
cont_12 <- select(ind12,bonica.cid)
cont_12$cont_12 <- 1
contAll <- left_join(contAll,cont_12)
rm(cont_04,cont_06,cont_08,cont_10,cont_12)

contAll[is.na(contAll)] <- 0
contAll <- contAll %>% mutate(total = cont_04 + cont_06 + cont_08 + cont_10 + cont_12)

ind <- left_join(ind,contAll,by="bonica.cid")

rm(contAll)

# Add variables: % of total contributions made to federal candidates out of total giving
ind <- left_join(ind,select(ind04,bonica.cid,contCandPerc,contCandTotal),by="bonica.cid")
ind <- left_join(ind,select(ind06,bonica.cid,contCandPerc,contCandTotal),by="bonica.cid")
ind <- left_join(ind,select(ind08,bonica.cid,contCandPerc,contCandTotal),by="bonica.cid")
ind <- left_join(ind,select(ind10,bonica.cid,contCandPerc,contCandTotal),by="bonica.cid")
ind <- left_join(ind,select(ind12,bonica.cid,contCandPerc,contCandTotal),by="bonica.cid")

# Add variables: % of total contributions made to PAC out of total giving
ind <- left_join(ind,select(ind04,bonica.cid,contPACPerc,contPACTotal),by="bonica.cid")
ind <- left_join(ind,select(ind06,bonica.cid,contPACPerc,contPACTotal),by="bonica.cid")
ind <- left_join(ind,select(ind08,bonica.cid,contPACPerc,contPACTotal),by="bonica.cid")
ind <- left_join(ind,select(ind10,bonica.cid,contPACPerc,contPACTotal),by="bonica.cid")
ind <- left_join(ind,select(ind12,bonica.cid,contPACPerc,contPACTotal),by="bonica.cid")

# Add variables: % of total contributions made to IE out of total giving
ind <- left_join(ind,select(ind04,bonica.cid,contIEPerc,contIETotal),by="bonica.cid")
ind <- left_join(ind,select(ind06,bonica.cid,contIEPerc,contIETotal),by="bonica.cid")
ind <- left_join(ind,select(ind08,bonica.cid,contIEPerc,contIETotal),by="bonica.cid")
ind <- left_join(ind,select(ind10,bonica.cid,contIEPerc,contIETotal),by="bonica.cid")
ind <- left_join(ind,select(ind12,bonica.cid,contIEPerc,contIETotal),by="bonica.cid")

# Add variables: % of total contributions made to party out of total giving
ind <- left_join(ind,select(ind04,bonica.cid,contPtyPerc,contPtyTotal),by="bonica.cid")
ind <- left_join(ind,select(ind06,bonica.cid,contPtyPerc,contPtyTotal),by="bonica.cid")
ind <- left_join(ind,select(ind08,bonica.cid,contPtyPerc,contPtyTotal),by="bonica.cid")
ind <- left_join(ind,select(ind10,bonica.cid,contPtyPerc,contPtyTotal),by="bonica.cid")
ind <- left_join(ind,select(ind12,bonica.cid,contPtyPerc,contPtyTotal),by="bonica.cid")

# Add variables: total contributions in a cycle
ind <- left_join(ind,select(ind04,bonica.cid,contTotal),by="bonica.cid")
ind <- left_join(ind,select(ind06,bonica.cid,contTotal),by="bonica.cid")
ind <- left_join(ind,select(ind08,bonica.cid,contTotal),by="bonica.cid")
ind <- left_join(ind,select(ind10,bonica.cid,contTotal),by="bonica.cid")
ind <- left_join(ind,select(ind12,bonica.cid,contTotal),by="bonica.cid")

colnames(ind) <- c("id","name","gender","cfScore","numDistinctRec","firstActive","lastActive", "cont04", "cont06","cont08","cont10","cont12","contTotCycles","candPerc04","candTot04","candPerc06","candTot06","candPerc08","candTot08","candPerc10","candTot10","candPerc12","candTot12","pacPerc04","pacTot04","pacPerc06","pacTot06","pacPerc08","pacTot08","pacPerc10","pacTot10","pacPerc12","pacTot12","iePerc04","ieTot04","iePerc06","ieTot06","iePerc08","ieTot08","iePerc10","ieTot10","iePerc12","ieTot12","ptyPerc04","ptyTot04","ptyPerc06","ptyTot06","ptyPerc08","ptyTot08","ptyPerc10","ptyTot10","ptyPerc12","ptyTot12","contTotal04","contTotal06","contTotal08","contTotal10","contTotal12")

ind[is.na(ind)] <- 0

# get % data in % terms, not decimal terms
ind$candPerc04 <- ind$candPerc04 * 100
ind$candPerc06 <- ind$candPerc06 * 100
ind$candPerc08 <- ind$candPerc08 * 100
ind$candPerc10 <- ind$candPerc10 * 100
ind$candPerc12 <- ind$candPerc12 * 100
ind$pacPerc04 <- ind$pacPerc04 * 100
ind$pacPerc06 <- ind$pacPerc06 * 100
ind$pacPerc08 <- ind$pacPerc08 * 100
ind$pacPerc10 <- ind$pacPerc10 * 100
ind$pacPerc12 <- ind$pacPerc12 * 100
ind$iePerc04 <- ind$iePerc04 * 100
ind$iePerc06 <- ind$iePerc06 * 100
ind$iePerc08 <- ind$iePerc08 * 100
ind$iePerc10 <- ind$iePerc10 * 100
ind$iePerc12 <- ind$iePerc12 * 100
ind$ptyPerc04 <- ind$ptyPerc04 * 100
ind$ptyPerc06 <- ind$ptyPerc06 * 100
ind$ptyPerc08 <- ind$ptyPerc08 * 100
ind$ptyPerc10 <- ind$ptyPerc10 * 100
ind$ptyPerc12 <- ind$ptyPerc12 * 100

ind$cycles <- ifelse(ind$cont04 == 1, 5, ifelse(ind$cont06==1,4,ifelse(ind$cont08==1,3,ifelse(ind$cont10==1,2,1))))
ind <- ind %>% mutate(frequency = contTotCycles/cycles)

ind$freqContrib <- 0 
ind$freqContrib[ind$frequency > .5 & ind$contTotCycles > 1] <- 1

ind$large <- 0
ind$large[ind$contTotal12 > 1500] <- 1

ind$ie12 <- 0
ind$ie12[ind$ieTot12 > 0] <- 1


# Motivation Analysis --------------------------------------------------
# Keep dimeComms, results12 and Cont12

# select nessary columns from data
cont <- cont12 %>% select(bonica.cid,bonica.rid,amount,recipient.party, seat,recipient.state,election.type,Incum.Chall,close.race)

# remove rows without necessary data (committee, 527, party, presidential,etc)
cont <- filter(cont,!is.na(close.race))
cont <- filter(cont,election.type == "G")
cont <- filter(cont,seat == "federal:house")

ind_12 <- filter(ind,cont12 == 1)
ind_12 <- ind_12 %>% select(id,freqContrib,large,ie12)

cont2 <- left_join(cont,ind_12,by=c("bonica.cid" = "id"))

candTotal <- cont2 %>% group_by(bonica.rid) %>% summarize(candTotal = sum(amount))
cont2 <- left_join(cont2,candTotal)

cont2 <- filter(cont2,election.type=="G")

cont2$rep <- 0
cont2$rep[cont2$recipient.party == 200] <- 1
cont2$dem <- 0
cont2$dem[cont2$recipient.party == 100] <- 1
cont2$chal <- 0
cont2$chal[cont2$Incum.Chall == "C"] <- 1
cont2$open <- 0
cont2$open[cont2$Incum.Chall == "O"] <- 1
cont2$inc <- 0
cont2$inc[cont2$Incum.Chall == "I"] <- 1
cont2$comp <- 0
cont2$comp[cont2$close.race == "Y"] <- 1

# Freqent Contributor analysis
contFreq <- cont2 %>% filter(freqContrib == 1) %>% select(bonica.rid,amount,candTotal,dem,rep,inc,chal,open,comp,recipient.state,freqContrib,ie12)
contFreq_dep <- contFreq %>% group_by(bonica.rid) %>% summarize(total = sum(amount))
contFreq_ind <- unique(contFreq %>% select(bonica.rid,candTotal,dem,rep,inc,chal,open,comp,recipient.state))
temp <- count(contFreq_ind,bonica.rid)
contFreq_ind <- left_join(contFreq_ind,temp)
contFreq_ind$comp[contFreq_ind$n == 2] <- 1
contFreq_ind <- unique(contFreq_ind)
contFreq_ind <- contFreq_ind %>% select(-n)

contFreq_final <- left_join(contFreq_dep,contFreq_ind)
contFreq_final$perc <- contFreq_final$total / contFreq_final$candTotal
contFreq_final <- contFreq_final %>% filter(total >= 0, candTotal >0, perc<=1,perc>=0)

fit1 <- lm(total ~ dem + rep + chal + open + inc + comp + chal*comp + open*comp + recipient.state, data=contFreq_final)
summary(fit1)

# Infreqent Contributor analysis
contInfreq <- cont2 %>% filter(freqContrib == 0) %>% select(bonica.rid,amount,candTotal,dem,rep,inc,chal,open,comp,recipient.state,freqContrib,ie12)
contInfreq_dep <- contInfreq %>% group_by(bonica.rid) %>% summarize(total = sum(amount))
contInfreq_ind <- unique(contInfreq %>% select(bonica.rid,candTotal,dem,rep,inc,chal,open,comp,recipient.state))
temp <- count(contInfreq_ind,bonica.rid)
contInfreq_ind <- left_join(contInfreq_ind,temp)
contInfreq_ind$comp[contInfreq_ind$n == 2] <- 1
contInfreq_ind <- unique(contInfreq_ind)
contInfreq_ind <- contInfreq_ind %>% select(-n)

contInfreq_final <- left_join(contInfreq_dep,contInfreq_ind)
contInfreq_final$perc <- contInfreq_final$total / contInfreq_final$candTotal
contInfreq_final <- contInfreq_final %>% filter(total >= 0, candTotal >0, perc<=1, perc>=0)

fit2 <- lm(total ~ dem + rep + chal + open + inc + comp + chal*comp + open*comp + recipient.state, data=contInfreq_final)
summary(fit2)

# IE Contributor analysis
contIE <- cont2 %>% filter(ie12 == 1) %>% select(bonica.rid,amount,candTotal,dem,rep,inc,chal,open,comp,recipient.state,freqContrib,ie12)
contIE_dep <- contIE %>% group_by(bonica.rid) %>% summarize(total = sum(amount))
contIE_ind <- unique(contIE %>% select(bonica.rid,candTotal,dem,rep,inc,chal,open,comp,recipient.state))
temp <- count(contIE_ind,bonica.rid)
contIE_ind <- left_join(contIE_ind,temp)
contIE_ind$comp[contIE_ind$n == 2] <- 1
contIE_ind <- unique(contIE_ind)
contIE_ind <- contIE_ind %>% select(-n)

contIE_final <- left_join(contIE_dep,contIE_ind)
contIE_final$perc <- contIE_final$total / contIE_final$candTotal
contIE_final <- contIE_final %>% filter(total >= 0, candTotal >0, perc<=1, perc>=0)

fit3 <- lm(total ~ dem + rep + chal + open + inc + comp + chal*comp + open*comp + recipient.state, data=contIE_final)
summary(fit3)

# NoIEent Contributor analysis
contNoIE <- cont2 %>% filter(ie12 == 0) %>% select(bonica.rid,amount,candTotal,dem,rep,inc,chal,open,comp,recipient.state,freqContrib,ie12)
contNoIE_dep <- contNoIE %>% group_by(bonica.rid) %>% summarize(total = sum(amount))
contNoIE_ind <- unique(contNoIE %>% select(bonica.rid,candTotal,dem,rep,inc,chal,open,comp,recipient.state))
temp <- count(contNoIE_ind,bonica.rid)
contNoIE_ind <- left_join(contNoIE_ind,temp)
contNoIE_ind$comp[contNoIE_ind$n == 2] <- 1
contNoIE_ind <- unique(contNoIE_ind)
contNoIE_ind <- contNoIE_ind %>% select(-n)

contNoIE_final <- left_join(contNoIE_dep,contNoIE_ind)
contNoIE_final$perc <- contNoIE_final$total / contNoIE_final$candTotal
contNoIE_final <- contNoIE_final %>% filter(total >= 0, candTotal >0, perc<=1, perc>=0)

fit4 <- lm(total ~ dem + rep + chal + open + inc + comp + chal*comp + open*comp + recipient.state, data=contNoIE_final)
summary(fit4)

test <- cbind.data.frame(names(fit1$coefficients),fit1$coefficients,fit2$coefficients,fit4$coefficients)
colnames(test) <- c("vars","frequent","infrequent","non-superPAC")
test2 <- as.data.frame(fit3$coefficients)
test2$vars <- row.names(test2)
test <- left_join(test,test2, by = c("vars"))
colnames(test) <- c("vars","frequent","infrequent","non-superPAC","superPAC")

a<-coef(summary(fit4))

regFigure <- cbind.data.frame(fit1$start,fit2$start,fit3$start,fit4$coefficients)


# Regression (non-matched dataset) ========================================

# Creat sub population of contributors active 2004(or earlier) and 2014
ind_12only <- ind %>% filter(cont12 == 1)

# Difference-in-means
# Mean 
meanDiff <-  ind_12only %>% group_by(ie12) %>%summarize(mean = mean(candPerc12))
meanDiff
meanDiff$mean[1] - meanDiff$mean[2]

### Using all individuals who made a contribution in 2012
# Naive-Regression
fit <- lm(candPerc12 ~ ie12, data=ind_12only)
summary(fit) # show results
rm(fit)

# Regression-Including 
fit_past1 <- lm(candPerc12 ~ ie12 + freqContrib, data=ind_12only)
summary(fit_past1) # show results
rm(fit_past1)

# Regression-Including past contribution history
fit_past <- lm(candPerc12 ~ ie12  + cfScore+  freqContrib + large + candPerc04 + candPerc06 + candPerc08 + candPerc10, data=ind_12only)
summary(fit_past) # show results
rm(fit_past)

# Create Matched Dataset =====================================================
ind_12only <- ind_12only %>% select(-name,-gender)
ind_04to12 <- ind_04to12 %>% select(-name,-gender)

ind_match_dat <- ind_12only %>% select(id,ie12,cfScore,candPerc12,candPerc08,candPerc04,freqContrib,large)

library(Matching)
library(MatchIt)
library(optmatch)

c <- Sys.time()
X <- ind_match_dat %>% select(candPerc08,candPerc04,freqContrib,large,cfScore)
ind_match <- Match(Tr=ind_match_dat$ie12, X=X)
ind_match <- matchit(ie12 ~ candPerc08 + candPerc04 + freqContrib + large + cfScore, data = ind_match_dat, method="nearest")
dat <- match.data(ind_match)
a <- mean(dat$candPerc12[dat$ie12 == 1])
b <- mean(dat$candPerc12[dat$ie12 == 0])
diff <- a-b
diff
d <- Sys.time()
d-c

c <- Sys.time()
ind_match <- matchit(ie12 ~ candPerc08 + candPerc04 + freqContrib + large + cfScore, data = ind_match_dat, method="nearest")
dat <- match.data(ind_match)
a <- mean(dat$candPerc12[dat$ie12 == 1])
b <- mean(dat$candPerc12[dat$ie12 == 0])
diff <- a-b
diff
d <- Sys.time()
d-c
write.csv(match.data(results),"match12_results.csv")

detach("package:Matching", unload=TRUE)
detach("package:MatchIt", unload=TRUE)
detach("package:optmatch", unload=TRUE)
detach("package:broom", unload=TRUE)
detach("package:dplyr", unload=TRUE)
library(dplyr)

#Coarsened Exact Matching
library("Matching")
library("cem")

vars <- c("freqContrib", "large", "cfScore", "candPerc08","candPerc04")
drop.vars <- c("id","candPerc12")

imbalance(group=ind_match_dat$ie12, data=ind_match_dat[vars])

ind_match <- cem(treatment="ie12", drop=drop.vars, data = ind_match_dat, keep.all=TRUE, k2k = TRUE)

imbalance(group=ind_match_dat$ie12, data=ind_match_dat[vars],weights = ind_match$w)

summary(lm(candPerc12 ~ ie12 + freqContrib + large + cfScore + candPerc08 + candPerc04, data=dat, weights = ind_match$w))
est <- att(ind_match, candPerc12 ~ ie12, data = dat)


# Regression (matched dataset) ========================================
# Difference-in-means
# Mean 
meanDiff <-  dat %>% group_by(ie12) %>%summarize(mean = mean(candPerc12))
meanDiff
meanDiff$mean[1] - meanDiff$mean[2]

fit <- lm(candPerc12 ~ ie12, data = dat)
summary(fit)
rm(fit)

fit <- lm(candPerc12 ~ ie12 + freqContrib, data = dat)
summary(fit)
rm(fit)

fit <- lm(candPerc12 ~ ie12 + freqContrib + large + cfScore + candPerc08 + candPerc04, data = dat)
summary(fit)
rm(fit)


