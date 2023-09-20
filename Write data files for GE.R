
#### Setup #### 
library(dplyr)
library(stringr)
library(data.table)
#### End #### 

######################## Creating "ge-programs.csv" ######################## 

#### P1: Load GE file, select variables ####
setwd("/Users/peter_granville/Various-TCF-Projects")
ge <- read.csv("nprm-2022ppd-public-suppressed.csv", header=TRUE)
ge <- ge %>% select(
  `schname`, 
  `inGE`, 
  `opeid6`, 
  `cip2_title_2010`, 
  `cipdesc`, 
  `cip4`,
  `cred_lvl`, 
  `control_peps`, 
  `st_fips`, 
  `earn_count_ne_3yr`, 
  `mdearnp3`, 
  `debtservicenpp_md`,
  `mdincearn_lf`, 
  `EP_lf_2019`,
  `fail_DTE_2019`, 
  `fail_EP_2019`, 
  `passfail_2019`, 
  `inexpfte`, 
  `hbcu`,      # HBCU flag assigned at the opeid6 [1=Yes 0=No]
  `tribal`,    # Tribally controlled college/university flag [1=Yes 0=No]
  `hsi`,       # Hispanic-serving college/university flag [1=Yes 0=No]
  `msi`        # Minority-serving college/university flag [1=Yes 0=No]
)
#### End #### 

#### P2: Format CIP names #### 
ge$cipdesc <- str_to_title(ge$cipdesc)
ge$cip2_title_2010 <- str_to_title(ge$cip2_title_2010)
#### End #### 

#### P3: Load MSI data ####
msis <- fread("Most-Recent-Cohorts-Institution-Apr2023.csv", header=TRUE, select=c(
  "OPEID6",
  "MAIN",
  "PBI",
  "ANNHI",
  "AANAPII",
  "NANTI"
))
suppressWarnings({
  msis$OPEID6 <- as.numeric(msis$OPEID6)
  msis <- msis %>% mutate(`PBI` = as.numeric(`PBI`))
  msis <- msis %>% mutate(`ANNHI` = as.numeric(`ANNHI`))
  msis <- msis %>% mutate(`AANAPII` = as.numeric(`AANAPII`))
  msis <- msis %>% mutate(`NANTI` = as.numeric(`NANTI`))
  msis <- msis %>% arrange(desc(MAIN)) %>% filter(duplicated(OPEID6)==FALSE)
  msis <- msis %>% select(!(MAIN))
})
names(msis) <- tolower(names(msis))

ge <- left_join(x=ge, y=msis, by="opeid6")
#### End ####

#### P4: Load data on the nearest college of failing GE programs ####
# Load in the dataset on nearest alternative
ge.fail.A_record <- read.csv("ge.fail.A_record.csv", header=TRUE)

# Remove programs where there was no alternative 
ge.fail.A_record <- ge.fail.A_record %>% filter(is.na(`alt_opeid6`)==FALSE)

# Create a unique identifier for each program: 
ge.fail.A_record <- ge.fail.A_record %>% mutate(`Prog_ID` = paste(`alt_opeid6`, `alt_cip4`, `alt_cred_lvl`, sep="-"))

# Keep in only the essentials 
ge.transfers <- ge.fail.A_record %>% select(`Prog_ID`, `count_AY1617`)
ge.transfers <- ge.transfers %>% rename(`TransferStudents` = `count_AY1617`)
ge.transfers <- aggregate(data=ge.transfers, `TransferStudents` ~ `Prog_ID`, FUN=sum)

# Here, we load in the data on transfers: 
ge <- ge %>% mutate(`Prog_ID` = paste(`opeid6`, `cip4`, `cred_lvl`, sep="-")) %>% select(-(`cip4`))
ge <- left_join(x=ge, y=ge.transfers, by="Prog_ID") %>% select(-(`Prog_ID`))
ge$`TransferStudents`[is.na(ge$`TransferStudents`)] <- 0
#### End #### 

#### P5: Write "ge-programs.csv ####
setwd("/Users/peter_granville/GE-Test/Data")
write.csv(ge, "ge-programs.csv", row.names=FALSE)
rm("ge", "msis")
#### End #### 

######################## Creating ge-colleges.csv ######################## 

#### C1: Load GE file, select variables ####
setwd("/Users/peter_granville/Various-TCF-Projects")
ge <- read.csv("nprm-2022ppd-public-suppressed.csv", header=TRUE)
ge <- ge %>% select(`opeid6`, `schname`, `st_fips`, `control_peps`, `inGE`, `passfail_2019`, `count_AY1617`)
#### End ####

#### C2: Count students by pass and fail ####
ge$count <- rep(1, nrow(ge))
ge <- ge %>% mutate(`Number of students in failing program` = ifelse((`inGE`==1) & (`passfail_2019` %in% c("Fail both DTE and EP", "Fail DTE only", "Fail EP only")), `count_AY1617`, 0))
ge <- ge %>% mutate(`Total number of students in program` = `count_AY1617`)
ge$`Number of students in failing program`[is.na(ge$`Number of students in failing program`)] <- 0
ge$`Total number of students in program`[is.na(ge$`Total number of students in program`)] <- 0
#### End ####

#### C3: Aggregate by opeid6, creating gec ####
ge2 <- aggregate(data=ge, cbind(inGE, count) ~ opeid6 + schname + st_fips + control_peps, FUN=sum)
ge2 <- ge2 %>% mutate(`Has a GE program` = ifelse(inGE > 0, "Has a GE program", "Does not have a GE program"))

ge3 <- aggregate(data=ge, cbind(`Number of students in failing program`, `Total number of students in program`) ~ opeid6 + schname + st_fips + control_peps, FUN=sum)
ge3 <- ge3 %>% mutate(`Share of students in failing programs` = `Number of students in failing program` / `Total number of students in program`)
ge3$`Share of students in failing programs`[is.nan(ge3$`Share of students in failing programs`)] <- 0

gec <- full_join(x=ge2, y=ge3, by=c("opeid6", "schname", "st_fips", "control_peps"))
gec <- gec %>% rename(`Total number of students in all programs` = `Total number of students in program`)

#### End ####

#### C4: Load IPEDS data on lat/longs and enrollment, merge with gec ####
hd <- fread("hd2021.csv", header=TRUE, select=c(
  "UNITID", 
  "LATITUDE",
  "LONGITUD", 
  "OPEID"
))
scorecard <- fread("Most-Recent-Cohorts-Institution.csv", header=TRUE, select=c(
  "UNITID", 
  "OPEID6"
))
hd <- left_join(x=hd, y=scorecard, by="UNITID")
effy <- fread("effy2021.csv", header=TRUE, select=c(
  "UNITID", 
  "EFFYLEV", 
  "EFYTOTLT"
)) %>% filter(EFFYLEV==1) %>% select(UNITID, EFYTOTLT)
hd <- left_join(x=hd, y=effy, by="UNITID")

hd <- hd %>% arrange(desc(EFYTOTLT))
hd <- hd %>% filter(duplicated(OPEID6)==FALSE)
hd <- hd %>% select(!(`EFYTOTLT`)) %>% select(!(`UNITID`))
names(hd) <- tolower(names(hd))
gec <- left_join(x=gec, y=hd, by="opeid6")
#### End ####

#### C5: Load data on MSI status, merge with gec ####
msis <- fread("Most-Recent-Cohorts-Institution-Apr2023.csv", header=TRUE, select=c(
  "OPEID6",
  "MAIN",
  "HBCU", 
  "HSI", 
  "TRIBAL", 
  "PBI",
  "ANNHI",
  "AANAPII",
  "NANTI"
))
msis$OPEID6 <- as.numeric(msis$OPEID6)
suppressWarnings({
  msis <- msis %>% mutate(`HBCU` = as.numeric(`HBCU`))
  msis <- msis %>% mutate(`HSI` = as.numeric(`HSI`))
  msis <- msis %>% mutate(`PBI` = as.numeric(`PBI`))
  msis <- msis %>% mutate(`TRIBAL` = as.numeric(`TRIBAL`))
  msis <- msis %>% mutate(`ANNHI` = as.numeric(`ANNHI`))
  msis <- msis %>% mutate(`AANAPII` = as.numeric(`AANAPII`))
  msis <- msis %>% mutate(`NANTI` = as.numeric(`NANTI`))
  msis <- msis %>% mutate(`MSI` = ifelse(((`HBCU`==1) | (`HSI`==1) | (`PBI`==1) | (`TRIBAL`==1) | (`ANNHI`==1) | (AANAPII==1) | (`NANTI`==1)), 1, 0))
  msis <- msis %>% arrange(desc(MAIN)) %>% filter(duplicated(OPEID6)==FALSE)
  msis <- msis %>% select(!(MAIN))
})
names(msis) <- tolower(names(msis))
gec <- left_join(x=gec, y=msis, by="opeid6")
#### End #### 

#### C6: Write "ge-colleges.csv" ####
setwd("/Users/peter_granville/GE-Test/Data")
write.csv(gec, "ge-colleges.csv", row.names=FALSE)
rm("effy", "ge", "ge2", "ge3", "gec", "hd", "msis", "scorecard")
#### End #### 

######################## Identifying fields with GE programs for fieldSelection ######################## 

#### F1: Load GE data #### 
setwd("/Users/peter_granville/Various-TCF-Projects")
ge <- read.csv("nprm-2022ppd-public-suppressed.csv", header=TRUE)
ge <- ge %>% filter(inGE==1) %>% filter(`passfail_2019` != "No DTE/EP data")
#### End ####

#### F2: Sort unique CIP2s and CIP4s, format #### 
ge$cipdesc <- str_to_title(ge$cipdesc)
ge$cip2_title_2010 <- str_to_title(ge$cip2_title_2010)

cip4 <- unique(ge$cipdesc)
cip2 <- unique(ge$cip2_title_2010)
cip4.edit <- sort(cip4)
cip2.edit <- sort(cip2)

ge4 <- ge %>% filter(duplicated(`cip4`)==FALSE) %>% select(`cipdesc`, `cip4`) %>% rename(`cip.edit` = `cipdesc`, `CIPnum` = `cip4`)
ge2 <- ge %>% filter(duplicated(`cip2`)==FALSE) %>% select(`cip2_title_2010`, `cip2`) %>% rename(`cip.edit` = `cip2_title_2010`, `CIPnum` = `cip2`)

ge2$`CIPnum` <- as.character(ge2$`CIPnum`)
ge2$`CIPnum` <- ifelse(nchar(ge2$`CIPnum`)==1, paste("0", ge2$`CIPnum`, sep=""), ge2$`CIPnum`)

ge4$`CIPnum` <- as.character(ge4$`CIPnum`)
ge4$`CIPnum` <- ifelse(nchar(ge4$`CIPnum`)==1, paste("0", ge4$`CIPnum`, sep=""), ge4$`CIPnum`)
ge4$`CIPnum` <- ifelse(nchar(ge4$`CIPnum`)==2, paste("0", ge4$`CIPnum`, sep=""), ge4$`CIPnum`)
ge4$`CIPnum` <- ifelse(nchar(ge4$`CIPnum`)==3, paste("0", ge4$`CIPnum`, sep=""), ge4$`CIPnum`)

cip4 <- data.frame("cip.edit" = cip4.edit, 
                   "Digits" = rep("4-digit CIP", length(cip4.edit)))
cip2 <- data.frame("cip.edit" = cip2.edit, 
                   "Digits" = rep("2-digit CIP", length(cip2.edit)))

cip4 <- left_join(x=cip4, y=ge4, by="cip.edit")
cip2 <- left_join(x=cip2, y=ge2, by="cip.edit")

cips <- rbind(cip2, cip4)
cips$FullDesc <- paste("(", cips$Digits, " ", cips$CIPnum, ") ", cips$cip.edit, sep="")

cips <- cips %>% arrange(`FullDesc`)
#### End #### 

#### F3: Write "ge-cips.csv" ####
setwd("/Users/peter_granville/GE-Test/Data")
write.csv(cips, "ge-cips.csv", row.names=FALSE)
#### End #### 

