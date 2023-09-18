############## GE Programs in Low-Wage Areas ###############
# Start by running the code on online programs in the main 'GE Geography.R' file. 

#### Loading Census Bureau data on income by ZIP code #### 
# Source: https://data.census.gov/table?q=income&g=010XX00US$8600000&tid=ACSST5Y2021.S1902

income <- read.csv("ACSST5Y2021.S1903-Data.csv", header=TRUE) 
income <- income %>% select(
  `NAME`,           # Geographic Area Name
  `S1903_C01_001E`,	# Estimate!!Number!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households
  `S1903_C03_001E`	# Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households
) %>% filter(NAME != "Geographic Area Name") %>% filter((NAME %in% c("ZCTA5 72405", "ZCTA5 72713", "ZCTA5 75036", "ZCTA5 75072", "ZCTA5 89437", "ZCTA5 97003"))==FALSE)

# Added for this test: 
names(income) <- c("NAME", "S1902_C01_003E", "S1902_C03_003E")
income <- income %>% mutate(`S1902_C01_001E` = `S1902_C01_003E`)
income <- income %>% mutate(`S1902_C01_002E` = `S1902_C01_003E`)
income <- income %>% mutate(`S1902_C01_003E` = `S1902_C01_003E`)
income <- income %>% mutate(`S1902_C03_001E` = `S1902_C03_003E`)
income <- income %>% mutate(`S1902_C03_002E` = `S1902_C03_003E`)
income <- income %>% mutate(`S1902_C03_003E` = `S1902_C03_003E`)

income$`Count` <- rep(1, nrow(income))

income$`S1902_C01_001E` <- as.numeric(income$`S1902_C01_001E`)
income$`S1902_C01_002E` <- as.numeric(income$`S1902_C01_002E`)
income$`S1902_C01_003E` <- as.numeric(income$`S1902_C01_003E`)
income$`S1902_C03_001E` <- as.numeric(income$`S1902_C03_001E`)
income$`S1902_C03_002E` <- as.numeric(income$`S1902_C03_002E`)
income$`S1902_C03_003E` <- as.numeric(income$`S1902_C03_003E`)

income <- income %>% filter(is.na(`S1902_C01_001E`) == FALSE)
income <- income %>% filter(is.na(`S1902_C01_002E`) == FALSE)
income <- income %>% filter(is.na(`S1902_C01_003E`) == FALSE)
income <- income %>% filter(is.na(`S1902_C03_001E`) == FALSE)
income <- income %>% filter(is.na(`S1902_C03_002E`) == FALSE)
income <- income %>% filter(is.na(`S1902_C03_003E`) == FALSE)

income$NAME <- as.character(substr(income$NAME, 7, 11))
income <- income %>% rename(`ZIP5` = `NAME`)

for(i in (1:nrow(income))){
  if(i==1){
    income$`State` <- rep(NA, nrow(income))
  }
  income$State[i] <- reverse_zipcode(income$`ZIP5`[i])$state
}

income$`ZIP4` <- substr(income$`ZIP5`, 1, 4) 
income$`ZIP3` <- substr(income$`ZIP5`, 1, 3) 
#### End #### 

#### Finding income for all households #### 
income1.Z5 <- income %>% select(`ZIP5`, `S1902_C03_001E`, `State`) %>% rename(`Mean income` = `S1902_C03_001E`)
income1.Z4 <- income %>% group_by(`ZIP4`) %>% summarize(`Mean income` = weighted.mean(`S1902_C03_001E`, w = `S1902_C01_001E`))
income1.Z3 <- income %>% group_by(`ZIP3`) %>% summarize(`Mean income` = weighted.mean(`S1902_C03_001E`, w = `S1902_C01_001E`))

for(i in (1:nrow(income1.Z4))){
  if(i==1){income1.Z4$`State` <- rep(NA, nrow(income1.Z4))}
  zips <- income %>% filter(`ZIP4`==(income1.Z4$`ZIP4`[i]))
  newtable <- aggregate(data=zips, `Count` ~ `State`, FUN=sum) %>% arrange(desc(`Count`))
  income1.Z4$`State`[i] <- newtable[1, 1]
  rm("zips", "newtable")
}

for(i in (1:nrow(income1.Z3))){
  if(i==1){income1.Z3$`State` <- rep(NA, nrow(income1.Z3))}
  zips <- income %>% filter(`ZIP3`==(income1.Z3$`ZIP3`[i]))
  newtable <- aggregate(data=zips, `Count` ~ `State`, FUN=sum) %>% arrange(desc(`Count`))
  income1.Z3$`State`[i] <- newtable[1, 1]
  rm("zips", "newtable")
}

#### End #### 

#### Finding income for households with earnings ####  
income2.Z5 <- income %>% select(`ZIP5`, `S1902_C03_002E`, `State`) %>% rename(`Mean income` = `S1902_C03_002E`)
income2.Z4 <- income %>% group_by(`ZIP4`) %>% summarize(`Mean income` = weighted.mean(`S1902_C03_002E`, w = `S1902_C01_002E`))
income2.Z3 <- income %>% group_by(`ZIP3`) %>% summarize(`Mean income` = weighted.mean(`S1902_C03_002E`, w = `S1902_C01_002E`))

for(i in (1:nrow(income2.Z4))){
  if(i==1){income2.Z4$`State` <- rep(NA, nrow(income2.Z4))}
  zips <- income %>% filter(`ZIP4`==(income2.Z4$`ZIP4`[i]))
  newtable <- aggregate(data=zips, `Count` ~ `State`, FUN=sum) %>% arrange(desc(`Count`))
  income2.Z4$`State`[i] <- newtable[1, 1]
  rm("zips", "newtable")
}

for(i in (1:nrow(income2.Z3))){
  if(i==1){income2.Z3$`State` <- rep(NA, nrow(income2.Z3))}
  zips <- income %>% filter(`ZIP3`==(income2.Z3$`ZIP3`[i]))
  newtable <- aggregate(data=zips, `Count` ~ `State`, FUN=sum) %>% arrange(desc(`Count`))
  income2.Z3$`State`[i] <- newtable[1, 1]
  rm("zips", "newtable")
}
#### End #### 

#### Finding income for households with wage or salary earnings ####
income3.Z5 <- income %>% select(`ZIP5`, `S1902_C03_003E`, `State`) %>% rename(`Mean income` = `S1902_C03_003E`)
income3.Z4 <- income %>% group_by(`ZIP4`) %>% summarize(`Mean income` = weighted.mean(`S1902_C03_003E`, w = `S1902_C01_003E`))
income3.Z3 <- income %>% group_by(`ZIP3`) %>% summarize(`Mean income` = weighted.mean(`S1902_C03_003E`, w = `S1902_C01_003E`))

for(i in (1:nrow(income3.Z4))){
  if(i==1){income3.Z4$`State` <- rep(NA, nrow(income3.Z4))}
  zips <- income %>% filter(`ZIP4`==(income3.Z4$`ZIP4`[i]))
  newtable <- aggregate(data=zips, `Count` ~ `State`, FUN=sum) %>% arrange(desc(`Count`))
  income3.Z4$`State`[i] <- newtable[1, 1]
  rm("zips", "newtable")
}

for(i in (1:nrow(income3.Z3))){
  if(i==1){income3.Z3$`State` <- rep(NA, nrow(income3.Z3))}
  zips <- income %>% filter(`ZIP3`==(income3.Z3$`ZIP3`[i]))
  newtable <- aggregate(data=zips, `Count` ~ `State`, FUN=sum) %>% arrange(desc(`Count`))
  income3.Z3$`State`[i] <- newtable[1, 1]
  rm("zips", "newtable")
}
#### End #### 

#### Calculate percentiles within states ####

run_percentiles <- function(incomedata){
  for(i in (1:nrow(incomedata))){
    if(i==1){incomedata$`Percentile within state` <- rep(NA, nrow(incomedata))}
    selected_state <- incomedata %>% filter(`State` == incomedata$`State`[i])
    income_distribution <- ecdf(selected_state$`Mean income`)
    incomedata$`Percentile within state`[i] <- income_distribution(incomedata$`Mean income`[i])
    rm("selected_state", "income_distribution")
  }
  return(incomedata)
}

income1.Z3 <- run_percentiles(income1.Z3)
income1.Z4 <- run_percentiles(income1.Z4)
income1.Z5 <- run_percentiles(income1.Z5)

income2.Z3 <- run_percentiles(income2.Z3)
income2.Z4 <- run_percentiles(income2.Z4)
income2.Z5 <- run_percentiles(income2.Z5)

income3.Z3 <- run_percentiles(income3.Z3)
income3.Z4 <- run_percentiles(income3.Z4)
income3.Z5 <- run_percentiles(income3.Z5)

#### End #### 

#### Formatting data ####
income1.Z3 <- income1.Z3 %>% rename(
  `Within-state percentile of mean income of all households, ZIP3` = `Percentile within state`, 
  `Mean income of all households, ZIP3` = `Mean income`) %>% select(-(`State`))
income1.Z4 <- income1.Z4 %>% rename(
  `Within-state percentile of mean income of all households, ZIP4` = `Percentile within state`, 
  `Mean income of all households, ZIP4` = `Mean income`) %>% select(-(`State`))
income1.Z5 <- income1.Z5 %>% rename(
  `Within-state percentile of mean income of all households, ZIP5` = `Percentile within state`, 
  `Mean income of all households, ZIP5` = `Mean income`) %>% select(-(`State`))

income2.Z3 <- income2.Z3 %>% rename(
  `Within-state percentile of mean income of households with earnings, ZIP3` = `Percentile within state`, 
  `Mean income of households with earnings, ZIP3` = `Mean income`) %>% select(-(`State`))
income2.Z4 <- income2.Z4 %>% rename(
  `Within-state percentile of mean income of households with earnings, ZIP4` = `Percentile within state`, 
  `Mean income of households with earnings, ZIP4` = `Mean income`) %>% select(-(`State`))
income2.Z5 <- income2.Z5 %>% rename(
  `Within-state percentile of mean income of households with earnings, ZIP5` = `Percentile within state`, 
  `Mean income of households with earnings, ZIP5` = `Mean income`) %>% select(-(`State`))

income3.Z3 <- income3.Z3 %>% rename(
  `Within-state percentile of mean income of households with wage or salary earnings, ZIP3` = `Percentile within state`, 
  `Mean income of households with wage or salary earnings, ZIP3` = `Mean income`) %>% select(-(`State`))
income3.Z4 <- income3.Z4 %>% rename(
  `Within-state percentile of mean income of households with wage or salary earnings, ZIP4` = `Percentile within state`, 
  `Mean income of households with wage or salary earnings, ZIP4` = `Mean income`) %>% select(-(`State`))
income3.Z5 <- income3.Z5 %>% rename(
  `Within-state percentile of mean income of households with wage or salary earnings, ZIP5` = `Percentile within state`, 
  `Mean income of households with wage or salary earnings, ZIP5` = `Mean income`) %>% select(-(`State`))

income.Z3 <- full_join(x=income1.Z3, y=full_join(x=income2.Z3, y=income3.Z3, by="ZIP3"), by="ZIP3")
income.Z4 <- full_join(x=income1.Z4, y=full_join(x=income2.Z4, y=income3.Z4, by="ZIP4"), by="ZIP4")
income.Z5 <- full_join(x=income1.Z5, y=full_join(x=income2.Z5, y=income3.Z5, by="ZIP5"), by="ZIP5")

#### End #### 

#### Loading GE data, merging with income percentiles datasets ####
ge <- read.csv("nprm-2022ppd-public-suppressed.csv", header=TRUE)
ge <- ge %>% filter((`control_peps` %in% c("Foreign For-Profit", "Foreign Private"))==FALSE)
ge$Count <- rep(1, nrow(ge))

# Removing online programs
ge <- left_join(x=ge, y=online.programs, by=c("opeid6", "cred_lvl", "cip4"))
ge$`Distance status`[is.na(ge$`Distance status`)] <- 0
ge <- ge %>% filter(`Distance status`==0)

ge$`ZIP5` <- substr(ge$`zip`, 1, 5)
ge$`ZIP4` <- substr(ge$`zip`, 1, 4)
ge$`ZIP3` <- substr(ge$`zip`, 1, 3)

ge <- left_join(x=ge, y=income.Z3, by="ZIP3")
ge <- left_join(x=ge, y=income.Z4, by="ZIP4")
ge <- left_join(x=ge, y=income.Z5, by="ZIP5")

ge1 <- ge %>% filter(`inGE` == 1) 

ge1$`fail_EP_2019` <- as.character(ge1$`fail_EP_2019`)
ge1$`fail_EP_2019`[ge1$`passfail_2019` == "No DTE/EP data"] <- "No data"

#### End #### 

#### Calculating share of *failing* programs in bottom quartile by income ####

agg1.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg2.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg3.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))  

agg1.Z3 <- aggregate(data=agg1.Z3, `Count` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg1.Z4 <- aggregate(data=agg1.Z4, `Count` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg1.Z5 <- aggregate(data=agg1.Z5, `Count` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

agg2.Z3 <- aggregate(data=agg2.Z3, `Count` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg2.Z4 <- aggregate(data=agg2.Z4, `Count` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg2.Z5 <- aggregate(data=agg2.Z5, `Count` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

agg3.Z3 <- aggregate(data=agg3.Z3, `Count` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg3.Z4 <- aggregate(data=agg3.Z4, `Count` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg3.Z5 <- aggregate(data=agg3.Z5, `Count` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

agg1.Z3
agg1.Z4
agg1.Z5

agg2.Z3
agg2.Z4
agg2.Z5

agg3.Z3
agg3.Z4
agg3.Z5

#### End #### 

#### Calculating share of *all* programs in bottom quartile by income ####

agg1.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg2.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg3.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))  

agg1.Z3 <- aggregate(data=agg1.Z3, `Count` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg1.Z4 <- aggregate(data=agg1.Z4, `Count` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg1.Z5 <- aggregate(data=agg1.Z5, `Count` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

agg2.Z3 <- aggregate(data=agg2.Z3, `Count` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg2.Z4 <- aggregate(data=agg2.Z4, `Count` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg2.Z5 <- aggregate(data=agg2.Z5, `Count` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

agg3.Z3 <- aggregate(data=agg3.Z3, `Count` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg3.Z4 <- aggregate(data=agg3.Z4, `Count` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg3.Z5 <- aggregate(data=agg3.Z5, `Count` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`Count`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

agg1.Z3
agg1.Z4
agg1.Z5

agg2.Z3
agg2.Z4
agg2.Z5

agg3.Z3
agg3.Z4
agg3.Z5

#### End #### 

#### Removing programs that lack cohort size data ####
ge2 <- ge1 %>% filter(is.na(`count_AY1617`)==FALSE)
#### End #### 

#### Calculating average cohort size in *failing* programs in bottom quartile by income ####

agg1.Z3 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z4 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z5 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg2.Z3 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z4 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z5 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg3.Z3 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z4 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z5 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))  

agg1.Z3 <- aggregate(data=agg1.Z3, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) 
agg1.Z4 <- aggregate(data=agg1.Z4, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) 
agg1.Z5 <- aggregate(data=agg1.Z5, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) 

agg2.Z3 <- aggregate(data=agg2.Z3, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) 
agg2.Z4 <- aggregate(data=agg2.Z4, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`)
agg2.Z5 <- aggregate(data=agg2.Z5, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) 

agg3.Z3 <- aggregate(data=agg3.Z3, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`)
agg3.Z4 <- aggregate(data=agg3.Z4, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) 
agg3.Z5 <- aggregate(data=agg3.Z5, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`)

agg1.Z3
agg1.Z4
agg1.Z5

agg2.Z3
agg2.Z4
agg2.Z5

agg3.Z3
agg3.Z4
agg3.Z5

#### End #### 

#### Calculating share of cohort in *all* programs in bottom quartile by income ####

agg1.Z3 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z4 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z5 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg2.Z3 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z4 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z5 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg3.Z3 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z4 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z5 <- ge2 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))  

agg1.Z3 <- aggregate(data=agg1.Z3, `count_AY1617` ~ `Quartile`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) 
agg1.Z4 <- aggregate(data=agg1.Z4, `count_AY1617` ~ `Quartile`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) 
agg1.Z5 <- aggregate(data=agg1.Z5, `count_AY1617` ~ `Quartile`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`)

agg2.Z3 <- aggregate(data=agg2.Z3, `count_AY1617` ~ `Quartile`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`)
agg2.Z4 <- aggregate(data=agg2.Z4, `count_AY1617` ~ `Quartile`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) 
agg2.Z5 <- aggregate(data=agg2.Z5, `count_AY1617` ~ `Quartile`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`)

agg3.Z3 <- aggregate(data=agg3.Z3, `count_AY1617` ~ `Quartile`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`)
agg3.Z4 <- aggregate(data=agg3.Z4, `count_AY1617` ~ `Quartile`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`)
agg3.Z5 <- aggregate(data=agg3.Z5, `count_AY1617` ~ `Quartile`, FUN=mean) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`)

agg1.Z3
agg1.Z4
agg1.Z5

agg2.Z3
agg2.Z4
agg2.Z5

agg3.Z3
agg3.Z4
agg3.Z5

#### End #### 

