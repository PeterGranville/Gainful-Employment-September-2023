
#### Setup #### 
library(dplyr)
library(ggplot2)
library(zipcodeR)
library(geosphere)
library(data.table)
#### End #### 

############## GE Programs in Low-Wage Areas ###############

#### Loading Census Bureau data on income by ZIP code #### 
# Source: https://data.census.gov/table?q=income&g=010XX00US$8600000&tid=ACSST5Y2021.S1901

income <- read.csv("ACSST5Y2021.S1902-Data.csv", header=TRUE) 
income <- income %>% select(
  `NAME`,           # Geographic Area Name
  `S1902_C01_001E`,	# Estimate!!Number!!HOUSEHOLD INCOME!!All households
  `S1902_C01_002E`,	# Estimate!!Number!!HOUSEHOLD INCOME!!All households!!With earnings
  `S1902_C01_003E`,	# Estimate!!Number!!HOUSEHOLD INCOME!!All households!!With earnings!!With wages or salary income
  `S1902_C03_001E`,	# Estimate!!Mean income (dollars)!!HOUSEHOLD INCOME!!All households
  `S1902_C03_002E`,	# Estimate!!Mean income (dollars)!!HOUSEHOLD INCOME!!All households!!With earnings
  `S1902_C03_003E`	# Estimate!!Mean income (dollars)!!HOUSEHOLD INCOME!!All households!!With earnings!!With wages or salary income
) %>% filter(NAME != "Geographic Area Name") %>% filter((NAME %in% c("ZCTA5 72405", "ZCTA5 72713", "ZCTA5 75036", "ZCTA5 75072", "ZCTA5 89437", "ZCTA5 97003"))==FALSE)

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
ge$Count <- rep(1, nrow(ge))

ge$`ZIP5` <- substr(ge$`zip`, 1, 5)
ge$`ZIP4` <- substr(ge$`zip`, 1, 4)
ge$`ZIP3` <- substr(ge$`zip`, 1, 3)

ge <- left_join(x=ge, y=income.Z3, by="ZIP3")
ge <- left_join(x=ge, y=income.Z4, by="ZIP4")
ge <- left_join(x=ge, y=income.Z5, by="ZIP5")

ge1 <- ge %>% filter(`inGE` == 1) %>% filter(`passfail_2019` != "No DTE/EP data")
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

#### Calculating share of cohort in *failing* programs in bottom quartile by income ####

agg1.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg2.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg3.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))  

agg1.Z3 <- aggregate(data=agg1.Z3, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg1.Z4 <- aggregate(data=agg1.Z4, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg1.Z5 <- aggregate(data=agg1.Z5, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

agg2.Z3 <- aggregate(data=agg2.Z3, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg2.Z4 <- aggregate(data=agg2.Z4, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg2.Z5 <- aggregate(data=agg2.Z5, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

agg3.Z3 <- aggregate(data=agg3.Z3, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg3.Z4 <- aggregate(data=agg3.Z4, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg3.Z5 <- aggregate(data=agg3.Z5, `count_AY1617` ~ `Quartile` + `fail_EP_2019`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

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

agg1.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg1.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of all households, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg2.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg2.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))

agg3.Z3 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP3` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z4 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP4` < 0.25, "Bottom quartile", "Not bottom quartile"))
agg3.Z5 <- ge1 %>% mutate(`Quartile` = ifelse(`Within-state percentile of mean income of households with wage or salary earnings, ZIP5` < 0.25, "Bottom quartile", "Not bottom quartile"))  

agg1.Z3 <- aggregate(data=agg1.Z3, `count_AY1617` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg1.Z4 <- aggregate(data=agg1.Z4, `count_AY1617` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg1.Z5 <- aggregate(data=agg1.Z5, `count_AY1617` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

agg2.Z3 <- aggregate(data=agg2.Z3, `count_AY1617` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg2.Z4 <- aggregate(data=agg2.Z4, `count_AY1617` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg2.Z5 <- aggregate(data=agg2.Z5, `count_AY1617` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

agg3.Z3 <- aggregate(data=agg3.Z3, `count_AY1617` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg3.Z4 <- aggregate(data=agg3.Z4, `count_AY1617` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))
agg3.Z5 <- aggregate(data=agg3.Z5, `count_AY1617` ~ `Quartile`, FUN=sum) %>% pivot_wider(names_from=`Quartile`, values_from=`count_AY1617`) %>% mutate(`Share in bottom quartile` = `Bottom quartile` / (`Bottom quartile` + `Not bottom quartile`))

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

############## Proximity of Alternative Options ###############

#### Load in GE program data ####

ge <- fread("nprm-2022ppd-public-suppressed.csv", header=TRUE, select=c(
  "schname", 
  "inGE", 
  "opeid6", 
  "stabbr", 
  "zip",
  "cip4", 
  "cipdesc", 
  "cip2", 
  "cip2_title_2010", 
  "cred_lvl", 
  "passfail_2019", 
  "mdearnp3",
  "count_AY1617"
))

ge.level.category <- data.table("cred_lvl" = c(
  "UG Certificates", 
  "Associate's", 
  "Bachelor's",
  "Post-BA Certs",
  "Grad Certs", 
  "Master's", 
  "Professional",
  "Doctoral"
), "Category" = c(
  "Undergraduate", 
  "Undergraduate", 
  "Undergraduate", 
  "Undergraduate", 
  "Graduate", 
  "Graduate", 
  "Graduate", 
  "Graduate"
))

ge <- left_join(x=ge, y=ge.level.category, by="cred_lvl")
ge$zip <- substr(ge$zip, 1, 5)
ge$`count_AY1617`[is.na(ge$`count_AY1617`)] <- 0

ge.fail <- ge %>% filter(`passfail_2019` %in% c("Fail both DTE and EP", "Fail DTE only", "Fail EP only")) %>% filter(inGE==1)
ge.pass <- ge %>% filter(`passfail_2019` %in% c("Pass", "No DTE/EP data"))

# For testing purposes only: 
# set.seed(1111)
# ge.fail <- ge.fail[sample(nrow(ge.fail), 50), ]

#### End #### 

#### ZIP distance function #### 

calc_dist <- function(gepassdata, gefaildata, levelSelection, cipSelection){
  
  gefaildata$`Distance to nearest alternative` <- rep(NA, nrow(gefaildata))
  
  for(i in (1:nrow(gefaildata))){
    
    print(i)
    
    gealternatives <- gepassdata
    
    if(gefaildata$`stabbr`[i]=="AL"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("AL", "MS", "TN", "GA", "FL"))}
    
    if(gefaildata$`stabbr`[i]=="AK"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("AK"))}
    
    if(gefaildata$`stabbr`[i]=="AZ"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("AZ", "CA", "NV", "UT", "CO", "NM"))}
    
    if(gefaildata$`stabbr`[i]=="AR"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("AR", "MO", "TN", "MS", "LA", "TX", "OK"))}
    
    if(gefaildata$`stabbr`[i]=="CA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("CA", "AZ", "NV", "OR"))}
    
    if(gefaildata$`stabbr`[i]=="CO"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("CO", "WY", "NE", "KS", "OK", "NM", "AZ", "UT"))}
    
    if(gefaildata$`stabbr`[i]=="CT"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("CT", "RI", "MA", "NY", "NJ"))}
    
    if(gefaildata$`stabbr`[i]=="DE"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("DE", "NJ", "MD", "PA"))}
    
    if(gefaildata$`stabbr`[i]=="DC"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("DC", "MD", "VA"))}
    
    if(gefaildata$`stabbr`[i]=="FL"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("FL", "GA", "MS", "AL"))}
    
    if(gefaildata$`stabbr`[i]=="GA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("GA", "FL", "AL", "TN", "SC", "NC"))}
    
    if(gefaildata$`stabbr`[i]=="HI"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("HI"))}
    
    if(gefaildata$`stabbr`[i]=="ID"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("ID", "WA", "OR", "NV", "UT", "WY", "MT"))}
    
    if(gefaildata$`stabbr`[i]=="IL"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("IL", "WI", "IA", "MO", "KY", "IN", "MI", "TN"))}
    
    if(gefaildata$`stabbr`[i]=="IN"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("IN", "MI", "OH", "KY", "IL", "WI"))}
    
    if(gefaildata$`stabbr`[i]=="IA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("IA", "SD", "NE", "MO", "KS", "IL", "WI", "MN"))}
    
    if(gefaildata$`stabbr`[i]=="KS"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("KS", "NE", "CO", "OK", "MO", "IA"))}
    
    if(gefaildata$`stabbr`[i]=="KY"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("KY", "IL", "IN", "OH", "WV", "VA", "TN", "MO"))}
    
    if(gefaildata$`stabbr`[i]=="LA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("LA", "MS", "AR", "TX"))}
    
    if(gefaildata$`stabbr`[i]=="ME"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("ME", "NH", "MA", "VT"))}
    
    if(gefaildata$`stabbr`[i]=="MD"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MD", "DC", "VA", "WV", "PA", "DE", "NJ"))}
    
    if(gefaildata$`stabbr`[i]=="MA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MA", "ME", "NH", "VT", "NY", "CT", "RI"))}
    
    if(gefaildata$`stabbr`[i]=="MI"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MI", "WI", "IL", "IN", "OH"))}
    
    if(gefaildata$`stabbr`[i]=="MN"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MN", "ND", "SD", "IA", "WI"))}
    
    if(gefaildata$`stabbr`[i]=="MS"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MS", "AL", "TN", "FL", "AR", "LA"))}
    
    if(gefaildata$`stabbr`[i]=="MO"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MO", "IA", "NE", "KS", "OK", "AR", "TN", "KY", "IL"))}
    
    if(gefaildata$`stabbr`[i]=="MT"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MT", "ID", "WY", "ND", "SD"))}
    
    if(gefaildata$`stabbr`[i]=="NE"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NE", "SD", "WY", "CO", "KS", "MO", "IA"))}
    
    if(gefaildata$`stabbr`[i]=="NV"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NV", "ID", "OR", "CA", "AZ", "UT"))}
    
    if(gefaildata$`stabbr`[i]=="NH"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NH", "ME", "VT", "MA"))}
    
    if(gefaildata$`stabbr`[i]=="NJ"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NJ", "NY", "CT", "PA", "DE", "MD"))}
    
    if(gefaildata$`stabbr`[i]=="NM"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NM", "TX", "OK", "CO", "UT", "AZ"))}
    
    if(gefaildata$`stabbr`[i]=="NY"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NY", "CT", "MA", "VT", "PA", "NJ"))}
    
    if(gefaildata$`stabbr`[i]=="NC"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NC", "VA", "TN", "GA", "SC"))}
    
    if(gefaildata$`stabbr`[i]=="ND"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("ND", "MT", "SD", "MN"))}
    
    if(gefaildata$`stabbr`[i]=="OH"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("OH", "MI", "IN", "KY", "WV", "MD", "PA"))}
    
    if(gefaildata$`stabbr`[i]=="OK"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("OK", "KS", "CO", "NM", "TX", "AR", "MO"))}
    
    if(gefaildata$`stabbr`[i]=="OR"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("OR", "WA", "CA", "NV", "ID"))}
    
    if(gefaildata$`stabbr`[i]=="PA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("PA", "NY", "NJ", "DE", "MD", "WV", "OH"))}
    
    if(gefaildata$`stabbr`[i]=="RI"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("RI", "CT", "MA"))}
    
    if(gefaildata$`stabbr`[i]=="SC"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("SC", "NC", "GA"))}
    
    if(gefaildata$`stabbr`[i]=="SD"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("SD", "ND", "MT", "WY", "NE", "IA", "MN"))}
    
    if(gefaildata$`stabbr`[i]=="TN"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("TN", "KY", "MO", "AR", "MS", "AL", "GA", "NC", "VA"))}
    
    if(gefaildata$`stabbr`[i]=="TX"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("TX", "NM", "OK", "AR", "LA"))}
    
    if(gefaildata$`stabbr`[i]=="UT"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("UT", "WY", "ID", "NV", "AZ", "NM", "CO"))}
    
    if(gefaildata$`stabbr`[i]=="VT"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("VT", "ME", "NH", "NY", "CT", "MA", "RI"))}
    
    if(gefaildata$`stabbr`[i]=="VA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("VA", "MD", "DC", "WV", "KY", "TN", "NC"))}
    
    if(gefaildata$`stabbr`[i]=="WA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("WA", "OR", "ID"))}
    
    if(gefaildata$`stabbr`[i]=="WV"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("WV", "PA", "OH", "KY", "MD", "VA"))}
    
    if(gefaildata$`stabbr`[i]=="WI"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("WI", "MN", "IA", "IL", "IN", "MI"))}
    
    if(gefaildata$`stabbr`[i]=="WY"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("WY", "MT", "ID", "UT", "CO", "NE", "SD"))}
    
    if(gefaildata$`stabbr`[i]=="AS"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("AS"))}
    
    if(gefaildata$`stabbr`[i]=="GU"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("GU"))}
    
    if(gefaildata$`stabbr`[i]=="MP"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MP"))}
    
    if(gefaildata$`stabbr`[i]=="PR"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("PR"))}
    
    if(gefaildata$`stabbr`[i]=="UM"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("UM"))}
    
    if(gefaildata$`stabbr`[i]=="VI"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("VI"))}
    
    gealternatives$`Distance` <- rep(NA, nrow(gealternatives))
    program.level <- gefaildata$`cred_lvl`[i]
    program.category <- gefaildata$`Category`[i]
    program.2digCIP <- gefaildata$`cip2`[i]
    program.4digCIP <- gefaildata$`cip4`[i]
    
    # Apply the proper level filter to gepassdata
    if(levelSelection=="Same credential level"){gealternatives <- gealternatives %>% filter(`cred_lvl` == program.level)}
    if(levelSelection=="Same credential category"){gealternatives <- gealternatives %>% filter(`Category` == program.category)}
    
    # Apply the proper CIP code filter to gepassdata
    if(cipSelection=="Same 4-digit CIP"){gealternatives <- gealternatives %>% filter(`cip4`==program.4digCIP)}
    if(cipSelection=="Same 2-digit CIP"){gealternatives <- gealternatives %>% filter(`cip2`==program.2digCIP)}  
    
    # Only run the next lines if there is remaining passing programs: 
    if(nrow(gealternatives) > 0){
      
      # Calculate distance for every other program
      for(j in (1:nrow(gealternatives))){
        gealternatives$`Distance`[j] <- zip_distance(gefaildata$`zip`[i], gealternatives$`zip`[j], units="miles")$distance
      }
      
      gefaildata$`Distance to nearest alternative`[i] <- suppressWarnings(min(gealternatives$`Distance`, na.rm=TRUE))
      
    }else{
      gefaildata$`Distance to nearest alternative`[i] <- NA
    }
    
    print(gefaildata$`Distance to nearest alternative`[i])
    
    rm("gealternatives", 
       "program.level", 
       "program.category", 
       "program.2digCIP", 
       "program.4digCIP")
  }
  return(gefaildata)
}

#### End #### 

#### Run distance function ####
# Make calculations (this will take a long time to run)
ge.fail.A <- calc_dist(ge.pass, ge.fail, "Same credential level", "Same 4-digit CIP")
ge.fail.B <- calc_dist(ge.pass, ge.fail, "Same credential level", "Same 2-digit CIP")
# ge.fail.C <- calc_dist(ge.pass, ge.fail, "Same credential level", "Any CIP")
ge.fail.D <- calc_dist(ge.pass, ge.fail, "Same credential category", "Same 4-digit CIP")
# ge.fail.E <- calc_dist(ge.pass, ge.fail, "Same credential category", "Same 2-digit CIP")
# ge.fail.F <- calc_dist(ge.pass, ge.fail, "Same credential category", "Any CIP")

# Count the number of students with no alternative within 30 miles 
ge.fail.A$`Students with no nearby options` <- ifelse(ge.fail.A$`Distance to nearest alternative` > 30, ge.fail.A$`count_AY1617`, 0)
ge.fail.B$`Students with no nearby options` <- ifelse(ge.fail.B$`Distance to nearest alternative` > 30, ge.fail.B$`count_AY1617`, 0)
# ge.fail.C$`Students with no nearby options` <- ifelse(ge.fail.C$`Distance to nearest alternative` > 30, ge.fail.C$`count_AY1617`, 0)
ge.fail.D$`Students with no nearby options` <- ifelse(ge.fail.D$`Distance to nearest alternative` > 30, ge.fail.D$`count_AY1617`, 0)
# ge.fail.E$`Students with no nearby options` <- ifelse(ge.fail.E$`Distance to nearest alternative` > 30, ge.fail.E$`count_AY1617`, 0)
# ge.fail.F$`Students with no nearby options` <- ifelse(ge.fail.F$`Distance to nearest alternative` > 30, ge.fail.F$`count_AY1617`, 0)

# Set programs with no alternative within 30 miles to NA
ge.fail.A$`Distance to nearest alternative`[ge.fail.A$`Distance to nearest alternative` > 30] <- NA
ge.fail.B$`Distance to nearest alternative`[ge.fail.B$`Distance to nearest alternative` > 30] <- NA
# ge.fail.C$`Distance to nearest alternative`[ge.fail.C$`Distance to nearest alternative` > 30] <- NA
ge.fail.D$`Distance to nearest alternative`[ge.fail.D$`Distance to nearest alternative` > 30] <- NA
# ge.fail.E$`Distance to nearest alternative`[ge.fail.E$`Distance to nearest alternative` > 30] <- NA
# ge.fail.F$`Distance to nearest alternative`[ge.fail.F$`Distance to nearest alternative` > 30] <- NA

# Average distance, excluding students with no option in 30 miles 
ge.fail.A$`count_AY1617`[is.na(ge.fail.A$`count_AY1617`)] <- 0
ge.fail.B$`count_AY1617`[is.na(ge.fail.B$`count_AY1617`)] <- 0
# ge.fail.C$`count_AY1617`[is.na(ge.fail.C$`count_AY1617`)] <- 0
ge.fail.D$`count_AY1617`[is.na(ge.fail.D$`count_AY1617`)] <- 0
# ge.fail.E$`count_AY1617`[is.na(ge.fail.E$`count_AY1617`)] <- 0
# ge.fail.F$`count_AY1617`[is.na(ge.fail.F$`count_AY1617`)] <- 0

weighted.mean(ge.fail.A$`Distance to nearest alternative`, w = ge.fail.A$`count_AY1617`, na.rm=TRUE)
weighted.mean(ge.fail.B$`Distance to nearest alternative`, w = ge.fail.B$`count_AY1617`, na.rm=TRUE)
# weighted.mean(ge.fail.C$`Distance to nearest alternative`, w = ge.fail.C$`count_AY1617`, na.rm=TRUE)
weighted.mean(ge.fail.D$`Distance to nearest alternative`, w = ge.fail.D$`count_AY1617`, na.rm=TRUE)
# weighted.mean(ge.fail.E$`Distance to nearest alternative`, w = ge.fail.E$`count_AY1617`, na.rm=TRUE)
# weighted.mean(ge.fail.F$`Distance to nearest alternative`, w = ge.fail.F$`count_AY1617`, na.rm=TRUE)

# Share with no option within 30 miles 
sum(ge.fail.A$`Students with no nearby options`, na.rm=TRUE) / sum(ge.fail.A$`count_AY1617`, na.rm=TRUE)
sum(ge.fail.B$`Students with no nearby options`, na.rm=TRUE) / sum(ge.fail.B$`count_AY1617`, na.rm=TRUE)
# sum(ge.fail.C$`Students with no nearby options`, na.rm=TRUE) / sum(ge.fail.C$`count_AY1617`, na.rm=TRUE)
sum(ge.fail.D$`Students with no nearby options`, na.rm=TRUE) / sum(ge.fail.D$`count_AY1617`, na.rm=TRUE)
# sum(ge.fail.E$`Students with no nearby options`, na.rm=TRUE) / sum(ge.fail.E$`count_AY1617`, na.rm=TRUE)
# sum(ge.fail.F$`Students with no nearby options`, na.rm=TRUE) / sum(ge.fail.F$`count_AY1617`, na.rm=TRUE)

#### End #### 

#### ZIP distance function: Record nearest program #### 

calc_dist_and_record <- function(gepassdata, gefaildata, levelSelection, cipSelection){
  
  gefaildata$`Distance to nearest alternative` <- rep(NA, nrow(gefaildata))
  gefaildata$`alt_schname` <- rep(NA, nrow(gefaildata))
  gefaildata$`alt_cred_lvl` <- rep(NA, nrow(gefaildata))
  gefaildata$`alt_cip4` <- rep(NA, nrow(gefaildata))
  gefaildata$`alt_zip` <- rep(NA, nrow(gefaildata))
  gefaildata$`alt_opeid6` <- rep(NA, nrow(gefaildata))
  
  for(i in (1:nrow(gefaildata))){
    
    print(i)
    
    gealternatives <- gepassdata
    
    if(gefaildata$`stabbr`[i]=="AL"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("AL", "MS", "TN", "GA", "FL"))}
    
    if(gefaildata$`stabbr`[i]=="AK"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("AK"))}
    
    if(gefaildata$`stabbr`[i]=="AZ"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("AZ", "CA", "NV", "UT", "CO", "NM"))}
    
    if(gefaildata$`stabbr`[i]=="AR"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("AR", "MO", "TN", "MS", "LA", "TX", "OK"))}
    
    if(gefaildata$`stabbr`[i]=="CA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("CA", "AZ", "NV", "OR"))}
    
    if(gefaildata$`stabbr`[i]=="CO"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("CO", "WY", "NE", "KS", "OK", "NM", "AZ", "UT"))}
    
    if(gefaildata$`stabbr`[i]=="CT"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("CT", "RI", "MA", "NY", "NJ"))}
    
    if(gefaildata$`stabbr`[i]=="DE"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("DE", "NJ", "MD", "PA"))}
    
    if(gefaildata$`stabbr`[i]=="DC"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("DC", "MD", "VA"))}
    
    if(gefaildata$`stabbr`[i]=="FL"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("FL", "GA", "MS", "AL"))}
    
    if(gefaildata$`stabbr`[i]=="GA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("GA", "FL", "AL", "TN", "SC", "NC"))}
    
    if(gefaildata$`stabbr`[i]=="HI"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("HI"))}
    
    if(gefaildata$`stabbr`[i]=="ID"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("ID", "WA", "OR", "NV", "UT", "WY", "MT"))}
    
    if(gefaildata$`stabbr`[i]=="IL"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("IL", "WI", "IA", "MO", "KY", "IN", "MI", "TN"))}
    
    if(gefaildata$`stabbr`[i]=="IN"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("IN", "MI", "OH", "KY", "IL", "WI"))}
    
    if(gefaildata$`stabbr`[i]=="IA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("IA", "SD", "NE", "MO", "KS", "IL", "WI", "MN"))}
    
    if(gefaildata$`stabbr`[i]=="KS"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("KS", "NE", "CO", "OK", "MO", "IA"))}
    
    if(gefaildata$`stabbr`[i]=="KY"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("KY", "IL", "IN", "OH", "WV", "VA", "TN", "MO"))}
    
    if(gefaildata$`stabbr`[i]=="LA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("LA", "MS", "AR", "TX"))}
    
    if(gefaildata$`stabbr`[i]=="ME"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("ME", "NH", "MA", "VT"))}
    
    if(gefaildata$`stabbr`[i]=="MD"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MD", "DC", "VA", "WV", "PA", "DE", "NJ"))}
    
    if(gefaildata$`stabbr`[i]=="MA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MA", "ME", "NH", "VT", "NY", "CT", "RI"))}
    
    if(gefaildata$`stabbr`[i]=="MI"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MI", "WI", "IL", "IN", "OH"))}
    
    if(gefaildata$`stabbr`[i]=="MN"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MN", "ND", "SD", "IA", "WI"))}
    
    if(gefaildata$`stabbr`[i]=="MS"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MS", "AL", "TN", "FL", "AR", "LA"))}
    
    if(gefaildata$`stabbr`[i]=="MO"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MO", "IA", "NE", "KS", "OK", "AR", "TN", "KY", "IL"))}
    
    if(gefaildata$`stabbr`[i]=="MT"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MT", "ID", "WY", "ND", "SD"))}
    
    if(gefaildata$`stabbr`[i]=="NE"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NE", "SD", "WY", "CO", "KS", "MO", "IA"))}
    
    if(gefaildata$`stabbr`[i]=="NV"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NV", "ID", "OR", "CA", "AZ", "UT"))}
    
    if(gefaildata$`stabbr`[i]=="NH"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NH", "ME", "VT", "MA"))}
    
    if(gefaildata$`stabbr`[i]=="NJ"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NJ", "NY", "CT", "PA", "DE", "MD"))}
    
    if(gefaildata$`stabbr`[i]=="NM"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NM", "TX", "OK", "CO", "UT", "AZ"))}
    
    if(gefaildata$`stabbr`[i]=="NY"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NY", "CT", "MA", "VT", "PA", "NJ"))}
    
    if(gefaildata$`stabbr`[i]=="NC"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("NC", "VA", "TN", "GA", "SC"))}
    
    if(gefaildata$`stabbr`[i]=="ND"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("ND", "MT", "SD", "MN"))}
    
    if(gefaildata$`stabbr`[i]=="OH"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("OH", "MI", "IN", "KY", "WV", "MD", "PA"))}
    
    if(gefaildata$`stabbr`[i]=="OK"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("OK", "KS", "CO", "NM", "TX", "AR", "MO"))}
    
    if(gefaildata$`stabbr`[i]=="OR"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("OR", "WA", "CA", "NV", "ID"))}
    
    if(gefaildata$`stabbr`[i]=="PA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("PA", "NY", "NJ", "DE", "MD", "WV", "OH"))}
    
    if(gefaildata$`stabbr`[i]=="RI"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("RI", "CT", "MA"))}
    
    if(gefaildata$`stabbr`[i]=="SC"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("SC", "NC", "GA"))}
    
    if(gefaildata$`stabbr`[i]=="SD"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("SD", "ND", "MT", "WY", "NE", "IA", "MN"))}
    
    if(gefaildata$`stabbr`[i]=="TN"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("TN", "KY", "MO", "AR", "MS", "AL", "GA", "NC", "VA"))}
    
    if(gefaildata$`stabbr`[i]=="TX"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("TX", "NM", "OK", "AR", "LA"))}
    
    if(gefaildata$`stabbr`[i]=="UT"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("UT", "WY", "ID", "NV", "AZ", "NM", "CO"))}
    
    if(gefaildata$`stabbr`[i]=="VT"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("VT", "ME", "NH", "NY", "CT", "MA", "RI"))}
    
    if(gefaildata$`stabbr`[i]=="VA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("VA", "MD", "DC", "WV", "KY", "TN", "NC"))}
    
    if(gefaildata$`stabbr`[i]=="WA"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("WA", "OR", "ID"))}
    
    if(gefaildata$`stabbr`[i]=="WV"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("WV", "PA", "OH", "KY", "MD", "VA"))}
    
    if(gefaildata$`stabbr`[i]=="WI"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("WI", "MN", "IA", "IL", "IN", "MI"))}
    
    if(gefaildata$`stabbr`[i]=="WY"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("WY", "MT", "ID", "UT", "CO", "NE", "SD"))}
    
    if(gefaildata$`stabbr`[i]=="AS"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("AS"))}
    
    if(gefaildata$`stabbr`[i]=="GU"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("GU"))}
    
    if(gefaildata$`stabbr`[i]=="MP"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("MP"))}
    
    if(gefaildata$`stabbr`[i]=="PR"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("PR"))}
    
    if(gefaildata$`stabbr`[i]=="UM"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("UM"))}
    
    if(gefaildata$`stabbr`[i]=="VI"){gealternatives <- gealternatives %>% filter(`stabbr` %in% c("VI"))}
    
    gealternatives$`Distance` <- rep(NA, nrow(gealternatives))
    program.level <- gefaildata$`cred_lvl`[i]
    program.category <- gefaildata$`Category`[i]
    program.2digCIP <- gefaildata$`cip2`[i]
    program.4digCIP <- gefaildata$`cip4`[i]
    
    # Apply the proper level filter to gepassdata
    if(levelSelection=="Same credential level"){gealternatives <- gealternatives %>% filter(`cred_lvl` == program.level)}
    if(levelSelection=="Same credential category"){gealternatives <- gealternatives %>% filter(`Category` == program.category)}
    
    # Apply the proper CIP code filter to gepassdata
    if(cipSelection=="Same 4-digit CIP"){gealternatives <- gealternatives %>% filter(`cip4`==program.4digCIP)}
    if(cipSelection=="Same 2-digit CIP"){gealternatives <- gealternatives %>% filter(`cip2`==program.2digCIP)}  
    
    # Only run the next lines if there is remaining passing programs: 
    if(nrow(gealternatives) > 0){
      
      # Calculate distance for every other program
      for(j in (1:nrow(gealternatives))){
        gealternatives$`Distance`[j] <- zip_distance(gefaildata$`zip`[i], gealternatives$`zip`[j], units="miles")$distance
      }
      
      gealternatives <- gealternatives %>% filter(is.na(`Distance`)==FALSE)
      gefaildata$`Distance to nearest alternative`[i] <- suppressWarnings(min(gealternatives$`Distance`, na.rm=TRUE))
      gealternatives <- gealternatives %>% filter(is.infinite(`Distance`)==FALSE) 
      gealternatives <- gealternatives %>% arrange(`Distance`, desc(`mdearnp3`))
      
      gefaildata$`alt_schname`[i] <- gealternatives$`schname`[1]
      gefaildata$`alt_cred_lvl`[i] <- gealternatives$`cred_lvl`[1]
      gefaildata$`alt_cip4`[i] <- gealternatives$`cip4`[1]
      gefaildata$`alt_zip`[i] <- gealternatives$`zip`[1]
      gefaildata$`alt_opeid6`[i] <- gealternatives$`opeid6`[1]
      
    }else{
      gefaildata$`Distance to nearest alternative`[i] <- NA
      gefaildata$`alt_schname`[i] <- NA
      gefaildata$`alt_cred_lvl`[i] <- NA
      gefaildata$`alt_cip4`[i] <- NA
      gefaildata$`alt_zip`[i] <- NA
      gefaildata$`alt_opeid6`[i] <- NA
    }
    
    print(gefaildata$`Distance to nearest alternative`[i])
    print(gefaildata$`alt_schname`[i])
    
    rm("gealternatives", 
       "program.level", 
       "program.category", 
       "program.2digCIP", 
       "program.4digCIP")
  }
  return(gefaildata)
}

ge.fail.A_record <- calc_dist_and_record(ge.pass, ge.fail, "Same credential level", "Same 4-digit CIP")
write.csv(ge.fail.A_record, "ge.fail.A_record.csv", row.names=FALSE)

#### End #### 

