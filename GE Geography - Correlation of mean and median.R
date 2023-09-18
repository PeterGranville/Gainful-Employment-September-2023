# https://data.census.gov/table?q=income&g=010XX00US$8600000&tid=ACSST5Y2021.S1902


means <- read.csv("ACSST5Y2021.S1902-Data.csv", header=TRUE) 
means <- means %>% select(
  `NAME`,           # Geographic Area Name
  `S1902_C03_001E`	# Estimate!!Mean income (dollars)!!HOUSEHOLD INCOME!!All households
) %>% filter(NAME != "Geographic Area Name") %>% filter((NAME %in% c("ZCTA5 72405", "ZCTA5 72713", "ZCTA5 75036", "ZCTA5 75072", "ZCTA5 89437", "ZCTA5 97003"))==FALSE)

means$`S1902_C03_001E` <- as.numeric(means$`S1902_C03_001E`)


# Source: https://data.census.gov/table?q=income&g=010XX00US$8600000&tid=ACSST5Y2021.S1902

medians <- read.csv("ACSST5Y2021.S1903-Data.csv", header=TRUE) 
medians <- medians %>% select(
  `NAME`,           # Geographic Area Name
  `S1903_C03_001E`	# Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households
) %>% filter(NAME != "Geographic Area Name") %>% filter((NAME %in% c("ZCTA5 72405", "ZCTA5 72713", "ZCTA5 75036", "ZCTA5 75072", "ZCTA5 89437", "ZCTA5 97003"))==FALSE)

medians$`S1903_C03_001E` <- as.numeric(medians$`S1903_C03_001E`)



income <- inner_join(x=means, y=medians, by="NAME")
cor(x=income$S1902_C03_001E, y=income$S1903_C03_001E, method="spearman", use="complete.obs")


