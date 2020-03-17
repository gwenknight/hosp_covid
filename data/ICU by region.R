### Data by region
library("tidyverse")
library("reshape2")
library("ggplot2")
datar <- read.csv("data/ICU by region.csv")[1:144,]

# Remove %
datar$Adult.critical.care.beds <- as.numeric(sub("%", "", datar$Adult.critical.care.beds ))

# Group by regions
region_data <- datar %>% group_by(Region) %>% summarise(nccbeds = sum(Number.of.Adult.critical.care.beds), occ = mean(Adult.critical.care.beds))

# Patient size by regions
region_pat <- read.csv("data/region_age.csv",stringsAsFactors = FALSE)[1:7,]

region_data$popsize <- region_pat$All.Ages

# Occupied beds 
occ_bed_pday <- (region_data$occ/100) * region_data$nccbeds # number of occupied beds per day
inc_rate <- occ_bed_pday / 7.9

region_data$occ_bed_pday <- occ_bed_pday
region_data$inc_rate <- inc_rate

# Wuhan like scenario
# 2.6 / 10,000 peak incidence 
# length of stay
region_data$wuhan_covid <- round(region_data$popsize*2.6/10000 / 10 ,0)


write.csv(region_data,"data/region_data.csv")

### output for doc
dd$occ3mo <- 90*dd$occ_bed_pday
dd$unique <- dd$occ3mo/7.9
dd_output <- dd[,c("nccbeds","popsize","occ",
                   "occ3mo","unique","inc_rate")]

dd_output$occ <- round(dd_output$occ,0)
dd_output$unique <- round(dd_output$unique,0)
dd_output$inc_rate <- round(dd_output$inc_rate,0)
dd_output$occ3mo <- round(dd_output$occ3mo,0)

# Cases by region
region_cases_orig <- read.csv("data/Local_regions.csv")[,1:12]

region_cases7 <- melt(region_cases_orig[,1:8], id.vars = "Date")

ggplot(region_cases7, aes(x=Date, y = value)) + geom_line(aes(group = variable, col = variable)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Imperial 
# 8 ICU beds per 100,000 population 
surge_level <- 8*region_data$popsize/100000 
surge_level / region_data$nccbeds
