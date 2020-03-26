### Region fit

data_region <- read.csv("data/regions_cases.csv")[,-1] # Taken from Ari Ecole github
region_cases <- melt(data_region)
region_cases$variable <- gsub("X", "", region_cases$variable)

region_cases$date <- as.Date(region_cases$variable, tryFormats = c("%d.%m.%Y"))

ggplot(region_cases, aes(x=date, y = value)) + 
  geom_point(aes(group = region, col=region)) + 
  scale_x_date("Date") + 
  scale_y_continuous("Number of cases") + 
  theme_bw()

region_names <- data_region[,1]
for(i in 1:length(region_names)){
    
  
  
}
