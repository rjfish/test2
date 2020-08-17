# Ryan Fisher
# Syracuse University
# IST719 - Fall 2019 
# Poster Creation

install.packages("rworldmap")
library(ggplot2)
library(rworldmap)
library(scales)
library(RColorBrewer)
#############################
# Data Loading
#############################
# Create the  fully qualified path 'winners' file
fname <- file.choose()
# read the CSV file is using read.csv and 'fname'
df <- read.csv(file = fname
                    , header = TRUE
                    , stringsAsFactors = FALSE
                    , sep = ","
)
# Formula Check via function
# (NumberOfColumns * 4) * (NumberOfRows/100) >= 100
datacheck <- function(x){
  x <- (ncol(x)*4) * (nrow(x)/100)
  #((ncol(x)*4) * (nrow(x)/100) >= 100)
  print(x)
  print(x >= 100)
}
datacheck(df)
# data meets the requirements

#############################
# Data Exploration
#############################
str(df)
summary(df)
colnames(df)
length(unique(df$Entity)) #233 Entities
length(unique(df$Code)) # 223 Codes
## this means that some entities have a blank code
length(df$Year) # 20853 observations

# create new DF before transformations
climate <- df

# change columns names b/c col4 is terrible
colnames(climate) <- c("entity", "code", "year", "co2")
colnames(climate)
head(climate)

# sort DF by years column
climate <- climate[order(climate$year),]
# there are rows with a blank Code
##sum(climate$code == "")

# there are some observations with negative CO2 values, which will be excluded
climate2 <- climate[climate$co2 >= 0,]
nrow(climate2) #20,813

#view the distribution of the year data
hist(climate2$year)
# data is very heavily weighted towards later years
mean(climate2$year)
median(climate2$year)
# will subset to 1940 to make data easier to work with visually
# check if 1940 will still meet requirements
datacheck(climate2[climate2$year >= "1940",])
# subset to year >= 1940
climate2 <- climate2[climate2$year >= "1940",]

head(climate2)

#looking at co2 values
mean(climate2$co2)
median(climate2$co2)
hist(climate2$year)
# these are in tonnes per annum, which will be converted to 'millions tonnes
# this will help get away from scientific notation hopefully
climate2$co2 <- climate2$co2/1000000
hist(climate2$co2)

# there are some aggregation groups that will be useful to have seperated
(unique(climate[climate$code == "",1]))
# create subset for summary graphs
# this will exclude some of the entities
library(dplyr)
sum_df <- climate2 %>%
  select(entity, code, year, co2) %>%
  filter(code %in% c("USA","CHN","IND") | 
           entity %in% c("Africa", "Americas (other)", "Asia and Pacific (other)", 
                         "EU-28", "Europe (other)", "Middle East"))
# check to make sure we got all 9 groups
unique(sum_df$entity)

colnames(climate2)
#############################
# Aggregation
#############################
# country by year 
storage <- climate2 %>%
  group_by(entity, year) %>%
  summarize(
  sum_ann = sum(co2)
  )
colnames(sum_df)

#By global by year
global_sum <- sum_df %>%
  group_by(year) %>%
  summarize(
    ann_sum = sum(co2)
  )
barplot(global_sum$ann_sum)

# Region Sum
region_sum <- sum_df %>%
  group_by(entity) %>%
  summarise(
    total = sum(co2)
  )
colnames(sum_df)

# Country Sum
country_sum <- climate2 %>%
  group_by(code) %>%
  summarise(
    total = sum(co2)
  )

barplot(region_sum$total
        , names.arg=region_sum$entity
        #, col = fill_colors
        , border = NA
        #, xlab = "Year (1940 - 2017)"
        #, ylab = "million tonnes of CO2 emissions"
        #, main = "US Emissions vs. Rest of World"
)
region_sum %>%
  arrange(desc(region_sum$total))
region_sum
sum(region_sum$total)
#US vs China
temp_df <- sum_df %>%
  select(code, year, co2) %>%
  filter(code %in% c("USA","CHN"))

temp_df <- NULL
temp_df <- sum_df %>%
  select(code,year, co2) %>%
  filter(code %in% c("CHN"))
temp_df <- temp_df[,2:3]
colnames(temp_df) <- c("Year", "China")
## adding US
temp2 <- sum_df %>%
  select(code, co2) %>%
  filter(code %in% c("USA"))

temp_df$USA <- temp2$co2
temp2 <- sum_df %>%
  select(entity, co2) %>%
  filter(entity %in% c("EU-28"))
temp_df$EU <- temp2$co2

####### CHINA took #1 spot in 2006

##############
# Global Heat Map
##############
colnames(country_sum)
colnames(country_sum) <- c("Country.Code", "Emissions")
htmap <- country_sum
#get ride of blanks
htmap <- htmap[! htmap$Country.Code == "",]
htmap$Emissions <- log(htmap$Emissions)
htmap$Emissions <- rescale(htmap$Emissions)
htmap$Emissions <- round(htmap$Emissions,2)
htmap <- htmap[htmap$Emissions > 0.1,]
htmap <- htmap[order(-htmap$Emissions),]
mapped_data <- joinCountryData2Map(htmap, joinCode = "ISO3", 
                                   nameJoinColumn = "Country.Code")
#
#Palette Ramp
palette <- colorRampPalette(c("white", "grey90", "#990000"))
pppar <- par()
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapDevice()
mapCountryData(mapped_data, 
               nameColumnToPlot = "Emissions"
               #, colourPalette = "heat"
               #, colourPalette = "palette"
               #, colourPalette = "diverging"
               #, colourPalette = "white2Black"
               , colourPalette = palette(79)
               , numCats = 79
               , catMethod = "pretty"
               #, catMethod = "diverging"
               , oceanCol = "#333333"
               , missingCountryCol = NA
               #, landCol ="gray"
               , addLegend = TRUE
               , mapRegion = "world"
               , borderCol = "black"
               , nameColumnToHatch = ""
)

head(htmap)

#############################
# Data Viz
#############################
par(mfrow=c(1,1))

# fill_colors array for country = US
fill_colors <- c()
for (i in 1: length(sum_df$entity)){
  # if entity is US, highlight in BLUE
  if (sum_df$entity[i] == "United States") {
    fill_colors <- c(fill_colors, "#4A6B8A")
    # else highlight in gray
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}
# shaded barplot for US
barplot(sum_df$co2
        , names.arg=sum_df$year
        , col = fill_colors
        , border = NA
        , xlab = "Year (1940 - 2017)"
        , ylab = "million tonnes of CO2 emissions"
        , main = "US Emissions vs. Rest of World"
)

# fill_colors array for country = EU-28
fill_colors <- c()
for (i in 1: length(sum_df$entity)){
  # if entity is India, highlight in GREEN
  if (sum_df$entity[i] == "EU-28") {
    fill_colors <- c(fill_colors, "#44B232")
    # else highlight in gray
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}
# shaded barplot for EU
barplot(sum_df$co2
        , names.arg=sum_df$year
        , col = fill_colors
        , border = NA
        , xlab = "Year (1940 - 2017)"
        , ylab = "million tonnes of CO2 emissions"
        , main = "EU Emissions vs. Rest of World"
)

#shaded barplot for China
# fill_colors array for country = CHINA
fill_colors <- c()
for (i in 1: length(sum_df$entity)){
  # if entity is US, highlight in BLUE
  if (sum_df$entity[i] == "China") {
    fill_colors <- c(fill_colors, "#970707")
    # else highlight in gray
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}
# shaded barplot for China
barplot(sum_df$co2
        , names.arg=sum_df$year
        , col = fill_colors
        , border = NA
        , xlab = "Year (1940 - 2017)"
        , ylab = "million tonnes of CO2 emissions"
        , main = "China Emissions vs. Rest of World"
)

#view the distribution of the year data
color.hist(climate$year
  #, border = TRUE
  , col = "gray"
  , xlab = "Year (1751 - 2017)"
  , ylab = "Freq of annual observations"
  , main = "Histogram of Annual CO2 Emissions Observations"
     
)
barplot(sum_df$co2, names.arg=sum_df$year)

par()

## histogram fuction
library(ggplot2)
HistFunc <- function(dataframe, plotVal, binDivider, lowCol, highCol, title) {
  ggplot(data = dataframe, aes(plotVal, fill = ..count..)) +
    geom_histogram(binwidth = sd(plotVal)/binDivider, color = "black", show.legend = FALSE) +
    geom_vline(aes(xintercept=mean(plotVal)), color="red1", linetype="dashed", size=1) +
    scale_fill_gradient(low = lowCol, high = highCol) +
    #ggtitle(title) +
    ylab("Frequency") +
    xlab(title)+
    #scale_y_continuous(limits = c(0, 13)) + 
    #geom_label(label=plotVal, color = "black", size = 4.5, nudge_x = 0.25, show.legend = FALSE)+
    theme_grey()
}


par(mfrow=c(1,1))
# climate Year Histogram
HistFunc(climate, climate$year, 5, "deepskyblue1","blue3", "Year (1751 - 2017)")

# CO2 Emissions by Year Histogram (1940 - 2017)
annual_df <- aggregate(climate2$co2, list(climate2$year), sum)
colnames(annual_df) <- c("year", "co2")
barplot(annual_df$co2
  , names.arg = annual_df$year
  , col = grDevices::heat.colors(length(annual_df$year)) 
  , xlab = "Year (1940 - 2017)"
  , ylab = "Million Tonnes CO2 Emissions"
  , main = "Global CO2 Emissions"
  
)
library(RColorBrewer)
# CO2 Emissions by Area total (1940-2017)
pie_df <- aggregate(sum_df$co2, list(sum_df$entity), sum)
colnames(pie_df) <- c("area","co2")
pie(pie_df$co2
    , labels = pie_df$area
    , main = "CO2 Emissions by Global Area (1940 - 2017)"
    , col = brewer.pal(9, "PRGn") 
)
    

