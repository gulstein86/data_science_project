install.packages('tidyverse')
install.packages('readxl')
install.packages('leaflet')
install.packages('rgdal')


library(tidyverse)
library(readxl)
library(leaflet) # Plot maps
library(rgdal)

df <- read_excel("input/DataDownload - Copy.xlsx", 2)
worksheet <- df %>% distinct(`Category Code`) %>% pull(`Category Code`)

for (sheets in worksheet){
  df_name <- paste('df',sheets,sep='_')
  # print(df_name)
  assign(paste(df_name), read_excel("input/DataDownload - Copy.xlsx", sheet = sheets))
}

state_data <- read_excel("input/DataDownload - Copy.xlsx", 4)

# US MAP - STATES
us.map.state <- readOGR(dsn= 'input/states_21basic', layer = "states", stringsAsFactors = FALSE)

# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60) Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map.state <- us.map.state[!us.map.state$STATE_FIPS %in% c("02","15","72", "66", "78", "60", "69","64", "68", "70", "74"),]

# Make sure other outling islands are removed.
us.map.state <- us.map.state[!us.map.state$STATE_FIPS %in% c("81", "84", "86", "87", "89", "71", "76","95", "79"),]

# US MAP - COUNTIES
us.map.county <- readOGR(dsn= 'input/UScounties', layer = "UScounties", stringsAsFactors = FALSE)
# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60) Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)

us.map.county <- us.map.county[!us.map.county$STATE_FIPS %in% c("02", "15", "72", "66", "78", "60", "69","64", "68", "70", "74"),]

# Make sure other outling islands are removed.
us.map.county <- us.map.county[!us.map.county$STATE_FIPS %in% c("81", "84", "86", "87", "89", "71", "76","95", "79"),]

# test state plot
test_state_data <- select(state_data[complete.cases(state_data), ],StateFIPS,'State Population, 2009') %>% mutate(popu=as.character(`State Population, 2009`))
names(test_state_data)[2] <- 'population'
leafmap2 <- merge(us.map.state, test_state_data, by.x= 'STATE_FIPS', by.y='StateFIPS')

popup_dat2 <- paste0("<strong>State: </strong>",
                    leafmap2$STATE_NAME,
                    "<br><strong>State Population, 2009 : </strong>",
                    leafmap2$popu)

pal <- colorNumeric("Spectral", NULL, n = 13)

leaflet(data = leafmap2) %>% 
  addTiles() %>%
  addPolygons(fillColor = ~pal(population),
              fillOpacity = 0.8,
              color = "#BDBDC3",
              weight = 1,
              popup = popup_dat2) %>%
  addLegend("bottomright", pal = pal, values = ~population,
            title = "State Population, 2009",
            opacity = 1 )

# test for county
test_county_data <- select(df_RESTAURANTS,FIPS,FFR14) %>% mutate(FFR14_chr=as.character(FFR14))
leafmap3 <- merge(us.map.county, test_county_data, by.x= 'FIPS', by.y='FIPS')

popup_dat3 <- paste0("<strong>County: </strong>",
                    leafmap3$NAME,
                    "<br><strong>Fast-food restaurants, 2014 : </strong>",
                    leafmap3$FFR14_chr)
pal <- colorQuantile("Spectral", NULL, n = 10)


leaflet(data = leafmap3) %>% 
  addTiles() %>%
  addPolygons(fillColor = ~pal(FFR14),
              fillOpacity = 0.8,
              color = "#BDBDC3",
              weight = 1,
              popup = popup_dat3)
