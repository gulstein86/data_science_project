install.packages('XLConnect')

library(XLConnect) # Read data
library(rgdal) # Read shapefile 
library(dplyr) # Clean data
library(leaflet) # Plot maps

life <- readWorksheetFromFile("input/IHME_USA_COUNTY_LE_MORTALITY_RISK_1980_2014_NATIONAL_Y2017M05D08.xlsx",
                              sheet=1,
                              startRow = 2,
                              endCol = 12)
glimpse(life)

life <- life[,c(2,10,11)]
life <- life[-1,]

colnames(life) <- c('GEOID', 'exp_leg', 'change_leg')

# The column with the values will be duplicated. A column  with the 95 % Confidence Interval and the value. The other only with the value.
life$exp <- life$exp_leg
life$change <- life$change_leg

life$exp <- gsub( " *\\(.*?\\) *", "", life$exp) # Delete values between parenthesis
life$change <- gsub( " *\\(.*?\\) *", "", life$change) # Delete values between parenthesis

life$exp <- as.numeric(as.character(life$exp))*100000
life$change <- as.numeric(as.character(life$change))
life$GEOID <- as.character(life$GEOID)

# There are some values that are not correctly converted to character class (eliminate the first zero). In this case, we select those that have a length of 1 or 4 and we add a zero.
life$GEOID <- ifelse(nchar(life$GEOID) == 1 | nchar(life$GEOID) == 4  , paste0('0',life$GEOID),life$GEOID)
glimpse(life)

# US MAP - STATES
us.map.state <- readOGR(dsn= 'input/states_21basic', layer = "states", stringsAsFactors = FALSE)

# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60) Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map.state <- us.map.state[!us.map.state$STATE_FIPS %in% c("02", "72", "66", "78", "60", "69","64", "68", "70", "74"),]

# Make sure other outling islands are removed.
us.map.state <- us.map.state[!us.map.state$STATE_FIPS %in% c("81", "84", "86", "87", "89", "71", "76","95", "79"),]

# US MAP - COUNTIES
us.map.county <- readOGR(dsn= 'input/UScounties', layer = "UScounties", stringsAsFactors = FALSE)
# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60) Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)

us.map.county <- us.map.county[!us.map.county$STATE_FIPS %in% c("02", "15", "72", "66", "78", "60", "69","64", "68", "70", "74"),]

# Make sure other outling islands are removed.
us.map.county <- us.map.county[!us.map.county$STATE_FIPS %in% c("81", "84", "86", "87", "89", "71", "76","95", "79"),]

# 4. Life expectancy map

leafmap <- merge(us.map.state, life, by.x='STATE_FIPS', by.y= 'GEOID')

popup_dat <- paste0("<strong>State: </strong>",
                    leafmap$STATE_NAME,
                    "<br><strong>Life expectancy with 95% CI : </strong>",
                    leafmap$exp_leg)

pal <- colorNumeric("Spectral", NULL, n = 13)

leaflet(data = leafmap) %>% 
  addTiles() %>%
  addPolygons(fillColor = ~pal(exp),
              fillOpacity = 0.8,
              color = "#BDBDC3",
              weight = 1,
              popup = popup_dat) %>%
  addLegend("bottomright", pal = pal, values = ~exp,
            title = "Life expectancy",
            opacity = 1 )

leafmap <- merge(us.map.county, life, by= 'GEOID')

popup_dat <- paste0("<strong>State: </strong>",
                    leafmap$NAME,
                    "<br><strong>Life expectancy with 95% CI : </strong>",
                    leafmap$exp_leg)
pal <- colorQuantile("Spectral", NULL, n = 10)


leaflet(data = leafmap) %>% 
  addTiles() %>%
  addPolygons(fillColor = ~pal(exp),
              fillOpacity = 0.8,
              color = "#BDBDC3",
              weight = 1,
              popup = popup_dat)
