library(XML)

web <- 'http://en.wikipedia.org/wiki/List_of_bicycle_sharing_systems'
tables <- readHTMLTable(web, stringsAsFactors = FALSE)
tb <- tables[[1]]

cities <- paste(tb$City, tb$Country, sep =',')
cities <- cities[-c(346,347)]
coord <- geocode(cities, output = 'latlon')
head(coord)
cities <- data.frame(cities, coord)
tb <-    tb[-c(346:347),]
cities <- data.frame(cities, status = tb$Status, year = tb$Year, station = tb$Stations, bike = tb$Bicycles)

cities$year <- as.character(cities$year)
get_year.f <- function(x){
  y <- substring(x, nchar(x)-3, nchar(x))
  return(y)
}

year <- sapply(cities$year, get_year.f)
cities$year <- year

cities$status <- as.character(cities$status)
cities$closedyear <- ifelse(grepl('Closed', cities$status), substring(cities$status, (nchar(cities$status)-4), (nchar(cities$status) -1)), NA)
cities$status[grep('Closed', cities$status)] = 'closed'

#need some cleaning
cities <- cities[-c(352,353),]
cities$year[which(cities$year == '974)')] = 2010
cities$year[which(cities$year == '237]')] = 2009
cities$year[which(cities$year == 'lot)')] = 2011
cities$year[which(cities$year == '006)')] = 2006
cities$status[which(cities$status == '  ?' | cities$status == 'Inactive')] = 'closed'
cities$status[which(cities$status == 'On Hold' | cities$status == 'Was due to open April 2013')] = 'Planned'
rg <- max(cities$year, na.rm = TRUE) - min(cities$year, na.rm = TRUE) + 1
dates <- data.frame(year = min(as.numeric(cities$year), na.rm = TRUE):max(as.numeric(cities$year), na.rm = TRUE), idx = 1:rg)

cities <- left_join(cities, dates)
save(dates, file = 'dates')
cities$idx[is.na(cities$idx)] = max(cities$idx, na.rm = TRUE)
save(cities, file = 'bikeshareCities')
