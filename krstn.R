datasourceConfirmed <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
datasourceDeaths <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

# use library curl to download the data (are there other ways?)
library(curl)

# load the number of confirmed cases [region x time]
tmp <- tempfile()
curl_download(datasourceConfirmed, tmp)
dataConfirmed <- read.csv(tmp)

# load the number of deaths [region x time]
tmp2 <- tempfile()
curl_download(datasourceDeaths, tmp2)
dataDeaths <- read.csv(tmp2)

# 121 Germany
# 138 Italy
# 202 Spain
# 206 Sweden
region <- 206

confirmed <- as.numeric(dataConfirmed[region, -(1:4)])
deaths <- as.numeric(dataDeaths[region, -(1:4)])

confirmedDiff <- c(0, diff(confirmed))
deathsDiff <- c(0, diff(deaths))

confirmedDoubleLogRatio <- log(log(confirmed / confirmedDiff)) 
deathsDoubleLogRatio <- log(log(deaths / deathsDiff))
confirmedDoubleLogRatio[!is.finite(confirmedDoubleLogRatio)] <- NA
deathsDoubleLogRatio[!is.finite(deathsDoubleLogRatio)] <- NA
now

dataRegion <- data.frame(days = 1:(dim(dataConfirmed)[2]-4),
                         date = as.Date(colnames(dataConfirmed)[-(1:4)], "X%m.%d.%y"),
                         confirmed = confirmed,
                         confirmedDiff = confirmedDiff,
                         confirmedRatio = confirmedDoubleLogRatio,
                         deaths = deaths,
                         deathsDiff = deathsDiff,
                         deathsRatio = deathsDoubleLogRatio)

modelConfirmed <- lm(confirmedRatio ~ days, dataRegion, na.action = na.omit)
modelDeaths <- lm(deathsRatio ~ days, dataRegion, na.action = na.omit)

library(ggplot2)
library(gridExtra)
DiffPlot <- ggplot(dataRegion, aes(date, confirmedRatio)) + 
  ggtitle(paste("A: log(log(N/deltaN)) for", dataConfirmed[region, 2])) +
  geom_point() +
  geom_point(data = dataRegion, aes(date, deathsRatio), colour = 'red') 
GumbelPlot <- ggplot(dataRegion, aes(date, confirmedDiff)) +
  ggtitle(paste("B: deltaN for", dataConfirmed[region, 2])) +
  geom_point() +
  geom_point(data = dataRegion, aes(date, deathsDiff), colour = 'red')
grid.arrange(DiffPlot, GumbelPlot, ncol=2)