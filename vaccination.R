# COVID data: git clone https://github.com/owid/covid-19-data.git
# GDP data: https://ourworldindata.org/grapher/gdp-per-capita-worldbank?tab=chart&region=World


dd <- as.matrix(read.csv2("vaccinations.csv", sep = ","))

bb <- NULL
country <- ""
countrynmbr <- 0
for (i in 1:dim(dd)[1]) {
  if (as.character(dd[i, 1]) != country) {
    bb <- rbind(bb, c(dd[i, 1], as.numeric(dd[i, 9])))
    country <- dd[i, 1]
    countrynmbr <- countrynmbr + 1
  } else {
    bb[countrynmbr, 2] <- as.numeric(dd[i, 9])
  }
}
# remove EU 19 and world 64

plot(density(log(as.numeric(bb[c(-19, -64), 2]))))
lines(c(log(as.numeric(bb[22, 2])), log(as.numeric(bb[22, 2]))), c(0, 0.45), col = 2)

ii <- read.csv("gdp-per-capita-worldbank.csv")
gg <- NULL
country <- ""
countrynmbr <- 0
for (i in 1:dim(ii)[1]) {
  if (as.character(ii[i, "Entity"]) != country) {
    gg <- rbind(gg, c(as.character(ii[i, "Entity"]), as.numeric(ii[i, 4])))
    country <- as.character(ii[i, "Entity"])
    countrynmbr <- countrynmbr + 1
  } else {
    gg[countrynmbr, 2] <- as.numeric(ii[i, 4])
  }
}

bb <- cbind(bb, rep(0, dim(bb)[1]))

l <- dim(gg)[1]
for (i in 1:dim(bb)[1]) {
  ind <- (1:l)[gg[, 1] == bb[i, 1]]
  if (length(ind == 1)) bb[i, 3] <- gg[ind, 2]
}

plot(log(as.numeric(bb[, 3])), log(as.numeric(bb[, 2])), xlab = "log-income per capita", ylab = "log vacciation rate")
