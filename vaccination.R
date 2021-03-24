# COVID data: git clone https://github.com/owid/covid-19-data.git
# GDP data: https://ourworldindata.org/grapher/gdp-per-capita-worldbank?tab=chart&region=World

library(tidyverse)
library(ggrepel)

dd <- as.matrix(da <- read.csv2("data/vaccinations.csv", sep = ","))

db <- read_csv("data/vaccinations.csv")

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


EU <- c("Austria",
        "Belgium",
        "Bulgaria")

(dc <- read_csv("gdp-per-capita-worldbank.csv") %>% 
  rename(GDP = 4)) %>% 
  ggplot(aes(Year, GDP, group=Entity))+
  geom_path(alpha=.3)+
  scale_y_log10()

(dc %>% group_by(Entity) %>% 
    arrange(desc(Year)) %>% 
    top_n(1) %>% 
    rename(location="Entity",
           iso_code="Code") %>% 
    right_join(db)%>% 
    mutate(pop = people_vaccinated/people_vaccinated_per_hundred*100) %>% 
    select(c(location, date, people_vaccinated_per_hundred, GDP, pop), everything()) ->de)

(bb %>% 
  as_tibble() %>% 
  mutate(V2 = as.numeric(V2),
         V3 = as.numeric(V3)) %>% 
  rename(income="V3",
         vaccrate = "V2") %>% 
    filter(!is.na(income),
           income>0,
           !is.na(vaccrate))-> bbdat)%>% 
  ggplot(aes(income, vaccrate, label=location,
             col = location == "Israel" | location=="Germany"))+
  geom_point()+
  geom_text_repel()+
  scale_x_log10()+
  scale_y_log10()+
  scale_color_manual(values=c("black","red"))+
  guides(color=F)
ggsave("vacc.png")


bbdatclean <- bbdat %>% filter(vaccrate>0)

cor.test(log(bbdatclean$income), log(bbdatclean$vaccrate), use = "complete.obs")
