library(xlsx)
library(reshape2)
library(ggplot2)
#########################loading and cleaning ##############################
migration <- read.xlsx("./data/12711-0002.xls", 1, startRow = 4, endRow = 166
                        )
head(migration)
names(migration)[1] <- "State"
names(migration)[2] <- "Sex"
names(migration)[3] <- "Arrival/Departure"
unique(migration$Unit)
migration <- migration[-4]
# remove extra X at the beggining of years
names(migration)[-c(1:3)] <- sub("X","", names(migration)[-c(1:3)])
# Fill names of state in the whole column
b <- ((1:length(migration$State)-1) %/% 6)*6 +1 
migration$State <- migration$State[b]
# fill sex in the whole column
c <- ((1:length(migration$Sex)-1) %/% 2)*2 +1 
migration$Sex <- migration$Sex[c]
# convert to numeric format
sapply(migration, class)
for (i in 4:ncol(migration)){
        migration[,i] <- as.numeric(as.character(migration[,i]))
}
#compute totals

totalFemaleArrival <- sapply(migration[migration$Sex == "Female" &
                                               migration$`Arrival/Departure` == "Arrivals from foreign countries",
                                       4:ncol(migration)], sum, na.rm=T)

totalMaleArrival <- sapply(migration[migration$Sex == "Male" &
                                             migration$`Arrival/Departure` == "Arrivals from foreign countries",
                                     4:ncol(migration)], sum, na.rm=T)
totalFemaleDeparture <- sapply(migration[migration$Sex == "Female" &
                                                 migration$`Arrival/Departure` == "Departures to foreign countries",
                                         4:ncol(migration)], sum, na.rm=T)
totalMaleDeparture <- sapply(migration[migration$Sex == "Male" &
                                               migration$`Arrival/Departure` == "Departures to foreign countries",
                                       4:ncol(migration)], sum, na.rm=T)
TotalArrival <- sapply(migration[migration$Sex == "Total" &
                                         migration$`Arrival/Departure` == "Arrivals from foreign countries",
                                 4:ncol(migration)], sum, na.rm=T)                                       
TotalDeparture <- sapply(migration[migration$Sex == "Total" &
                                           migration$`Arrival/Departure` == "Departures to foreign countries",
                                   4:ncol(migration)], sum, na.rm=T)
years <- names(migration[4:ncol(migration)])
# par(mfrow=c(1,2))
# plot(years,TotalArrival, type = "l", 
#      xlim = c(1974,2014), ylim = c(49000, 8e+05), col = "yellow",
#      xlab = "Year", ylab = "Total Arrivals from forein country")
# lines(years, totalFemaleArrival, col="orange")
# lines(years, totalMaleArrival, col="blue")
# # plotting Departures
# plot(years,TotalDeparture, type = "l", 
#      xlim = c(1974,2014), ylim = c(49000, 8e+05),
#      xlab = "Year")
# lines(years,totalFemaleDeparture)
# lines(years, totalMaleDeparture)

# # Arrivals
# arrivals <- rbind(totalFemaleArrival, totalMaleArrival, TotalArrival)
# arrivals <- t(arrivals)
# arrivals <- melt(arrivals, 1)
# names(arrivals)[1:2] <- c("Year", "Sex")
# qplot(Year, value, data= arrivals, color = Sex) + geom_line(size=1.5)
# # Departures
# departures <- rbind(totalFemaleDeparture, totalMaleDeparture, TotalDeparture)
# departures <- t(departures)
# names(departures)[1:2] <- c("Year", "Sex")
# qplot(Year, value, data= departures, color = Sex) + geom_line(size=1.5)

#Total plot
arrivals <- data.frame(Year = years,"D/A" = rep("Arrival", length(TotalArrival)), 
                       Female = totalFemaleArrival, Male = totalMaleArrival, Total = TotalArrival)
departures <- data.frame(Year = years,"D/A" = rep("Departures", length(TotalDeparture)), 
                         Female = totalFemaleDeparture, Male = totalMaleDeparture, Total = TotalDeparture)

totalmig <- rbind(arrivals, departures)
totalmig <- melt(totalmig, c(1,2), variable.name = "Sex", value.name = "Total.Number")
totalmig$Year <- as.integer(as.character(totalmig$Year))
qplot(Year, Total.Number, data=totalmig, color=Sex, facets = .~D.A) + 
        geom_line(size=1) + ylab("Totla Number")

# In which year and country was the highest number of departures?
totaldep <- migration[migration$Sex == "Total" & 
                              migration$`Arrival/Departure` == "Departures to foreign countries",
                              ]
totaldep <- totaldep[,-c(2,3)]
totaldep <- melt(totaldep, 1, variable.name = "Year", value.name = "Departures",
                 na.rm = T)
ord  <- order(totaldep$Departures)
dev.off()
par(mfrow = c(1,2))
mp <- barplot(tail(totaldep$Departures[ord])/1000, 
        names.arg = paste(droplevels(tail(totaldep$State[ord])), tail(totaldep$Year[ord]), sep = "\n")
        , col = "lightblue",
        xlab = "Country, Year", ylab = "Number of departures (thousand)", 
        main = "Year, countries with the most number of departures to")
text(mp, tail(totaldep$Departures[ord])/1000, tail(totaldep$Departures[ord])%/%1000, pos=1, col="red")


# Which country shows the most arrivals and in which year?
totalarr <- migration[migration$Sex == "Total" & 
                              migration$`Arrival/Departure` == "Arrivals from foreign countries",
                      ]
totalarr <- totalarr[,-c(2,3)]
totalarr <- melt(totalarr, 1, variable.name = "Year", value.name = "Arrivals",
                 na.rm = T)
ord  <- order(totalarr$Arrivals)
mp <- barplot(tail(totalarr$Arrivals[ord])/1000, 
              names.arg = paste(droplevels(tail(totalarr$State[ord])), tail(totalarr$Year[ord]), sep = "\n")
              , col = "lightblue",
              xlab = "Country, Year", ylab = "Number of arrivals from (thousand)", 
              main = "Year, countries with the most number of arrivals")
text(mp, tail(totalarr$Arrivals[ord])/1000, tail(totalarr$Arrivals[ord])%/%1000, 
     pos=1, col="red")

# What total numbers for departures and arrivals are to be expected for 2015 or
# 2016 for each country?
library(forecast)
totals <- migration[migration$Sex == "Total",]
tss <- lapply(1:nrow(totals), function(i) ts(as.numeric(totals[i,4:ncol(totals)]),
                                             start = 1974))
names(tss) <- totals$State
etss <- lapply(1:length(tss), function(i){
        if (sum(is.na(tss[[i]])) < 35){
                rwf(tss[[i]])
        }else{
                tss[[i]][41]
        }
}) 
fcasts <- lapply(1:length(etss), function(i) forecast(etss[[i]]) )

country <- "Belgium"
da <- "Departures to foreign countries"
i <- grep(country, totals$State)[grep(da,totals$`Arrival/Departure`)[1]]
autoplot(fcasts[[i]])
forecasts <- data.frame(State = totals$State,  
                        Arrival.Departure = sub(" from foreign countries| to foreign countries", "", totals$`Arrival/Departure`),
                        Year2015 = sapply(fcasts, function(x) floor(x$mean[1])),
                        Lower95percent = sapply(fcasts, function(x) floor(x$lower[1,1])),
                        Upper95percent = sapply(fcasts, function(x) floor(x$upper[1,1])),
                        Year2016 = sapply(fcasts, function(x) floor(x$mean[2])),
                        Lower95percent = sapply(fcasts, function(x) floor(x$lower[2,1])),
                        Upper95percent = sapply(fcasts, function(x) floor(x$upper[2,1]))
)
forecasts$confint2015 <- paste("(",forecasts$Lower95percent,",", forecasts$Upper95percent,
                 ")", sep="")
forecasts$confint2016 <- paste("(",forecasts$Lower95percent.1,",", forecasts$Upper95percent.1,
                               ")", sep="")



#forest plot
RelConf <- forecasts$Upper95percent - forecasts$Lower95percent
p <- ggplot(forecasts, aes(x=State,  y=Year2015, ymin=Lower95percent, ymax=Upper95percent))
p <-  p + geom_pointrange() + coord_flip() + xlab('Country') + facet_grid(Arrival.Departure~. )
p 

## forest plot too show forecasts
forecasts2 <- forecasts[complete.cases(forecasts),]
arrivalForecast <- forecasts2[forecasts2$Arrival.Departure == "Arrivals",]
departureForecast <- forecasts2[forecasts2$Arrival.Departure == "Departures",]

####forestplot 2015
means <- paste(arrivalForecast$Year2015, departureForecast$Year2015, sep = "\n")
text <- cbind(c("Country",as.character(arrivalForecast$State)), 
                   c("Forecast",means),
              c("95% Conf. Interval",paste(arrivalForecast$confint2015,
                    departureForecast$confint2015, sep="\n"))
                   )
forestplot(text,
           txt_gp = fpTxtGp(label = list(gpar(fontfamily = ""),
                                         gpar(fontfamily = "",
                                              col = "#660000", fontsize=7),
                                         gpar(fontfamily = "",
                                              col = "#660000", fontsize=7))),
           legend = c("Arrivals", "Departures"),
           legend_args = fpLegend(pos = list(x=1, y=0.05), 
                                  gp=gpar(col="#CCCCCC", fill="#F9F9F9")),
           mean = cbind(c(NA,arrivalForecast$Year2015), c(NA,departureForecast$Year2015)),
           lower = cbind(c(NA,arrivalForecast$Lower95percent), c(NA,departureForecast$Lower95percent)),
           upper = cbind(c(NA,arrivalForecast$Upper95percent), c(NA,departureForecast$Upper95percent)),
           col = fpColors(box=c("blue", "darkred")),
           boxsize = .1,
           title = "Forecasting Arrivals and Departures for 2015"
           )
##forestplot 2016
means <- paste(arrivalForecast$Year2016, departureForecast$Year2016, sep = "\n")
text <- cbind(c("Country",as.character(arrivalForecast$State)), 
              c("Forecast",means),
              c("95% Conf. Interval",paste(arrivalForecast$confint2016,
                                           departureForecast$confint2016, sep="\n"))
)
forestplot(text,
           txt_gp = fpTxtGp(label = list(gpar(fontfamily = ""),
                                         gpar(fontfamily = "",
                                              col = "#660000", fontsize=7),
                                         gpar(fontfamily = "",
                                              col = "#660000", fontsize=7))),
           legend = c("Arrivals", "Departures"),
           legend_args = fpLegend(pos = list(x=1, y=0.05), 
                                  gp=gpar(col="#CCCCCC", fill="#F9F9F9")),
           mean = cbind(c(NA,arrivalForecast$Year2016), c(NA,departureForecast$Year2016)),
           lower = cbind(c(NA,arrivalForecast$Lower95percent.1), c(NA,departureForecast$Lower95percent.1)),
           upper = cbind(c(NA,arrivalForecast$Upper95percent.1), c(NA,departureForecast$Upper95percent.1)),
           col = fpColors(box=c("blue", "darkred")),
           boxsize = .1,
           hrzl_lines = as.list(c(1:(length(means)+2))),
           title = "Forecasting Arrivals and Departures for 2016"
)
# 
# a <- holt(tss[[1]])
# fcast <- forecast(a)
# autoplot(=fcast) + ggtitle(paste("Number of", 
#                              sub("foreign countries","",totals[1,3]),
#                              totals[1,1])) +
#         ylab("Number")
# 
# etss[[7]]
# fcasts[[7]]
for (i in 1:length(fcasts)){
        plot(fcasts[[i]])
        # print(p)
        line <- readline()
}
for (i in 1:length(fcasts)){
        plot(fcasts[[i]]$fitted, resid(fcasts[[i]]), type = "p")
        line <- readline()
}
 
# Is there a significant difference between the numbers of female and male
# departures across all countries in the years 2013 and 2014, respectively?
totaldep <- totalmig[totalmig$D.A == "Departures" & totalmig$Sex != "Total",]



###################
library(lmtest)
femaledep2013 <- migration[migration$Sex == "Female" & migration$`Arrival/Departure` == "Departures to foreign countries",
                 c("State","2013")]
maledep2013 <- migration[migration$Sex == "Male" & migration$`Arrival/Departure` == "Departures to foreign countries",
                     c("State","2013")]
dep2013 <- data.frame(State = femaledep2013[,1], Female = femaledep2013[,2], Male = maledep2013[,2])
dep2013melted <- melt(dep2013, 1, variable.name = "Sex", value.name = "Count")
fit1 <- glm(Count~State, data=dep2013melted, family = poisson())
fit2 <- glm(Count~State+Sex, data = dep2013melted, family = poisson())
fit1 <- lm(Count~State, data=dep2013melted)
fit2 <- lm(Count~State+Sex, data = dep2013melted)
anova(fit1, fit2, test = "Chi")
summary(an)
a <- aov(fit2)
summary(a)


# dep2013$Female.Ratio <- dep2013$Female/(dep2013$Female+dep2013$Male)
# dep2013$Male.Ratio <- dep2013$Male/(dep2013$Female+dep2013$Male)
hist(dep2013$Female)
hist(dep2013$Male)
abline(v=mean(dep2013$Male.Ratio))
chi2013 <- chisq.test(dep2013[,names(dep2013)[2:3]], p = c(.5,.5))
t2013 <- t.test(dep2013[,2], dep2013[,3], paired = T)
wil2013 <- wilcox.test(dep2013[,2], dep2013[,3], paired = T, alternative = "two.sided")
fish2013 <- fisher.test(as.matrix(dep2013[,names(dep2013)[2:3]]), workspace = 20000000,
                        alternative = "two.sided", hybrid = TRUE)



femaledep2014 <- migration[migration$Sex == "Female" & migration$`Arrival/Departure` == "Departures to foreign countries",
                           c("State","2014")]
maledep2014 <- migration[migration$Sex == "Male" & migration$`Arrival/Departure` == "Departures to foreign countries",
                         c("State","2014")]
dep2014 <- data.frame(State = femaledep2014[,1], Female = femaledep2014[,2], Male = maledep2014[,2])

chi2014 <- chisq.test(dep2014[,names(dep2014)[2:3]], p = c(.5,.5))
