options(scipen = 25, max.print=10000000)
library(jsonlite)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(TTR)
# library(zoo)
library(jtools)


#########################################################################################################################
#   What I think I did was in a note from Cynthia.  I have no confidence whether I did this correctly or not            #
#########################################################################################################################
# Start on the day of 100 confirmed, drop countries with less than 100 confirmed or less than 10 dead                  
# Regressions should go through origin                                                                                  #
#                                                                                                                       #
# calculate log of difference: log(7dMA at time T - 7dMA at time t-1) vs. log of cumulative confirmed (x-axis)          #
# calculate difference of log: log of 7dMA at time T - log of 7dMA at time T-1 vs. log of cumulative confirmed (x-axis) #
# raw difference: 7dMA at time T - 7dMA at time T-1 vs. total cumulative confirmed                                      #
#########################################################################################################################
# Added Bootstrap from Roy #
############################


#########################################################################################
### Funtion to create a regression plot and annotate the Y intercept, P, and Formula ###
#########################################################################################
# ggplotRegression <- function (fit) {
#   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
#     geom_point() +
#     stat_smooth(method = "lm",
#                 col = "red",
#                 formula = y ~ 0 + x) +       # force through origin
#     theme(plot.title = element_text(size = 10, face = "bold")) +
#     labs(
#       title = strsplit(as.character(fit$call[2]), split = "=")
#     )
# }

# ggplotRegression <- function (fit) {
#   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
#     geom_point() +
#     stat_smooth(method = "lm", col = "red") +
#     labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
#                        "Intercept =",signif(fit$coef[[1]],5 )))
#                       # " Slope =",signif(fit$coef[[2]], 5),
#                       # " P =",signif(summary(fit)$coef[2,4], 5)))
# }

#########################################################################################
### Funtions from Roy to do the bootstrapping                                         ###
#########################################################################################

lm_boot <- function(data,ind){lm(log_of_difference ~   0 + log_cum_confirmed, data = data[ind,])$coef} 

rsq_boot <- function(data,ind){
  fit <- lm(log_of_difference ~   0 + log_cum_confirmed, data = data[ind,])
  return(summary(fit)$r.square)
}

#########################################################################################
### Function to plot regression                                                        ###
#########################################################################################


plot.countries <- function(bigtable = C19DF) {
  if (dir.exists("output")) { }
  else {dir.create("output")}
  countries <- unique(bigtable$country)
  for (i in seq_along(countries)) {
    item <- C19DF[(C19DF$country == countries[i]),]
    plot <- item %>%
      ggplot(aes(x = ln_total_cases, y = ln_new_ma_cases )) + geom_point() +
      geom_smooth(method="lm", formula = y ~ 0 + x)
    
    ggsave(filename = paste0("output/",
                             countries[i],
                             "_xy_plot.png"),
           plot = plot,
           width = 11, height = 8.5, units = "in")
    print(plot)
  }
}


#########################################################################################
###           PROGRAM START                                                           ###
#########################################################################################








#
# The following lines read the JHS data for all countries  - I just grab it all, but easy to take a subset
#
base_url <-
  "https://pomber.github.io/covid19/timeseries.json" # REST API for JHS data


if (!file.exists("timeseries.json")) {                            # Weather stations
  download.file(
    base_url,
    "timeseries.json",
    "wget"
  )
}

c19JSON <-
  fromJSON(txt = "timeseries.json") # Get all country, all date, confirmed, deaths, recovered


C19DF <- do.call(rbind, unname(Map(cbind, country = names(c19JSON), c19JSON)))
C19DF$country <- as.character(C19DF$country)
C19DF <- C19DF[order(C19DF$country),]

############  At this point C19DF is a dataframe with all JHS country data

#######  DATAFRAME ####
#  country        - country
#  date           - date of reporting
#  confirmed      - total reported cases
#  deaths         - deaths
#  recovered      - recovered
#  new_cases      - first difference of total reported cases
#  ma_cases       - 7 day moving average of total cases
#  new_ma_cases   - period (daily) change in 7 day ma
#  ln_total_cases - ln of total cases
#  ln_new_cases   - ln of new cases
#  ln_ma_cases    - ln of movin average of total cases
#  ln_new_ma_cases- (ln of moving average - ln of previous (daily) moving average)

cnames <- c("country", "date",  "total_cases", "deaths", "recovered", "new_cases", "ma_cases", "new_ma_cases",
           "ln_total_cases","ln_new_cases", "ln_ma_cases", "ln_new_ma_cases", "diff_ln_ma_cases", "new_deaths",
           "ln_deaths", "ln_new_deaths")

options(warn = -1)

C19DF <- C19DF[!(C19DF$country == "Diamond Princess"),]

# ----- Next line to simplify table for debugging -----"
mask <- startsWith(as.character(C19DF$country),"U")
C19DF <- C19DF[mask,]
#------------------------------------------------------

C19DF <- C19DF %>% group_by(country) %>% mutate(new_cases = confirmed - lag(confirmed))
C19DF <- C19DF %>% group_by(country) %>% mutate(ma_cases = runMean(confirmed, 7))
C19DF <- C19DF %>% group_by(country) %>% mutate(new_ma_cases = ma_cases - lag(ma_cases))
C19DF <- C19DF %>% group_by(country) %>% mutate(ln_total_cases = log(confirmed))
C19DF <- C19DF %>% group_by(country) %>% mutate(ln_new_cases = log(new_cases))
C19DF <- C19DF %>% group_by(country) %>% mutate(ln_ma_cases = log(ma_cases))
C19DF <- C19DF %>% group_by(country) %>% mutate(ln_new_ma_cases = log(new_ma_cases))
C19DF <- C19DF %>% group_by(country) %>% mutate(diff_ln_ma_cases = ln_ma_cases - lag(ln_ma_cases))
C19DF <- C19DF %>% group_by(country) %>% mutate(new_deaths = deaths - lag(deaths))
C19DF <- C19DF %>% group_by(country) %>% mutate(ln_deaths = log(deaths))
C19DF <- C19DF %>% group_by(country) %>% mutate(ln_new_deaths = log(new_deaths))
colnames(C19DF) <- cnames

C19DF <-  do.call(data.frame,lapply(C19DF, function(x) replace(x, is.infinite(x),NA)))
C19DF <- subset(C19DF, total_cases >= 100 & deaths > 10)       # Should it be 100 OR 10?????
D1 <- table(C19DF$country)                                     #
C19DF <- C19DF[C19DF$country %in% names(D1[D1 >= 25]), ]       # AND at least 25 observations

countries <- as.character(unique(C19DF$country))




plot.countries(C19DF)
