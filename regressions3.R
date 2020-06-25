options(scipen = 8, digits = 4)
set.seed(12345)
library(jsonlite)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(zoo)
library(tibble)
library(boot)
library(plotrix)
library(grid)
library(lattice)
library(ggplotify)
library(broom)




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
ggplotRegression <- function (fit) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm",
                col = "red",
                formula = y ~ 0 + x) +       # force through origin
    theme(plot.title = element_text(size = 10, face = "bold")) +
    labs(
      title = strsplit(as.character(fit$call[2]), split = "=")
    )
}

#########################################################################################
### Funtions from Roy to do the bootstrapping                                         ###
#########################################################################################

lm_boot <- function(data,ind){lm(log_of_difference ~   0 + log_cum_confirmed, data = data[ind,])$coef} 

rsq_boot <- function(data,ind){
  fit <- lm(log_of_difference ~   0 + log_cum_confirmed, data = data[ind,])
  return(summary(fit)$r.square)
}



#
# The following lines read the JHS data for all countries  - I just grab it all, but easy to take a subset
#
base_url <-
  "https://pomber.github.io/covid19/timeseries.json" # REST API for JHS data
c19ts <-
  fromJSON(txt = base_url) # Get all country, all date, confirmed, deaths, recovered



for (i in 1:length(c19ts)) {
  #This loops through all countries
  temp <- c19ts[[i]]          # pull the data frame to play with it
  names <- c19ts[i]           # Country name
  
  # Start from the 100th death AND 10th death
  temp <- subset(temp, subset = (confirmed >= 100))
  temp <-
    subset(temp, subset = (deaths >= 10))  # Drop rows til 10 deaths

  
  # Skip if less than 25 observations
  
  if (nrow(temp) > 24) {
    temp$ma_confirmed <-
      rollmeanr(temp$confirmed, 7, na.pad = TRUE)  # 7 Day MA(moving average) of cases
    temp$diff_ma_confirmed <-
      c(0, diff(temp$ma_confirmed))           # Diff of MA
    temp$raw_diff_confirmed <-
      c(0, diff(temp$confirmed))             # Diff of daily cases
    temp$total_cum_confirmed <-
      temp$confirmed                        # Total cases
    temp$log_of_difference <-
      # ln (1st difference of cases)
      log(temp$ma_confirmed - temp$diff_ma_confirmed)
    temp$diff_of_log <-
      # differences of logs
      log(temp$ma_confirmed) - log(temp$diff_ma_confirmed)
    temp$raw_diff <-
      temp$ma_confirmed - temp$diff_ma_confirmed       # raw difference of MA of cases
    temp$log_cum_confirmed <-
      log(temp$total_cum_confirmed)           # ln(total cases)
  } else
    temp <- temp[FALSE]  # empty country if not at least 7 datapoints (should never be, because to have 100 confirmed in 7 days and no more is weird)
  c19ts[[i]] <- temp                                                  # put computed data back into list of country data
}

c19ts <-
  # Drop all empty dataframes like magic
  c19ts[sapply(c19ts, function(x)
    dim(x)[1]) > 0]
# c19ts <- Filter(function(x) ncol(x)==12, c19ts)
############################################################
### Start outout stuff - I separated this for simplicity ###
############################################################




bigdf <- as.data.frame(NULL)

for (i in 1:length(c19ts)-1) {

# Country <- names(c19ts[i])
# temp <- c19ts[[i]]
# temp$country <- Country
  
  # print(i)
  # print(ncol(c19ts[i]))



# bigdf <- rbind(bigdf,temp)
}

pdf(
  # Output as Landscape PDF with narrow margins
  "plots.pdf",
  bg = "white",
  paper = "USr",
  width = 9.5,
  height = 7.5
)

for (i in 1:(length(c19ts))) {
  # Start loop through countries again
  skip_to_next <-
    FALSE                                               # needed to catch math errors because
  tryCatch({
    # some countries had decreasing total cases
    # So this catches errors and skips that country
    
    temp <-
      na.omit(c19ts[[i]])                                       # Grab DF and COuntry name
    Country <- names(c19ts[i])
    
    ### Make R%egression PLot Objects - force X intercept to 0
    
    p1.fit <-
      lm(log_of_difference ~   0 + log_cum_confirmed, data = temp)  # Log of differences ~ log of cases
    p2.fit <-
      lm(diff_of_log ~ 0 + log_cum_confirmed, data = temp)          # Diff of logs ~  log of cases
    p3.fit <-
      lm(raw_diff_confirmed ~ 0 + total_cum_confirmed, data = temp) # Raw diff ~ cases
    
#############################  Create a grob with the 3 regression plots ###########################
regressions <- ggarrange(ggplotRegression(p1.fit),ggplotRegression(p2.fit),ggplotRegression(p3.fit),ncol = 3)
    
    
    
###############  Bootstrap Code from Roy ################    

results_coef <- boot(temp, lm_boot, 1000)
    results_rsq <- boot(temp, rsq_boot, 1000)
    
    results_coef_ci <- boot.ci(results_coef, type = "bca")
    results_rsq_ci <- boot.ci(results_rsq, type = "bca")
    
    results_coef_ci_lower <- results_coef_ci$bca[4]
    results_coef_ci_upper <- results_coef_ci$bca[5]
    
    results_rsq_ci_lower <- results_rsq_ci$bca[4]
    results_rsq_ci_upper <- results_rsq_ci$bca[5]
    
###################  Make Whisker plots of the 95% CI from bootstrap  ################
###################  The values are in the table #####################################
###################  Why was this so hard for me to do? ##############################   
whiskers <-
  ggarrange(
    (ggplot(tidy(results_coef), aes("coef", statistic)) +
      geom_point() + ggtitle("Log of Differences - Bootstrap Coefficients") + xlab("") +
      geom_errorbar(aes(ymin = results_coef_ci_lower, ymax = results_coef_ci_upper))),
    (ggplot(tidy(results_rsq), aes("rsq", statistic)) +
      geom_point() + ggtitle("Log of Differences - Bootstrap R^2") + xlab("") +
      geom_errorbar(aes(ymin = results_rsq_ci_lower, ymax = results_rsq_ci_upper)))  ,
    ncol = 2
  )


##############################  CObbled the table together - PITA  ###################
    
    math1 <- cbind(data.frame(summary(p1.fit)$coefficients,data.frame(summary(p1.fit)$r.squared)))
    math2 <- cbind(data.frame(summary(p2.fit)$coefficients,data.frame(summary(p2.fit)$r.squared)))
    math3 <- cbind(data.frame(summary(p3.fit)$coefficients,data.frame(summary(p3.fit)$r.squared)))
    cn1 <- c("BCoef", "Std Error","t-Statistic", "p-Value", "R^2" )
    cn2 <- c("Confidence", "Lower", "Upper")
    rn1 <- c("Log of Diff", "Diff of Logs", "Differences")
    rn2 <- c("Boot ln(Diff) Coef", "Boot ln(Diff) R^2")
    colnames(math1) <- cn1
    colnames(math2) <- cn1
    colnames(math3) <- cn1
    math4 <- rbind(math1,math2)
    tab1  <- rbind(math4,math3)
    rownames(tab1) <- rn1
    math5 <- data.frame(results_coef_ci[4])
    math6 <- data.frame(results_rsq_ci[4])
    tab2 <- rbind(math5[,c(1,4:5)],math6[,c(1,4:5)])
    colnames(tab2) <- cn2
    rownames(tab2) <- rn2

#############################  Table of all coefficients is rendered ###################################    
    
    info <- ggarrange(tableGrob(format(tab1, digits = 4)),tableGrob(format(tab2, digits = 4)),ncol = 2)
    
#############################   Then put it all on one sheet of papaer per Country #######################
    
        print(annotate_figure(
      # Push page to PDF with Country Name
     ggarrange(regressions, whiskers, info, nrow = 3),

      top = text_grob(Country, color = "blue", size = 14)
    ))
  }, error = function(e) {
    # Error routine - Set skip_to_next = TRUE
    skip_to_next <- TRUE
  })
  if (skip_to_next) {
    next
  }
}
dev.off()                                                             # Close PDF

##############################  Done #####################################################################







