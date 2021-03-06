options(scipen = 25)
library(jsonlite)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(zoo)
library(stargazer)




#########################################################################################################################
#   What I think I did was in a note from Cynthia.  I have no confidence whether I did this correctly or not                               #
#########################################################################################################################
# Start on the day of 100 confirmed, drop countries with less than 100 confirmed or less than 10 dead
# Regressions should go through origin
#                                                                                                                       #
# calculate log of difference: log(7dMA at time T - 7dMA at time t-1) vs. log of cumulative confirmed (x-axis)          #
# calculate difference of log: log of 7dMA at time T - log of 7dMA at time T-1 vs. log of cumulative confirmed (x-axis) #
# raw difference: 7dMA at time T - 7dMA at time T-1 vs. total cumulative confirmed                                      #
#########################################################################################################################



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
    temp <-
    temp[FALSE]  # empty country if not at least 7 datapoints (should never be, because to have 100 confirmed in 7 days and no more is weird)
  
  c19ts[[i]] <-
    temp                                                  # put computed data back into list of country data
}

c19ts <-
  # Drop all empty dataframes like magic
  c19ts[sapply(c19ts, function(x)
    dim(x)[1]) > 0]
############################################################
### Start outout stuff - I separated this for simplicity ###
############################################################

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
    
    p1 <- ggplotRegression(p1.fit)
    p2 <- ggplotRegression(p2.fit)
    p3 <- ggplotRegression(p3.fit)
    
    text <-
      toString(paste0(
        stargazer(
          p1.fit,
          p2.fit,
          p3.fit,
          style = "all",
          font.size = "normalsize",
          type = "text",
          omit.stat = c(
            "adj.rsq",
            "aic",
            "bic",
            "chi2",
            "f",
            "ll",
            "logrank",
            "lr",
            "max.rsq",
            "n",
            "null.dev",
            "res.dev",
            "rsq",
            "scale",
            "theta",
            "ser",
            "sigma2",
            "ubre",
            "wald"
          ),
          omit.table.layout = "n",
          omit.summary.stat = c("max", "min", "median", "mean", "n", "p25", "p75", "sd"),
          single.row = TRUE
        ),
        "\n"
      ))
    
    p4 <- ggplot() +
      annotation_custom(
        text_grob(text, family = "Courier", size = 8),
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = Inf
      ) +
      theme_void()
    

    
    print(annotate_figure(
      # Push page to PDF with Country Name
      ggarrange(p4,ggarrange(p1,p2,p3,ncol=3),nrow=2)
      ,
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


