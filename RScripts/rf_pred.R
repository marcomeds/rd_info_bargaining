library(tidyverse)
library(grf)
library(ggplot2)

# SET WORKING DIRECTORY
setwd('C:/Users/xps-seira/Dropbox/Apps/ShareLaTeX/quality_lawyers/Rscripts')
set.seed(5289374)


data <- read_csv('../_aux/pred_scores.csv')


require("dplyr")
data_frame <- data %>%
  select(-c(preg1calif, preg2calif, preg3calif, preg4calif, preg5calif, califglobal,
            perc_pos_rec, ratio_win_asked, ratio_winpos_asked), califglobal ) %>%
  drop_na()   

# PREPARE VARIABLES
X <- select(data_frame,-c(califglobal))
W <- data_frame$califglobal


# ESTINATE MODEL
propensity.forest = regression_forest(X, W)
rf_pred = predict(propensity.forest)$predictions
hist(rf_pred, xlab = "predictions")
data.out <- add_column(data_frame, rf_pred)
write_csv(data.out,"../_aux/pred_scores1.csv")


#####################################################

