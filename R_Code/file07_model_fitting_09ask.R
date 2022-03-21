# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2022.03.17. ask
rm(list=ls(all=TRUE))

# library for glm.nb 
# i.e. for negative binomial
library(MASS)


# Library pscl zero-inflated poisson
# install.packages("pscl")
library(pscl)

# Loading package for Lag() function with capital L
library(Hmisc)

# Old tries
# library(glmmTMB)


# Setting the correct working directory.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Weppelman Legionella"

# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()




# Lists to subset

# variables_list_inc_temp_perc <- c("Year_Month_fixed", "Counts", "TEMP", "SLP", "WDSP", "PRCP" )
variables_list_inc_temp_perc <- c("Year_Month_fixed", "Counts", "TEMP", "PRCP" )

variables_list_river         <- c("YEAR_MONTH", "pH", "NITRATE", "ORTHOPHOSPHATE", "MANGANESE", "STRONTIUM", "BARIUM", "NICKEL", "TOTAL.ORGANIC.CARBON", "TURBIDITY", "ALUMINUM", "TOTAL.COLIFORM", "ZINC", "IRON")
variables_list_dalecarlia    <- c("YEAR_MONTH", "pH", "NITRATE", "ORTHOPHOSPHATE", "MANGANESE", "STRONTIUM", "BARIUM", "NICKEL", "TOTAL.ORGANIC.CARBON", "TURBIDITY", "ALUMINUM", "TOTAL.COLIFORM....positive.", 
                                  "HETEROTROPHIC.PLATE.COUNT", "ZINC", "IRON", "CHLORINE" )

variables_list_mcmillan      <- variables_list_dalecarlia
variables_list_both_plants   <- intersect(variables_list_dalecarlia, variables_list_mcmillan)


# Reading -> merged_dalecarlia_mcmillan_plus_incidence
merged_dalecarlia_mcmillan_plus_incidence_rdata_path <- paste0("R_Data/merged_dalecarlia_mcmillan_plus_incidence.RData")
load( file = merged_dalecarlia_mcmillan_plus_incidence_rdata_path )
names(merged_dalecarlia_mcmillan_plus_incidence)
head(merged_dalecarlia_mcmillan_plus_incidence)

# Reading -> merged_river_plus_incidence
merged_river_plus_incidence_rdata_path <- paste0("R_Data/merged_river_plus_incidence.RData")
load( file = merged_river_plus_incidence_rdata_path )
names(merged_river_plus_incidence)
head(merged_river_plus_incidence)



# Reading -> lag_frame_xmo_dalecarlia_mcmillan
lag_frame_xmo_dalecarlia_mcmillan_path <- paste0("R_Data/lag_frame_xmo_dalecarlia_mcmillan.RData")
load( file = lag_frame_xmo_dalecarlia_mcmillan_path )
names(lag_frame_xmo_dalecarlia_mcmillan)
head(lag_frame_xmo_dalecarlia_mcmillan)

# Reading -> lag_frame_xmo_river
lag_frame_xmo_river_path <- paste0("R_Data/lag_frame_xmo_river.RData")
load( file = lag_frame_xmo_river_path )
names(lag_frame_xmo_dalecarlia_mcmillan)
head(lag_frame_xmo_dalecarlia_mcmillan)


# Defining the models to run as text
# Covariate sets
input00 <- paste0("Counts ~ TEMP + PRCP + month_num")

input01 <- paste0("Counts ~ TEMP + PRCP + pH + NITRATE + ORTHOPHOSPHATE + MANGANESE + STRONTIUM + BARIUM + NICKEL + TOTAL.ORGANIC.CARBON + TURBIDITY + ALUMINUM + ZINC + IRON")

input02 <- paste0("Counts ~ TEMP + PRCP + pH + NITRATE + ORTHOPHOSPHATE + MANGANESE + STRONTIUM + BARIUM + NICKEL + TOTAL.ORGANIC.CARBON + TURBIDITY + ALUMINUM + TOTAL.COLIFORM....positive. + HETEROTROPHIC.PLATE.COUNT + ZINC + IRON + CHLORINE")



# datasets
data01 <- "merged_river_plus_incidence"
data02 <- "merged_dalecarlia_mcmillan_plus_incidence"

# List of datasets
set_of_datasets <- c(data01, data02)
# List of predictors
names(merged_river_plus_incidence)
names(merged_dalecarlia_mcmillan_plus_incidence)



# Fix 2022.03.08
# river
merged_river_plus_incidence$month_num <- merged_river_plus_incidence$month

merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "Jan"] <- "01-Jan"
merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "Feb"] <- "02-Feb"
merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "Mar"] <- "03-Mar"
merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "Apr"] <- "04-Apr"
merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "May"] <- "05-May"
merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "Jun"] <- "06-Jun"
merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "Jul"] <- "07-Jul"
merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "Aug"] <- "08-Aug"
merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "Sep"] <- "09-Sep"
merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "Oct"] <- "10-Oct"
merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "Nov"] <- "11-Nov"
merged_river_plus_incidence$month_num[merged_river_plus_incidence$month_num == "Dec"] <- "12-Dec"
# Extra check
sum(!merged_river_plus_incidence$month == substr(x = merged_river_plus_incidence$month_num, start = 4, stop = 7))



# plants
merged_dalecarlia_mcmillan_plus_incidence$month_num <- merged_dalecarlia_mcmillan_plus_incidence$month

merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "Jan"] <- "01-Jan"
merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "Feb"] <- "02-Feb"
merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "Mar"] <- "03-Mar"
merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "Apr"] <- "04-Apr"
merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "May"] <- "05-May"
merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "Jun"] <- "06-Jun"
merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "Jul"] <- "07-Jul"
merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "Aug"] <- "08-Aug"
merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "Sep"] <- "09-Sep"
merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "Oct"] <- "10-Oct"
merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "Nov"] <- "11-Nov"
merged_dalecarlia_mcmillan_plus_incidence$month_num[merged_dalecarlia_mcmillan_plus_incidence$month_num == "Dec"] <- "12-Dec"
# Extra check
sum(!merged_dalecarlia_mcmillan_plus_incidence$month == substr(x = merged_dalecarlia_mcmillan_plus_incidence$month_num, start = 4, stop = 7))




# Fixing levels for month for fitting
levels(merged_river_plus_incidence$month) <- substr(x = month.name, start = 1, stop = 3)
levels(merged_dalecarlia_mcmillan_plus_incidence$month) <- substr(x = month.name, start = 1, stop = 3)





# List of predictors
# River
set_of_predictors_river      <- c(input00, input01)
set_of_predictors_river_text <- c("input00", "input01")
# Plant
set_of_predictors_plant      <- c(input02)
set_of_predictors_plant_text <- c("input02")








# River data i.e. data01
for( current_set_index in c(1:length(set_of_predictors_river)) )
{
  # Debugging step
  # current_set_index <- 1
  

  
  # Poisson model
  eval( parse( text = paste0( "poisson_fit_current <- glm( ", set_of_predictors_river[current_set_index], ", family = poisson, data = ", data01, " )" )  ) ) 
  
  poisson_fit_current
  summary(poisson_fit_current)
  deviance(poisson_fit_current)
  df.residual(poisson_fit_current)
  pchisq( q = deviance(poisson_fit_current), df = df.residual(poisson_fit_current), lower.tail = FALSE )
  # Exporting output
  sink(paste0("R_Output/poisson_fit_current_set_index_data01_", set_of_predictors_river_text[current_set_index],".txt"))
  print(summary(poisson_fit_current))
  # Debugging step for convergence
  print(poisson_fit_current$converged)
  sink()
  

  # Negative Binomial model
  eval( parse( text = paste0( "negative_binomial_fit_current <- glm.nb( ", set_of_predictors_river[current_set_index], ", data = ", data01, ", control = glm.control(maxit=100) )" )  ) ) 
  
  negative_binomial_fit_current
  summary(negative_binomial_fit_current)
  deviance(negative_binomial_fit_current)
  df.residual(negative_binomial_fit_current)
  pchisq( q = deviance(negative_binomial_fit_current), df = df.residual(negative_binomial_fit_current), lower.tail = FALSE )
  # Exporting output
  sink(paste0("R_Output/negative_binomial_fit_current_set_index_data01_", set_of_predictors_river_text[current_set_index],".txt"))
  print(summary(negative_binomial_fit_current))
  # Debugging step for convergence
  print(negative_binomial_fit_current$converged)
  sink()
  

  

  # Zero-Inflated Poisson model
  distribution_name_to_pass <- "poisson"
  eval( parse( text = paste0( "zinf_poisson_fit_current <- zeroinfl( ", set_of_predictors_river[current_set_index], ", dist = distribution_name_to_pass, data = ", data01, " )" )  ) ) 
  
  zinf_poisson_fit_current
  summary(zinf_poisson_fit_current)
  deviance(zinf_poisson_fit_current)
  df.residual(zinf_poisson_fit_current)
  # Exporting output
  sink(paste0("R_Output/zinf_poisson_fit_current_set_index_data01_", set_of_predictors_river_text[current_set_index],".txt"))
  print(summary(zinf_poisson_fit_current))
  # Debugging step for convergence
  print(zinf_poisson_fit_current$converged)
  sink()

  
  # Zero-Inflated Negative Binomial model
  distribution_name_to_pass <- "negbin"
  eval( parse( text = paste0( "zinf_negative_binomial_fit_current <- zeroinfl( ", set_of_predictors_river[current_set_index], ", dist = distribution_name_to_pass, data = ", data01, " )" )  ) ) 
  
  zinf_negative_binomial_fit_current
  summary(zinf_negative_binomial_fit_current)
  deviance(zinf_negative_binomial_fit_current)
  df.residual(zinf_negative_binomial_fit_current)
  # Exporting output
  sink(paste0("R_Output/zinf_negative_binomial_fit_current_set_index_data01_", set_of_predictors_river_text[current_set_index],".txt"))
  print(summary(zinf_negative_binomial_fit_current))
  # Debugging step for convergence
  print(zinf_negative_binomial_fit_current$converged)
  sink()

  
  
  
  # Hurdle Poisson model
  distribution_name_to_pass <- "poisson"
  eval( parse( text = paste0( "hurdle_poisson_fit_current <- hurdle( ", set_of_predictors_river[current_set_index], ", dist = distribution_name_to_pass, data = ", data01, " )" )  ) ) 
  
  hurdle_poisson_fit_current
  summary(hurdle_poisson_fit_current)
  deviance(hurdle_poisson_fit_current)
  df.residual(hurdle_poisson_fit_current)
  # Exporting output
  sink(paste0("R_Output/hurdle_poisson_fit_current_set_index_data01_", set_of_predictors_river_text[current_set_index],".txt"))
  print(summary(hurdle_poisson_fit_current))
  # Debugging step for convergence
  print(hurdle_poisson_fit_current$converged)
  sink()
  
  
  # Zero-Inflated Negative Binomial model
  distribution_name_to_pass <- "negbin"
  eval( parse( text = paste0( "hurdle_negative_binomial_fit_current <- hurdle( ", set_of_predictors_river[current_set_index], ", dist = distribution_name_to_pass, data = ", data01, " )" )  ) ) 
  
  hurdle_negative_binomial_fit_current
  summary(hurdle_negative_binomial_fit_current)
  deviance(hurdle_negative_binomial_fit_current)
  df.residual(hurdle_negative_binomial_fit_current)
  # Exporting output
  sink(paste0("R_Output/hurdle_negative_binomial_fit_current_set_index_data01_", set_of_predictors_river_text[current_set_index],".txt"))
  print(summary(hurdle_negative_binomial_fit_current))
  # Debugging step for convergence
  print(hurdle_negative_binomial_fit_current$converged)
  sink()
  


  # End of -> for( current_set_index in c(1:length(set_of_predictors_river)) )
}  

  
  



  
  
  
  



# Plant data i.e. data02
for( current_set_index in c(1:length(set_of_predictors_plant)) )
{
  # Debugging step
  # current_set_index <- 1
  
  
  
  # Poisson model
  eval( parse( text = paste0( "poisson_fit_current <- glm( ", set_of_predictors_plant[current_set_index], ", family = poisson, data = ", data02, " )" )  ) ) 
  
  poisson_fit_current
  summary(poisson_fit_current)
  deviance(poisson_fit_current)
  df.residual(poisson_fit_current)
  pchisq( q = deviance(poisson_fit_current), df = df.residual(poisson_fit_current), lower.tail = FALSE )
  # Exporting output
  sink(paste0("R_Output/poisson_fit_current_set_index_data02_", set_of_predictors_plant_text[current_set_index],".txt"))
  print(summary(poisson_fit_current))
  # Debugging step for convergence
  print(poisson_fit_current$converged)
  sink()
  
  
  
  # Negative Binomial model
  eval( parse( text = paste0( "negative_binomial_fit_current <- glm.nb( ", set_of_predictors_plant[current_set_index], ", data = ", data02, ", control = glm.control(maxit=100) )" )  ) ) 
  
  negative_binomial_fit_current
  summary(negative_binomial_fit_current)
  deviance(negative_binomial_fit_current)
  df.residual(negative_binomial_fit_current)
  pchisq( q = deviance(negative_binomial_fit_current), df = df.residual(negative_binomial_fit_current), lower.tail = FALSE )
  # Exporting output
  sink(paste0("R_Output/negative_binomial_fit_current_set_index_data02_", set_of_predictors_plant_text[current_set_index],".txt"))
  print(summary(negative_binomial_fit_current))
  # Debugging step for convergence
  print(negative_binomial_fit_current$converged)
  sink()
  
  
  # Zero-Inflated Poisson model
  distribution_name_to_pass <- "poisson"
  eval( parse( text = paste0( "zinf_poisson_fit_current <- zeroinfl( ", set_of_predictors_plant[current_set_index], ", dist = distribution_name_to_pass, data = ", data02, " )" )  ) ) 
  
  zinf_poisson_fit_current
  summary(zinf_poisson_fit_current)
  deviance(zinf_poisson_fit_current)
  df.residual(zinf_poisson_fit_current)
  # Exporting output
  sink(paste0("R_Output/zinf_poisson_fit_current_set_index_data02_", set_of_predictors_plant_text[current_set_index],".txt"))
  print(summary(zinf_poisson_fit_current))
  # Debugging step for convergence
  print(zinf_poisson_fit_current$converged)
  sink()
  
  
  # Zero-Inflated Negative Binomial model
  distribution_name_to_pass <- "negbin"
  eval( parse( text = paste0( "zinf_negative_binomial_fit_current <- zeroinfl( ", set_of_predictors_plant[current_set_index], ", dist = distribution_name_to_pass, data = ", data02, " )" )  ) ) 
  
  zinf_negative_binomial_fit_current
  summary(zinf_negative_binomial_fit_current)
  deviance(zinf_negative_binomial_fit_current)
  df.residual(zinf_negative_binomial_fit_current)
  # Exporting output
  sink(paste0("R_Output/zinf_negative_binomial_fit_current_set_index_data02_", set_of_predictors_plant_text[current_set_index],".txt"))
  print(summary(zinf_negative_binomial_fit_current))
  # Debugging step for convergence
  print(zinf_negative_binomial_fit_current$converged)
  sink()
  
  
  
  
  # Hurdle Poisson model
  distribution_name_to_pass <- "poisson"
  eval( parse( text = paste0( "hurdle_poisson_fit_current <- hurdle( ", set_of_predictors_plant[current_set_index], ", dist = distribution_name_to_pass, data = ", data02, " )" )  ) ) 
  
  hurdle_poisson_fit_current
  summary(hurdle_poisson_fit_current)
  deviance(hurdle_poisson_fit_current)
  df.residual(hurdle_poisson_fit_current)
  # Exporting output
  sink(paste0("R_Output/hurdle_poisson_fit_current_set_index_data02_", set_of_predictors_plant_text[current_set_index],".txt"))
  print(summary(hurdle_poisson_fit_current))
  # Debugging step for convergence
  print(hurdle_poisson_fit_current$converged)
  sink()
  
  
  # Hurdle Negative Binomial model
  distribution_name_to_pass <- "negbin"
  eval( parse( text = paste0( "hurdle_negative_binomial_fit_current <- hurdle( ", set_of_predictors_plant[current_set_index], ", dist = distribution_name_to_pass, data = ", data02, " )" )  ) ) 
  
  hurdle_negative_binomial_fit_current
  summary(hurdle_negative_binomial_fit_current)
  deviance(hurdle_negative_binomial_fit_current)
  df.residual(hurdle_negative_binomial_fit_current)
  # Exporting output
  sink(paste0("R_Output/hurdle_negative_binomial_fit_current_set_index_data02_", set_of_predictors_plant_text[current_set_index],".txt"))
  print(summary(hurdle_negative_binomial_fit_current))
  # Debugging step for convergence
  print(hurdle_negative_binomial_fit_current$converged)
  sink()
  

  
# End of -> for( current_set_index in c(1:length(set_of_predictors_plant)) )
}  












