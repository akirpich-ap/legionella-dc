# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2022.02.18. ask
rm(list=ls(all=TRUE))

# library for glm.nb 
# i.e. for negative binomial
library(MASS)


# Library pscl zero-inflated poisson
# install.packages("pscl")
library(pscl)

# Loading package for Lag() function with capital L
library(Hmisc)

# To convert dates from text to dates
library(lubridate)



# Setting the correct working directory.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Weppelman Legionella"

# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()




# Reading all merged data sets.

merged_inc_and_arpt_path <- paste0("R_Data/merged_inc_and_arpt.RData")
load( file = merged_inc_and_arpt_path )
names(merged_inc_and_arpt)

merged_river_path <- paste0("R_Data/merged_river.RData")
load( file = merged_river_path )
names(merged_river)

merged_dalecarlia_path <- paste0("R_Data/merged_dalecarlia.RData")
load( file = merged_dalecarlia_path )
names(merged_dalecarlia)

merged_mcmillan_path <- paste0("R_Data/merged_mcmillan.RData")
load( file = merged_mcmillan_path )
names(merged_mcmillan)

# Defining variables to use in the model.
# Nitrate, Orthophosphate (PO4-), Strontium, Barium, Nickel, Turbidity

# Lists to subset
variables_list_inc_temp_perc <- c("Year_Month_fixed", "Counts", "TEMP", "SLP", "WDSP", "PRCP" )
variables_list_river         <- c("YEAR_MONTH", "pH", "NITRATE", "ORTHOPHOSPHATE", "MANGANESE", "STRONTIUM", "BARIUM", "NICKEL", "TOTAL.ORGANIC.CARBON", "TURBIDITY", "ALUMINUM", "TOTAL.COLIFORM", "ZINC", "IRON")
variables_list_dalecarlia    <- c("YEAR_MONTH", "pH", "NITRATE", "ORTHOPHOSPHATE", "MANGANESE", "STRONTIUM", "BARIUM", "NICKEL", "TOTAL.ORGANIC.CARBON", "TURBIDITY", "ALUMINUM", "TOTAL.COLIFORM....positive.", 
                                  "HETEROTROPHIC.PLATE.COUNT", "ZINC", "IRON", "CHLORINE" )
variables_list_mcmillan      <- variables_list_dalecarlia
variables_list_both_plants   <- intersect(variables_list_dalecarlia, variables_list_mcmillan)
# Checking the summaries.
sum( !variables_list_both_plants == variables_list_dalecarlia )
sum( !variables_list_both_plants == variables_list_mcmillan )


# Subsisting the right columns.
merged_inc_and_arpt_subset  <- merged_inc_and_arpt[,variables_list_inc_temp_perc]
merged_river_subset         <- merged_river[, variables_list_river]
merged_dalecarlia_subset    <- merged_dalecarlia[,variables_list_dalecarlia]
merged_mcmillan_subset      <- merged_mcmillan[,variables_list_mcmillan]

head(merged_inc_and_arpt_subset)
head(merged_river_subset)
head(merged_dalecarlia_subset)
head(merged_mcmillan_subset)


# Fix 2022.01.14.
# Fixing plants data for HETEROTROPHIC.PLATE.COUNT
# dalecarlia
step1_dalecarlia <- gsub(pattern = "<", replacement = "", x = merged_dalecarlia_subset$HETEROTROPHIC.PLATE.COUNT)
merged_dalecarlia_subset$HETEROTROPHIC.PLATE.COUNT <- as.numeric( gsub(pattern = "-", replacement = "", x = step1_dalecarlia) )
# mcmillan
step1_mcmillan <- gsub(pattern = "<", replacement = "", x = merged_mcmillan_subset$HETEROTROPHIC.PLATE.COUNT)
merged_mcmillan_subset$HETEROTROPHIC.PLATE.COUNT <- as.numeric( gsub(pattern = "-", replacement = "", x = step1_mcmillan) )
# Fixing NA mcmillan
merged_mcmillan_subset$HETEROTROPHIC.PLATE.COUNT[ is.na(merged_mcmillan_subset$HETEROTROPHIC.PLATE.COUNT) ] <- 0



# Merging and fixing treatment plants
merged_dalecarlia_mcmillan <- merge(x = merged_dalecarlia_subset, y = merged_mcmillan_subset, by = "YEAR_MONTH", all = TRUE, sort = FALSE)
head(merged_dalecarlia_mcmillan)
tail(merged_dalecarlia_mcmillan)


# Here we are processing if missing in one but not in the other then taking that value.
for ( variable_current in variables_list_both_plants[-which(variables_list_both_plants == "YEAR_MONTH")] )
{
  # Debugging
  # variable_current <- variables_list_both_plants[2]
  
  # Re-saving into temporary strings
  eval( parse( text=paste0("value_current_1 <- merged_dalecarlia_mcmillan$", variable_current, ".x") ))
  eval( parse( text=paste0("value_current_2 <- merged_dalecarlia_mcmillan$", variable_current, ".y") ))
  
  # Creating numeric analogs
  value_current_1_numeric <- as.numeric(value_current_1)
  value_current_2_numeric <- as.numeric(value_current_2)
  
  # Checking NA values
  # Checking for each NA independently
  which_isna_value_current_1_numeric <- which( is.na(value_current_1_numeric) )
  which_isna_value_current_2_numeric <- which( is.na(value_current_2_numeric) )
  # Checking for intersection of NA-s
  which_isna_value_both_numeric <- intersect( which_isna_value_current_1_numeric, which_isna_value_current_2_numeric )

  
  # Filling the NA value

  # Averages first. Computing the averages when they are available.
  value_current_filled_numeric <- (value_current_1_numeric + value_current_2_numeric)/2
  
  # Filling NA-s
  # Filling value present only in value 1.
  value_current_filled_numeric[which_isna_value_current_2_numeric] <- value_current_1_numeric[which_isna_value_current_2_numeric]
  # Filling value present only in value 2.
  value_current_filled_numeric[which_isna_value_current_1_numeric] <- value_current_2_numeric[which_isna_value_current_1_numeric]
  
  # Filling both with zeroes. This step fixes 
  value_current_filled_numeric[which_isna_value_both_numeric] <- 0
    
  # Re-saving back into a new variables
  eval( parse( text=paste0("merged_dalecarlia_mcmillan$", variable_current, " <- value_current_filled_numeric") ))
  
# End of -> for ( variable_current in variables_list_both_plants[-which(variables_list_both_plants == "YEAR_MONTH")] )    
}

dim(merged_dalecarlia_mcmillan)
names(merged_dalecarlia_mcmillan)
head(merged_dalecarlia_mcmillan)
tail(merged_dalecarlia_mcmillan)



# Saving the merged treatment plants data
merged_dalecarlia_mcmillan_path <- paste0("R_Data/merged_dalecarlia_mcmillan.RData")
save( merged_dalecarlia_mcmillan, file = merged_dalecarlia_mcmillan_path )



# Fixing river data plants
# Removing MONTH.x and MONTH.y after checking that they are the same.
sum(!merged_river$MONTH.x == merged_river$MONTH.y)
dim(merged_river)
merged_river <- subset(merged_river, select = - c(MONTH.x, MONTH.y) )
dim(merged_river)
# Fixing all other variables
head(merged_river)


# List of variables we wan to remove from the final dataset.
merged_river$ANIONIC.SURFACTANTS
merged_river$SILICATE
merged_river$ALGAE.COUNT
merged_river$IODIDE
merged_river$CHROMIUM
merged_river$HEXAVALENT.CHROMIUM
merged_river$GIARDIA...Little.Falls.Intake
merged_river$CRYPTOSPORIDIUM...Little.Falls.Intake
merged_river$MERCURY
merged_river$POTASSIUM
merged_river$VANADIUM
merged_river$THALLIUM


# Removing variables from above
merged_river <-  subset(merged_river, select = -c(ANIONIC.SURFACTANTS, SILICATE, ALGAE.COUNT, IODIDE, CHROMIUM, HEXAVALENT.CHROMIUM, GIARDIA...Little.Falls.Intake, 
                                                  CRYPTOSPORIDIUM...Little.Falls.Intake, MERCURY, POTASSIUM, VANADIUM, THALLIUM ) )
dim(merged_river)
head(merged_river)




# Looping over the list and fixing each variable individually
for ( variable_current in variables_list_river[-which(variables_list_river == "YEAR_MONTH")] )
{
  # Debugging
  # variable_current <- variables_list_river[2]
  
  # Re-saving into temporary strings
  eval( parse( text=paste0("value_current <- merged_river$", variable_current ) ))

  # Creating a numeric analog
  value_current_numeric <- as.numeric(value_current)
  # Filling NA-s with zeroes
  which_na_value_current_numeric <- which( is.na(value_current_numeric) )
  value_current_numeric_fixed <- value_current_numeric
  value_current_numeric_fixed[which_na_value_current_numeric] <- 0
  
  # Re-saving back into a new variables
  eval( parse( text=paste0("merged_river$", variable_current, " <- value_current_numeric_fixed") ))
  
  # End of -> for ( variable_current in variables_list_river[-which(variables_list_river == "YEAR_MONTH")] )    
}


# At this point all fixes have been done to BOTH: river and COMBINED treatment plant dataset.





# Fix 2021.11.18
# Merging incidence with treatment plants.
# Here merging only those values that are presented in both.
merged_dalecarlia_mcmillan_plus_incidence <- merge(x = merged_inc_and_arpt_subset, y = merged_dalecarlia_mcmillan, 
                                                   by.x = "Year_Month_fixed", by.y = "YEAR_MONTH", all = FALSE, sort = FALSE)
head(merged_inc_and_arpt_subset)
head(merged_dalecarlia_mcmillan)
head(merged_dalecarlia_mcmillan_plus_incidence)

tail(merged_inc_and_arpt_subset)
tail(merged_dalecarlia_mcmillan)
tail(merged_dalecarlia_mcmillan_plus_incidence)


# DEWP - Dew Point
# SLP  - Sea Level Pressure
# WDSP - Wind Speed


sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$Counts) )
sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$NITRATE) )
sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$ORTHOPHOSPHATE) )
sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$STRONTIUM) )
sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$BARIUM) )
sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$NICKEL) )
sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$TOTAL.ORGANIC.CARBON) )
sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$TURBIDITY) )
sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$ALUMINUM) )
sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$STRONTIUM) )
sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$ZINC) )
sum( is.na(merged_dalecarlia_mcmillan_plus_incidence$CHLORINE) )


# Fix 2022.01.14.
# Fixing dates, year and month
# year
merged_dalecarlia_mcmillan_plus_incidence$year  <- as.numeric(substr(x = merged_dalecarlia_mcmillan_plus_incidence$Year_Month_fixed, start = 1, stop = 4 ))
# month
merged_dalecarlia_mcmillan_plus_incidence$month <- substr(x = merged_dalecarlia_mcmillan_plus_incidence$Year_Month_fixed, start = 6, stop = 8 )
# dates
merged_dalecarlia_mcmillan_plus_incidence$date  <- ymd(paste0(merged_dalecarlia_mcmillan_plus_incidence$Year_Month_fixed, "-15"))


# Saving all years fixed
# RData
merged_dalecarlia_mcmillan_plus_incidence_rdata_path <- paste0("R_Data/merged_dalecarlia_mcmillan_plus_incidence.RData")
save( merged_dalecarlia_mcmillan_plus_incidence, file = merged_dalecarlia_mcmillan_plus_incidence_rdata_path )




# Fix 2021.11.18
# Merging incidence with river.
# Here merging only those values that are presented in both.
merged_river_plus_incidence <- merge(x = merged_inc_and_arpt_subset, y = merged_river, 
                                                   by.x = "Year_Month_fixed", by.y = "YEAR_MONTH", all = FALSE, sort = FALSE)

names(merged_river_plus_incidence)
head(merged_inc_and_arpt_subset)
head(merged_river)
head(merged_river_plus_incidence)

tail(merged_inc_and_arpt_subset)
tail(merged_river)
tail(merged_river_plus_incidence)



# DEWP - Dew Point
# SLP  - Sea Level Pressure
# WDSP - Wind Speed


sum( is.na(merged_river_plus_incidence$Counts) )
sum( is.na(merged_river_plus_incidence$NITRATE) )
sum( is.na(merged_river_plus_incidence$ORTHOPHOSPHATE) )
sum( is.na(merged_river_plus_incidence$STRONTIUM) )
sum( is.na(merged_river_plus_incidence$BARIUM) )
sum( is.na(merged_river_plus_incidence$NICKEL) )
sum( is.na(merged_river_plus_incidence$TOTAL.ORGANIC.CARBON) )
sum( is.na(merged_river_plus_incidence$TURBIDITY) )
sum( is.na(merged_river_plus_incidence$ALUMINUM) )
sum( is.na(merged_river_plus_incidence$STRONTIUM) )
sum( is.na(merged_river_plus_incidence$ZINC) )
sum( is.na(merged_river_plus_incidence$CHLORINE) )





# Fix 2022.01.14.
# Fixing dates, year and month
# year
merged_river_plus_incidence$year  <- as.numeric(substr(x = merged_river_plus_incidence$Year_Month_fixed, start = 1, stop = 4 ))
# month
merged_river_plus_incidence$month <- substr(x = merged_river_plus_incidence$Year_Month_fixed, start = 6, stop = 8 )
# dates
merged_river_plus_incidence$date  <- ymd(paste0(merged_river_plus_incidence$Year_Month_fixed, "-15"))




# Saving all years fixed
# RData
merged_river_plus_incidence_rdata_path <- paste0("R_Data/merged_river_plus_incidence.RData")
save( merged_river_plus_incidence, file = merged_river_plus_incidence_rdata_path )



# Plotting treatment plant data and producing corresponding summaries.

# Generating list of variables to plot.
# List of variables to delete 
list_of_variables_to_delete_dalecarlia_mcmillan <- c( "Year_Month_fixed", paste0( variables_list_dalecarlia[ -which( variables_list_dalecarlia == "YEAR_MONTH"  ) ], ".x"), 
                                                      paste0( variables_list_dalecarlia[ -which( variables_list_dalecarlia == "YEAR_MONTH"  ) ], ".y") )
list_of_variables_dalecarlia_mcmillan <- names(merged_dalecarlia_mcmillan_plus_incidence)[ -which(names(merged_dalecarlia_mcmillan_plus_incidence) %in% list_of_variables_to_delete_dalecarlia_mcmillan) ]
length_list_of_variables_dalecarlia_mcmillan <- length(list_of_variables_dalecarlia_mcmillan)




# Fix 2021.12.04
# Creating a table to save data from ccf functions so that the appropriate lags can be saved to regression.
no_month_back <- 5
  

# Creating lag frame to save.
lag_frame_xmo_dalecarlia_mcmillan <- data.frame( matrix(0, nrow = length_list_of_variables_dalecarlia_mcmillan-1, ncol = no_month_back + 4 ) )
names(lag_frame_xmo_dalecarlia_mcmillan) <- c( "Variable", paste0("lag", c(no_month_back:0)), "colmax", "colmax_lag" )
lag_frame_xmo_dalecarlia_mcmillan$Variable <- as.character(lag_frame_xmo_dalecarlia_mcmillan$Variable)



# Generating pdf output.
pdf( paste( "Plots/file05_final_data_merges_and_ccf_plots_dalecarlia_mcmillan.pdf", sep = ""), height = 24, width = 12 )


par( mfrow=c( 6, 3 ), oma=c(1, 1, 2, 1)  )

# Looping over the Variables.
for (  current_parameter1 in list_of_variables_dalecarlia_mcmillan[-which(list_of_variables_dalecarlia_mcmillan %in% c("Counts", "year", "month") )]  )
{
  
  # Debugging step
  # current_parameter1 <- list_of_variables_dalecarlia_mcmillan[2]

  # Saving values
  case_values <- merged_dalecarlia_mcmillan_plus_incidence$Counts
  eval( parse( text = paste("predictor_values <- merged_dalecarlia_mcmillan_plus_incidence$", current_parameter1, sep ="") )  )  

  # Creating ccf objects
  ccf_values  <- ccf( x = case_values,  y = predictor_values, plot = FALSE  )
  
  # Fix 2021.12.04.
  # Saving data into the frame
  # Getting Index
  current_parameter1_index <- which( list_of_variables_dalecarlia_mcmillan[-which(list_of_variables_dalecarlia_mcmillan %in% c("Counts", "year", "month") )] == current_parameter1 ) 
  # Name first
  lag_frame_xmo_dalecarlia_mcmillan$Variable[current_parameter1_index] <- current_parameter1
  # Lag Values
  values_to_extract_dalecarlia_mcmillan <- round( x = ccf_values$acf[ which( ccf_values$lag %in% c(-no_month_back:0) ) ], digits = 5 )
  # Saving those values
  lag_frame_xmo_dalecarlia_mcmillan[ current_parameter1_index, -c(1, no_month_back + 3, no_month_back + 4) ] <-  values_to_extract_dalecarlia_mcmillan
  # finding the largest (absolute) value
  which_lag_max_dalecarlia_mcmillan <-  which( abs(values_to_extract_dalecarlia_mcmillan) == max( abs(values_to_extract_dalecarlia_mcmillan) ) )
  # Saving the corresponding value
  lag_frame_xmo_dalecarlia_mcmillan[ current_parameter1_index, no_month_back + 3 ]  <- lag_frame_xmo_dalecarlia_mcmillan[ current_parameter1_index, which_lag_max_dalecarlia_mcmillan + 1 ]
  lag_frame_xmo_dalecarlia_mcmillan[ current_parameter1_index, no_month_back + 4 ]  <- as.numeric(substr( x = names(lag_frame_xmo_dalecarlia_mcmillan)[which_lag_max_dalecarlia_mcmillan + 1], start= 4, stop = 5 ))
  # ccf_values
  # lag_frame_xmo_dalecarlia_mcmillan[current_parameter1_index, -1 ]
  

  # Current variable plot
  plot(ccf_values,
       col = "darkblue",
       lwd = 5,
       main = paste0("Counts vs ", current_parameter1),
       cex = 1,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 2
  )
    

}  
# Adding the overall title
title( main = list(paste( "Cross Correlation Cunctions", sep = "" ), cex = 1.5), outer=TRUE)

dev.off()


# Fix 2021.12.04.
# RData
lag_frame_xmo_dalecarlia_mcmillan_path <- paste0("R_Data/lag_frame_xmo_dalecarlia_mcmillan.RData")
save( lag_frame_xmo_dalecarlia_mcmillan, file = lag_frame_xmo_dalecarlia_mcmillan_path )


# CSV table for data
merged_dalecarlia_mcmillan_plus_incidence_csv_path <- paste0("R_Output/merged_dalecarlia_mcmillan_plus_incidence.csv")
write.table( merged_dalecarlia_mcmillan_plus_incidence, file = merged_dalecarlia_mcmillan_plus_incidence_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
merged_dalecarlia_mcmillan_plus_incidence_csv_nonames_path <- paste0("R_Output/merged_dalecarlia_mcmillan_plus_incidence_nonames.csv")
write.table( merged_dalecarlia_mcmillan_plus_incidence, file = merged_dalecarlia_mcmillan_plus_incidence_csv_nonames_path, sep = ",", quote = TRUE, row.names = FALSE, col.names = FALSE)

# CSV table for lag correlations
lag_frame_xmo_dalecarlia_mcmillan_csv_path <- paste0("R_Output/lag_frame_xmo_dalecarlia_mcmillan.csv")
write.table( lag_frame_xmo_dalecarlia_mcmillan, file = lag_frame_xmo_dalecarlia_mcmillan_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
lag_frame_xmo_dalecarlia_mcmillan_csv_nonames_path <- paste0("R_Output/lag_frame_xmo_dalecarlia_mcmillan_nonames.csv")
write.table( lag_frame_xmo_dalecarlia_mcmillan, file = lag_frame_xmo_dalecarlia_mcmillan_csv_nonames_path, sep = ",", quote = TRUE, row.names = FALSE, col.names = FALSE)








# Plotting river data and producing corresponding summaries.

# Generating list of variables to plot.
# List of variables to delete 
indexes_to_extract_river <- which( names(merged_river_plus_incidence) %in% c(variables_list_inc_temp_perc[-which(variables_list_inc_temp_perc %in% "Year_Month_fixed")], variables_list_river) )
list_of_variables_river  <- names(merged_river_plus_incidence)[ indexes_to_extract_river  ]
length_list_of_variables_river <- length(list_of_variables_river)




# Fix 2021.12.04
# Creating a table to save data from ccf functions so that the appropriate lags can be saved to regression.
no_month_back <- 5


# Creating lag frame to save.
lag_frame_xmo_river <- data.frame( matrix(0, nrow = length_list_of_variables_river-1, ncol = no_month_back + 4 ) )
names(lag_frame_xmo_river) <- c( "Variable", paste0("lag", c(no_month_back:0)), "colmax", "colmax_lag" )
lag_frame_xmo_river$Variable <- as.character(lag_frame_xmo_river$Variable)




# Generating pdf output.
pdf( paste( "Plots/file05_final_data_merges_and_ccf_plots_river.pdf", sep = ""), height = 24, width = 12 )


par( mfrow=c( 6, 3 ), oma=c(1, 1, 2, 1)  )

# Looping over the Variables.
for (  current_parameter1 in list_of_variables_river[-which(list_of_variables_river == "Counts")]  )
{
  
  # Debugging step
  # current_parameter1 <- list_of_variables_river[2]
  
  # Saving values
  case_values <- merged_river_plus_incidence$Counts
  eval( parse( text = paste("predictor_values <- merged_river_plus_incidence$", current_parameter1, sep ="") )  )  
  
  # Creating ccf objects
  ccf_values  <- ccf(x = case_values,     y = predictor_values, plot = FALSE  )
  
  # Fix 2021.12.04.
  # Saving data into the frame
  # Getting Index
  current_parameter1_index <- which( list_of_variables_river[-which(list_of_variables_river == "Counts")] == current_parameter1 ) 
  # Name first
  lag_frame_xmo_river$Variable[current_parameter1_index] <- current_parameter1
  # Lag Values
  values_to_extract_river <- round( x = ccf_values$acf[ which( ccf_values$lag %in% c(-no_month_back:0) ) ], digits = 5 )
  # Saving those values
  lag_frame_xmo_river[ current_parameter1_index, -c(1, no_month_back + 3, no_month_back + 4) ] <-  values_to_extract_river
  # finding the largest (absolute) value
  which_lag_max_river <-  which( abs(values_to_extract_river) == max( abs(values_to_extract_river) ) )
  # Saving the corresponding value
  lag_frame_xmo_river[ current_parameter1_index, no_month_back + 3 ]  <- lag_frame_xmo_river[ current_parameter1_index, which_lag_max_river + 1 ]
  lag_frame_xmo_river[ current_parameter1_index, no_month_back + 4 ]  <- as.numeric(substr( x = names(lag_frame_xmo_river)[which_lag_max_river + 1], start= 4, stop = 5 ))
  # ccf_values
  # lag_frame_xmo_river[current_parameter1_index, -1 ]
  
  
  # Current variable plot
  plot(ccf_values,
       col = "darkblue",
       lwd = 5,
       main = paste0("Counts vs ", current_parameter1),
       cex = 1,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 2
  )
  
  
}  
# Adding the overall title
title( main = list(paste( "Cross Correlation Cunctions", sep = "" ), cex = 1.5), outer=TRUE)

dev.off()


# RData
lag_frame_xmo_river_path <- paste0("R_Data/lag_frame_xmo_river.RData")
save( lag_frame_xmo_river, file = lag_frame_xmo_river_path )


# CSV table for data.
merged_river_plus_incidence_csv_path <- paste0("R_Output/merged_river_plus_incidence.csv")
write.table( merged_river_plus_incidence, file = merged_river_plus_incidence_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
merged_river_plus_incidence_csv_nonames_path <- paste0("R_Output/merged_river_plus_incidence_nonames.csv")
write.table( merged_river_plus_incidence, file = merged_river_plus_incidence_csv_nonames_path, sep = ",", quote = TRUE, row.names = FALSE, col.names = FALSE)

# CSV table for lag correlations
lag_frame_xmo_river_csv_path <- paste0("R_Output/lag_frame_xmo_river.csv")
write.table( lag_frame_xmo_river, file = lag_frame_xmo_river_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
lag_frame_xmo_river_csv_nonames_path <- paste0("R_Output/lag_frame_xmo_river_nonames.csv")
write.table( lag_frame_xmo_river, file = lag_frame_xmo_river_csv_nonames_path, sep = ",", quote = TRUE, row.names = FALSE, col.names = FALSE)


