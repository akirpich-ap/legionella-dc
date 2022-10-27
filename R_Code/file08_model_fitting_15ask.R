# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2022.10.24. ask
rm(list=ls(all=TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation
options(scipen=20)


# library for glm.nb 
# i.e. for negative binomial
library(MASS)

# For Columns standard deviations
library(matrixStats)

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

# variables_list_inc_temp_perc <- c("Year_Month_fixed", "Counts", "TEMP", "PRCP" )
# variables_list_inc_temp_perc <- c("Year_Month_fixed", "Counts", "TEMP", "SLP", "WDSP", "PRCP" )
variables_list_inc_temp_perc <- c("Year_Month_fixed", "Counts", "TEMP_C", "WDSP_MS", "PRCP_CM" )


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
# input00 <- paste0("Counts ~ TEMP_C + PRCP_CM + SLP + WDSP_MS + month_num")
input00 <- paste0("Counts ~ TEMP_C + PRCP_CM + WDSP_MS + month_num")

# input01 <- paste0("Counts ~ TEMP_C + PRCP_CM + SLP + WDSP_MS + pH + NITRATE + ORTHOPHOSPHATE + MANGANESE + STRONTIUM + BARIUM + NICKEL + TOTAL.ORGANIC.CARBON + TURBIDITY + ALUMINUM + ZINC + IRON")
input01 <- paste0("Counts ~ TEMP_C + PRCP_CM + WDSP_MS + pH + NITRATE + ORTHOPHOSPHATE + MANGANESE + STRONTIUM + BARIUM + NICKEL + TOTAL.ORGANIC.CARBON + TURBIDITY + ALUMINUM + ZINC + IRON")

# input02 <- paste0("Counts ~ TEMP_C + PRCP_CM + SLP + WDSP_MS + pH + NITRATE + ORTHOPHOSPHATE + MANGANESE + STRONTIUM + BARIUM + NICKEL + TOTAL.ORGANIC.CARBON + TURBIDITY + ALUMINUM + TOTAL.COLIFORM....positive. + HETEROTROPHIC.PLATE.COUNT + ZINC + IRON + CHLORINE")
input02 <- paste0("Counts ~ TEMP_C + PRCP_CM + WDSP_MS + pH + NITRATE + ORTHOPHOSPHATE + MANGANESE + STRONTIUM + BARIUM + NICKEL + TOTAL.ORGANIC.CARBON + TURBIDITY + ALUMINUM + TOTAL.COLIFORM....positive. + HETEROTROPHIC.PLATE.COUNT + ZINC + IRON + CHLORINE")


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



# Exporting

# CSV table for data
merged_river_plus_incidence_csv_path <- paste0("R_Output/merged_river_plus_incidence2.csv")
write.table( merged_river_plus_incidence, file = merged_river_plus_incidence_csv_path, sep = ",", quote = TRUE, row.names = FALSE)

merged_dalecarlia_mcmillan_plus_incidence_csv_path <- paste0("R_Output/merged_dalecarlia_mcmillan_plus_incidence2.csv")
write.table( merged_dalecarlia_mcmillan_plus_incidence, file = merged_dalecarlia_mcmillan_plus_incidence_csv_path, sep = ",", quote = TRUE, row.names = FALSE)





# Fix 2022.10.20.
# Extracting the subsets to produce the summaries for the compounds.
# Loading lists of variables first
variables_list_river_adjusted_frame_modified_rdata_path               <- paste("R_Data/variables_list_river_adjusted_frame_modified.RData", sep = "")
variables_list_dalecarlia_mcmillan_adjusted_frame_modified_rdata_path <- paste("R_Data/variables_list_dalecarlia_mcmillan_adjusted_frame_modified.RData", sep = "")
load( file = variables_list_river_adjusted_frame_modified_rdata_path)
load( file = variables_list_dalecarlia_mcmillan_adjusted_frame_modified_rdata_path)

# Extracting subsets
merged_river_plus_incidence_subset               <- merged_river_plus_incidence[, variables_list_river_adjusted_frame_modified$var_code]
merged_dalecarlia_mcmillan_plus_incidence_subset <- merged_dalecarlia_mcmillan_plus_incidence[, variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_code]


# Producing columns summaries

# Defining desired quantiles for every column
qunatiles_list <- c( 0, 0.024, 0.25, 0.50, 0.75, 0.976, 1)

# quantiles only
merged_river_plus_incidence_subset_quantiles               <- sapply(X = merged_river_plus_incidence_subset, FUN = function(x) quantile(x, probs = qunatiles_list))
merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles <- sapply(X = merged_dalecarlia_mcmillan_plus_incidence_subset, FUN = function(x) quantile(x, probs = qunatiles_list))


# Adding means and standard deviations
# Rounding to two digits
# Fixing column names
# Adding column with summaries

# river
merged_river_plus_incidence_subset_quantiles_mean_sd_round <- round( x = rbind( merged_river_plus_incidence_subset_quantiles,
                                                                         colMeans(merged_river_plus_incidence_subset),
                                                                         sapply(merged_river_plus_incidence_subset, sd)), digits = 2)
# Fixing names
lenght_rownames_river <- length(rownames(merged_river_plus_incidence_subset_quantiles_mean_sd_round))
rownames(merged_river_plus_incidence_subset_quantiles_mean_sd_round)[c(lenght_rownames_river-1, lenght_rownames_river)] <- c("Mean", "St.Dev.")

# Checking that the columns are saved according to the order in -> variables_list_river_adjusted_frame_modified)
sum(!merged_river_plus_incidence_subset_quantiles_mean_sd_round == merged_river_plus_incidence_subset_quantiles_mean_sd_round[, variables_list_river_adjusted_frame_modified$var_code])
sum( !colnames(merged_river_plus_incidence_subset_quantiles_mean_sd_round) == variables_list_river_adjusted_frame_modified$var_code)
# Converting to data.frame
merged_river_plus_incidence_subset_quantiles_mean_sd_round <- data.frame(merged_river_plus_incidence_subset_quantiles_mean_sd_round)
# names(merged_river_plus_incidence_subset_quantiles_mean_sd_round) <- variables_list_river_adjusted_frame_modified$var_name
names(merged_river_plus_incidence_subset_quantiles_mean_sd_round) <- paste0( variables_list_river_adjusted_frame_modified$var_name, 
                                                                             " (", variables_list_river_adjusted_frame_modified$var_unit, ")")
# Expanded version
merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded <- data.frame( Statistic = rownames(merged_river_plus_incidence_subset_quantiles_mean_sd_round),
                                                                                   merged_river_plus_incidence_subset_quantiles_mean_sd_round,
                                                                                   check.names = FALSE)
rownames(merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded) <- NULL

# river subsets
river_length <- length(variables_list_river_adjusted_frame_modified$var_code)
merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt1 <- 
      merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded[, c( 1, (2:(ceil(river_length/2)+1)) )  ]
merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt2 <- 
      merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded[, c( 1, ((ceil(river_length/2)+2):(river_length+1)) )  ]





# dalecarlia_mcmillan
merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round <- round( x = rbind( merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles,
                                                                                colMeans(merged_dalecarlia_mcmillan_plus_incidence_subset),
                                                                                sapply(merged_dalecarlia_mcmillan_plus_incidence_subset, sd)), digits = 2)
# Fixing names
lenght_rownames_dalecarlia_mcmillan <- length(rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round))
rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round)[c(lenght_rownames_dalecarlia_mcmillan-1, lenght_rownames_dalecarlia_mcmillan)] <- c("Mean", "St.Dev.")

# Checing that the columns are saved according to the order in -> variables_list_dalecarlia_mcmillan_adjusted_frame_modified)
sum(!merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round == merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round[, variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_code])
sum( !colnames(merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round) == variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_code)
# Converting to data.frame
merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round <- data.frame(merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round)
# names(merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round) <- variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_name
names(merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round) <- paste0( variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_name, 
                                                                             " (", variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_unit, ")")
# Expanded version
merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded <- data.frame( Statistic = rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round),
                                                                                   merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round,
                                                                                   check.names = FALSE)
rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded) <- NULL

# dalecarlia_mcmillan subsets
dalecarlia_mcmillan_length <- length(variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_code)
merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt1 <- 
      merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded[, c( 1, (2:(ceil(dalecarlia_mcmillan_length/2)+1)) )  ]
merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt2 <- 
      merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded[, c( 1, ((ceil(dalecarlia_mcmillan_length/2)+2):(dalecarlia_mcmillan_length+1)) )  ]






# csv
# Creating paths
merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_csv_path                <- paste("R_Output/merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded.csv", sep = "")
merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_csv_path  <- paste("R_Output/merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded.csv", sep = "")
# Actual exports
write.table(x = merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded,               file = merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
write.table(x = merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded, file = merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_csv_path, sep = ",", quote = TRUE, row.names = FALSE)

# cvs subsets
# Creating paths
merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt1_csv_path                <- paste("R_Output/merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt1.csv", sep = "")
merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt2_csv_path                <- paste("R_Output/merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt2.csv", sep = "")
merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt1_csv_path  <- paste("R_Output/merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt1.csv", sep = "")
merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt2_csv_path  <- paste("R_Output/merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt2.csv", sep = "")
# Actual exports
write.table(x = merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt1,               file = merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt1_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
write.table(x = merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt2,               file = merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt2_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
write.table(x = merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt1, file = merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt1_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
write.table(x = merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt2, file = merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_pt2_csv_path, sep = ",", quote = TRUE, row.names = FALSE)


# RData
# Creating paths
merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_rdata_path                <- paste("R_Data/merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded.RData", sep = "")
merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_rdata_path  <- paste("R_Data/merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded.RData", sep = "")
# Actual exports
save(x = merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded,               file = merged_river_plus_incidence_subset_quantiles_mean_sd_round_expanded_rdata_path)
save(x = merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded, file = merged_dalecarlia_mcmillan_plus_incidence_subset_quantiles_mean_sd_round_expanded_rdata_path)






# Fix 2022.10.21
# Doing the same summaries across years
years_list <- c(2001:2019)

for ( i in c(1:length(years_list)) )
{
  # Debugging step
  # i <- 1

  
  # Defining desired quantiles for every column
  qunatiles_list <- c( 0, 0.024, 0.25, 0.50, 0.75, 0.976, 1)
  
    
  # river data
  # Extracting subsets
  merged_river_plus_incidence_subset_year_all <- merged_river_plus_incidence[, c(variables_list_river_adjusted_frame_modified$var_code, "year") ]
  which_rows_current_year <-  which( merged_river_plus_incidence_subset_year_all$year == years_list[i] )
  merged_river_plus_incidence_subset_year_current <- merged_river_plus_incidence_subset_year_all[which_rows_current_year, -which(names(merged_river_plus_incidence_subset_year_all) == "year") ]
  
  
  # Producing columns summaries
  # quantiles only
  merged_river_plus_incidence_subset_year_current_quantiles <- sapply(X = merged_river_plus_incidence_subset_year_current, FUN = function(x) quantile(x, probs = qunatiles_list))

  # Adding means and standard deviations
  # Rounding to two digits
  # Fixing column names
  # Adding column with summaries
  
  # river
  merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round <- round( x = rbind( merged_river_plus_incidence_subset_year_current_quantiles,
                                                                                        colMeans(merged_river_plus_incidence_subset_year_current),
                                                                                        sapply(merged_river_plus_incidence_subset_year_current, sd)), digits = 2)
  # Fixing names
  lenght_rownames_river <- length(rownames(merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round))
  rownames(merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round)[c(lenght_rownames_river-1, lenght_rownames_river)] <- c("Mean", "St.Dev.")
  
  # Checing that the columns are saved according to the order in -> variables_list_river_adjusted_frame_modified)
  sum(!merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round == merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round[, variables_list_river_adjusted_frame_modified$var_code])
  sum( !colnames(merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round) == variables_list_river_adjusted_frame_modified$var_code)
  # Converting to data.frame
  merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round <- data.frame(merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round)
  # names(merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round) <- variables_list_river_adjusted_frame_modified$var_name
  names(merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round) <- paste0( variables_list_river_adjusted_frame_modified$var_name, 
                                                                               " (", variables_list_river_adjusted_frame_modified$var_unit, ")")
  # Expanded version
  merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded <- data.frame( Statistic = rownames(merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round),
                                                                                     merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round,
                                                                                     check.names = FALSE)
  rownames(merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded) <- NULL
  


  # dalecarlia_mcmillan data
  # Extracting subsets
  merged_dalecarlia_mcmillan_plus_incidence_subset_year_all <- merged_dalecarlia_mcmillan_plus_incidence[, c(variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_code, "year") ]
  which_rows_current_year <-  which( merged_dalecarlia_mcmillan_plus_incidence_subset_year_all$year == years_list[i] )
  merged_dalecarlia_mcmillan_plus_incidence_subset_year_current <- merged_dalecarlia_mcmillan_plus_incidence_subset_year_all[which_rows_current_year, -which(names(merged_dalecarlia_mcmillan_plus_incidence_subset_year_all) == "year") ]
  
  
  # Producing columns summaries
  # quantiles only
  merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles <- sapply(X = merged_dalecarlia_mcmillan_plus_incidence_subset_year_current, FUN = function(x) quantile(x, probs = qunatiles_list))
  
  # Adding means and standard deviations
  # Rounding to two digits
  # Fixing column names
  # Adding column with summaries
  
  # dalecarlia_mcmillan
  merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round <- round( x = rbind( merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles,
                                                                                               colMeans(merged_dalecarlia_mcmillan_plus_incidence_subset_year_current),
                                                                                               sapply(merged_dalecarlia_mcmillan_plus_incidence_subset_year_current, sd)), digits = 2)
  # Fixing names
  lenght_rownames_dalecarlia_mcmillan <- length(rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round))
  rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round)[c(lenght_rownames_dalecarlia_mcmillan-1, lenght_rownames_dalecarlia_mcmillan)] <- c("Mean", "St.Dev.")
  
  # Checing that the columns are saved according to the order in -> variables_list_dalecarlia_mcmillan_adjusted_frame_modified)
  sum(!merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round == merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round[, variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_code])
  sum( !colnames(merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round) == variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_code)
  # Converting to data.frame
  merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round <- data.frame(merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round)
  # names(merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round) <- variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_name
  names(merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round) <- paste0( variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_name, 
                                                                                            " (", variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_unit, ")")
  # Expanded version
  merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded <- data.frame( Statistic = rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round),
                                                                                                  merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round,
                                                                                                  check.names = FALSE)
  rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded) <- NULL
  
  
  # csv
  # Creating paths
  merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_csv_path  <- 
        # paste("R_Output/merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_", years_list[i], ".csv", sep = "")
        paste("R_Output/River Compounds Summaries Year - ", years_list[i], ".csv", sep = "")
  merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_csv_path  <- 
        # paste("R_Output/merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_", years_list[i], ".csv", sep = "")
        paste("R_Output/Treatment Plants Compounds Summaries Year - ", years_list[i], ".csv", sep = "")

  # Actual exports
  write.table(x = merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded,               file = merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
  write.table(x = merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded, file = merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
  
  # RData
  # Creating paths
  merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_rdata_path                <- paste("R_Data/merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_", years_list[i], ".RData", sep = "")
  merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_rdata_path  <- paste("R_Data/merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_", years_list[i], ".RData", sep = "")
  # Actual exports
  save(x = merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded,               file = merged_river_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_rdata_path)
  save(x = merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded, file = merged_dalecarlia_mcmillan_plus_incidence_subset_year_current_quantiles_mean_sd_round_expanded_rdata_path)
  

    
# End of -> for ( i in c(1:length(years_list)) )  
}  













# Fix 2022.10.21
# Doing the same summaries across months
months_list <- month.abb

for ( i in c(1:length(months_list)) )
{
  # Debugging step
  # i <- 1
  
  
  # Defining desired quantiles for every column
  qunatiles_list <- c( 0, 0.024, 0.25, 0.50, 0.75, 0.976, 1)
  
  
  # river data
  # Extracting subsets
  merged_river_plus_incidence_subset_month_all <- merged_river_plus_incidence[, c(variables_list_river_adjusted_frame_modified$var_code, "month") ]
  which_rows_current_month <-  which( merged_river_plus_incidence_subset_month_all$month == months_list[i] )
  merged_river_plus_incidence_subset_month_current <- merged_river_plus_incidence_subset_month_all[which_rows_current_month, -which(names(merged_river_plus_incidence_subset_month_all) == "month") ]
  
  
  # Producing columns summaries
  # quantiles only
  merged_river_plus_incidence_subset_month_current_quantiles <- sapply(X = merged_river_plus_incidence_subset_month_current, FUN = function(x) quantile(x, probs = qunatiles_list))
  
  # Adding means and standard deviations
  # Rounding to two digits
  # Fixing column names
  # Adding column with summaries
  
  # river
  merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round <- round( x = rbind( merged_river_plus_incidence_subset_month_current_quantiles,
                                                                                               colMeans(merged_river_plus_incidence_subset_month_current),
                                                                                               sapply(merged_river_plus_incidence_subset_month_current, sd)), digits = 2)
  # Fixing names
  lenght_rownames_river <- length(rownames(merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round))
  rownames(merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round)[c(lenght_rownames_river-1, lenght_rownames_river)] <- c("Mean", "St.Dev.")
  
  # Checing that the columns are saved according to the order in -> variables_list_river_adjusted_frame_modified)
  sum(!merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round == merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round[, variables_list_river_adjusted_frame_modified$var_code])
  sum( !colnames(merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round) == variables_list_river_adjusted_frame_modified$var_code)
  # Converting to data.frame
  merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round <- data.frame(merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round)
  # names(merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round) <- variables_list_river_adjusted_frame_modified$var_name
  names(merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round) <- paste0( variables_list_river_adjusted_frame_modified$var_name, 
                                                                                            " (", variables_list_river_adjusted_frame_modified$var_unit, ")")
  # Expanded version
  merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded <- data.frame( Statistic = rownames(merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round),
                                                                                                  merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round,
                                                                                                  check.names = FALSE)
  rownames(merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded) <- NULL
  
  
  
  # dalecarlia_mcmillan data
  # Extracting subsets
  merged_dalecarlia_mcmillan_plus_incidence_subset_month_all <- merged_dalecarlia_mcmillan_plus_incidence[, c(variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_code, "month") ]
  which_rows_current_month <-  which( merged_dalecarlia_mcmillan_plus_incidence_subset_month_all$month == months_list[i] )
  merged_dalecarlia_mcmillan_plus_incidence_subset_month_current <- merged_dalecarlia_mcmillan_plus_incidence_subset_month_all[which_rows_current_month, -which(names(merged_dalecarlia_mcmillan_plus_incidence_subset_month_all) == "month") ]
  
  
  # Producing columns summaries
  # quantiles only
  merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles <- sapply(X = merged_dalecarlia_mcmillan_plus_incidence_subset_month_current, FUN = function(x) quantile(x, probs = qunatiles_list))
  
  # Adding means and standard deviations
  # Rounding to two digits
  # Fixing column names
  # Adding column with summaries
  
  # dalecarlia_mcmillan
  merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round <- round( x = rbind( merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles,
                                                                                                             colMeans(merged_dalecarlia_mcmillan_plus_incidence_subset_month_current),
                                                                                                             sapply(merged_dalecarlia_mcmillan_plus_incidence_subset_month_current, sd)), digits = 2)
  # Fixing names
  lenght_rownames_dalecarlia_mcmillan <- length(rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round))
  rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round)[c(lenght_rownames_dalecarlia_mcmillan-1, lenght_rownames_dalecarlia_mcmillan)] <- c("Mean", "St.Dev.")
  
  # Checing that the columns are saved according to the order in -> variables_list_dalecarlia_mcmillan_adjusted_frame_modified)
  sum(!merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round == merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round[, variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_code])
  sum( !colnames(merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round) == variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_code)
  # Converting to data.frame
  merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round <- data.frame(merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round)
  # names(merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round) <- variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_name
  names(merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round) <- paste0( variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_name, 
                                                                                                          " (", variables_list_dalecarlia_mcmillan_adjusted_frame_modified$var_unit, ")")
  # Expanded version
  merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded <- data.frame( Statistic = rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round),
                                                                                                                merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round,
                                                                                                                check.names = FALSE)
  rownames(merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded) <- NULL
  
  
  # csv
  # Creating paths
  merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_csv_path  <- 
    # paste("R_Output/merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_", months_list[i], ".csv", sep = "")
    paste("R_Output/River Compounds Summaries Month - ", months_list[i], ".csv", sep = "")
  merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_csv_path  <- 
    # paste("R_Output/merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_", months_list[i], ".csv", sep = "")
    paste("R_Output/Treatment Plants Compounds Summaries Month - ", months_list[i], ".csv", sep = "")

  # Actual exports
  write.table(x = merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded,               file = merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
  write.table(x = merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded, file = merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
  
  # RData
  # Creating paths
  merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_rdata_path                <- paste("R_Data/merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_", months_list[i], ".RData", sep = "")
  merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_rdata_path  <- paste("R_Data/merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_", months_list[i], ".RData", sep = "")
  # Actual exports
  save(x = merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded,               file = merged_river_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_rdata_path)
  save(x = merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded, file = merged_dalecarlia_mcmillan_plus_incidence_subset_month_current_quantiles_mean_sd_round_expanded_rdata_path)
  
  
  
  # End of -> for ( i in c(1:length(months_list)) )  
}  






























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
  cat("\n\nModel Fit Output:\n")
  print(summary(hurdle_poisson_fit_current))
  # Exponent of the coefficients.  
  cat("\n\nCounts component (non-zero counts) coefficients on the exp() scale:\n")
  print(round( x = exp(unlist(hurdle_poisson_fit_current$coefficients[1])), digits = 3))
  cat("\n\nHurdle component (zero vs non-zero) coefficients on the exp() scale:\n")
  print(round( x = exp(unlist(hurdle_poisson_fit_current$coefficients[2])), digits = 3))
  # Debugging step for convergence
  cat("\n\nModel convergence check:\n")
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
  cat("\n\nModel Fit Output:\n")
  print(summary(hurdle_negative_binomial_fit_current))
  # Exponent of the coefficients.  
  cat("\n\nCounts component (non-zero counts) coefficients on the exp() scale:\n")
  print(round( x = exp(unlist(hurdle_negative_binomial_fit_current$coefficients[1])), digits = 3))
  cat("\n\nHurdle component (zero vs non-zero) coefficients on the exp() scale:\n")
  print(round( x = exp(unlist(hurdle_negative_binomial_fit_current$coefficients[2])), digits = 3))
  # Debugging step for convergence
  cat("\n\nModel convergence check:\n")
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
  cat("\n\nModel Fit Output:\n")
  print(summary(hurdle_poisson_fit_current))
  # Exponent of the coefficients.  
  cat("\n\nCounts component (non-zero counts) coefficients on the exp() scale:\n")
  print(round( x = exp(unlist(hurdle_poisson_fit_current$coefficients[1])), digits = 3))
  cat("\n\nHurdle component (zero vs non-zero) coefficients on the exp() scale:\n")
  print(round( x = exp(unlist(hurdle_poisson_fit_current$coefficients[2])), digits = 3))
  # Debugging step for convergence
  cat("\n\nModel convergence check:\n")
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
  cat("\n\nModel Fit Output:\n")
  print(summary(hurdle_negative_binomial_fit_current))
  # Exponent of the coefficients.  
  cat("\n\nCounts component (non-zero counts) coefficients on the exp() scale:\n")
  print(round( x = exp(unlist(hurdle_negative_binomial_fit_current$coefficients[1])), digits = 3))
  cat("\n\nHurdle component (zero vs non-zero) coefficients on the exp() scale:\n")
  print(round( x = exp(unlist(hurdle_negative_binomial_fit_current$coefficients[2])), digits = 3))
  # Debugging step for convergence
  cat("\n\nModel convergence check:\n")
  print(hurdle_negative_binomial_fit_current$converged)
  sink()
  

  
# End of -> for( current_set_index in c(1:length(set_of_predictors_plant)) )
}  












