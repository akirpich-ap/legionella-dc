# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.11.04. ask
rm(list=ls(all=TRUE))

# Library for merging
library(plyr)


# Setting the correct working directory.
# Debugging step to run on local machine instead instead of the code right above used for HiPer Gator.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Weppelman Legionella"

# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()



# Reading all files and fixing names
# -------------------------------------------------------------------------------------
years_list <- c(2001:2019)

for (current_year in years_list)
{
  # Debugging step
  # current_year <- years_list[1]
  # current_year <- years_list[7]
  
  load( file = paste0("R_Data/year_", current_year, "_river_table01.RData") )
  load( file = paste0("R_Data/year_", current_year, "_river_table02.RData") )
  load( file = paste0("R_Data/year_", current_year, "_dalecarlia_table01.RData") )
  load( file = paste0("R_Data/year_", current_year, "_mcmillan_table01.RData") )
  load( file = paste0("R_Data/year_", current_year, "_dalecarlia_table02.RData") )
  load( file = paste0("R_Data/year_", current_year, "_mcmillan_table02.RData") )

  

  # Fixing names
  
  # river_table01
  # Printing names
  cat("Year -> ", current_year, "\n")
  eval( parse( text=paste0("print(names(year_", current_year, "_river_table01))") ))
  # Saving extra copy
  eval( parse( text=paste0("current_year_data <- year_", current_year, "_river_table01") ))
  # TOTAL AMMONIA 
  which_TOTAL_AMMONIA_N <- which(names(current_year_data) == "TOTAL AMMONIA - N" )
  names(current_year_data)[which_TOTAL_AMMONIA_N] <- "TOTAL AMMONIA"
  # TOTAL ORG. CARBON 
  which_TOTAL_ORG_CARBON_N <- which(names(current_year_data) == "TOTAL ORG. CARBON" )
  names(current_year_data)[which_TOTAL_ORG_CARBON_N] <- "TOTAL ORGANIC CARBON"
  # NITRATE
  which_NITRATE_N <- which(names(current_year_data) == "NITRATE - N" )
  names(current_year_data)[which_NITRATE_N] <- "NITRATE"
  # NITRITE
  which_NITRITE_N <- which(names(current_year_data) == "NITRITE - N" )
  names(current_year_data)[which_NITRITE_N] <- "NITRITE"
  # ORTHOPHOSPHATE
  which_ORTHOPHOSPHATE_PO4 <- which(names(current_year_data) == "ORTHOPHOSPHATE - PO4" )
  names(current_year_data)[which_ORTHOPHOSPHATE_PO4] <- "ORTHOPHOSPHATE"
  which_ORTHOPHOSPHATE_as_PO4 <- which(names(current_year_data) == "ORTHOPHOSPHATE as PO4" )
  names(current_year_data)[which_ORTHOPHOSPHATE_as_PO4] <- "ORTHOPHOSPHATE"
  # GIARDIA - Great Falls Intake
  which_GIARDIA_Great_Falls_Intake <- which(names(current_year_data) == "GIARDIA - Great Falls Intake" )
  names(current_year_data)[which_GIARDIA_Great_Falls_Intake] <- "GIARDIA"
  # CRYPTOSPORIDIUM - Great Falls Intake
  which_CRYPTOSPORIDIUM_Great_Falls_Intake <- which(names(current_year_data) == "CRYPTOSPORIDIUM - Great Falls Intake" )
  names(current_year_data)[which_CRYPTOSPORIDIUM_Great_Falls_Intake] <- "CRYPTOSPORIDIUM"
  # Saving names back
  eval( parse( text=paste0("names(year_", current_year, "_river_table01) <- names(current_year_data)") ))
  cat("Year -> ", current_year, "\n")
  eval( parse( text=paste0("print(names(year_", current_year, "_river_table01))") ))
  


  # dalecarlia_table01
  # Printing names
  cat("Year -> ", current_year, "\n")
  eval( parse( text=paste0("print(names(year_", current_year, "_dalecarlia_table01))") ))
  # Saving extra copy
  eval( parse( text=paste0("current_year_data <- year_", current_year, "_dalecarlia_table01") ))
  # TOTAL AMMONIA 
  which_TOTAL_AMMONIA_N <- which(names(current_year_data) == "TOTAL AMMONIA - N" )
  names(current_year_data)[which_TOTAL_AMMONIA_N] <- "TOTAL AMMONIA"
  # NITRATE
  which_NITRATE_N <- which(names(current_year_data) == "NITRATE - N" )
  names(current_year_data)[which_NITRATE_N] <- "NITRATE"
  # NITRITE
  which_NITRITE_N <- which(names(current_year_data) == "NITRITE - N" )
  names(current_year_data)[which_NITRITE_N] <- "NITRITE"
  # ORTHOPHOSPHATE
  which_ORTHOPHOSPHATE_PO4 <- which(names(current_year_data) == "ORTHOPHOSPHATE - PO4" )
  names(current_year_data)[which_ORTHOPHOSPHATE_PO4] <- "ORTHOPHOSPHATE"
  which_ORTHOPHOSPHATE_as_PO4 <- which(names(current_year_data) == "ORTHOPHOSPHATE as PO4" )
  names(current_year_data)[which_ORTHOPHOSPHATE_as_PO4] <- "ORTHOPHOSPHATE"
  # GIARDIA - Great Falls Intake
  which_GIARDIA_Great_Falls_Intake <- which(names(current_year_data) == "GIARDIA - Great Falls Intake" )
  names(current_year_data)[which_GIARDIA_Great_Falls_Intake] <- "GIARDIA"
  # CRYPTOSPORIDIUM - Great Falls Intake
  which_CRYPTOSPORIDIUM_Great_Falls_Intake <- which(names(current_year_data) == "CRYPTOSPORIDIUM - Great Falls Intake" )
  names(current_year_data)[which_CRYPTOSPORIDIUM_Great_Falls_Intake] <- "CRYPTOSPORIDIUM"
  # Saving names back
  eval( parse( text=paste0("names(year_", current_year, "_dalecarlia_table01) <- names(current_year_data)") ))
  cat("Year -> ", current_year, "\n")
  eval( parse( text=paste0("print(names(year_", current_year, "_dalecarlia_table01))") ))
  
  

  # mcmillan_table01
  # Printing names
  cat("Year -> ", current_year, "\n")
  eval( parse( text=paste0("print(names(year_", current_year, "_mcmillan_table01))") ))
  # Saving extra copy
  eval( parse( text=paste0("current_year_data <- year_", current_year, "_mcmillan_table01") ))
  # TOTAL AMMONIA 
  which_TOTAL_AMMONIA_N <- which(names(current_year_data) == "TOTAL AMMONIA - N" )
  names(current_year_data)[which_TOTAL_AMMONIA_N] <- "TOTAL AMMONIA"
  # NITRATE
  which_NITRATE_N <- which(names(current_year_data) == "NITRATE - N" )
  names(current_year_data)[which_NITRATE_N] <- "NITRATE"
  # NITRITE
  which_NITRITE_N <- which(names(current_year_data) == "NITRITE - N" )
  names(current_year_data)[which_NITRITE_N] <- "NITRITE"
  # ORTHOPHOSPHATE
  which_ORTHOPHOSPHATE_PO4 <- which(names(current_year_data) == "ORTHOPHOSPHATE - PO4" )
  names(current_year_data)[which_ORTHOPHOSPHATE_PO4] <- "ORTHOPHOSPHATE"
  which_ORTHOPHOSPHATE_as_PO4 <- which(names(current_year_data) == "ORTHOPHOSPHATE as PO4" )
  names(current_year_data)[which_ORTHOPHOSPHATE_as_PO4] <- "ORTHOPHOSPHATE"
  # GIARDIA - Great Falls Intake
  which_GIARDIA_Great_Falls_Intake <- which(names(current_year_data) == "GIARDIA - Great Falls Intake" )
  names(current_year_data)[which_GIARDIA_Great_Falls_Intake] <- "GIARDIA"
  # CRYPTOSPORIDIUM - Great Falls Intake
  which_CRYPTOSPORIDIUM_Great_Falls_Intake <- which(names(current_year_data) == "CRYPTOSPORIDIUM - Great Falls Intake" )
  names(current_year_data)[which_CRYPTOSPORIDIUM_Great_Falls_Intake] <- "CRYPTOSPORIDIUM"
  # Saving names back
  eval( parse( text=paste0("names(year_", current_year, "_mcmillan_table01) <- names(current_year_data)") ))
  cat("Year -> ", current_year, "\n")
  eval( parse( text=paste0("print(names(year_", current_year, "_mcmillan_table01))") ))

  
  
  
  # dalecarlia_table02
  # Printing names
  cat("Year -> ", current_year, "\n")
  eval( parse( text=paste0("print(names(year_", current_year, "_dalecarlia_table02))") ))
  # Saving extra copy
  eval( parse( text=paste0("current_year_data <- year_", current_year, "_dalecarlia_table02") ))
  # TOTAL ORG. CARBON 
  which_TOTAL_ORG_CARBON_N <- which(names(current_year_data) == "TOTAL ORG. CARBON" )
  names(current_year_data)[which_TOTAL_ORG_CARBON_N] <- "TOTAL ORGANIC CARBON"
  # TURBIDITY
  which_TURBIDITY <- which( substr( names(current_year_data), start  = 1, stop = 9 ) == "TURBIDITY" )
  names(current_year_data)[which_TURBIDITY] <- "TURBIDITY"
  # Saving names back
  eval( parse( text=paste0("names(year_", current_year, "_dalecarlia_table02) <- names(current_year_data)") ))
  cat("Year -> ", current_year, "\n")
  eval( parse( text=paste0("print(names(year_", current_year, "_dalecarlia_table02))") ))
  
  

  
  # mcmillan_table02
  # Printing names
  cat("Year -> ", current_year, "\n")
  eval( parse( text=paste0("print(names(year_", current_year, "_mcmillan_table02))") ))
  # Saving extra copy
  eval( parse( text=paste0("current_year_data <- year_", current_year, "_mcmillan_table02") ))
  # TOTAL ORG. CARBON 
  which_TOTAL_ORG_CARBON_N <- which(names(current_year_data) == "TOTAL ORG. CARBON" )
  names(current_year_data)[which_TOTAL_ORG_CARBON_N] <- "TOTAL ORGANIC CARBON"
  # TURBIDITY
  which_TURBIDITY <- which( substr( names(current_year_data), start  = 1, stop = 9 ) == "TURBIDITY" )
  names(current_year_data)[which_TURBIDITY] <- "TURBIDITY"
  # Saving names back
  eval( parse( text=paste0("names(year_", current_year, "_mcmillan_table02) <- names(current_year_data)") ))
  cat("Year -> ", current_year, "\n")
  eval( parse( text=paste0("print(names(year_", current_year, "_mcmillan_table02))") ))
  
      

  
# End of -> for (current_year in years_list)  
}  


  
for (current_year in years_list)
{

  # Saving YEAR MONTH identifiers
  # river_table01
  eval( parse( text=paste0("month_current <- year_", current_year, "_river_table01$MONTH") ))
  year_month_current <- paste0(current_year, "-", month_current)
  eval( parse( text = paste0("year_", current_year, "_river_table01$YEAR_MONTH <- year_month_current") )  )  

  # river_table02
  eval( parse( text=paste0("month_current <- year_", current_year, "_river_table02$MONTH") ))
  year_month_current <- paste0(current_year, "-", month_current)
  eval( parse( text = paste0("year_", current_year, "_river_table02$YEAR_MONTH <- year_month_current") )  )  
  
  # dalecarlia_table01
  eval( parse( text=paste0("month_current <- year_", current_year, "_dalecarlia_table01$MONTH") ))
  year_month_current <- paste0(current_year, "-", month_current)
  eval( parse( text = paste0("year_", current_year, "_dalecarlia_table01$YEAR_MONTH <- year_month_current") )  )  
  
  # dalecarlia_table02
  eval( parse( text=paste0("month_current <- year_", current_year, "_dalecarlia_table02$MONTH") ))
  year_month_current <- paste0(current_year, "-", month_current)
  eval( parse( text = paste0("year_", current_year, "_dalecarlia_table02$YEAR_MONTH <- year_month_current") )  )  
  
  # mcmillan_table01
  eval( parse( text=paste0("month_current <- year_", current_year, "_mcmillan_table01$MONTH") ))
  year_month_current <- paste0(current_year, "-", month_current)
  eval( parse( text = paste0("year_", current_year, "_mcmillan_table01$YEAR_MONTH <- year_month_current") )  )  
  
  # mcmillan_table02
  eval( parse( text=paste0("month_current <- year_", current_year, "_mcmillan_table02$MONTH") ))
  year_month_current <- paste0(current_year, "-", month_current)
  eval( parse( text = paste0("year_", current_year, "_mcmillan_table02$YEAR_MONTH <- year_month_current") )  )  
  

  # Converting all frame columns to character so that they can be combined
  # river_table01
  eval( parse( text=paste0("year_", current_year, "_river_table01 <- data.frame(lapply(year_", current_year, "_river_table01, as.character))") ))
  # river_table02
  eval( parse( text=paste0("year_", current_year, "_river_table02 <- data.frame(lapply(year_", current_year, "_river_table02, as.character))") ))
  # dalecarlia_table01
  eval( parse( text=paste0("year_", current_year, "_dalecarlia_table01 <- data.frame(lapply(year_", current_year, "_dalecarlia_table01, as.character))") ))
  # dalecarlia_table02
  eval( parse( text=paste0("year_", current_year, "_dalecarlia_table02 <- data.frame(lapply(year_", current_year, "_dalecarlia_table02, as.character))") ))
  # mcmillan_table01
  eval( parse( text=paste0("year_", current_year, "_mcmillan_table01 <- data.frame(lapply(year_", current_year, "_mcmillan_table01, as.character))") ))
  # mcmillan_table02
  eval( parse( text=paste0("year_", current_year, "_mcmillan_table02 <- data.frame(lapply(year_", current_year, "_mcmillan_table02, as.character))") ))
  
  
  
  # Binding in a loop using bind rows
  # Year 2001 case
  if ( current_year ==  years_list[1] )
  {
    # river_table01
    eval( parse( text = paste0("year_all_river_table01 <- year_", current_year, "_river_table01") )  )  
    # river_table02
    eval( parse( text = paste0("year_all_river_table02 <- year_", current_year, "_river_table02") )  )  
    # dalecarlia_table01
    eval( parse( text = paste0("year_all_dalecarlia_table01 <- year_", current_year, "_dalecarlia_table01") )  )  
    # dalecarlia_table02
    eval( parse( text = paste0("year_all_dalecarlia_table02 <- year_", current_year, "_dalecarlia_table02") )  )  
    # mcmillan_table01
    eval( parse( text = paste0("year_all_mcmillan_table01 <- year_", current_year, "_mcmillan_table01") )  )  
    # mcmillan_table02
    eval( parse( text = paste0("year_all_mcmillan_table02 <- year_", current_year, "_mcmillan_table02") )  )  

  # End of -> if ( current_year ==  years_list[1] )  
  }

  # Year >=2002 case
  if ( current_year >=  years_list[2] )
  {
    # river_table01
    eval( parse( text = paste0("year_all_river_table01 <-  dplyr::bind_rows(year_all_river_table01,  year_", current_year, "_river_table01)") )  )  
    # river_table02
    eval( parse( text = paste0("year_all_river_table02 <-  dplyr::bind_rows(year_all_river_table02,  year_", current_year, "_river_table02)") )  )  
    # dalecarlia_table01
    eval( parse( text = paste0("year_all_dalecarlia_table01 <-  dplyr::bind_rows(year_all_dalecarlia_table01,  year_", current_year, "_dalecarlia_table01)") )  )  
    # dalecarlia_table02
    eval( parse( text = paste0("year_all_dalecarlia_table02 <-  dplyr::bind_rows(year_all_dalecarlia_table02,  year_", current_year, "_dalecarlia_table02)") )  )  
    # mcmillan_table01
    eval( parse( text = paste0("year_all_mcmillan_table01 <-  dplyr::bind_rows(year_all_mcmillan_table01,  year_", current_year, "_mcmillan_table01)") )  )  
    # mcmillan_table02
    eval( parse( text = paste0("year_all_mcmillan_table02 <-  dplyr::bind_rows(year_all_mcmillan_table02,  year_", current_year, "_mcmillan_table02)") )  )  

  # End of -> if ( current_year >=  years_list[2] )
  }

# End of -> for (current_year in years_list)  
}  





names(year_all_river_table01)
names(year_all_river_table02)
names(year_all_dalecarlia_table01)
names(year_all_dalecarlia_table02)
names(year_all_mcmillan_table01)
names(year_all_mcmillan_table02)


sort(names(year_all_river_table01)) 
sort(names(year_all_river_table02))
sort(names(year_all_dalecarlia_table01))
sort(names(year_all_dalecarlia_table02))
sort(names(year_all_mcmillan_table01))
sort(names(year_all_mcmillan_table02))



# Removing Min, Max and Avg
# year_all_river_table01
which_to_remove_river_table01 <- which( year_all_river_table01$MONTH %in% c("Min", "Max", "Avg"))
length(which_to_remove_river_table01)
year_all_river_table02$YEAR_MONTH[which_to_remove_river_table01]
dim(year_all_river_table01)
# Extract
year_all_river_table01_cleaned <- year_all_river_table01[-which_to_remove_river_table01, ]
dim(year_all_river_table01_cleaned)

# year_all_river_table02
which_to_remove_river_table02 <- which( year_all_river_table02$MONTH %in% c("Min", "Max", "Avg"))
length(which_to_remove_river_table02)
year_all_river_table02$YEAR_MONTH[which_to_remove_river_table02]
dim(year_all_river_table02)
# Extract
year_all_river_table02_cleaned <- year_all_river_table02[-which_to_remove_river_table02, ]
dim(year_all_river_table02_cleaned)

# year_all_dalecarlia_table01
which_to_remove_dalecarlia_table01 <- which( year_all_dalecarlia_table01$MONTH %in% c("Min", "Max", "Avg"))
length(which_to_remove_dalecarlia_table01)
year_all_dalecarlia_table01$YEAR_MONTH[which_to_remove_dalecarlia_table01]
dim(year_all_dalecarlia_table01)
# Extract
year_all_dalecarlia_table01_cleaned <- year_all_dalecarlia_table01[-which_to_remove_dalecarlia_table01, ]
dim(year_all_dalecarlia_table01_cleaned)

# year_all_dalecarlia_table02
which_to_remove_dalecarlia_table02 <- which( year_all_dalecarlia_table02$MONTH %in% c("Min", "Max", "Avg"))
length(which_to_remove_dalecarlia_table02)
year_all_dalecarlia_table02$YEAR_MONTH[which_to_remove_dalecarlia_table02]
dim(year_all_dalecarlia_table02)
# Extract
year_all_dalecarlia_table02_cleaned <- year_all_dalecarlia_table02[-which_to_remove_dalecarlia_table02, ]
dim(year_all_dalecarlia_table02_cleaned)

# year_all_mcmillan_table01
which_to_remove_mcmillan_table01 <- which( year_all_mcmillan_table01$MONTH %in% c("Min", "Max", "Avg"))
length(which_to_remove_mcmillan_table01)
year_all_mcmillan_table01$YEAR_MONTH[which_to_remove_mcmillan_table01]
dim(year_all_mcmillan_table01)
# Extract
year_all_mcmillan_table01_cleaned <- year_all_mcmillan_table01[-which_to_remove_mcmillan_table01, ]
dim(year_all_mcmillan_table01_cleaned)

# year_all_mcmillan_table02
which_to_remove_mcmillan_table02 <- which( year_all_mcmillan_table02$MONTH %in% c("Min", "Max", "Avg"))
length(which_to_remove_mcmillan_table02)
year_all_mcmillan_table02$YEAR_MONTH[which_to_remove_mcmillan_table02]
dim(year_all_mcmillan_table02)
# Extract
year_all_mcmillan_table02_cleaned <- year_all_mcmillan_table02[-which_to_remove_mcmillan_table02, ]
dim(year_all_mcmillan_table02_cleaned)



names(year_all_river_table01)
names(year_all_river_table02)
names(year_all_dalecarlia_table01)
names(year_all_dalecarlia_table02)
names(year_all_mcmillan_table01)
names(year_all_mcmillan_table02)


# Saving all datasets

# year_all_river_table01_cleaned
year_all_river_table01_cleaned_path <- paste0("R_Data/year_all_river_table01_cleaned.RData")
save( year_all_river_table01_cleaned, file = year_all_river_table01_cleaned_path )

# year_all_river_table02_cleaned
year_all_river_table02_cleaned_path <- paste0("R_Data/year_all_river_table02_cleaned.RData")
save( year_all_river_table02_cleaned, file = year_all_river_table02_cleaned_path )

# year_all_dalecarlia_table01_cleaned
year_all_dalecarlia_table01_cleaned_path <- paste0("R_Data/year_all_dalecarlia_table01_cleaned.RData")
save( year_all_dalecarlia_table01_cleaned, file = year_all_dalecarlia_table01_cleaned_path )

# year_all_dalecarlia_table02_cleaned
year_all_dalecarlia_table02_cleaned_path <- paste0("R_Data/year_all_dalecarlia_table02_cleaned.RData")
save( year_all_dalecarlia_table02_cleaned, file = year_all_dalecarlia_table02_cleaned_path )

# year_all_mcmillan_table01_cleaned
year_all_mcmillan_table01_cleaned_path <- paste0("R_Data/year_all_mcmillan_table01_cleaned.RData")
save( year_all_mcmillan_table01_cleaned, file = year_all_mcmillan_table01_cleaned_path )

# year_all_mcmillan_table02_cleaned
year_all_mcmillan_table02_cleaned_path <- paste0("R_Data/year_all_mcmillan_table02_cleaned.RData")
save( year_all_mcmillan_table02_cleaned, file = year_all_mcmillan_table02_cleaned_path )








