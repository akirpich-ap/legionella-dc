# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.11.18. ask
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



# Loading all years incidence
monthly_incidence_dc_all_years_processed_path <- paste0("R_Data/monthly_incidence_dc_all_years_processed.RData")
load( file = monthly_incidence_dc_all_years_processed_path )
dim(monthly_incidence_dc_all_years_processed)


# Loading combined temperature/precipitation
year_weather_all_monthly_path <- "R_Data/year_weather_all_monthly.RData"
load( file = year_weather_all_monthly_path  )
dim(year_weather_all_monthly)


# Reading all water quality datasets

# year_all_river_table01_cleaned
year_all_river_table01_cleaned_path <- paste0("R_Data/year_all_river_table01_cleaned.RData")
load( file = year_all_river_table01_cleaned_path )

# year_all_river_table02_cleaned
year_all_river_table02_cleaned_path <- paste0("R_Data/year_all_river_table02_cleaned.RData")
load( file = year_all_river_table02_cleaned_path )

# year_all_dalecarlia_table01_cleaned
year_all_dalecarlia_table01_cleaned_path <- paste0("R_Data/year_all_dalecarlia_table01_cleaned.RData")
load( file = year_all_dalecarlia_table01_cleaned_path )

# year_all_dalecarlia_table02_cleaned
year_all_dalecarlia_table02_cleaned_path <- paste0("R_Data/year_all_dalecarlia_table02_cleaned.RData")
load( file = year_all_dalecarlia_table02_cleaned_path )

# year_all_mcmillan_table01_cleaned
year_all_mcmillan_table01_cleaned_path <- paste0("R_Data/year_all_mcmillan_table01_cleaned.RData")
load( file = year_all_mcmillan_table01_cleaned_path )

# year_all_mcmillan_table02_cleaned
year_all_mcmillan_table02_cleaned_path <- paste0("R_Data/year_all_mcmillan_table02_cleaned.RData")
load( file = year_all_mcmillan_table02_cleaned_path )


# Merging incidence and temperature/water
names(monthly_incidence_dc_all_years_processed)
names(year_weather_all_monthly)
dim(monthly_incidence_dc_all_years_processed)
dim(year_weather_all_monthly)
# Merge itself
merged_inc_and_arpt <- merge(x = monthly_incidence_dc_all_years_processed, y = year_weather_all_monthly, by.x = "Year_Month", by.y = "DATE", all = TRUE, sort = FALSE)
dim(merged_inc_and_arpt)

Year_Month_temp <-  as.Date( paste0( merged_inc_and_arpt$Year_Month, "-01"), format = "%Y-%m-%d" )
merged_inc_and_arpt$Year_Month_fixed <- format( Year_Month_temp ,"%Y-%b")
names(merged_inc_and_arpt)
head(merged_inc_and_arpt)
tail(merged_inc_and_arpt)

# Saving
merged_inc_and_arpt_path <- paste0("R_Data/merged_inc_and_arpt.RData")
save( merged_inc_and_arpt, file = merged_inc_and_arpt_path )




# Merging river characteristics
names(year_all_river_table01_cleaned)
names(year_all_river_table02_cleaned)
dim(year_all_river_table01_cleaned)
dim(year_all_river_table01_cleaned)
# Merge itself
merged_river <- merge(x = year_all_river_table01_cleaned, y = year_all_river_table02_cleaned, by = "YEAR_MONTH", all = TRUE, sort = FALSE)
dim(merged_river)
names(merged_river)
head(merged_river)
tail(merged_river)

# Fixing aluminium
which_aluminium_not_na_y <- which( !is.na(merged_river$ALUMINUM.y) )
which_aluminium_not_na_x <- which( !is.na(merged_river$ALUMINUM.x) )
intersect(which_aluminium_not_na_x, which_aluminium_not_na_y)
# Filling
merged_river$ALUMINUM <- merged_river$ALUMINUM.x
merged_river$ALUMINUM[which_aluminium_not_na_y] <- merged_river$ALUMINUM.y[which_aluminium_not_na_y]
# Dropping original columns
drop_aluminium <- c("ALUMINUM.x","ALUMINUM.y")
merged_river = merged_river[,!(names(merged_river) %in% drop_aluminium)]
names(merged_river)

# Fixing antimony
which_antimony_not_na_y <- which( !is.na(merged_river$ANTIMONY.y) )
which_antimony_not_na_x <- which( !is.na(merged_river$ANTIMONY.x) )
intersect(which_antimony_not_na_x, which_antimony_not_na_y)
# Filling
merged_river$ANTIMONY <- merged_river$ANTIMONY.x
merged_river$ANTIMONY[which_antimony_not_na_y] <- merged_river$ANTIMONY.y[which_antimony_not_na_y]
# Dropping original columns
drop_antimony <- c("ANTIMONY.x","ANTIMONY.y")
merged_river = merged_river[,!(names(merged_river) %in% drop_antimony)]
names(merged_river)

# Fixing phosphate-orthoposphate
which_phosphate_not_na <- which( !is.na(merged_river$PHOSPHATE) )
which_orthophosphate_not_na <- which( !is.na(merged_river$ORTHOPHOSPHATE) )
intersect(which_phosphate_not_na, which_orthophosphate_not_na)
# Filling
merged_river$ORTHOPHOSPHATE[which_phosphate_not_na] <- merged_river$PHOSPHATE[which_phosphate_not_na]
# Dropping original columns
drop_phosphate_orthophosphate <- c("PHOSPHATE")
merged_river = merged_river[,!(names(merged_river) %in% drop_phosphate_orthophosphate)]
names(merged_river)

# Saving
merged_river_path <- paste0("R_Data/merged_river.RData")
save( merged_river, file = merged_river_path )




# Merging dalecarlia characteristics
names(year_all_dalecarlia_table01_cleaned)
names(year_all_dalecarlia_table02_cleaned)
dim(year_all_dalecarlia_table01_cleaned)
dim(year_all_dalecarlia_table01_cleaned)
# Merge itself
merged_dalecarlia <- merge(x = year_all_dalecarlia_table01_cleaned, y = year_all_dalecarlia_table02_cleaned, by = "YEAR_MONTH", all = TRUE, sort = FALSE)
dim(merged_dalecarlia)
names(merged_dalecarlia)
head(merged_dalecarlia)
tail(merged_dalecarlia)

# Fixing phosphate-orthoposphate
which_phosphate_not_na <- which( !is.na(merged_dalecarlia$PHOSPHATE) )
which_orthophosphate_not_na <- which( !is.na(merged_dalecarlia$ORTHOPHOSPHATE) )
intersect(which_phosphate_not_na, which_orthophosphate_not_na)
# Filling
merged_dalecarlia$ORTHOPHOSPHATE[which_phosphate_not_na] <- merged_dalecarlia$PHOSPHATE[which_phosphate_not_na]
# Dropping original columns
drop_phosphate_orthophosphate <- c("PHOSPHATE")
merged_dalecarlia = merged_dalecarlia[,!(names(merged_dalecarlia) %in% drop_phosphate_orthophosphate)]
names(merged_dalecarlia)

# Fixing chlorine total-chlorine
which_chlorine_not_na <- which( !is.na(merged_dalecarlia$CHLORINE) )
which_total_chlorine_not_na <- which( !is.na(merged_dalecarlia$TOTAL.CHLORINE) )
intersect(which_chlorine_not_na, which_total_chlorine_not_na)
# Filling
merged_dalecarlia$CHLORINE[which_total_chlorine_not_na] <- merged_dalecarlia$TOTAL.CHLORINE[which_total_chlorine_not_na]
# Dropping original columns
drop_chlorine_total_chlorine <- c("TOTAL.CHLORINE")
merged_dalecarlia = merged_dalecarlia[,!(names(merged_dalecarlia) %in% drop_chlorine_total_chlorine)]
names(merged_dalecarlia)

# Saving
merged_dalecarlia_path <- paste0("R_Data/merged_dalecarlia.RData")
save( merged_dalecarlia, file = merged_dalecarlia_path )



# Merging mcmillan characteristics
names(year_all_mcmillan_table01_cleaned)
names(year_all_mcmillan_table02_cleaned)
dim(year_all_mcmillan_table01_cleaned)
dim(year_all_mcmillan_table01_cleaned)
# Merge itself
merged_mcmillan <- merge(x = year_all_mcmillan_table01_cleaned, y = year_all_mcmillan_table02_cleaned, by = "YEAR_MONTH", all = TRUE, sort = FALSE)
dim(merged_mcmillan)
names(merged_mcmillan)
head(merged_mcmillan)
tail(merged_mcmillan)

# Fixing phosphate-orthoposphate
which_phosphate_not_na <- which( !is.na(merged_mcmillan$PHOSPHATE) )
which_orthophosphate_not_na <- which( !is.na(merged_mcmillan$ORTHOPHOSPHATE) )
intersect(which_phosphate_not_na, which_orthophosphate_not_na)
# Filling
merged_mcmillan$ORTHOPHOSPHATE[which_phosphate_not_na] <- merged_mcmillan$PHOSPHATE[which_phosphate_not_na]
# Dropping original columns
drop_phosphate_orthophosphate <- c("PHOSPHATE")
merged_mcmillan = merged_mcmillan[,!(names(merged_mcmillan) %in% drop_phosphate_orthophosphate)]
names(merged_mcmillan)

# Fixing chlorine total-chlorine
which_chlorine_not_na <- which( !is.na(merged_mcmillan$CHLORINE) )
which_total_chlorine_not_na <- which( !is.na(merged_mcmillan$TOTAL.CHLORINE) )
intersect(which_chlorine_not_na, which_total_chlorine_not_na)
# Filling
merged_mcmillan$CHLORINE[which_total_chlorine_not_na] <- merged_mcmillan$TOTAL.CHLORINE[which_total_chlorine_not_na]
# Dropping original columns
drop_chlorine_total_chlorine <- c("TOTAL.CHLORINE")
merged_mcmillan = merged_mcmillan[,!(names(merged_mcmillan) %in% drop_chlorine_total_chlorine)]
names(merged_mcmillan)



# Fix 2021.11.05.
# Fixing dates
merged_mcmillan$YEAR_MONTH[ which(merged_mcmillan$YEAR_MONTH == "2007-Nov**") ] <- "2007-Nov"

# Fix 2021.11.11.
# Fixing dates
merged_mcmillan$TOTAL.COLIFORM....positive.[ which(merged_mcmillan$TOTAL.COLIFORM....positive. == "0.8**") ] <- "0.8"


# Saving
merged_mcmillan_path <- paste0("R_Data/merged_mcmillan.RData")
save( merged_mcmillan, file = merged_mcmillan_path )


