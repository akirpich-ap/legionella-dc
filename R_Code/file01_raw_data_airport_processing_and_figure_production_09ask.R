# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2022.10.07. ask
rm(list=ls(all=TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation
# options(scipen=20)


# Remove duplicate rows of the data frame
library(dplyr)

# To convert dates from text to dates
library(lubridate)



# Setting the correct working directory.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Weppelman Legionella"

# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()


# Creating a list of years to work with
years_list <- c(2001:2020) 


# Working with weather temperatures first i.e. ref 18.
# ---------------------------------------------------------------

# Looping over years and saving the corresponding objects

for( year in years_list )
{
  # Debugging step
  # year <- 2001 
  
  # Getting current date
  current_year_path <- paste0( "Data_new/Ref18_data/", year, ".csv" )
  
  # Reading data
  current_year <- read.table( file = current_year_path, sep =",", header = TRUE )
  
  
  # Fixing data 
  current_year$DATE <- as.Date(current_year$DATE)
  
  # Saving with its own ending
  eval( parse( text=paste( "year_weather_", year, " <- current_year", sep ="") )  )  
  
  # Printing check on the screen
  cat("current_year = year_weather_", year, " -> ", sep ="", eval( parse( text= paste( "sum(!current_year == year_weather_", year, ")", sep ="") )  )  , "\n")
  cat("dim(current_year)[1] -> ", dim(current_year)[1], "dim(current_year)[2] -> ", dim(current_year)[2], "\n")

  # Saving RData file
  current_year_rdata_path <- paste0( "R_Data/year_weather_", year, ".RData" )
  eval( parse( text=paste( "save( year_weather_", year, ", file = current_year_rdata_path )", sep ="") )  )  
    

  # Combining the results
  
  if (year == 2001)
  {
    year_weather_all_daily <- current_year
  }

  if (year >= 2002)
  {
    year_weather_all_daily <- rbind(year_weather_all_daily, current_year)
  }

# End of -> for( year in c(2001:2021) )
}

# Weather abbreviations
# Some have to be converted to IS system.
# https://www.ncei.noaa.gov/data/global-summary-of-the-day/doc/readme.txt
# https://www.fltplan.com/abbreviations.htm



# Fix 2022.02.18
# Fixing mixing values.
names(year_weather_all_daily)
summary(year_weather_all_daily$DATE)
summary(year_weather_all_daily$TEMP)
summary(year_weather_all_daily$DEWP)
summary(year_weather_all_daily$SLP)
summary(year_weather_all_daily$WDSP)
summary(year_weather_all_daily$PRCP)

# Fix 2022.10.07
# Extra checks for missing values
sum(year_weather_all_daily$TEMP == 9999.9)
sum(year_weather_all_daily$DEWP == 9999.9)
sum(year_weather_all_daily$SLP  == 9999.9)
sum(year_weather_all_daily$WDSP == 999.9)
sum(year_weather_all_daily$PRCP == 99.99)



# Fixing precipitation missing values
year_weather_all_daily$PRCP[year_weather_all_daily$PRCP == 99.99] <- NA


# Saving the combined results 
year_weather_all_daily_path <- "R_Data/year_weather_all_daily.RData"
save( year_weather_all_daily, file = year_weather_all_daily_path  )
dim(year_weather_all_daily)

# Saving the columns we need only
names(year_weather_all_daily)
year_weather_all_daily_subset <- year_weather_all_daily[, c("DATE","TEMP","DEWP","SLP","WDSP","PRCP")]
dim(year_weather_all_daily_subset)

# Creating Year-month only
year_weather_all_daily_subset$DATE_ym <- substr( x = as.character(year_weather_all_daily_subset$DATE), start = 1, stop = 7 )
# Extracting unique list of ym
unique_list_ym <- unique(year_weather_all_daily_subset$DATE_ym)


# Computing the average temperatures and total precipitations monthly

for( i in c(1:length(unique_list_ym)) )
{
  # Debugging step
  # i <- 1
  which_indexes <-  which( year_weather_all_daily_subset$DATE_ym == unique_list_ym[i]  )

  # Current sub-dataset
  year_weather_all_daily_subset_current_month <- year_weather_all_daily_subset[which_indexes,]
  
  # Saving the results 
  if (i == 1) 
  {
    year_weather_all_monthly <- data.frame( DATE = unique_list_ym[i], 
                                            TEMP = mean(year_weather_all_daily_subset_current_month$TEMP),
                                            DEWP = mean(year_weather_all_daily_subset_current_month$DEWP),
                                            SLP  = mean(year_weather_all_daily_subset_current_month$SLP),
                                            WDSP = mean(year_weather_all_daily_subset_current_month$WDSP),
                                            PRCP = sum(na.omit(year_weather_all_daily_subset_current_month$PRCP)) )
  }  
  
  if (i >= 2) 
  {
    year_weather_all_monthly_subset_current_record  <- data.frame( DATE = unique_list_ym[i], 
                                                                   TEMP = mean(year_weather_all_daily_subset_current_month$TEMP),
                                                                   DEWP = mean(year_weather_all_daily_subset_current_month$DEWP),
                                                                   SLP  = mean(year_weather_all_daily_subset_current_month$SLP),
                                                                   WDSP = mean(year_weather_all_daily_subset_current_month$WDSP),                                                                   
                                                                   PRCP = sum(na.omit(year_weather_all_daily_subset_current_month$PRCP)) )
    
    year_weather_all_monthly <- rbind( year_weather_all_monthly,
                                       year_weather_all_monthly_subset_current_record )
    
  }  


# End of -> for( i in c(1:length(unique_list_ym)) )  
}  


# Fix 2022.10.07
# Adding the international unit standards
year_weather_all_monthly$TEMP_C  <-  5 * (year_weather_all_monthly$TEMP - 32)/9
year_weather_all_monthly$DEWP_C  <-  5 * (year_weather_all_monthly$DEWP - 32)/9
year_weather_all_monthly$WDSP_MS <-  year_weather_all_monthly$WDSP * 0.514444
year_weather_all_monthly$PRCP_CM <-  year_weather_all_monthly$PRCP * 2.54       


# Saving the combined results 
year_weather_all_monthly_path <- "R_Data/year_weather_all_monthly.RData"
save( year_weather_all_monthly, file = year_weather_all_monthly_path  )
dim(year_weather_all_monthly)
head(year_weather_all_monthly)
tail(year_weather_all_monthly)

# Generating list of variables
list_of_variables <- names(year_weather_all_monthly)[ -which(names(year_weather_all_monthly) == "DATE") ]
length_list_of_variables <- length(list_of_variables)


# Generating pdf output.
pdf( paste( "Plots/file01_weather_pairwise_plots.pdf", sep = ""), height = length_list_of_variables*4, width = length_list_of_variables*4 )


par( mfrow=c(length_list_of_variables, length_list_of_variables), oma=c(1, 1, 2, 1)  )

# looping over the compartments.
for (  current_parameter1 in list_of_variables  )
{
  
  # looping over the compartments.
  for (  current_parameter2 in list_of_variables  )
  {
    
    # Debugging step
    # current_parameter1 <- list_of_variables[1]
    # current_parameter2 <- list_of_variables[2]
    
    # Saving values
    eval( parse( text = paste("x_values <- year_weather_all_monthly$", current_parameter1, sep ="") )  )  
    eval( parse( text = paste("y_values <- year_weather_all_monthly$", current_parameter2, sep ="") )  )  
    
    # Standardizing
    x_values <- ( x_values - min(x_values) ) / max( x_values - min(x_values) ) 
    y_values <- ( y_values - min(y_values) ) / max( y_values - min(y_values) ) 
    
    # Correlation rho_current b/w x_values and y_values
    rho_current <- cor( x_values, y_values )
    
    
    # Plotting x_values vs y_values
    plot(x = x_values,
         y = y_values,
         col = "darkblue", 
         lwd = 3,
         pch = 16,
         # type = "l",
         main = paste( current_parameter1, "  vs  ", current_parameter2, "\n rho = ", round(rho_current, 2),  sep = ""),
         # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
         # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
         xlim = c( - 0.1, 1.1 ),
         ylim = c( - 0.1, 1.1 ),
         xlab = current_parameter1,
         ylab = current_parameter2,
         # xaxt='n',
         cex = 1.55,
         cex.axis = 1.55,
         cex.lab = 1.55,
         cex.main = 1.55,
         cex.sub = 1.55)
    
    abline(a = 0, b = 1, col = "black", lwd = 2, lty = 3)

    # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
    # Creating labels by month and converting.
    # tlab <- c( 1:10 )/10
    # lablist <- as.character( tlab ) 
    # axis(1, at = tlab, labels = FALSE)
    # text(x = tlab, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), labels=lablist, srt=45, adj=1, xpd=TRUE)
    
    # End of -> for (  current_parameter1 in c(1:length(no_parameters_log))  )  
  }  
  
}  
# Adding the overall title
title( main = list(paste( "Pairwise Correlations", sep = "" ), cex = 1.5), outer=TRUE)

dev.off()




# Working with incidence data now i.e. ref 18.
# ---------------------------------------------------------------



# Creating list of weeks 
weeks_list <- c( paste0("0", c(1:9)), paste0(c(10:52))  )


year_table_columns <- data.frame( matrix( 0, nrow  = length(years_list), ncol =  6  ) )
names(year_table_columns) <- c("Year", "Table","SkipRow", "RowDC", "Column1", "Column2" )
year_table_columns$Year <- years_list
year_table_columns$Table <- as.character(year_table_columns$Table)



# Filling "Table", "Column1", "Column2" by hand using info from
# https://wonder.cdc.gov/nndss/nndss_weekly_tables_menu.asp

# 2001
year_table_columns$Table[1]    <- "2B"
year_table_columns$SkipRow[1]  <- 17
year_table_columns$RowDC[1]    <- 31
year_table_columns$Column1[1]  <- 6
year_table_columns$Column2[1]  <- 7

# 2002
year_table_columns$Table[2]   <- "2D"
year_table_columns$SkipRow[2]  <- 17
year_table_columns$RowDC[2]   <- 31
year_table_columns$Column1[2] <- 2
year_table_columns$Column2[2] <- 3


# 2003
year_table_columns$Table[3]   <- "2D"
year_table_columns$SkipRow[3]  <- 17
year_table_columns$RowDC[3]   <- 31
year_table_columns$Column1[3] <- 6
year_table_columns$Column2[3] <- 7


# 2004
year_table_columns$Table[4]   <- "2D"
year_table_columns$SkipRow[4]  <- 17
year_table_columns$RowDC[4]   <- 31
year_table_columns$Column1[4] <- 6
year_table_columns$Column2[4] <- 7


# 2005
year_table_columns$Table[5]   <- "2E"
year_table_columns$SkipRow[5]  <- 15
year_table_columns$RowDC[5]   <- 31
year_table_columns$Column1[5] <- 2
year_table_columns$Column2[5] <- 3


# 2006
year_table_columns$Table[6]   <- "2C"
year_table_columns$SkipRow[6]  <- 22
year_table_columns$RowDC[6]   <- 30
year_table_columns$Column1[6] <- 15
year_table_columns$Column2[6] <- 16


# 2007
year_table_columns$Table[7]   <- "2C"
year_table_columns$SkipRow[7]  <- 22
year_table_columns$RowDC[7]   <- 30
year_table_columns$Column1[7] <- 15
year_table_columns$Column2[7] <- 16


# 2008
year_table_columns$Table[8]   <- "2C"
year_table_columns$SkipRow[8]  <- 22
year_table_columns$RowDC[8]   <- 30
year_table_columns$Column1[8] <- 15
year_table_columns$Column2[8] <- 16


# 2009
year_table_columns$Table[9]   <- "2C"
year_table_columns$SkipRow[9]  <- 22
year_table_columns$RowDC[9]   <- 30
year_table_columns$Column1[9] <- 15
year_table_columns$Column2[9] <- 16


# 2010
year_table_columns$Table[10]   <- "2F"
year_table_columns$SkipRow[10]  <- 22
year_table_columns$RowDC[10]   <- 30
year_table_columns$Column1[10] <- 15
year_table_columns$Column2[10] <- 16


# 2011
year_table_columns$Table[11]   <- "2F"
year_table_columns$SkipRow[11]  <- 22
year_table_columns$RowDC[11]   <- 30
year_table_columns$Column1[11] <- 5
year_table_columns$Column2[11] <- 6


# 2012
year_table_columns$Table[12]   <- "2F"
year_table_columns$SkipRow[12]  <- 22
year_table_columns$RowDC[12]   <- 30
year_table_columns$Column1[12] <- 5
year_table_columns$Column2[12] <- 6


# 2013
year_table_columns$Table[13]   <- "2F"
year_table_columns$SkipRow[13]  <- 22
year_table_columns$RowDC[13]   <- 30
year_table_columns$Column1[13] <- 5
year_table_columns$Column2[13] <- 6


# 2014
year_table_columns$Table[14]   <- "2F"
year_table_columns$SkipRow[14]  <- 22
year_table_columns$RowDC[14]   <- 30
year_table_columns$Column1[14] <- 15
year_table_columns$Column2[14] <- 16


# 2015
year_table_columns$Table[15]   <- "2G"
year_table_columns$SkipRow[15]  <- 22
year_table_columns$RowDC[15]   <- 30
year_table_columns$Column1[15] <- 15
year_table_columns$Column2[15] <- 16


# 2016
year_table_columns$Table[16]   <- "2G"
year_table_columns$SkipRow[16]  <- 22
year_table_columns$RowDC[16]   <- 30
year_table_columns$Column1[16] <- 15
year_table_columns$Column2[16] <- 16


# 2017
year_table_columns$Table[17]   <- "2J"
year_table_columns$SkipRow[17]  <- 15
year_table_columns$RowDC[17]   <- 30
year_table_columns$Column1[17] <- 5
year_table_columns$Column2[17] <- 6


# 2018
year_table_columns$Table[18]   <- "2L"
year_table_columns$SkipRow[18]  <- 15
year_table_columns$RowDC[18]   <- 30
year_table_columns$Column1[18] <- 5
year_table_columns$Column2[18] <- 6


# 2019
year_table_columns$Table[19]   <- "1u"
year_table_columns$SkipRow[19]  <- 17
year_table_columns$RowDC[19]   <- 30
year_table_columns$Column1[19] <- 4
year_table_columns$Column2[19] <- 5


# 2020
year_table_columns$Table[20]   <- "1t"
year_table_columns$SkipRow[20]  <- 18
year_table_columns$RowDC[20]   <- 30
year_table_columns$Column1[20] <- 12
year_table_columns$Column2[20] <- 13






# year_current_index <- 1
# year_current_index <- 2
# year_current_index <- 3
# year_current_index <- 4
# year_current_index <- 5
# year_current_index <- 6
# year_current_index <- 7
# year_current_index <- 7
# year_current_index <- 8
# year_current_index <- 9
# year_current_index <- 10
# year_current_index <- 11
# year_current_index <- 12
# year_current_index <- 13
# year_current_index <- 14
# year_current_index <- 15
# year_current_index <- 16
# year_current_index <- 17
# year_current_index <- 18
# year_current_index <- 19
# year_current_index <- 20


years_list_1 <- c(2001:2016)
years_list_2 <- c(2017:2020)



for( year_current_index in c(1:20) )
{
  # Debugging step
  # year_current_index <- 1

  year_current    <- year_table_columns$Year[year_current_index] 
  table_current   <- year_table_columns$Table[year_current_index]
  skiprow_current <- year_table_columns$SkipRow[year_current_index]
  rowdc_current   <- year_table_columns$RowDC[year_current_index]
  column1_current <- year_table_columns$Column1[year_current_index] 
  column2_current <- year_table_columns$Column2[year_current_index] 

  print(year_table_columns)
    
  cat("year_current    ", year_current, "\n")     
  cat("table_current   ", table_current, "\n")
  cat("skiprow_current ", skiprow_current, "\n")
  cat("rowdc_current   ", rowdc_current, "\n")
  cat("column1_current ", column1_current, "\n")
  cat("column2_current ", column2_current, "\n")
  

  # Looping over weeks for the current year
  for (w in c(1:52))
  # for (w in c(1,26:28))  
  {
    # Debugging step
    # w <- 1
    # w <- 26
    cat("w ->", w, "\n")
    
    # Reading data (after 2017 URL-s are different)
    
    if ( year_current %in% years_list_1 )
    {
      # Current week and year URL
      url_current <- paste0("https://wonder.cdc.gov/nndss/nndss_weekly_tables_1995_2014.asp?request=Export&mmwr_location=&mmwr_table=", table_current,"&mmwr_year=", year_current, "&mmwr_week=", weeks_list[w] )
      
      # Raw file current 
      file_raw_current <- readLines(url_current)
      file_raw_current_line2 <- file_raw_current[2]
      file_raw_current_line2_extract  <- substr(x = file_raw_current_line2, start = 87, stop = 128)
      
    }  
    
    if ( year_current %in% years_list_2 )
    {
      # Current week and year URL
      url_current <- paste0("https://wonder.cdc.gov/nndss/static/", year_current,"/",weeks_list[w],"/", year_current,"-",weeks_list[w],"-table", table_current,".txt" )
      
      # Raw file current 
      file_raw_current <- readLines(url_current)
      
      # For year 2019 removing the first 2 rows for weeks 42-46
      if ( (year_current == 2019) && (w %in% c(42:46)) )
      {
        # Dropping the first two lines message
        file_raw_current <- file_raw_current[-c(1,2)]
      }      

      # For year 2020 removing the first 2 rows for week 32
      if ( (year_current == 2020) && (w %in% c(32)) )
      {
        # Dropping the first two lines message
        file_raw_current <- file_raw_current[-c(1,2)]
      }      
      
            
      file_raw_current_line2 <- file_raw_current[1]
      # file_raw_current_line2_extract  <- substr(x = file_raw_current_line2, start = 100, stop = 220)
      file_raw_current_line2_extract  <- substr(x = file_raw_current_line2, start = nchar(file_raw_current_line2)-40, stop =  nchar(file_raw_current_line2) )
      
    }  
    
    cat("file_raw_current_line2 ->", file_raw_current_line2, "\n")
    cat("file_raw_current_line2_extract ->", file_raw_current_line2_extract, "\n")
    
    # File table current
    # For year 2019 removing the first 2 rows for weeks 42-46
    if ( (year_current != 2019) )
    {
      # Dropping the first two lines message
      # Remove duplicate rows of the dataframe -> important for year 2006 and week 48 where rows are duplicated
      file_table_current <- read.table( url(url_current), skip = skiprow_current, sep = "\t", fill = TRUE ) 

    }      

    # File table current
    # Remove duplicate rows of the dataframe -> important for year 2006 and week 48 where rows are duplicated
    if ( (w == 48) && (year_current == 2006) )
    {
      # Dropping the first two lines message PLUS uning function distinct
      file_table_current <- distinct( read.table( url(url_current), skip = skiprow_current, sep = "\t", fill = TRUE ) )
      
    }      
    
    # For year 2019 removing the first 2 rows for weeks 42-46
    if ( (year_current == 2019) && (w %in% c(42:46)) )
    {
      # Dropping the first two lines message + accounting for weeks 42-46
      file_table_current <- read.table( url(url_current), skip = (skiprow_current+2), sep = "\t", fill = TRUE )
    }      

    
    # For year 2020 removing the first 2 rows for week 32
    if ( (year_current == 2020) && (w %in% c(32)) )
    {
      # Dropping the first two lines message + accounting for weeks 42-46
      file_table_current <- read.table( url(url_current), skip = (skiprow_current+2), sep = "\t", fill = TRUE )
    }      
    
        
    
    file_table_current_row_dc <- c( unlist( file_table_current[ rowdc_current, c(1,column1_current,column2_current) ] ), file_raw_current_line2_extract )  
    cat("file_table_current_row_dc ->", file_table_current_row_dc, "\n")
    
    # Debugging step
    file_table_current_row_first <- c( unlist( file_table_current[ 1, c(1,column1_current,column2_current) ] ), file_raw_current_line2_extract )  
    cat("file_table_current_row_first ->", file_table_current_row_first, "\n")
    
    
    # Saving the results
    
    # Raw file
    file_raw_current_path <- paste0("R_Data/file_raw_", year_current,"_week_", weeks_list[w], ".RData")
    eval( parse( text = paste( "file_raw_", year_current,"_week_", weeks_list[w], " <- file_raw_current", sep ="") )  ) 
    eval( parse( text = paste( "save( file_raw_", year_current,"_week_", weeks_list[w], ", file = file_raw_current_path )", sep ="") )  ) 
    
    # File table raw
    file_table_current_path <- paste0("R_Data/file_table_", year_current,"_week_", weeks_list[w], ".RData")
    eval( parse( text = paste( "file_table_", year_current,"_week_", weeks_list[w], " <- file_table_current", sep ="") )  ) 
    eval( parse( text = paste( "save( file_table_", year_current,"_week_", weeks_list[w], ", file = file_table_current_path )", sep ="") )  ) 
    
    
    if( w == 1)
    {
      file_table_current_year_dc <- data.frame(t(file_table_current_row_dc))
      names(file_table_current_year_dc) <- c("Region", "Value1", "Value2", "Date_text")
    }  
    
    
    if( w >= 2)
    {
      file_table_current_year_dc <- rbind(file_table_current_year_dc,
                                          file_table_current_row_dc ) 
      
    }  
    
    
  # End of -> for (w in c(1:52))
  }  
  
  
  # Adding year column and creating a new dataset
  file_table_current_year_dc_with_year <- data.frame (Year = rep(year_current, dim(file_table_current_year_dc)[1] ),  file_table_current_year_dc )

  
  # Saving annual results for the current year
  file_table_current_year_dc_path <- paste0("R_Data/weekly_cummulative_dc_", year_current,".RData")
  eval( parse( text = paste( "weekly_cummulative_dc_", year_current," <- file_table_current_year_dc", sep ="") )  ) 
  eval( parse( text = paste( "save( weekly_cummulative_dc_", year_current,", file = file_table_current_year_dc_path )", sep ="") )  ) 
  
  
  # Combining the results across years
  
  if( year_current_index == 1)
  {
    weekly_cummulative_dc_all_years_raw <- file_table_current_year_dc_with_year 
  }  

  if( year_current_index >= 2)
  {
    weekly_cummulative_dc_all_years_raw <- rbind(weekly_cummulative_dc_all_years_raw,
                                                 file_table_current_year_dc_with_year )
  }  

  
  
# End of -> for( year_current in years_list)   
}  

# Saving all years raw
weekly_cummulative_dc_all_years_raw_path <- paste0("R_Data/weekly_cummulative_dc_all_years_raw.RData")
save( weekly_cummulative_dc_all_years_raw, file = weekly_cummulative_dc_all_years_raw_path )
# Debugging
# load(file = weekly_cummulative_dc_all_years_raw_path )


# Making a copy with different name 
weekly_cummulative_dc_all_years_processed <- weekly_cummulative_dc_all_years_raw
dim(weekly_cummulative_dc_all_years_processed)


# Getting number of records
no_of_records <- dim(weekly_cummulative_dc_all_years_processed)[1]
# Empty arrays of starts to fill
start_positions <- rep(1, no_of_records)
# Empty arrays of ends to fill
end_positions <- rep(100, no_of_records)

# Getting a list of month
month_list <- month.name


for (month_current in month_list)
{
  # Debugging step
  # month_current  <- month_list[2]

  # Filling starting positions
  current_month_positions <- regexpr( month_current, weekly_cummulative_dc_all_years_processed$Date_text)
  which_current_month_positions <-  which(current_month_positions>0)
  start_positions[which_current_month_positions] <- current_month_positions[which_current_month_positions]

}  

# Saving the intermediate truncation 
weekly_cummulative_dc_all_years_processed$Date_text_week <- substr(x = weekly_cummulative_dc_all_years_processed$Date_text, start = start_positions, stop = 100)



# Filling ending positions
caps_indexes <- which( regexpr( "WEEK", weekly_cummulative_dc_all_years_processed$Date_text_week) > 0  )
end_positions[caps_indexes] <- regexpr( "WEEK", weekly_cummulative_dc_all_years_processed$Date_text_week)[caps_indexes]

low_indexes <- which( regexpr( "week", weekly_cummulative_dc_all_years_processed$Date_text_week) > 0  )
end_positions[low_indexes] <- regexpr( "week", weekly_cummulative_dc_all_years_processed$Date_text_week)[low_indexes]

prop_indexes <- which( regexpr( "Week", weekly_cummulative_dc_all_years_processed$Date_text_week) > 0  )
end_positions[prop_indexes] <- regexpr( "Week", weekly_cummulative_dc_all_years_processed$Date_text_week)[prop_indexes]

# Saving the intermediate truncation 
weekly_cummulative_dc_all_years_processed$Date_text_final <- substr(x = weekly_cummulative_dc_all_years_processed$Date_text_week, start = 1, stop = (end_positions-3) ) 


# Converting to proper dates
weekly_cummulative_dc_all_years_processed$Date <- lubridate::mdy(weekly_cummulative_dc_all_years_processed$Date_text_final)

# Extraction Month and Year only as text
weekly_cummulative_dc_all_years_processed$Year_Month <- substr( as.character(weekly_cummulative_dc_all_years_processed$Date) , start= 1, stop = 7)


# Processing value 1
value1_which_empty <- which( weekly_cummulative_dc_all_years_processed$Value1 %in% c("-","N")  )
value1_which_empty_alternative <- which( is.na( as.numeric(weekly_cummulative_dc_all_years_processed$Value1) )  )
# Extra check 
sum( !value1_which_empty == value1_which_empty_alternative )

weekly_cummulative_dc_all_years_processed$Value1_filled <- weekly_cummulative_dc_all_years_processed$Value1

weekly_cummulative_dc_all_years_processed$Value1_filled[value1_which_empty] <- "0"
weekly_cummulative_dc_all_years_processed$Value1_filled_num <- as.numeric(weekly_cummulative_dc_all_years_processed$Value1_filled)


# Dropping unused columns
list_to_remove <- c("Date_text") 
weekly_cummulative_dc_all_years_processed <- weekly_cummulative_dc_all_years_processed[, -which(names(weekly_cummulative_dc_all_years_processed) %in% list_to_remove)]


# Value to fill
# Putting placeholder -1984 for now.
weekly_cummulative_dc_all_years_processed$Counts1 <- rep(-1984, no_of_records)


# Fixing values by year
for( year_current_index in c(1:20) )
{
  # Debugging step
  # year_current_index <- 1
  # year_current_index <- 8  
  
  # Extracting current year
  current_year_indexes <- which( weekly_cummulative_dc_all_years_processed$Year == years_list[year_current_index]  )
  current_year_frame <- weekly_cummulative_dc_all_years_processed[current_year_indexes,]
  # print(current_year_frame)
  
  # Debugging step
  # Printing the current year values.
  cat("Year -> ", years_list[year_current_index], current_year_frame$Value1_filled_num, "\n")
  cat("Year -> !is.unsorted() ", !is.unsorted(current_year_frame$Value1_filled_num), "\n\n")
  
  # Value1_shifted
  current_year_frame$Value1_shifted <- c(0, current_year_frame$Value1_filled_num[-length(current_year_frame$Value1_filled_num)] )
  
  
  
  # Adding results to the original frame
  weekly_cummulative_dc_all_years_processed$Counts1[current_year_indexes] <- current_year_frame$Value1_filled_num - current_year_frame$Value1_shifted
  
  
# End of -> for( year_current_index in c(1:20) )
}  


# Fix 2021.10.27
# Debugging step: nothing should be negative
# Indexes
discrepancy_indexes <- which( weekly_cummulative_dc_all_years_processed$Counts1<0 )
# Corresponding years
discrepancy_years <- weekly_cummulative_dc_all_years_processed$Year[discrepancy_indexes]

# Combining into a matrix to feed easier
discrepancy_indexes_years_all <- which( weekly_cummulative_dc_all_years_processed$Year %in% discrepancy_years )
# Number of years with discrepancy
no_of_years_with_discrepancy <- length(discrepancy_years) 
# Split of indexes
split_of_indexes_bw_years <- split(discrepancy_indexes_years_all, sort(discrepancy_indexes_years_all%%no_of_years_with_discrepancy))

# Summaries
weekly_cummulative_dc_all_years_processed[unlist(split_of_indexes_bw_years[1]),]
weekly_cummulative_dc_all_years_processed[unlist(split_of_indexes_bw_years[2]),]  
weekly_cummulative_dc_all_years_processed[unlist(split_of_indexes_bw_years[3]),]


# Fixing discrepancy by overwriting manually
weekly_cummulative_dc_all_years_processed$Counts1_fixed <- weekly_cummulative_dc_all_years_processed$Counts1

# Year 1
weekly_cummulative_dc_all_years_processed$Counts1_fixed[380:386] <- 0
weekly_cummulative_dc_all_years_processed$Counts1_fixed[596:623] <- 0
# More confident about the ones below since I checked cummulative
weekly_cummulative_dc_all_years_processed$Counts1_fixed[1002:1005] <- 0


# Summaries
weekly_cummulative_dc_all_years_processed[unlist(split_of_indexes_bw_years[1]),]
weekly_cummulative_dc_all_years_processed[unlist(split_of_indexes_bw_years[2]),]  
weekly_cummulative_dc_all_years_processed[unlist(split_of_indexes_bw_years[3]),]


# Saving all years raw
weekly_cummulative_dc_all_years_processed_path <- paste0("R_Data/weekly_cummulative_dc_all_years_processed.RData")
save( weekly_cummulative_dc_all_years_processed, file = weekly_cummulative_dc_all_years_processed_path )



# Fix 2021.10.25.
# Getting the same data monthly

# Unique list of month and the corresponding length
unique_year_month_list <- unique(weekly_cummulative_dc_all_years_processed$Year_Month)
list_length <- length(unique_year_month_list)

# Creating data frame to save the results.
monthly_incidence_dc_all_years_processed <- data.frame( Year = rep("XXXX", list_length ),  Year_Month =  unique_year_month_list, Counts = rep(-1984, list_length ) )


# Looping and saving the values
for( current_year_month in c(1:list_length) )
{
  # Debugging step
  # current_year_month <- 1 
  
  # Extracting subframe
  which_current_current_year_month <- which( weekly_cummulative_dc_all_years_processed$Year_Month == unique_year_month_list[current_year_month] )
  current_subframe <- weekly_cummulative_dc_all_years_processed[which_current_current_year_month, ]
  
  # Debugging
  print(current_subframe)
  print(sum(current_subframe$Counts1_fixed))
  
  # Filling values
  monthly_incidence_dc_all_years_processed$Year[current_year_month]   <- current_subframe$Year[1]
  monthly_incidence_dc_all_years_processed$Counts[current_year_month] <- sum(current_subframe$Counts1_fixed)

# End of -> for( current_year_month in c(1:list_length) )  
}  

summary(monthly_incidence_dc_all_years_processed$Counts)

  
# Saving all years raw
monthly_incidence_dc_all_years_processed_path <- paste0("R_Data/monthly_incidence_dc_all_years_processed.RData")
save( monthly_incidence_dc_all_years_processed, file = monthly_incidence_dc_all_years_processed_path )

