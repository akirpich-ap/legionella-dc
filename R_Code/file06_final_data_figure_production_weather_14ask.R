# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2022.11.15. ask
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


# Lists to subset
variables_list_inc_temp_perc <- c("Year_Month_fixed", "Counts", "TEMP_C", "SLP", "WDSP_MS", "PRCP_CM" )
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



# Fix 2022.01.15
# Computing data
# Short month names
month.name_short <- substr(x= month.name, start = 1, stop = 3)

data_monthly_annually <- data.frame(  Year = merged_river_plus_incidence$year[merged_river_plus_incidence$month == month.name_short[1]],
                                      Jan  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[1]],
                                      Feb  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[2]],
                                      Mar  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[3]],
                                      Apr  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[4]],
                                      May  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[5]],
                                      Jun  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[6]],
                                      Jul  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[7]],
                                      Aug  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[8]],
                                      Sep  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[9]],
                                      Oct  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[10]],
                                      Nov  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[11]],
                                      Dec  = merged_river_plus_incidence$Counts[merged_river_plus_incidence$month == month.name_short[12]] )
# Row names
rownames(data_monthly_annually) <- data_monthly_annually$Year
# Summaries
colSums(x = data_monthly_annually[,-1])
rowSums(x = data_monthly_annually[,-1])


# 2022.10.21
# Total sum of cases
sum( colSums(x = data_monthly_annually[,-1]) )
sum( rowSums(x = data_monthly_annually[,-1]) )
# Counting zero vs non-zero cells
sum( data_monthly_annually[,-1] == 0 )
sum( data_monthly_annually[,-1] > 0 )
# The total number of month
dim(data_monthly_annually[,-1])[1] * dim(data_monthly_annually[,-1])[2]
sum( data_monthly_annually[,-1] == 0 ) + sum( data_monthly_annually[,-1] > 0 )



# Cross table
# RData
data_monthly_annually_path <- paste0("R_Data/data_monthly_annually.RData")
save( data_monthly_annually, file = data_monthly_annually_path )
# CSV table for data
data_monthly_annually_csv_path <- paste0("R_Output/data_monthly_annually.csv")
write.table( data_monthly_annually, file = data_monthly_annually_csv_path, sep = ",", quote = TRUE, row.names = FALSE)




# Fix 2022.10.20.
# Computing summary statistics of the data

# Monthly
summaries_monthly <- data.frame(              aggregate( Counts ~ month, merged_river_plus_incidence ,     mean ),
                                 StDev      = aggregate( Counts ~ month, merged_river_plus_incidence ,       sd )[, -1],       
                                 Qunatile   = aggregate( Counts ~ month, merged_river_plus_incidence , quantile )[,-1],
                                 TotalMonth = aggregate( Counts ~ month, merged_river_plus_incidence ,      sum )[, -1])
# Fixing the names
names(summaries_monthly)[1] <- "Month"
names(summaries_monthly)[2] <- "Mean"
names(summaries_monthly)[3] <- "St.Dev."
names(summaries_monthly)[4] <- "Min (0%) "
names(summaries_monthly)[5] <- "Lw.Qrtl. (25%)"
names(summaries_monthly)[6] <- "Median (50%)"
names(summaries_monthly)[7] <- "Up.Qrtl. (75%)"
names(summaries_monthly)[8] <- "Max (100%)"
names(summaries_monthly)[9] <- "Total"

# Rounding counts
summaries_monthly$Mean       <- round( x = summaries_monthly$Mean, digits = 2 )
summaries_monthly$`St.Dev.` <- round( x = summaries_monthly$`St.Dev.`, digits = 2 )

# Creating factors
summaries_monthly$Month <- factor(x = summaries_monthly$Month, levels = substr(x= month.name, start = 1, stop = 3) )
# Sorting after accounting for factors
summaries_monthly <-  summaries_monthly[order(summaries_monthly$Month), ]
summaries_monthly$Month <- as.character(summaries_monthly$Month)

# Extra check
sum(!summaries_monthly$Total == colSums(x = data_monthly_annually[,-1]))





# Yearly
summaries_yearly <- data.frame(              aggregate( Counts ~ year, merged_river_plus_incidence ,     mean ),
                                StDev      = aggregate( Counts ~ year, merged_river_plus_incidence ,       sd )[, -1],       
                                Qunatile   = aggregate( Counts ~ year, merged_river_plus_incidence , quantile )[,-1],
                                TotalMonth = aggregate( Counts ~ year, merged_river_plus_incidence ,      sum )[, -1])
# Fixing the names
names(summaries_yearly)[1] <- "Month"
names(summaries_yearly)[2] <- "Mean"
names(summaries_yearly)[3] <- "St.Dev."
names(summaries_yearly)[4] <- "Min (0%) "
names(summaries_yearly)[5] <- "Lw.Qrtl. (25%)"
names(summaries_yearly)[6] <- "Median (50%)"
names(summaries_yearly)[7] <- "Up.Qrtl. (75%)"
names(summaries_yearly)[8] <- "Max (100%)"
names(summaries_yearly)[9] <- "Total"

# Rounding counts
summaries_yearly$Mean       <- round( x = summaries_yearly$Mean, digits = 2 )
summaries_yearly$`St.Dev.` <- round( x = summaries_yearly$`St.Dev.`, digits = 2 )

# Extra check
sum(!summaries_yearly$Total == rowSums(x = data_monthly_annually[,-1]))





# RData
# monthly
summaries_monthly_path <- paste0("R_Data/summaries_monthly.RData")
save( summaries_monthly, file = summaries_monthly_path )
# yearly
summaries_yearly_path <- paste0("R_Data/summaries_yearly.RData")
save( summaries_yearly, file = summaries_yearly_path )


# CSV table for data
# monthly
summaries_monthly_csv_path <- paste0("R_Output/summaries_monthly.csv")
write.table( summaries_monthly, file = summaries_monthly_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
# yearly
summaries_yearly_csv_path <- paste0("R_Output/summaries_yearly.csv")
write.table( summaries_yearly, file = summaries_yearly_csv_path, sep = ",", quote = TRUE, row.names = FALSE)















# Creating labeling function

label_function <- function(label_value = "A", label_cex = 4) {
  
  par(xpd = NA )
  
  di <- dev.size("in")
  x <- grconvertX(c(0, di[1]), from="in", to="user")
  y <- grconvertY(c(0, di[2]), from="in", to="user")
  
  fig <- par("fig")
  x <- x[1] + (x[2] - x[1]) * fig[1:2]
  y <- y[1] + (y[2] - y[1]) * fig[3:4]
  
  txt <- label_value
  x <- x[1] + strwidth(txt, cex=4) * 6 / 5
  y <- y[2] - strheight(txt, cex=4) * 4 / 5
  text(x, y, txt, cex = label_cex )
  
}









# Fix 2022.01.14.
# Generating pdf output.
pdf( paste( "Plots/file06_indidence_data_plots.pdf", sep = ""), height = 10, width = 10 )

# Defining the number of plots
# par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )
par( par(mfrow=c(1,2)) )
# Defining layout
layout(matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = TRUE))


# Panel A
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$Counts,
     col = "darkblue", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Incidence Over Time (Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)
    
#abline(a = 0, b = 1, col = "black", lwd = 2, lty = 3)
    
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label A
label_function(label_value = "A", label_cex = 4)




# Panel B
boxplot(x = data_monthly_annually[,-1], 
        col = "darkorange", 
        xaxt = "n",
        ylab = "Counts",
        main = "Incidence Box Plots (Monthly)")
axis(side = 1, las = 2, at = c(1:12), labels = colnames(data_monthly_annually[,-1]), cex = 1.5)
# legend("topleft", fill = rainbow(3, s = 0.5), legend = c(1,2,3), horiz = T)

# Label B
label_function(label_value = "B", label_cex = 4)




# Panel C
boxplot(x = t(data_monthly_annually[,-1]), 
        col = "darkorange", 
        xaxt = "n",
        ylab = "Counts",        
        main = "Incidence Box Plots (Annualy)")
axis(side = 1, las = 2, at = c(1:19), labels = colnames(t(data_monthly_annually[,-1])), cex = 1.5)
# legend("topleft", fill = rainbow(3, s = 0.5), legend = c(1,2,3), horiz = T)

# Label C
label_function(label_value = "C", label_cex = 4)

dev.off()


  
  
  
  
  
  
  
  
  






# Generating list of variables
list_of_variables_river <- c(variables_list_inc_temp_perc, variables_list_river)[ -which( c(variables_list_inc_temp_perc, variables_list_river) %in% c("Year_Month_fixed", "YEAR_MONTH") ) ]
length_list_of_variables_river <- length(list_of_variables_river)


# Generating pdf output.
pdf( paste( "Plots/file06_final_data_river_pairwise_plots.pdf", sep = ""), height = length_list_of_variables_river*5, width = length_list_of_variables_river*5 )


par( mfrow=c(length_list_of_variables_river, length_list_of_variables_river), oma=c(1, 1, 2, 1)  )

# looping over the compartments.
for (  current_parameter1 in list_of_variables_river  )
{
  
  # looping over the compartments.
  for (  current_parameter2 in list_of_variables_river  )
  {
    
    # Debugging step
    # current_parameter1 <- list_of_variables_river[1]
    # current_parameter2 <- list_of_variables_river[2]
    
    # Saving values
    eval( parse( text = paste("x_values <- merged_river_plus_incidence$", current_parameter1, sep ="") )  )  
    eval( parse( text = paste("y_values <- merged_river_plus_incidence$", current_parameter2, sep ="") )  )  
    
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








# Generating list of variables
list_of_variables_dalecarlia_mcmillan <- c(variables_list_inc_temp_perc, variables_list_both_plants)[ -which( c(variables_list_inc_temp_perc, variables_list_both_plants) %in% c("Year_Month_fixed", "YEAR_MONTH") ) ]
length_list_of_variables_dalecarlia_mcmillan <- length(list_of_variables_dalecarlia_mcmillan)



# Generating pdf output.
pdf( paste( "Plots/file06_final_data_dalecarlia_mcmillan_pairwise_plots.pdf", sep = ""), height = length_list_of_variables_dalecarlia_mcmillan*5, width = length_list_of_variables_dalecarlia_mcmillan*5 )


par( mfrow=c(length_list_of_variables_dalecarlia_mcmillan, length_list_of_variables_dalecarlia_mcmillan), oma=c(1, 1, 2, 1)  )

# looping over the compartments.
for (  current_parameter1 in list_of_variables_dalecarlia_mcmillan  )
{
  
  # looping over the compartments.
  for (  current_parameter2 in list_of_variables_dalecarlia_mcmillan  )
  {
    
    # Debugging step
    # current_parameter1 <- list_of_variables_dalecarlia_mcmillan[1]
    # current_parameter2 <- list_of_variables_dalecarlia_mcmillan[2]
    
    # Saving values
    eval( parse( text = paste("x_values <- merged_dalecarlia_mcmillan_plus_incidence$", current_parameter1, sep ="") )  )  
    eval( parse( text = paste("y_values <- merged_dalecarlia_mcmillan_plus_incidence$", current_parameter2, sep ="") )  )  
    
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














# Fix 2022.02.17.
# Generating pdf output.
pdf( paste( "Plots/file06_final_data_figure_production_weather_a.pdf", sep = ""), height = 20, width = 10 )

# Defining the number of plots
# par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(1,4)) )
# Defining layout
layout(matrix(c(1,1,2,2,3,3,4,5), nrow = 4, ncol = 2, byrow = TRUE))


# Panel A
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$Counts,
     col = "darkblue", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Incidence (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

#abline(a = 0, b = 1, col = "black", lwd = 2, lty = 3)

# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label A
label_function(label_value = "A", label_cex = 4)



# Panel B
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$TEMP_C,
     # col = "darkblue", 
     col = "dodgerblue3", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Averaged Monthly Temperature (C)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Temperature",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

#abline(a = 0, b = 1, col = "black", lwd = 2, lty = 3)

# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label B
label_function(label_value = "B", label_cex = 4)




# Panel C
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$PRCP_CM,
     # col = "darkblue", 
     col = "seagreen4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Total Precipitaiton Monthly (cm)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Precipitation",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

#abline(a = 0, b = 1, col = "black", lwd = 2, lty = 3)

# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label C
label_function(label_value = "C", label_cex = 4)





# Plot D
boxplot(x = data_monthly_annually[,-1], 
        col = "darkorange", 
        xaxt = "n",
        ylab = "Counts",
        main = "Incidence Box Plots (Monthly)")
axis(side = 1, las = 2, at = c(1:12), labels = colnames(data_monthly_annually[,-1]), cex = 1.5)
# legend("topleft", fill = rainbow(3, s = 0.5), legend = c(1,2,3), horiz = T)

# Label C
label_function(label_value = "D", label_cex = 4)




# Plot E
boxplot(x = t(data_monthly_annually[,-1]), 
        col = "darkorange", 
        xaxt = "n",
        ylab = "Counts",        
        main = "Incidence Box Plots (Annualy)")
axis(side = 1, las = 2, at = c(1:19), labels = colnames(t(data_monthly_annually[,-1])), cex = 1.5)
# legend("topleft", fill = rainbow(3, s = 0.5), legend = c(1,2,3), horiz = T)

# Label 
label_function(label_value = "E", label_cex = 4)



dev.off()




















# Fix 2022.02.17.
# Generating pdf output.
pdf( paste( "Plots/file06_final_data_figure_production_weather_b.pdf", sep = ""), height = 10, width = 14 )

# Defining the number of plots
# par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(1,4)) )
# Defining layout
# layout(matrix(c(1,1,2,2,3,3,4,5), nrow = 4, ncol = 2, byrow = TRUE))
#par( par(mfrow=c(2,)) )

# Defining layout
# Matrix first
layout_matrix <- matrix( c(1,1,2,2,3,3,1,1,2,2,3,3,4,4,4,5,5,5), nrow = 6, ncol = 3, byrow = FALSE)
# Setting layaout
layout(layout_matrix)




# Panel A
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$Counts,
     col = "darkblue", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Incidence (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

#abline(a = 0, b = 1, col = "black", lwd = 2, lty = 3)

# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label A
label_function(label_value = "A", label_cex = 4)



# Panel B
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$TEMP_C,
     # col = "darkblue", 
     col = "dodgerblue3", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Averaged Monthly Temperature (C)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Temperature",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

#abline(a = 0, b = 1, col = "black", lwd = 2, lty = 3)

# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label B
label_function(label_value = "B", label_cex = 4)




# Panel C
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$PRCP_CM,
     # col = "darkblue", 
     col = "seagreen4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Total Precipitaiton Monthly (cm)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Precipitation",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

#abline(a = 0, b = 1, col = "black", lwd = 2, lty = 3)

# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label C
label_function(label_value = "C", label_cex = 4)






# Plot D
boxplot(x = data_monthly_annually[,-1], 
        col = "darkorange", 
        xaxt = "n",
        ylab = "Counts",
        main = "Incidence Box Plots (Monthly)")
axis(side = 1, las = 2, at = c(1:12), labels = colnames(data_monthly_annually[,-1]), cex = 1.5)
# legend("topleft", fill = rainbow(3, s = 0.5), legend = c(1,2,3), horiz = T)

# Label C
label_function(label_value = "D", label_cex = 4)




# Plot E
boxplot(x = t(data_monthly_annually[,-1]), 
        col = "darkorange", 
        xaxt = "n",
        ylab = "Counts",        
        main = "Incidence Box Plots (Annualy)")
axis(side = 1, las = 2, at = c(1:19), labels = colnames(t(data_monthly_annually[,-1])), cex = 1.5)
# legend("topleft", fill = rainbow(3, s = 0.5), legend = c(1,2,3), horiz = T)

# Label 
label_function(label_value = "E", label_cex = 4)


dev.off()







# Fix 2022.02.17.
# Generating pdf output.
pdf( paste( "Plots/file06_final_data_figure_production_weather_c.pdf", sep = ""), height = 8.5, width = 16.5 )

# Defining the number of plots
# par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(1,4)) )
# Defining layout
# layout(matrix(c(1,1,2,2,3,3,4,5), nrow = 4, ncol = 2, byrow = TRUE))
#par( par(mfrow=c(2,)) )

# Defining layout
# Matrix first
layout_matrix <- matrix( c(1,1,2,2,3,3,1,1,2,2,3,3,1,1,2,2,3,3,4,4,4,4,4,4,5,5,5,5,5,5), nrow = 6, ncol = 5, byrow = FALSE)
# Setting layaout
layout(layout_matrix)




# Panel A
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$Counts,
     col = "darkblue", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Incidence (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

#abline(a = 0, b = 1, col = "black", lwd = 2, lty = 3)

# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label A
label_function(label_value = "A", label_cex = 4)



# Panel B
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$TEMP_C,
     # col = "darkblue", 
     col = "dodgerblue3", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Averaged Monthly Temperature (C)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Temperature",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

#abline(a = 0, b = 1, col = "black", lwd = 2, lty = 3)

# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label B
label_function(label_value = "B", label_cex = 4)




# Panel C
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$PRCP_CM,
     # col = "darkblue", 
     col = "seagreen4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Total Precipitaiton Monthly (cm)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Precipitation",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

#abline(a = 0, b = 1, col = "black", lwd = 2, lty = 3)

# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label C
label_function(label_value = "C", label_cex = 4)




# Plot D
# defining boxplot limits
boxplot_max_value <- max(as.vector(unlist(data_monthly_annually[,-1])))
boxplot_lower_limit <- 0 
# boxplot_upper_limit <- quantile(x = as.vector(unlist(data_monthly_annually[,-1])), probs = c(0.996))
boxplot_upper_limit <- 12

# boxplot
boxplot(x = data_monthly_annually[,-1], 
        col = "darkorange", 
        xaxt = "n",
        ylab = "Counts",
        cex = 1.55,
        cex.axis = 1.55,
        cex.lab = 1.55,
        cex.main = 1.55,
        cex.sub = 1.55,
        ylim = c(boxplot_lower_limit, boxplot_upper_limit),
        main = "Incidence Box Plots (Monthly)")
axis(side = 1, las = 2, at = c(1:12), labels = colnames(data_monthly_annually[,-1]), cex.axis = 1.55)
# legend("topleft", fill = rainbow(3, s = 0.5), legend = c(1,2,3), horiz = T)

# Points and color only for Oct 
value_add <- rep(x = 11.75, times = 12)
month_add <- c(1,2,3,4,5,6,7,8,9,10,11,12)
points_color_list <- rep(x ="white", times = 12)
points_color_list[10] <- "black"
stripchart(value_add ~ month_add,   # Data
           pch = 8,                # Pch symbols
           cex = 2,
           col = points_color_list, # Color of the symbol
           vertical = TRUE,         # Vertical mode
           add = TRUE)              # Add it over
# text coordinates
x_text_coordinate <- 10.75
y_text_coordinate <- 12
text(x = x_text_coordinate, y = y_text_coordinate, labels = boxplot_max_value, srt=0,  cex = 1.3)


# Adding legend
legend( x = "topleft",
        inset = c(0.08, 0.04),
        legend = c("Up to Scale", "Not Up to Scale"),
        #col = "black",
        #fill = c("dodgerblue3", "darkorange3"),
        pt.cex = c(2, 2),
        # pch = c(19, 20),
        cex = 1.25,
        pch = c(1, 8)
        #ncol = 3
)

# Label C
label_function(label_value = "D", label_cex = 4)







# Plot E
# defining boxplot limits
boxplot_max_value <- max(as.vector(unlist(t(data_monthly_annually[,-1]))))
boxplot_lower_limit <- 0 
# boxplot_upper_limit <- quantile(x = as.vector(unlist(data_monthly_annually[,-1])), probs = c(0.996))
boxplot_upper_limit <- 12

boxplot(x = t(data_monthly_annually[,-1]), 
        col = "darkorange", 
        xaxt = "n",
        ylab = "Counts",       
        cex = 1.55,
        cex.axis = 1.55,
        cex.lab = 1.55,
        cex.main = 1.55,
        cex.sub = 1.55,
        ylim = c(boxplot_lower_limit, boxplot_upper_limit),
        main = "Incidence Box Plots (Annualy)")
axis(side = 1, las = 2, at = c(1:19), labels = colnames(t(data_monthly_annually[,-1])), cex.axis = 1.55)
# legend("topleft", fill = rainbow(3, s = 0.5), legend = c(1,2,3), horiz = T)

# Points and color only for Oct 
value_add <- rep(x = 11.75, times = 19)
month_add <- c(1:19)
points_color_list <- rep(x ="white", times = 12)
points_color_list[19] <- "black"
stripchart(value_add ~ month_add,   # Data
           pch = 8,                # Pch symbols
           cex = 2,
           col = points_color_list, # Color of the symbol
           vertical = TRUE,         # Vertical mode
           add = TRUE)              # Add it over
# text coordinates
x_text_coordinate <- 18
y_text_coordinate <- 12
text(x = x_text_coordinate, y = y_text_coordinate, labels = boxplot_max_value, srt=0,  cex = 1.3)

# Adding legend
legend( x = "topleft",
        inset = c(0.08, 0.14),
        legend = c("Up to Scale", "Not Up to Scale"),
        #col = "black",
        #fill = c("dodgerblue3", "darkorange3"),
        pt.cex = c(2, 2),
        # pch = c(19, 20),
        cex = 1.25,
        pch = c(1, 8)
        #ncol = 3
)



# Label 
label_function(label_value = "E", label_cex = 4)





dev.off()











# Fix 2022.10.07.
# Generating pdf output.
# pdf( paste( "Plots/file06_final_data_figure_production_weather_d.pdf", sep = ""), height = 8.5, width = 16.5 )
png( paste( "Plots/file06_final_data_figure_production_weather_d.png", sep = ""), height = 8.5, width = 16.5, res = 600, units="in" )

# Defining the number of plots
# par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(1,4)) )
# Defining layout
# layout(matrix(c(1,1,2,2,3,3,4,5), nrow = 4, ncol = 2, byrow = TRUE))
#par( par(mfrow=c(2,)) )

# Defining layout
# Matrix first
layout_matrix <- matrix( c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3), nrow = 6, ncol = 5, byrow = FALSE)
# Setting layaout
layout(layout_matrix)




# Panel A
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$Counts,
     col = "darkblue", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Incidence (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# Panel A
# Plotting x_values vs y_values
lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$Counts,
      col = "darkblue", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Incidence (Total Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Counts",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label A
label_function(label_value = "A", label_cex = 4)




# Plot B
# defining boxplot limits
boxplot_max_value <- max(as.vector(unlist(data_monthly_annually[,-1])))
boxplot_lower_limit <- 0 
# boxplot_upper_limit <- quantile(x = as.vector(unlist(data_monthly_annually[,-1])), probs = c(0.996))
boxplot_upper_limit <- 12

# boxplot
boxplot(x = data_monthly_annually[,-1], 
        col = "darkorange", 
        xaxt = "n",
        ylab = "Counts",
        cex = 1.55,
        cex.axis = 1.55,
        cex.lab = 1.55,
        cex.main = 1.55,
        cex.sub = 1.55,
        ylim = c(boxplot_lower_limit, boxplot_upper_limit),
        main = "Incidence Box Plots (Monthly)")
axis(side = 1, las = 2, at = c(1:12), labels = colnames(data_monthly_annually[,-1]), cex.axis = 1.55)
# legend("topleft", fill = rainbow(3, s = 0.5), legend = c(1,2,3), horiz = T)

# Points and color only for Oct 
value_add <- rep(x = 11.75, times = 12)
month_add <- c(1,2,3,4,5,6,7,8,9,10,11,12)
points_color_list <- rep(x ="white", times = 12)
points_color_list[10] <- "black"
stripchart(value_add ~ month_add,   # Data
           pch = 8,                # Pch symbols
           cex = 2,
           col = points_color_list, # Color of the symbol
           vertical = TRUE,         # Vertical mode
           add = TRUE)              # Add it over
# text coordinates
x_text_coordinate <- 10.75
y_text_coordinate <- 12
text(x = x_text_coordinate, y = y_text_coordinate, labels = boxplot_max_value, srt=0,  cex = 1.3)


# Adding legend
legend( x = "topleft",
        inset = c(0.08, 0.04),
        legend = c("Up to Scale", "Not Up to Scale"),
        #col = "black",
        #fill = c("dodgerblue3", "darkorange3"),
        pt.cex = c(2, 2),
        # pch = c(19, 20),
        cex = 1.25,
        pch = c(1, 8)
        #ncol = 3
)

# Label B
label_function(label_value = "B", label_cex = 4)







# Plot C
# defining boxplot limits
boxplot_max_value <- max(as.vector(unlist(t(data_monthly_annually[,-1]))))
boxplot_lower_limit <- 0 
# boxplot_upper_limit <- quantile(x = as.vector(unlist(data_monthly_annually[,-1])), probs = c(0.996))
boxplot_upper_limit <- 12

boxplot(x = t(data_monthly_annually[,-1]), 
        col = "darkorange", 
        xaxt = "n",
        ylab = "Counts",       
        cex = 1.55,
        cex.axis = 1.55,
        cex.lab = 1.55,
        cex.main = 1.55,
        cex.sub = 1.55,
        ylim = c(boxplot_lower_limit, boxplot_upper_limit),
        main = "Incidence Box Plots (Annualy)")
axis(side = 1, las = 2, at = c(1:19), labels = colnames(t(data_monthly_annually[,-1])), cex.axis = 1.55)
# legend("topleft", fill = rainbow(3, s = 0.5), legend = c(1,2,3), horiz = T)

# Points and color only for Oct 
value_add <- rep(x = 11.75, times = 19)
month_add <- c(1:19)
points_color_list <- rep(x ="white", times = 12)
points_color_list[19] <- "black"
stripchart(value_add ~ month_add,   # Data
           pch = 8,                # Pch symbols
           cex = 2,
           col = points_color_list, # Color of the symbol
           vertical = TRUE,         # Vertical mode
           add = TRUE)              # Add it over
# text coordinates
x_text_coordinate <- 18
y_text_coordinate <- 12
text(x = x_text_coordinate, y = y_text_coordinate, labels = boxplot_max_value, srt=0,  cex = 1.3)

# Adding legend
legend( x = "topleft",
        inset = c(0.08, 0.14),
        legend = c("Up to Scale", "Not Up to Scale"),
        #col = "black",
        #fill = c("dodgerblue3", "darkorange3"),
        pt.cex = c(2, 2),
        # pch = c(19, 20),
        cex = 1.25,
        pch = c(1, 8)
        #ncol = 3
)



# Label C
label_function(label_value = "C", label_cex = 4)





dev.off()
















# Fix 2022.10.07.
# Generating pdf output.
# pdf( paste( "Plots/file06_final_data_figure_production_weather_e.pdf", sep = ""), height = 15.50, width = 13.25 )
png( paste( "Plots/file06_final_data_figure_production_weather_e.png", sep = ""), height = 15.50, width = 13.25, res = 600, units="in" )

# Defining the number of plots
# par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(1,4)) )
# Defining layout
# layout(matrix(c(1,1,2,2,3,3,4,5), nrow = 4, ncol = 2, byrow = TRUE))
#par( par(mfrow=c(2,)) )

# Defining layout
# Matrix first
# layout_matrix <- matrix( c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3), nrow = 6, ncol = 5, byrow = FALSE)
layout_matrix <- matrix( 1, nrow = 20, ncol = 5, byrow = FALSE)

# Cells definition (column 1)
layout_matrix[c(1:4),   c(1:3)] <- 1
layout_matrix[c(5:8),   c(1:3)] <- 2
layout_matrix[c(9:12),  c(1:3)] <- 3
layout_matrix[c(13:16), c(1:3)] <- 4
layout_matrix[c(17:20), c(1:3)] <- 5

# Cells definition (column 2)
layout_matrix[c(1:5),   c(4:5)] <- 6
layout_matrix[c(6:10),  c(4:5)] <- 7
layout_matrix[c(11:15), c(4:5)] <- 8
layout_matrix[c(16:20), c(4:5)] <- 9

# Setting layaout
layout(layout_matrix)




# Panel A
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$Counts,
     col = "darkblue", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Incidence (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# Panel A
# Plotting x_values vs y_values
lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$Counts,
      col = "darkblue", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Incidence (Total Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Counts",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label A
label_function(label_value = "A", label_cex = 4)




# Panel B
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$TEMP_C,
     # col = "darkblue", 
     col = "dodgerblue3", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Temperature (Averaged Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Degree (C)",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$TEMP_C,
     # col = "darkblue", 
     col = "dodgerblue3", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Temperature (Averaged Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Degree (C)",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label B
label_function(label_value = "B", label_cex = 4)




# Panel C
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$PRCP_CM,
     # col = "darkblue", 
     col = "seagreen4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Precipitaiton (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "cm",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$PRCP_CM,
     # col = "darkblue", 
     col = "seagreen4", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Precipitaiton (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "cm",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label C
label_function(label_value = "C", label_cex = 4)





# Panel D
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$WDSP_MS,
     # col = "darkblue", 
     col = "dimgrey", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Wind Speed (Average Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Meters per Second",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$WDSP_MS,
      # col = "darkblue", 
      col = "dimgrey", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Wind Speed (Average Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Meters per Second",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label D
label_function(label_value = "D", label_cex = 4)







# Panel E
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$SLP,
     # col = "darkblue", 
     col = "darkorchid4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Sea Level Pressure (Average Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Mbar",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$SLP,
      # col = "darkblue", 
      col = "darkorchid4", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Sea Level Pressure (Average Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Mbar",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label E
label_function(label_value = "E", label_cex = 4)




# Panel F
counts_transformed <- log(merged_river_plus_incidence$Counts + 1)

# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$TEMP_C,
     y = counts_transformed,
     col = "dodgerblue3", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Log(Counts+1) vs Temperature (C)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_TEMP_C <- lm( counts_transformed ~ merged_river_plus_incidence$TEMP_C) 
summary(lm_fit_TEMP_C)
# lm fit extracts
a_TEMP_C <- lm_fit_TEMP_C$coefficients[2]
b_TEMP_C <- lm_fit_TEMP_C$coefficients[1]

# Summary statistics
r_TEMP_C      <- round( x = sqrt(summary(lm_fit_TEMP_C)$r.squared), digits = 2)
pvalue_TEMP_C <- round( x = summary(lm_fit_TEMP_C)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(merged_river_plus_incidence$TEMP_C)[1], range(merged_river_plus_incidence$TEMP_C)[2])
range_y <- a_TEMP_C * range_x + b_TEMP_C

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="red", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("dodgerblue3", "red"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_TEMP_C))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_TEMP_C)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$TEMP_C), to  = max(merged_river_plus_incidence$TEMP_C),  
                              by = (range(merged_river_plus_incidence$TEMP_C)[2] - range(merged_river_plus_incidence$TEMP_C)[1])/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label F
label_function(label_value = "F", label_cex = 4)









# Panel G
counts_transformed <- log(merged_river_plus_incidence$Counts + 1)

# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$PRCP_CM,
     y = counts_transformed,
     col = "seagreen4", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Log(Counts+1) vs Precipitation (cm)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_PRCP_CM <- lm( counts_transformed ~ merged_river_plus_incidence$PRCP_CM) 
summary(lm_fit_PRCP_CM)
# lm fit extracts
a_PRCP_CM <- lm_fit_PRCP_CM$coefficients[2]
b_PRCP_CM <- lm_fit_PRCP_CM$coefficients[1]

# Summary statistics
r_PRCP_CM      <- round( x = sqrt(summary(lm_fit_PRCP_CM)$r.squared), digits = 2)
pvalue_PRCP_CM <- round( x = summary(lm_fit_PRCP_CM)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(merged_river_plus_incidence$PRCP_CM)[1], range(merged_river_plus_incidence$PRCP_CM)[2])
range_y <- a_PRCP_CM * range_x + b_PRCP_CM

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="red", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("seagreen4", "red"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_PRCP_CM))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_PRCP_CM)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$PRCP_CM), to  =  max(merged_river_plus_incidence$PRCP_CM),  
                              by = (range(merged_river_plus_incidence$PRCP_CM)[2] - range(merged_river_plus_incidence$PRCP_CM)[1])/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label G
label_function(label_value = "G", label_cex = 4)






# Panel H
counts_transformed <- log(merged_river_plus_incidence$Counts + 1)

# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$WDSP_MS,
     y = counts_transformed,
     col = "dimgrey", 
     lwd = 5,
     pch = 16,
     type = "p",
       main = "Log(Counts+1) vs Wind Speed (m/s)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_WDSP_MS <- lm( counts_transformed ~ merged_river_plus_incidence$WDSP_MS) 
summary(lm_fit_WDSP_MS)
# lm fit extracts
a_WDSP_MS <- lm_fit_WDSP_MS$coefficients[2]
b_WDSP_MS <- lm_fit_WDSP_MS$coefficients[1]

# Summary statistics
r_WDSP_MS      <- round( x = sqrt(summary(lm_fit_WDSP_MS)$r.squared), digits = 2)
pvalue_WDSP_MS <- round( x = summary(lm_fit_WDSP_MS)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(merged_river_plus_incidence$WDSP_MS)[1], range(merged_river_plus_incidence$WDSP_MS)[2])
range_y <- a_WDSP_MS * range_x + b_WDSP_MS

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="red", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("dimgrey", "red"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_WDSP_MS))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_WDSP_MS)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$WDSP_MS), to  = max(merged_river_plus_incidence$WDSP_MS),  
                              by = (range(merged_river_plus_incidence$WDSP_MS)[2] - range(merged_river_plus_incidence$WDSP_MS)[1])/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label H
label_function(label_value = "H", label_cex = 4)







# Panel I
counts_transformed <- log(merged_river_plus_incidence$Counts + 1)

# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$SLP,
     y = counts_transformed,
     col = "darkorchid4", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Log(Counts+1) vs Sea Level Pressure (Mbar)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_SLP <- lm( counts_transformed ~ merged_river_plus_incidence$SLP) 
summary(lm_fit_SLP)
# lm fit extracts
a_SLP <- lm_fit_SLP$coefficients[2]
b_SLP <- lm_fit_SLP$coefficients[1]

# Summary statistics
r_SLP      <- round( x = sqrt(summary(lm_fit_SLP)$r.squared), digits = 2)
pvalue_SLP <- round( x = summary(lm_fit_SLP)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(merged_river_plus_incidence$SLP)[1], range(merged_river_plus_incidence$SLP)[2])
range_y <- a_SLP * range_x + b_SLP

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="red", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("darkorchid4", "red"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_SLP))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_SLP)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$SLP), to  = max(merged_river_plus_incidence$SLP),  
                              by = (range(merged_river_plus_incidence$SLP)[2] - range(merged_river_plus_incidence$SLP)[1])/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label I
label_function(label_value = " I", label_cex = 4)



dev.off()


















# Fix 2022.10.07.
# Generating pdf output.
# pdf( paste( "Plots/file06_final_data_figure_production_weather_f.pdf", sep = ""), height = 15.50, width = 13.25 )
png( paste( "Plots/file06_final_data_figure_production_weather_f.png", sep = ""), height = 15.50, width = 13.25, res = 600, units="in" )

# Defining the number of plots
# par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(1,4)) )
# Defining layout
# layout(matrix(c(1,1,2,2,3,3,4,5), nrow = 4, ncol = 2, byrow = TRUE))
#par( par(mfrow=c(2,)) )

# Defining layout
# Matrix first
# layout_matrix <- matrix( c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3), nrow = 6, ncol = 5, byrow = FALSE)
layout_matrix <- matrix( 1, nrow = 20, ncol = 5, byrow = FALSE)

# Cells definition (column 1)
layout_matrix[c(1:4),   c(1:3)] <- 1
layout_matrix[c(5:8),   c(1:3)] <- 2
layout_matrix[c(9:12),  c(1:3)] <- 3
layout_matrix[c(13:16), c(1:3)] <- 4
layout_matrix[c(17:20), c(1:3)] <- 5

# Cells definition (column 2)
layout_matrix[c(1:5),   c(4:5)] <- 6
layout_matrix[c(6:10),  c(4:5)] <- 7
layout_matrix[c(11:15), c(4:5)] <- 8
layout_matrix[c(16:20), c(4:5)] <- 9

# Setting layaout
layout(layout_matrix)




# Panel A
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$Counts,
     col = "darkblue", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Incidence (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# Panel A
# Plotting x_values vs y_values
lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$Counts,
      col = "darkblue", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Incidence (Total Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Counts",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label A
label_function(label_value = "A", label_cex = 4)




# Panel B
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$TEMP_C,
     # col = "darkblue", 
     col = "dodgerblue3", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Temperature (Averaged Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Degree (C)",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$TEMP_C,
      # col = "darkblue", 
      col = "dodgerblue3", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Temperature (Averaged Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Degree (C)",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label B
label_function(label_value = "B", label_cex = 4)




# Panel C
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$PRCP_CM,
     # col = "darkblue", 
     col = "seagreen4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Precipitaiton (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "cm",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$PRCP_CM,
      # col = "darkblue", 
      col = "seagreen4", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Precipitaiton (Total Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "cm",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label C
label_function(label_value = "C", label_cex = 4)





# Panel D
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$WDSP_MS,
     # col = "darkblue", 
     col = "dimgrey", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Wind Speed (Average Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Meters per Second",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$WDSP_MS,
      # col = "darkblue", 
      col = "dimgrey", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Wind Speed (Average Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Meters per Second",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label D
label_function(label_value = "D", label_cex = 4)







# Panel E
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$SLP,
     # col = "darkblue", 
     col = "darkorchid4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Sea Level Pressure (Average Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Mbar",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$SLP,
      # col = "darkblue", 
      col = "darkorchid4", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Sea Level Pressure (Average Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Mbar",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label E
label_function(label_value = "E", label_cex = 4)




# Panel F
counts_transformed <- log(merged_river_plus_incidence$Counts)
variable_data <- merged_river_plus_incidence$TEMP_C

counts_transformed[ is.infinite(counts_transformed)] <- NA
# Making corresponding dates NA
variable_data[ is.na(counts_transformed)] <- NA

# Removing NA-s
counts_transformed <- counts_transformed[!is.na(counts_transformed)]
variable_data      <- variable_data[!is.na(variable_data)] 


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "dodgerblue3", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Zeroes Omitted: Log(Counts) vs Temperature (C)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_TEMP_C <- lm( counts_transformed ~ variable_data) 
summary(lm_fit_TEMP_C)
# lm fit extracts
a_TEMP_C <- lm_fit_TEMP_C$coefficients[2]
b_TEMP_C <- lm_fit_TEMP_C$coefficients[1]

# Summary statistics
r_TEMP_C      <- round( x = sqrt(summary(lm_fit_TEMP_C)$r.squared), digits = 2)
pvalue_TEMP_C <- round( x = summary(lm_fit_TEMP_C)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(variable_data)[1], range(variable_data)[2])
range_y <- a_TEMP_C * range_x + b_TEMP_C

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("dodgerblue3", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_TEMP_C))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_TEMP_C)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(variable_data), to  = max(variable_data),  
                              by = (range(variable_data)[2] - range(variable_data)[1])/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label F
label_function(label_value = "F", label_cex = 4)









# Panel G
counts_transformed <- log(merged_river_plus_incidence$Counts)
variable_data <- merged_river_plus_incidence$PRCP_CM

counts_transformed[ is.infinite(counts_transformed)] <- NA
# Making corresponding dates NA
variable_data[ is.na(counts_transformed)] <- NA

# Removing NA-s
counts_transformed <- counts_transformed[!is.na(counts_transformed)]
variable_data      <- variable_data[!is.na(variable_data)] 


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "seagreen4", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Zeroes Omitted: Log(Counts) vs Precipitation (cm)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_PRCP_CM <- lm( counts_transformed ~ variable_data) 
summary(lm_fit_PRCP_CM)
# lm fit extracts
a_PRCP_CM <- lm_fit_PRCP_CM$coefficients[2]
b_PRCP_CM <- lm_fit_PRCP_CM$coefficients[1]

# Summary statistics
r_PRCP_CM      <- round( x = sqrt(summary(lm_fit_PRCP_CM)$r.squared), digits = 2)
pvalue_PRCP_CM <- round( x = summary(lm_fit_PRCP_CM)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(variable_data)[1], range(variable_data)[2])
range_y <- a_PRCP_CM * range_x + b_PRCP_CM

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("seagreen4", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_PRCP_CM))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_PRCP_CM)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(variable_data), to  = max(variable_data),  
                              by = (range(variable_data)[2] - range(variable_data)[1])/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label G
label_function(label_value = "G", label_cex = 4)






# Panel H
counts_transformed <- log(merged_river_plus_incidence$Counts)
variable_data <- merged_river_plus_incidence$WDSP_MS

counts_transformed[ is.infinite(counts_transformed)] <- NA
# Making corresponding dates NA
variable_data[ is.na(counts_transformed)] <- NA

# Removing NA-s
counts_transformed <- counts_transformed[!is.na(counts_transformed)]
variable_data      <- variable_data[!is.na(variable_data)] 


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "dimgrey", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Zeroes Omitted: Log(Counts) vs Wind Speed (m/s)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_WDSP_MS <- lm( counts_transformed ~ variable_data) 
summary(lm_fit_WDSP_MS)
# lm fit extracts
a_WDSP_MS <- lm_fit_WDSP_MS$coefficients[2]
b_WDSP_MS <- lm_fit_WDSP_MS$coefficients[1]

# Summary statistics
r_WDSP_MS      <- round( x = sqrt(summary(lm_fit_WDSP_MS)$r.squared), digits = 2)
pvalue_WDSP_MS <- round( x = summary(lm_fit_WDSP_MS)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(variable_data)[1], range(variable_data)[2])
range_y <- a_WDSP_MS * range_x + b_WDSP_MS

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("dimgrey", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_WDSP_MS))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_WDSP_MS)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(variable_data), to  = max(variable_data),  
                              by = (range(variable_data)[2] - range(variable_data)[1])/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label H
label_function(label_value = "H", label_cex = 4)







# Panel I
counts_transformed <- log(merged_river_plus_incidence$Counts)
variable_data <- merged_river_plus_incidence$SLP

counts_transformed[ is.infinite(counts_transformed)] <- NA
# Making corresponding dates NA
variable_data[ is.na(counts_transformed)] <- NA

# Removing NA-s
counts_transformed <- counts_transformed[!is.na(counts_transformed)]
variable_data      <- variable_data[!is.na(variable_data)] 


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "darkorchid4", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Zeroes Omitted: Log(Counts) vs Sea Level Pressure (Mbar)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_SLP <- lm( counts_transformed ~ variable_data) 
summary(lm_fit_SLP)
# lm fit extracts
a_SLP <- lm_fit_SLP$coefficients[2]
b_SLP <- lm_fit_SLP$coefficients[1]

# Summary statistics
r_SLP      <- round( x = sqrt(summary(lm_fit_SLP)$r.squared), digits = 2)
pvalue_SLP <- round( x = summary(lm_fit_SLP)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(variable_data)[1], range(variable_data)[2])
range_y <- a_SLP * range_x + b_SLP

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("darkorchid4", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_SLP))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_SLP)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(variable_data), to  = max(variable_data),  
                              by = (range(variable_data)[2] - range(variable_data)[1])/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label I
label_function(label_value = " I", label_cex = 4)



dev.off()

































# Fix 2022.10.07.
# Generating pdf output.
# pdf( paste( "Plots/file06_final_data_figure_production_weather_g.pdf", sep = ""), height = 16, width = 20.50 )
png( paste( "Plots/file06_final_data_figure_production_weather_g.png", sep = ""), height = 16, width = 20.50, res = 600, units="in" )

# Defining the number of plots
# par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(1,4)) )
# Defining layout
# layout(matrix(c(1,1,2,2,3,3,4,5), nrow = 4, ncol = 2, byrow = TRUE))
#par( par(mfrow=c(2,)) )

# Defining layout
# Matrix first
# layout_matrix <- matrix( c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3), nrow = 6, ncol = 5, byrow = FALSE)
layout_matrix <- matrix( 1, nrow = 20, ncol = 7, byrow = FALSE)

# Cells definition (column 1)
layout_matrix[c(1:4),   c(1:3)] <- 1
layout_matrix[c(5:8),   c(1:3)] <- 2
layout_matrix[c(9:12),  c(1:3)] <- 3
layout_matrix[c(13:16), c(1:3)] <- 4
layout_matrix[c(17:20), c(1:3)] <- 5

# Cells definition (column 2)
layout_matrix[c(1:5),   c(4:5)] <- 6
layout_matrix[c(1:5),   c(6:7)] <- 7
layout_matrix[c(6:10),  c(4:5)] <- 8
layout_matrix[c(6:10),  c(6:7)] <- 9
layout_matrix[c(11:15), c(4:5)] <- 10
layout_matrix[c(11:15), c(6:7)] <- 11
layout_matrix[c(16:20), c(4:5)] <- 12
layout_matrix[c(16:20), c(6:7)] <- 13

# Setting layaout
layout(layout_matrix)




# Panel A
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$Counts,
     col = "darkblue", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Incidence (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# Panel A
# Plotting x_values vs y_values
lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$Counts,
      col = "darkblue", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Incidence (Total Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Counts",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label A
label_function(label_value = "A", label_cex = 4)




# Panel B
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$TEMP_C,
     # col = "darkblue", 
     col = "dodgerblue3", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Temperature (Averaged Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Degree (C)",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$TEMP_C,
      # col = "darkblue", 
      col = "dodgerblue3", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Temperature (Averaged Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Degree (C)",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label B
label_function(label_value = "B", label_cex = 4)




# Panel C
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$PRCP_CM,
     # col = "darkblue", 
     col = "seagreen4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Precipitaiton (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "cm",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$PRCP_CM,
      # col = "darkblue", 
      col = "seagreen4", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Precipitaiton (Total Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "cm",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label C
label_function(label_value = "C", label_cex = 4)





# Panel D
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$WDSP_MS,
     # col = "darkblue", 
     col = "dimgrey", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Wind Speed (Average Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Meters per Second",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$WDSP_MS,
      # col = "darkblue", 
      col = "dimgrey", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Wind Speed (Average Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Meters per Second",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label D
label_function(label_value = "D", label_cex = 4)







# Panel E
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$SLP,
     # col = "darkblue", 
     col = "darkorchid4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Sea Level Pressure (Average Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Mbar",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$SLP,
      # col = "darkblue", 
      col = "darkorchid4", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Sea Level Pressure (Average Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Mbar",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label E
label_function(label_value = "E", label_cex = 4)








# Panel F
counts_transformed <- merged_river_plus_incidence$Counts
variable_data <- merged_river_plus_incidence$TEMP_C

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

# Re-coding as 1
counts_transformed[ counts_transformed >=1 ] <- 1


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "dodgerblue3", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Binomial Fit (O vs 1 and up) vs Temperature (C)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts/Probability",
     xaxt='n',
     yaxt='n',     
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
glm_fit_TEMP_C <- glm( counts_transformed ~ variable_data, family = binomial) 
summary(glm_fit_TEMP_C)

# Prediction
variable_data_prediction     <- data.frame( variable_data = seq( variable_data_min, variable_data_max,len = 500 ) )
counts_transformed_predicted <- predict(glm_fit_TEMP_C, newdata = variable_data_prediction , type="response")


# Adding the corresponding regression fit.
lines(x = variable_data_prediction$variable_data, 
      y = counts_transformed_predicted, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topleft",
       inset= c(0.10, 0.10), 
       legend = c( "Data Points", "Binomial Fit"), 
       col = "black", 
       fill = c("dodgerblue3", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- 0
y_max_value <- 1
y_tlab  <- c(0, 0.25, 0.5, 0.75,  1)
y_lablist <- c(0, 0.25, 0.5, 0.75,  1)
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)

# Adding Counts
y_tlab  <- c(0.05, 0.95)
y_lablist <- c("0 Counts", "1 and up")
axis(2, at = y_tlab, tick = FALSE, labels = y_lablist, las = 1, cex.axis = 1.25)

# Label F
label_function(label_value = "F", label_cex = 4)






# Panel G
counts_transformed <- log(merged_river_plus_incidence$Counts)
variable_data <- merged_river_plus_incidence$TEMP_C

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

counts_transformed[ is.infinite(counts_transformed)] <- NA
# Making corresponding dates NA
variable_data[ is.na(counts_transformed)] <- NA

# Removing NA-s
counts_transformed <- counts_transformed[!is.na(counts_transformed)]
variable_data      <- variable_data[!is.na(variable_data)] 


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "dodgerblue3", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Zeroes Omitted: Log(Counts) vs Temperature (C)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_TEMP_C <- lm( counts_transformed ~ variable_data) 
summary(lm_fit_TEMP_C)
# lm fit extracts
a_TEMP_C <- lm_fit_TEMP_C$coefficients[2]
b_TEMP_C <- lm_fit_TEMP_C$coefficients[1]

# Summary statistics
r_TEMP_C      <- round( x = sqrt(summary(lm_fit_TEMP_C)$r.squared), digits = 2)
pvalue_TEMP_C <- round( x = summary(lm_fit_TEMP_C)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(variable_data)[1], range(variable_data)[2])
range_y <- a_TEMP_C * range_x + b_TEMP_C

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("dodgerblue3", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_TEMP_C))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_TEMP_C)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label G
label_function(label_value = "G", label_cex = 4)











# Panel H
counts_transformed <- merged_river_plus_incidence$Counts
variable_data <- merged_river_plus_incidence$PRCP_CM

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

# Re-coding as 1
counts_transformed[ counts_transformed >=1 ] <- 1


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "seagreen4", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Binomial Fit (O vs 1 and up) vs Precipitation (cm)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts/Probability",
     xaxt='n',
     yaxt='n',     
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
glm_fit_PRCP_CM <- glm( counts_transformed ~ variable_data, family = binomial) 
summary(glm_fit_PRCP_CM)

# Prediction
variable_data_prediction     <- data.frame( variable_data = seq( variable_data_min, variable_data_max, len = 500 ) )
counts_transformed_predicted <- predict(glm_fit_PRCP_CM, newdata = variable_data_prediction , type="response")


# Adding the corresponding regression fit.
lines(x = variable_data_prediction$variable_data, 
      y = counts_transformed_predicted, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topleft",
       inset= c(0.10, 0.10), 
       legend = c( "Data Points", "Binomial Fit"), 
       col = "black", 
       fill = c("seagreen4", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- 0
y_max_value <- 1
y_tlab  <- c(0, 0.25, 0.5, 0.75,  1)
y_lablist <- c(0, 0.25, 0.5, 0.75,  1)
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)

# Adding Counts
y_tlab  <- c(0.05, 0.95)
y_lablist <- c("0 Counts", "1 and up")
axis(2, at = y_tlab, tick = FALSE, labels = y_lablist, las = 1, cex.axis = 1.25)

# Label H
label_function(label_value = "H", label_cex = 4)








# Panel I
counts_transformed <- log(merged_river_plus_incidence$Counts)
variable_data <- merged_river_plus_incidence$PRCP_CM

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

counts_transformed[ is.infinite(counts_transformed)] <- NA
# Making corresponding dates NA
variable_data[ is.na(counts_transformed)] <- NA

# Removing NA-s
counts_transformed <- counts_transformed[!is.na(counts_transformed)]
variable_data      <- variable_data[!is.na(variable_data)] 


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "seagreen4", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Zeroes Omitted: Log(Counts) vs Precipitation (cm)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_PRCP_CM <- lm( counts_transformed ~ variable_data) 
summary(lm_fit_PRCP_CM)
# lm fit extracts
a_PRCP_CM <- lm_fit_PRCP_CM$coefficients[2]
b_PRCP_CM <- lm_fit_PRCP_CM$coefficients[1]

# Summary statistics
r_PRCP_CM      <- round( x = sqrt(summary(lm_fit_PRCP_CM)$r.squared), digits = 2)
pvalue_PRCP_CM <- round( x = summary(lm_fit_PRCP_CM)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(variable_data)[1], range(variable_data)[2])
range_y <- a_PRCP_CM * range_x + b_PRCP_CM

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("seagreen4", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_PRCP_CM))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_PRCP_CM)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label I
label_function(label_value = " I", label_cex = 4)









# Panel J
counts_transformed <- merged_river_plus_incidence$Counts
variable_data <- merged_river_plus_incidence$WDSP_MS

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

# Re-coding as 1
counts_transformed[ counts_transformed >=1 ] <- 1


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "dimgrey", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Binomial Fit (O vs 1 and up) vs Wind Speed (m/s)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts/Probability",
     xaxt='n',
     yaxt='n',     
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
glm_fit_WDSP_MS <- glm( counts_transformed ~ variable_data, family = binomial) 
summary(glm_fit_WDSP_MS)

# Prediction
variable_data_prediction     <- data.frame( variable_data = seq( variable_data_min, variable_data_max, len = 500 ) )
counts_transformed_predicted <- predict(glm_fit_WDSP_MS, newdata = variable_data_prediction , type="response")


# Adding the corresponding regression fit.
lines(x = variable_data_prediction$variable_data, 
      y = counts_transformed_predicted, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topleft",
       inset= c(0.10, 0.10), 
       legend = c( "Data Points", "Binomial Fit"), 
       col = "black", 
       fill = c("dimgrey", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- 0
y_max_value <- 1
y_tlab  <- c(0, 0.25, 0.5, 0.75,  1)
y_lablist <- c(0, 0.25, 0.5, 0.75,  1)
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)

# Adding Counts
y_tlab  <- c(0.05, 0.95)
y_lablist <- c("0 Counts", "1 and up")
axis(2, at = y_tlab, tick = FALSE, labels = y_lablist, las = 1, cex.axis = 1.25)

# Label J
label_function(label_value = "J", label_cex = 4)





# Panel K
counts_transformed <- log(merged_river_plus_incidence$Counts)
variable_data <- merged_river_plus_incidence$WDSP_MS

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

counts_transformed[ is.infinite(counts_transformed)] <- NA
# Making corresponding dates NA
variable_data[ is.na(counts_transformed)] <- NA

# Removing NA-s
counts_transformed <- counts_transformed[!is.na(counts_transformed)]
variable_data      <- variable_data[!is.na(variable_data)] 


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "dimgrey", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Zeroes Omitted: Log(Counts) vs Wind Speed (m/s)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_WDSP_MS <- lm( counts_transformed ~ variable_data) 
summary(lm_fit_WDSP_MS)
# lm fit extracts
a_WDSP_MS <- lm_fit_WDSP_MS$coefficients[2]
b_WDSP_MS <- lm_fit_WDSP_MS$coefficients[1]

# Summary statistics
r_WDSP_MS      <- round( x = sqrt(summary(lm_fit_WDSP_MS)$r.squared), digits = 2)
pvalue_WDSP_MS <- round( x = summary(lm_fit_WDSP_MS)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(variable_data)[1], range(variable_data)[2])
range_y <- a_WDSP_MS * range_x + b_WDSP_MS

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("dimgrey", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_WDSP_MS))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_WDSP_MS)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label K
label_function(label_value = "K", label_cex = 4)












# Panel L
counts_transformed <- merged_river_plus_incidence$Counts
variable_data <- merged_river_plus_incidence$SLP

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

# Re-coding as 1
counts_transformed[ counts_transformed >=1 ] <- 1


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "darkorchid4", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Binomial Fit (O vs 1 and up) vs Sea Level Pressure (Mbar)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts/Probability",
     xaxt='n',
     yaxt='n',     
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
glm_fit_SLP <- glm( counts_transformed ~ variable_data, family = binomial) 
summary(glm_fit_SLP)

# Prediction
variable_data_prediction     <- data.frame( variable_data = seq( variable_data_min, variable_data_max, len = 500 ) )
counts_transformed_predicted <- predict(glm_fit_SLP, newdata = variable_data_prediction , type="response")


# Adding the corresponding regression fit.
lines(x = variable_data_prediction$variable_data, 
      y = counts_transformed_predicted, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topleft",
       inset= c(0.10, 0.10), 
       legend = c( "Data Points", "Binomial Fit"), 
       col = "black", 
       fill = c("darkorchid4", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- 0
y_max_value <- 1
y_tlab  <- c(0, 0.25, 0.5, 0.75,  1)
y_lablist <- c(0, 0.25, 0.5, 0.75,  1)
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)

# Adding Counts
y_tlab  <- c(0.05, 0.95)
y_lablist <- c("0 Counts", "1 and up")
axis(2, at = y_tlab, tick = FALSE, labels = y_lablist, las = 1, cex.axis = 1.25)

# Label L
label_function(label_value = "L", label_cex = 4)






# Panel M
counts_transformed <- log(merged_river_plus_incidence$Counts)
variable_data <- merged_river_plus_incidence$SLP

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

counts_transformed[ is.infinite(counts_transformed)] <- NA
# Making corresponding dates NA
variable_data[ is.na(counts_transformed)] <- NA

# Removing NA-s
counts_transformed <- counts_transformed[!is.na(counts_transformed)]
variable_data      <- variable_data[!is.na(variable_data)] 


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "darkorchid4", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Zeroes Omitted: Log(Counts) vs Sea Level Pressure (Mbar)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_SLP <- lm( counts_transformed ~ variable_data) 
summary(lm_fit_SLP)
# lm fit extracts
a_SLP <- lm_fit_SLP$coefficients[2]
b_SLP <- lm_fit_SLP$coefficients[1]

# Summary statistics
r_SLP      <- round( x = sqrt(summary(lm_fit_SLP)$r.squared), digits = 2)
pvalue_SLP <- round( x = summary(lm_fit_SLP)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(variable_data)[1], range(variable_data)[2])
range_y <- a_SLP * range_x + b_SLP

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("darkorchid4", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_SLP))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_SLP)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label M
label_function(label_value = "M", label_cex = 4)



dev.off()

























# Fix 2022.10.07.
# Generating pdf output.
# pdf( paste( "Plots/file06_final_data_figure_production_weather_h.pdf", sep = ""), height = 16, width = 20.50 )
png( paste( "Plots/file06_final_data_figure_production_weather_h.png", sep = ""), height = 16, width = 20.50, res = 600, units="in" )

# Defining the number of plots
# par( par(mfrow=c(1,2)),  mar=c(5.1, 5.1, 5.1, 2.1)  )
# par( par(mfrow=c(1,4)) )
# Defining layout
# layout(matrix(c(1,1,2,2,3,3,4,5), nrow = 4, ncol = 2, byrow = TRUE))
#par( par(mfrow=c(2,)) )

# Defining layout
# Matrix first
# layout_matrix <- matrix( c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3), nrow = 6, ncol = 5, byrow = FALSE)
layout_matrix <- matrix( 1, nrow = 12, ncol = 7, byrow = FALSE)

# Cells definition (column 1)
layout_matrix[c(1:3),   c(1:3)] <- 1
layout_matrix[c(4:6),   c(1:3)] <- 2
layout_matrix[c(7:9),   c(1:3)] <- 3
layout_matrix[c(10:12), c(1:3)] <- 4


# Cells definition (column 2)
layout_matrix[c(1:4),  c(4:5)] <- 5
layout_matrix[c(1:4),  c(6:7)] <- 6
layout_matrix[c(5:8),  c(4:5)] <- 7
layout_matrix[c(5:8),  c(6:7)] <- 8
layout_matrix[c(9:12), c(4:5)] <- 9
layout_matrix[c(9:12), c(6:7)] <- 10

# Setting layaout
layout(layout_matrix)




# Panel A
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$Counts,
     col = "darkblue", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Incidence (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# Panel A
# Plotting x_values vs y_values
lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$Counts,
      col = "darkblue", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Incidence (Total Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Counts",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label A
label_function(label_value = "A", label_cex = 4)




# Panel B
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$TEMP_C,
     # col = "darkblue", 
     col = "dodgerblue3", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Temperature (Averaged Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Degree (C)",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$TEMP_C,
      # col = "darkblue", 
      col = "dodgerblue3", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Temperature (Averaged Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Degree (C)",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label B
label_function(label_value = "B", label_cex = 4)




# Panel C
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$PRCP_CM,
     # col = "darkblue", 
     col = "seagreen4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Precipitaiton (Total Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "cm",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$PRCP_CM,
      # col = "darkblue", 
      col = "seagreen4", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Precipitaiton (Total Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "cm",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label C
label_function(label_value = "C", label_cex = 4)





# Panel D
# Plotting x_values vs y_values
plot(x = merged_river_plus_incidence$date,
     y = merged_river_plus_incidence$WDSP_MS,
     # col = "darkblue", 
     col = "dimgrey", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Wind Speed (Average Monthly)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     #xlim = c( - 0.1, 1.1 ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Meters per Second",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

lines(x = merged_river_plus_incidence$date,
      y = merged_river_plus_incidence$WDSP_MS,
      # col = "darkblue", 
      col = "dimgrey", 
      lwd = 5,
      pch = 16,
      type = "p",
      main = "Wind Speed (Average Monthly)",
      # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
      # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
      #xlim = c( - 0.1, 1.1 ),
      #ylim = c( - 0.1, 1.1 ),
      xlab = "",
      ylab = "Meters per Second",
      xaxt='n',
      cex = 1.55,
      cex.axis = 1.55,
      cex.lab = 1.55,
      cex.main = 1.55,
      cex.sub = 1.55
)


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  min(merged_river_plus_incidence$date), to  = max(merged_river_plus_incidence$date),  by = 180 )
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label D
label_function(label_value = "D", label_cex = 4)







# Panel E
counts_transformed <- merged_river_plus_incidence$Counts
variable_data <- merged_river_plus_incidence$TEMP_C

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

# Re-coding as 1
counts_transformed[ counts_transformed >=1 ] <- 1


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "dodgerblue3", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Binomial Fit (O vs 1 and up) vs Temperature (C)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts/Probability",
     xaxt='n',
     yaxt='n',     
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
glm_fit_TEMP_C <- glm( counts_transformed ~ variable_data, family = binomial) 
summary(glm_fit_TEMP_C)

# Prediction
variable_data_prediction     <- data.frame( variable_data = seq( variable_data_min, variable_data_max,len = 500 ) )
counts_transformed_predicted <- predict(glm_fit_TEMP_C, newdata = variable_data_prediction , type="response")


# Adding the corresponding regression fit.
lines(x = variable_data_prediction$variable_data, 
      y = counts_transformed_predicted, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topleft",
       inset= c(0.10, 0.10), 
       legend = c( "Data Points", "Binomial Fit"), 
       col = "black", 
       fill = c("dodgerblue3", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- 0
y_max_value <- 1
y_tlab  <- c(0, 0.25, 0.5, 0.75,  1)
y_lablist <- c(0, 0.25, 0.5, 0.75,  1)
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)

# Adding Counts
y_tlab  <- c(0.05, 0.95)
y_lablist <- c("0 Counts", "1 and up")
axis(2, at = y_tlab, tick = FALSE, labels = y_lablist, las = 1, cex.axis = 1.25)

# Label E
label_function(label_value = "E", label_cex = 4)






# Panel F
counts_transformed <- log(merged_river_plus_incidence$Counts)
variable_data <- merged_river_plus_incidence$TEMP_C

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

counts_transformed[ is.infinite(counts_transformed)] <- NA
# Making corresponding dates NA
variable_data[ is.na(counts_transformed)] <- NA

# Removing NA-s
counts_transformed <- counts_transformed[!is.na(counts_transformed)]
variable_data      <- variable_data[!is.na(variable_data)] 


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "dodgerblue3", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Zeroes Omitted: Log(Counts) vs Temperature (C)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_TEMP_C <- lm( counts_transformed ~ variable_data) 
summary(lm_fit_TEMP_C)
# lm fit extracts
a_TEMP_C <- lm_fit_TEMP_C$coefficients[2]
b_TEMP_C <- lm_fit_TEMP_C$coefficients[1]

# Summary statistics
r_TEMP_C      <- round( x = sqrt(summary(lm_fit_TEMP_C)$r.squared), digits = 2)
pvalue_TEMP_C <- round( x = summary(lm_fit_TEMP_C)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(variable_data)[1], range(variable_data)[2])
range_y <- a_TEMP_C * range_x + b_TEMP_C

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("dodgerblue3", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_TEMP_C))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_TEMP_C)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label F
label_function(label_value = "F", label_cex = 4)











# Panel G
counts_transformed <- merged_river_plus_incidence$Counts
variable_data <- merged_river_plus_incidence$PRCP_CM

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

# Re-coding as 1
counts_transformed[ counts_transformed >=1 ] <- 1


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "seagreen4", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Binomial Fit (O vs 1 and up) vs Precipitation (cm)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts/Probability",
     xaxt='n',
     yaxt='n',     
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
glm_fit_PRCP_CM <- glm( counts_transformed ~ variable_data, family = binomial) 
summary(glm_fit_PRCP_CM)

# Prediction
variable_data_prediction     <- data.frame( variable_data = seq( variable_data_min, variable_data_max, len = 500 ) )
counts_transformed_predicted <- predict(glm_fit_PRCP_CM, newdata = variable_data_prediction , type="response")


# Adding the corresponding regression fit.
lines(x = variable_data_prediction$variable_data, 
      y = counts_transformed_predicted, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topleft",
       inset= c(0.10, 0.10), 
       legend = c( "Data Points", "Binomial Fit"), 
       col = "black", 
       fill = c("seagreen4", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- 0
y_max_value <- 1
y_tlab  <- c(0, 0.25, 0.5, 0.75,  1)
y_lablist <- c(0, 0.25, 0.5, 0.75,  1)
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)

# Adding Counts
y_tlab  <- c(0.05, 0.95)
y_lablist <- c("0 Counts", "1 and up")
axis(2, at = y_tlab, tick = FALSE, labels = y_lablist, las = 1, cex.axis = 1.25)

# Label G
label_function(label_value = "G", label_cex = 4)








# Panel H
counts_transformed <- log(merged_river_plus_incidence$Counts)
variable_data <- merged_river_plus_incidence$PRCP_CM

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

counts_transformed[ is.infinite(counts_transformed)] <- NA
# Making corresponding dates NA
variable_data[ is.na(counts_transformed)] <- NA

# Removing NA-s
counts_transformed <- counts_transformed[!is.na(counts_transformed)]
variable_data      <- variable_data[!is.na(variable_data)] 


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "seagreen4", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Zeroes Omitted: Log(Counts) vs Precipitation (cm)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_PRCP_CM <- lm( counts_transformed ~ variable_data) 
summary(lm_fit_PRCP_CM)
# lm fit extracts
a_PRCP_CM <- lm_fit_PRCP_CM$coefficients[2]
b_PRCP_CM <- lm_fit_PRCP_CM$coefficients[1]

# Summary statistics
r_PRCP_CM      <- round( x = sqrt(summary(lm_fit_PRCP_CM)$r.squared), digits = 2)
pvalue_PRCP_CM <- round( x = summary(lm_fit_PRCP_CM)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(variable_data)[1], range(variable_data)[2])
range_y <- a_PRCP_CM * range_x + b_PRCP_CM

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("seagreen4", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_PRCP_CM))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_PRCP_CM)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label H
label_function(label_value = "H", label_cex = 4)









# Panel I
counts_transformed <- merged_river_plus_incidence$Counts
variable_data <- merged_river_plus_incidence$WDSP_MS

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

# Re-coding as 1
counts_transformed[ counts_transformed >=1 ] <- 1


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "dimgrey", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Binomial Fit (O vs 1 and up) vs Wind Speed (m/s)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts/Probability",
     xaxt='n',
     yaxt='n',     
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
glm_fit_WDSP_MS <- glm( counts_transformed ~ variable_data, family = binomial) 
summary(glm_fit_WDSP_MS)

# Prediction
variable_data_prediction     <- data.frame( variable_data = seq( variable_data_min, variable_data_max, len = 500 ) )
counts_transformed_predicted <- predict(glm_fit_WDSP_MS, newdata = variable_data_prediction , type="response")


# Adding the corresponding regression fit.
lines(x = variable_data_prediction$variable_data, 
      y = counts_transformed_predicted, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topleft",
       inset= c(0.10, 0.10), 
       legend = c( "Data Points", "Binomial Fit"), 
       col = "black", 
       fill = c("dimgrey", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 


# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)


# Y-axis
# Adding axis label
# labels FAQ -> https://stackoverflow.com/questions/26180178/r-boxplot-how-to-move-the-x-axis-label-down
y_min_value <- 0
y_max_value <- 1
y_tlab  <- c(0, 0.25, 0.5, 0.75,  1)
y_lablist <- c(0, 0.25, 0.5, 0.75,  1)
axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.25)

# Adding Counts
y_tlab  <- c(0.05, 0.95)
y_lablist <- c("0 Counts", "1 and up")
axis(2, at = y_tlab, tick = FALSE, labels = y_lablist, las = 1, cex.axis = 1.25)

# Label I
label_function(label_value = " I", label_cex = 4)





# Panel J
counts_transformed <- log(merged_river_plus_incidence$Counts)
variable_data <- merged_river_plus_incidence$WDSP_MS

variable_data_min <- min(variable_data)
variable_data_max <- max(variable_data)

counts_transformed[ is.infinite(counts_transformed)] <- NA
# Making corresponding dates NA
variable_data[ is.na(counts_transformed)] <- NA

# Removing NA-s
counts_transformed <- counts_transformed[!is.na(counts_transformed)]
variable_data      <- variable_data[!is.na(variable_data)] 


# Plotting x_values vs y_values
plot(x = variable_data,
     y = counts_transformed,
     col = "dimgrey", 
     lwd = 5,
     pch = 16,
     type = "p",
     main = "Zeroes Omitted: Log(Counts) vs Wind Speed (m/s)",
     # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
     # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
     xlim = c( variable_data_min, variable_data_max ),
     #ylim = c( - 0.1, 1.1 ),
     xlab = "",
     ylab = "Counts",
     xaxt='n',
     cex = 1.55,
     cex.axis = 1.55,
     cex.lab = 1.55,
     cex.main = 1.55,
     cex.sub = 1.55
)

# lm fit
lm_fit_WDSP_MS <- lm( counts_transformed ~ variable_data) 
summary(lm_fit_WDSP_MS)
# lm fit extracts
a_WDSP_MS <- lm_fit_WDSP_MS$coefficients[2]
b_WDSP_MS <- lm_fit_WDSP_MS$coefficients[1]

# Summary statistics
r_WDSP_MS      <- round( x = sqrt(summary(lm_fit_WDSP_MS)$r.squared), digits = 2)
pvalue_WDSP_MS <- round( x = summary(lm_fit_WDSP_MS)$coefficients[2,4], digits = 2)

# Range for the lm line
range_x <- c(range(variable_data)[1], range(variable_data)[2])
range_y <- a_WDSP_MS * range_x + b_WDSP_MS

# Adding the corresponding regression fit.
lines(x = range_x, 
      y = range_y, 
      type = "l",
      col="darkorange2", 
      lwd = 5, 
      lty = 2)

legend(x = "topright",
       inset= c(0.05, 0.05), 
       legend = c( "Data Points", "LM Fit"), 
       col = "black", 
       fill = c("dimgrey", "darkorange2"),   
       pt.cex = c(4, 2),
       # pch = c(19, 20),  
       cex = 1.5 ) 

legend(x = "topleft",
       inset= c(0.00, 0.025), 
       legend = c( as.expression(bquote(r ~ " = " ~ .(r_WDSP_MS))),
                   as.expression(bquote(p ~ "- value < " ~ .(pvalue_WDSP_MS)))), 
       cex = 1.5,
       pt.cex = 0.0001,
       box.col=0, 
       bty="n")


# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating positions where to put vertical bars.
x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                              by = (variable_data_max - variable_data_min)/20 )
x_indexes_to_display <- round(x = x_indexes_to_display, digits = 2)
# Actual lab elements
x_tlab <- x_indexes_to_display
# Actual lab labels
x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
# x_lablist  <- merged_river_plus_incidence$date
axis(1, at = x_tlab, labels = FALSE)
# axis(1, at = merged_river_plus_incidence$date, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)

# Label J
label_function(label_value = "J", label_cex = 4)



dev.off()
