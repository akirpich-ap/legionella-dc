# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2022.02.18. ask
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
variables_list_inc_temp_perc <- c("Year_Month_fixed", "Counts", "TEMP", "SLP", "WDSP", "PRCP" )
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


# Cross table
# RData
data_monthly_annually_path <- paste0("R_Data/data_monthly_annually.RData")
save( data_monthly_annually, file = data_monthly_annually_path )
# CSV table for data
data_monthly_annually_csv_path <- paste0("R_Output/data_monthly_annually.csv")
write.table( data_monthly_annually, file = data_monthly_annually_csv_path, sep = ",", quote = TRUE, row.names = FALSE)




# Fix 2022.01.15
# Computing summary statistics of the data
# month
summaries_monthly <- data.frame(       aggregate( Counts ~ month, merged_river_plus_incidence , mean ),
                                 sum = aggregate( Counts ~ month, merged_river_plus_incidence , sum )[, -1],       
                                 min = aggregate( Counts ~ month, merged_river_plus_incidence , min )[, -1],
                                 max = aggregate( Counts ~ month, merged_river_plus_incidence , max )[, -1],
                                 sd  = aggregate( Counts ~ month, merged_river_plus_incidence , sd )[, -1]   )
# Fixing the name "counts" to "mean"
names(summaries_monthly)[2] <- "mean"
# Creating factors
summaries_monthly$month <- factor(x = summaries_monthly$month, levels = substr(x= month.name, start = 1, stop = 3) )
# Sorting after accounting for factors
summaries_monthly <-  summaries_monthly[order(summaries_monthly$month), ]
summaries_monthly$month <- as.character(summaries_monthly$month)

# Extra check
sum(!summaries_monthly$sum == colSums(x = data_monthly_annually[,-1]))



# yearly
summaries_yearly  <- data.frame(       aggregate( Counts ~ year, merged_river_plus_incidence , mean ),
                                 sum = aggregate( Counts ~ year, merged_river_plus_incidence , sum )[, -1],                                       
                                 min = aggregate( Counts ~ year, merged_river_plus_incidence , min )[, -1],
                                 max = aggregate( Counts ~ year, merged_river_plus_incidence , max )[, -1],
                                 sd  = aggregate( Counts ~ year, merged_river_plus_incidence , sd )[, -1]   )
# Fixing the name "counts" to "mean"
names(summaries_yearly)[2] <- "mean"
# Extra check
sum(!summaries_yearly$sum == rowSums(x = data_monthly_annually[,-1]))


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
list_of_variables_dalecarlia_mcmillan <- c(variables_list_inc_temp_perc, variables_list_dalecarlia_mcmillan)[ -which( c(variables_list_inc_temp_perc, variables_list_dalecarlia_mcmillan) %in% c("Year_Month_fixed", "YEAR_MONTH") ) ]
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
pdf( paste( "Plots/file06_temperature_precipitation_data_plots_a.pdf", sep = ""), height = 20, width = 10 )

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
     y = merged_river_plus_incidence$TEMP,
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
     y = merged_river_plus_incidence$PRCP,
     # col = "darkblue", 
     col = "seagreen4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Total Precipitaiton Monthly (In)",
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
pdf( paste( "Plots/file06_temperature_precipitation_data_plots_b.pdf", sep = ""), height = 10, width = 14 )

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
     y = merged_river_plus_incidence$TEMP,
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
     y = merged_river_plus_incidence$PRCP,
     # col = "darkblue", 
     col = "seagreen4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Total Precipitaiton Monthly (In)",
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
pdf( paste( "Plots/file06_temperature_precipitation_data_plots_c.pdf", sep = ""), height = 8.5, width = 16.5 )

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
     y = merged_river_plus_incidence$TEMP,
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
     y = merged_river_plus_incidence$PRCP,
     # col = "darkblue", 
     col = "seagreen4", 
     lwd = 3,
     pch = 16,
     type = "l",
     main = "Total Precipitaiton Monthly (In)",
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









