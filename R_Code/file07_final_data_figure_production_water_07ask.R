# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2022.10.18. ask
rm(list=ls(all=TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation
# options(scipen=20)


# Remove duplicate rows of the data frame
library(dplyr)

# To convert dates from text to dates
library(lubridate)

# Library to put names to proper case
library(stringr)


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
variables_list_dalecarlia_mcmillan   <- intersect(variables_list_dalecarlia, variables_list_mcmillan)



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




# Fix 2022.10.18.
variables_list_river_adjusted <- variables_list_river
variables_list_river_adjusted[1] <- "Counts"

variables_list_dalecarlia_mcmillan_adjusted <- variables_list_dalecarlia_mcmillan
variables_list_dalecarlia_mcmillan_adjusted[1] <- "Counts"


# Creating frames to loop over
variables_list_river_adjusted_frame <- data.frame(var_code = variables_list_river_adjusted,
                                                  var_name = str_to_title(string = gsub( pattern = ".", replacement = " ", fixed = TRUE, x = variables_list_river_adjusted ), locale = "en"), 
                                                  var_unit = variables_list_river_adjusted,
                                                  var_table_position = rep("topright", length(variables_list_river_adjusted)))
variables_list_river_adjusted_frame$var_name[2] <- "pH"

variables_list_river_adjusted_frame$var_unit[1]  <- ""
variables_list_river_adjusted_frame$var_unit[2]  <- ""
variables_list_river_adjusted_frame$var_unit[3]  <- "ppm"
variables_list_river_adjusted_frame$var_unit[4]  <- "ppm"
variables_list_river_adjusted_frame$var_unit[5]  <- "ppb"

variables_list_river_adjusted_frame$var_unit[6]  <- "ppb"
variables_list_river_adjusted_frame$var_unit[7]  <- "ppb"
variables_list_river_adjusted_frame$var_unit[8]  <- "ppb"
variables_list_river_adjusted_frame$var_unit[9]  <- "ppb"
variables_list_river_adjusted_frame$var_unit[10] <- "NTU"

variables_list_river_adjusted_frame$var_unit[11] <- "ppb"
variables_list_river_adjusted_frame$var_unit[12] <- "MPN/100mL"
variables_list_river_adjusted_frame$var_unit[13] <- "ppb"
variables_list_river_adjusted_frame$var_unit[14] <- "ppb"


# Positions
variables_list_river_adjusted_frame$var_table_position[13] <- "bottomright"





# Creating frames to loop over
variables_list_dalecarlia_mcmillan_adjusted_frame <- data.frame(var_code = variables_list_dalecarlia_mcmillan_adjusted,
                                                        var_name = str_to_title(string = gsub( pattern = ".", replacement = " ", fixed = TRUE, x = variables_list_dalecarlia_mcmillan_adjusted ), locale = "en"),
                                                        var_unit = variables_list_dalecarlia_mcmillan_adjusted,
                                                        var_table_position = rep("topright", length(variables_list_dalecarlia_mcmillan_adjusted)))
variables_list_dalecarlia_mcmillan_adjusted_frame$var_name[2]  <- "pH"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_name[12] <- "Total Coliform (Positive)"

variables_list_dalecarlia_mcmillan_adjusted_frame$var_name[2] <- "pH"

variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[1]  <- ""
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[2]  <- ""
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[3]  <- "ppm"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[4]  <- "ppm"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[5]  <- "ppb"

variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[6]  <- "ppb"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[7]  <- "ppb"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[8]  <- "ppb"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[9]  <- "ppb"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[10] <- "NTU"

variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[11] <- "ppb"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[12] <- "MPN/100mL"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[13] <- "CFU/mL"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[14] <- "ppb"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[15] <- "ppb"
variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[16] <- "ppm"




# Positions
variables_list_dalecarlia_mcmillan_adjusted_frame$var_table_position[c(11,16)] <- "bottomright"






# Fix 2022.10.13
# Subsets for exports.

variables_list_river_adjusted_frame_subset <- variables_list_river_adjusted_frame[,c(2:3)]
names(variables_list_river_adjusted_frame_subset) <- c("Variable", "Units")

variables_list_dalecarlia_mcmillan_adjusted_frame_subset <- variables_list_dalecarlia_mcmillan_adjusted_frame[,c(2:3)]
names(variables_list_dalecarlia_mcmillan_adjusted_frame_subset) <- c("Variable", "Units")


# csv
# Creating paths
variables_list_river_adjusted_frame_subset_csv_path                <- paste("R_Output/variables_list_river_adjusted_frame_subset.csv", sep = "")
variables_list_dalecarlia_mcmillan_adjusted_frame_subset_csv_path  <- paste("R_Output/variables_list_dalecarlia_mcmillan_adjusted_frame_subset.csv", sep = "")
# Actual exports
write.table(x = variables_list_river_adjusted_frame_subset,               file = variables_list_river_adjusted_frame_subset_csv_path, sep = ",", quote = TRUE, row.names = FALSE)
write.table(x = variables_list_dalecarlia_mcmillan_adjusted_frame_subset, file = variables_list_dalecarlia_mcmillan_adjusted_frame_subset_csv_path, sep = ",", quote = TRUE, row.names = FALSE)


# RData
# Creating paths
variables_list_river_adjusted_frame_subset_rdata_path                <- paste("R_Data/variables_list_river_adjusted_frame_subset.RData", sep = "")
variables_list_dalecarlia_mcmillan_adjusted_frame_subset_rdata_path  <- paste("R_Data/variables_list_dalecarlia_mcmillan_adjusted_frame_subset.RData", sep = "")
# Actual exports
save(x = variables_list_river_adjusted_frame_subset,               file = variables_list_river_adjusted_frame_subset_rdata_path)
save(x = variables_list_dalecarlia_mcmillan_adjusted_frame_subset, file = variables_list_dalecarlia_mcmillan_adjusted_frame_subset_rdata_path)



# Fix 2022.10.20.
# Expanding variable set
table_extension <- data.frame( var_code = c("TEMP_C", "PRCP_CM", "WDSP_MS" ),
                               var_name = c("Temperature", "Precipitation", "Wind Speed" ),
                               var_unit = c("C", "cm", "m/s" ) )
                               
# Generating extended sets
variables_list_river_adjusted_frame_modified <- rbind(table_extension,
                                                      variables_list_river_adjusted_frame[-1, -dim(variables_list_river_adjusted_frame)[2]] )

variables_list_dalecarlia_mcmillan_adjusted_frame_modified <- rbind(table_extension,
                                                                    variables_list_dalecarlia_mcmillan_adjusted_frame[-1, -dim(variables_list_dalecarlia_mcmillan_adjusted_frame)[2]])
# Checking the summaries
variables_list_river_adjusted_frame_modified
variables_list_dalecarlia_mcmillan_adjusted_frame_modified


# Exporting
# RData
# Creating paths
variables_list_river_adjusted_frame_modified_rdata_path               <- paste("R_Data/variables_list_river_adjusted_frame_modified.RData", sep = "")
variables_list_dalecarlia_mcmillan_adjusted_frame_modified_rdata_path <- paste("R_Data/variables_list_dalecarlia_mcmillan_adjusted_frame_modified.RData", sep = "")
# Actual exports
save(x = variables_list_river_adjusted_frame_modified,               file = variables_list_river_adjusted_frame_modified_rdata_path)
save(x = variables_list_dalecarlia_mcmillan_adjusted_frame_modified, file = variables_list_dalecarlia_mcmillan_adjusted_frame_modified_rdata_path)





# The entire dataset including zeroes

# Looping to plot -> variables_list_river_adjusted

for ( current_index in c(2:dim(variables_list_river_adjusted_frame)[1]) )
{

  # Debudding step
  # current_index <- 3
  
  # getting summaries of interest
  counts_data <- merged_river_plus_incidence$Counts
  
  variable_data <- merged_river_plus_incidence[,variables_list_river_adjusted_frame$var_code[current_index] ]
  variable_name <- variables_list_river_adjusted_frame$var_name[current_index]
  variable_unit <- variables_list_river_adjusted_frame$var_unit[current_index]
  
  

  # Fix 2022.10.11.
  # Generating pdf output.
  # pdf( paste( "Plots/file07_final_data_figure_production_water_river_",   variable_name, ".pdf", sep = ""), height = 6, width = 16 )
  png( paste( "Plots/file07_final_data_figure_production_water_river_",   variable_name, ".png", sep = ""), height = 6, width = 16, res = 600, units = "in" )
  
  # Defining layout
  # Matrix first
  layout_matrix <- matrix( 1, nrow = 6, ncol = 5, byrow = FALSE )
  
  # Cells definition (column 1)
  layout_matrix[c(1:3),   c(1:3)] <- 1
  layout_matrix[c(4:6),   c(1:3)] <- 2
  
  # Cells definition (column 2)
  layout_matrix[c(1:6),   c(4:5)] <- 3
  
  # Setting layaout
  layout(layout_matrix)
  
  
  
  # Panel A
  # Plotting x_values vs y_values
  plot(x = merged_river_plus_incidence$date,
       y = counts_data,
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
        y = counts_data,
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
       y = variable_data,
       # col = "darkblue", 
       col = "dodgerblue3", 
       lwd = 3,
       pch = 16,
       type = "l",
       main = paste( variable_name, sep ="" ),
       # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
       # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = paste( variable_unit, sep ="" ),
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )
  
  lines(x = merged_river_plus_incidence$date,
        y = variable_data,
        # col = "darkblue", 
        col = "dodgerblue3", 
        lwd = 5,
        pch = 16,
        type = "p",
        main = paste( variable_name, sep ="" ),
        # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
        # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
        #xlim = c( - 0.1, 1.1 ),
        #ylim = c( - 0.1, 1.1 ),
        xlab = "",
        ylab = paste( variable_unit, sep ="" ),
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
  counts_transformed <- log(counts_data + 1)
  
  
  # lm fit
  lm_fit_variable_data <- lm( counts_transformed ~ variable_data) 
  summary(lm_fit_variable_data)
  # lm fit extracts
  a_variable_data <- lm_fit_variable_data$coefficients[2]
  b_variable_data <- lm_fit_variable_data$coefficients[1]
  
  # Summary statistics
  r_variable_data      <- round( x = sqrt(summary(lm_fit_variable_data)$r.squared), digits = 2)
  pvalue_variable_data <- round( x = summary(lm_fit_variable_data)$coefficients[2,4], digits = 2)
  
  # Range for the lm line
  range_x <- c(range(variable_data)[1], range(variable_data)[2])
  range_y <- a_variable_data * range_x + b_variable_data
  
  
  # Plotting x_values vs y_values
  plot(x = variable_data,
       y = counts_transformed,
       # col = "dodgerblue3", 
       col = "darkcyan", 
       lwd = 5,
       pch = 16,
       type = "p",
       main = paste("Log(Counts+1) vs ",  variable_name, " (", variable_unit, ")", sep ="" ),
       # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
       ylim = c( min(c(counts_transformed, range_y)), max(c(counts_transformed, range_y)) ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = "Log(Counts+1)",
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )

  
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
         fill = c("darkcyan", "red"),   
         pt.cex = c(4, 2),
         # pch = c(19, 20),  
         cex = 1.5 ) 
  
  legend(x = "topleft",
         inset= c(0.00, 0.025), 
         legend = c( as.expression(bquote(r ~ " = " ~ .(r_variable_data))),
                     as.expression(bquote(p ~ "- value < " ~ .(pvalue_variable_data)))), 
         cex = 1.5,
         pt.cex = 0.0001,
         box.col=0, 
         bty="n")
  
  
  # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
  # Creating positions where to put vertical bars.
  x_indexes_to_display <-  seq( from  =  min(variable_data), to  = max(variable_data),  
                                by = (range(variable_data)[2] - range(variable_data)[1])/20 )
  x_indexes_to_display <- round(x = x_indexes_to_display, digits = 3)
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
  

  
  dev.off()
  
  
  # Exporting Corresponding label text.
  sink( paste( "R_Output/file07_final_data_figure_production_water_river_",   variable_name, ".txt", sep ="") ) 
  cat( "The summaries of the A) reported incidence, B) river ", variable_name, " (",variable_unit,") together with C) Log(Counts+1) vs river ",  variable_name, " (", variable_unit, ").",
         "The panel C also contains the linear regression fitted line of Log(Counts+1) vs river ",  variable_name, ".",
         "The corresponding linear regression statistics (correlation estimate and p-value for the statistical test of slope to be zero) are also provided.", sep = "") 
  sink()
  



# End of -> for ( current_index in c(2:dim(variables_list_river_adjusted_frame)[1]) )
}  
  










# Looping to plot -> variables_list_dalecarlia_mcmillan_adjusted

for ( current_index in c(2:dim(variables_list_dalecarlia_mcmillan_adjusted_frame)[1]) )
{
  
  # Debudding step
  # current_index <- 3
  
  # getting summaries of interest
  counts_data <- merged_dalecarlia_mcmillan_plus_incidence$Counts
  
  variable_data <- merged_dalecarlia_mcmillan_plus_incidence[,variables_list_dalecarlia_mcmillan_adjusted_frame$var_code[current_index] ]
  variable_name <- variables_list_dalecarlia_mcmillan_adjusted_frame$var_name[current_index]
  variable_unit <- variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[current_index]
  
  
  
  # Fix 2022.10.11.
  # Generating pdf output.
  # pdf( paste( "Plots/file07_final_data_figure_production_water_dalecarlia_mcmillan_",   variable_name, ".pdf", sep = ""), height = 6, width = 16 )
  png( paste( "Plots/file07_final_data_figure_production_water_dalecarlia_mcmillan_",   variable_name, ".png", sep = ""), height = 6, width = 16, res = 600, units="in" )
  
  # Defining layout
  # Matrix first
  layout_matrix <- matrix( 1, nrow = 6, ncol = 5, byrow = FALSE)
  
  # Cells definition (column 1)
  layout_matrix[c(1:3),   c(1:3)] <- 1
  layout_matrix[c(4:6),   c(1:3)] <- 2
  
  # Cells definition (column 2)
  layout_matrix[c(1:6),   c(4:5)] <- 3
  
  # Setting layaout
  layout(layout_matrix)
  
  
  
  # Panel A
  # Plotting x_values vs y_values
  plot(x = merged_dalecarlia_mcmillan_plus_incidence$date,
       y = counts_data,
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
  lines(x = merged_dalecarlia_mcmillan_plus_incidence$date,
        y = counts_data,
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
  x_indexes_to_display <-  seq( from  =  min(merged_dalecarlia_mcmillan_plus_incidence$date), to  = max(merged_dalecarlia_mcmillan_plus_incidence$date),  by = 180 )
  # Actual lab elements
  x_tlab <- x_indexes_to_display
  # Actual lab labels
  x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
  # x_lablist  <- merged_dalecarlia_mcmillan_plus_incidence$date
  axis(1, at = x_tlab, labels = FALSE)
  # axis(1, at = merged_dalecarlia_mcmillan_plus_incidence$date, labels = FALSE)
  text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)
  
  # Label A
  label_function(label_value = "A", label_cex = 4)
  
  
  
  
  # Panel B
  # Plotting x_values vs y_values
  plot(x = merged_dalecarlia_mcmillan_plus_incidence$date,
       y = variable_data,
       # col = "darkblue", 
       col = "dodgerblue3", 
       lwd = 3,
       pch = 16,
       type = "l",
       main = paste( variable_name, sep ="" ),
       # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
       # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = paste( variable_unit, sep ="" ),
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )
  
  lines(x = merged_dalecarlia_mcmillan_plus_incidence$date,
        y = variable_data,
        # col = "darkblue", 
        col = "dodgerblue3", 
        lwd = 5,
        pch = 16,
        type = "p",
        main = paste( variable_name, sep ="" ),
        # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
        # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
        #xlim = c( - 0.1, 1.1 ),
        #ylim = c( - 0.1, 1.1 ),
        xlab = "",
        ylab = paste( variable_unit, sep ="" ),
        xaxt='n',
        cex = 1.55,
        cex.axis = 1.55,
        cex.lab = 1.55,
        cex.main = 1.55,
        cex.sub = 1.55
  )
  
  
  # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
  # Creating positions where to put vertical bars.
  x_indexes_to_display <-  seq( from  =  min(merged_dalecarlia_mcmillan_plus_incidence$date), to  = max(merged_dalecarlia_mcmillan_plus_incidence$date),  by = 180 )
  # Actual lab elements
  x_tlab <- x_indexes_to_display
  # Actual lab labels
  x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
  # x_lablist  <- merged_dalecarlia_mcmillan_plus_incidence$date
  axis(1, at = x_tlab, labels = FALSE)
  # axis(1, at = merged_dalecarlia_mcmillan_plus_incidence$date, labels = FALSE)
  text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)
  
  # Label B
  label_function(label_value = "B", label_cex = 4)
  
  
  
  
  
  # Panel C
  counts_transformed <- log(counts_data + 1)
  
  # lm fit
  lm_fit_variable_data <- lm( counts_transformed ~ variable_data ) 
  summary(lm_fit_variable_data)
  # lm fit extracts
  a_variable_data <- lm_fit_variable_data$coefficients[2]
  b_variable_data <- lm_fit_variable_data$coefficients[1]
  
  # Summary statistics
  r_variable_data      <- round( x = sqrt(summary(lm_fit_variable_data)$r.squared), digits = 2)
  pvalue_variable_data <- round( x = summary(lm_fit_variable_data)$coefficients[2,4], digits = 2)
  
  # Range for the lm line
  range_x <- c(range(variable_data)[1], range(variable_data)[2])
  range_y <- a_variable_data * range_x + b_variable_data

  
  # Plotting x_values vs y_values
  plot(x = variable_data,
       y = counts_transformed,
       # col = "dodgerblue3", 
       col = "darkcyan", 
       lwd = 5,
       pch = 16,
       type = "p",
       main = paste("Log(Counts+1) vs ",  variable_name, " (", variable_unit, ")", sep ="" ),
       # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
       ylim = c( min(c(counts_transformed, range_y)), max(c(counts_transformed, range_y)) ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = "Log(Counts+1)",
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )
  
    
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
         fill = c("darkcyan", "red"),   
         pt.cex = c(4, 2),
         # pch = c(19, 20),  
         cex = 1.5 ) 
  
  legend(x = "topleft",
         inset= c(0.00, 0.025), 
         legend = c( as.expression(bquote(r ~ " = " ~ .(r_variable_data))),
                     as.expression(bquote(p ~ "- value < " ~ .(pvalue_variable_data)))), 
         cex = 1.5,
         pt.cex = 0.0001,
         box.col=0, 
         bty="n")
  
  
  # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
  # Creating positions where to put vertical bars.
  x_indexes_to_display <-  seq( from  =  min(variable_data), to  = max(variable_data),  
                                by = (range(variable_data)[2] - range(variable_data)[1])/20 )
  x_indexes_to_display <- round(x = x_indexes_to_display, digits = 3)
  # Actual lab elements
  x_tlab <- x_indexes_to_display
  # Actual lab labels
  x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
  # x_lablist  <- merged_dalecarlia_mcmillan_plus_incidence$date
  axis(1, at = x_tlab, labels = FALSE)
  # axis(1, at = merged_dalecarlia_mcmillan_plus_incidence$date, labels = FALSE)
  text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)
  
  # Label C
  label_function(label_value = "C", label_cex = 4)
  
  
  
  dev.off()
  
  
  # Exporting Corresponding label text.
  sink( paste( "R_Output/file07_final_data_figure_production_water_dalecarlia_mcmillan_",   variable_name, ".txt", sep ="") ) 
  cat( "The summaries of the A) reported incidence, B) treatment plant ", variable_name, " (",variable_unit,") together with C) Log(Counts+1) vs treatment plant ",  variable_name, " (", variable_unit, ").",
         "The panel C also contains the linear regression fitted line of Log(Counts+1) vs treatment plant ",  variable_name, ".",
         "The corresponding linear regression statistics (correlation estimate and p-value for the statistical test of slope to be zero) are also provided.", sep = "") 
  sink()
  
  

  # End of -> for ( current_index in c(2:dim(variables_list_dalecarlia_mcmillan_adjusted_frame)[1]) )
}  





































# The subset of the dataset without zeroes

# Looping to plot -> variables_list_river_adjusted

for ( current_index in c(2:dim(variables_list_river_adjusted_frame)[1]) )
{
  
  # Debudding step
  # current_index <- 3
  
  # getting summaries of interest
  counts_data <- merged_river_plus_incidence$Counts
  
  variable_data <- merged_river_plus_incidence[,variables_list_river_adjusted_frame$var_code[current_index] ]
  variable_name <- variables_list_river_adjusted_frame$var_name[current_index]
  variable_unit <- variables_list_river_adjusted_frame$var_unit[current_index]
  
  
  
  # Fix 2022.10.11.
  # Generating pdf output.
  # pdf( paste( "Plots/file07_final_data_figure_production_water_river_", variable_name, "_no_zeroes.pdf", sep = ""), height = 6, width = 16 )
  png( paste( "Plots/file07_final_data_figure_production_water_river_", variable_name, "_no_zeroes.png", sep = ""), height = 6, width = 16, res = 600, units="in" )
  
  # Defining layout
  # Matrix first
  layout_matrix <- matrix( 1, nrow = 6, ncol = 5, byrow = FALSE)
  
  # Cells definition (column 1)
  layout_matrix[c(1:3),   c(1:3)] <- 1
  layout_matrix[c(4:6),   c(1:3)] <- 2
  
  # Cells definition (column 2)
  layout_matrix[c(1:6),   c(4:5)] <- 3
  
  # Setting layaout
  layout(layout_matrix)
  
  
  
  # Panel A
  # Plotting x_values vs y_values
  plot(x = merged_river_plus_incidence$date,
       y = counts_data,
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
        y = counts_data,
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
       y = variable_data,
       # col = "darkblue", 
       col = "dodgerblue3", 
       lwd = 3,
       pch = 16,
       type = "l",
       main = paste( variable_name, sep ="" ),
       # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
       # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = paste( variable_unit, sep ="" ),
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )
  
  lines(x = merged_river_plus_incidence$date,
        y = variable_data,
        # col = "darkblue", 
        col = "dodgerblue3", 
        lwd = 5,
        pch = 16,
        type = "p",
        main = paste( variable_name, sep ="" ),
        # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
        # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
        #xlim = c( - 0.1, 1.1 ),
        #ylim = c( - 0.1, 1.1 ),
        xlab = "",
        ylab = paste( variable_unit, sep ="" ),
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
  counts_transformed <- log(counts_data)
  counts_transformed[ is.infinite(counts_transformed)] <- NA
  # Making corresponding dates NA
  variable_data[ is.na(counts_transformed)] <- NA
  
  # Removing NA-s
  counts_transformed <- counts_transformed[!is.na(counts_transformed)]
  variable_data      <- variable_data[!is.na(variable_data)] 

  
  # lm fit
  lm_fit_variable_data <- lm( counts_transformed ~ variable_data) 
  summary(lm_fit_variable_data)
  # lm fit extracts
  a_variable_data <- lm_fit_variable_data$coefficients[2]
  b_variable_data <- lm_fit_variable_data$coefficients[1]
  
  # Summary statistics
  r_variable_data      <- round( x = sqrt(summary(lm_fit_variable_data)$r.squared), digits = 2)
  pvalue_variable_data <- round( x = summary(lm_fit_variable_data)$coefficients[2,4], digits = 2)
  
  # Range for the lm line
  range_x <- c(range(variable_data)[1], range(variable_data)[2])
  range_y <- a_variable_data * range_x + b_variable_data
  
    
  # Plotting x_values vs y_values
  plot(x = variable_data,
       y = counts_transformed,
       # col = "dodgerblue3", 
       col = "darkcyan", 
       lwd = 5,
       pch = 16,
       type = "p",
       main = paste("Zeroes Omitted: Log(Counts) vs ",  variable_name, " (", variable_unit, ")", sep ="" ),
       # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
       ylim = c( min(c(counts_transformed, range_y)), max(c(counts_transformed, range_y)) ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = "Log(Counts)",
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )
  
  
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
         fill = c("darkcyan", "darkorange2"),   
         pt.cex = c(4, 2),
         # pch = c(19, 20),  
         cex = 1.5 ) 
  
  legend(x = "topleft",
         inset= c(0.00, 0.025), 
         legend = c( as.expression(bquote(r ~ " = " ~ .(r_variable_data))),
                     as.expression(bquote(p ~ "- value < " ~ .(pvalue_variable_data)))), 
         cex = 1.5,
         pt.cex = 0.0001,
         box.col=0, 
         bty="n")
  
  
  # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
  # Creating positions where to put vertical bars.
  x_indexes_to_display <-  seq( from  =  min(variable_data), to  = max(variable_data),  
                                by = (range(variable_data)[2] - range(variable_data)[1])/20 )
  x_indexes_to_display <- round(x = x_indexes_to_display, digits = 3)
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
  
  
  
  dev.off()
  
  # Exporting Corresponding label text.
  sink( paste( "R_Output/file07_final_data_figure_production_water_river_",   variable_name, "_no_zeroes.txt", sep ="") ) 
  cat( "The summaries after zero counts are omitted of the A) reported incidence, B) river ", variable_name, " (",variable_unit,") together with C) Log(Counts) vs river ",  variable_name, " (", variable_unit, ").",
         "The panel C also contains the linear regression fitted line of Log(Counts) vs river ",  variable_name, ".",
         "The corresponding linear regression statistics (correlation estimate and p-value for the statistical test of slope to be zero) are also provided.", sep = "") 
  sink()
  
  
  
  
  # End of -> for ( current_index in c(2:dim(variables_list_river_adjusted_frame)[1]) )
}  











# Looping to plot -> variables_list_dalecarlia_mcmillan_adjusted

for ( current_index in c(2:dim(variables_list_dalecarlia_mcmillan_adjusted_frame)[1]) )
{
  
  # Debudding step
  # current_index <- 3
  
  # getting summaries of interest
  counts_data <- merged_dalecarlia_mcmillan_plus_incidence$Counts
  
  variable_data <- merged_dalecarlia_mcmillan_plus_incidence[,variables_list_dalecarlia_mcmillan_adjusted_frame$var_code[current_index] ]
  variable_name <- variables_list_dalecarlia_mcmillan_adjusted_frame$var_name[current_index]
  variable_unit <- variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[current_index]
  
  
  
  # Fix 2022.10.11.
  # Generating pdf output.
  # pdf( paste( "Plots/file07_final_data_figure_production_water_dalecarlia_mcmillan_", variable_name, "_no_zeroes.pdf", sep = ""), height = 6, width = 16 )
  png( paste( "Plots/file07_final_data_figure_production_water_dalecarlia_mcmillan_", variable_name, "_no_zeroes.png", sep = ""), height = 6, width = 16, res = 600, units="in" )
  
  # Defining layout
  # Matrix first
  layout_matrix <- matrix( 1, nrow = 6, ncol = 5, byrow = FALSE)
  
  # Cells definition (column 1)
  layout_matrix[c(1:3),   c(1:3)] <- 1
  layout_matrix[c(4:6),   c(1:3)] <- 2
  
  # Cells definition (column 2)
  layout_matrix[c(1:6),   c(4:5)] <- 3
  
  # Setting layaout
  layout(layout_matrix)
  
  
  
  # Panel A
  # Plotting x_values vs y_values
  plot(x = merged_dalecarlia_mcmillan_plus_incidence$date,
       y = counts_data,
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
  lines(x = merged_dalecarlia_mcmillan_plus_incidence$date,
        y = counts_data,
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
  x_indexes_to_display <-  seq( from  =  min(merged_dalecarlia_mcmillan_plus_incidence$date), to  = max(merged_dalecarlia_mcmillan_plus_incidence$date),  by = 180 )
  # Actual lab elements
  x_tlab <- x_indexes_to_display
  # Actual lab labels
  x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
  # x_lablist  <- merged_dalecarlia_mcmillan_plus_incidence$date
  axis(1, at = x_tlab, labels = FALSE)
  # axis(1, at = merged_dalecarlia_mcmillan_plus_incidence$date, labels = FALSE)
  text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)
  
  # Label A
  label_function(label_value = "A", label_cex = 4)
  
  
  
  
  # Panel B
  # Plotting x_values vs y_values
  plot(x = merged_dalecarlia_mcmillan_plus_incidence$date,
       y = variable_data,
       # col = "darkblue", 
       col = "dodgerblue3", 
       lwd = 3,
       pch = 16,
       type = "l",
       main = paste( variable_name, sep ="" ),
       # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
       # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = paste( variable_unit, sep ="" ),
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )
  
  lines(x = merged_dalecarlia_mcmillan_plus_incidence$date,
        y = variable_data,
        # col = "darkblue", 
        col = "dodgerblue3", 
        lwd = 5,
        pch = 16,
        type = "p",
        main = paste( variable_name, sep ="" ),
        # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
        # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
        #xlim = c( - 0.1, 1.1 ),
        #ylim = c( - 0.1, 1.1 ),
        xlab = "",
        ylab = paste( variable_unit, sep ="" ),
        xaxt='n',
        cex = 1.55,
        cex.axis = 1.55,
        cex.lab = 1.55,
        cex.main = 1.55,
        cex.sub = 1.55
  )
  
  
  # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
  # Creating positions where to put vertical bars.
  x_indexes_to_display <-  seq( from  =  min(merged_dalecarlia_mcmillan_plus_incidence$date), to  = max(merged_dalecarlia_mcmillan_plus_incidence$date),  by = 180 )
  # Actual lab elements
  x_tlab <- x_indexes_to_display
  # Actual lab labels
  x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
  # x_lablist  <- merged_dalecarlia_mcmillan_plus_incidence$date
  axis(1, at = x_tlab, labels = FALSE)
  # axis(1, at = merged_dalecarlia_mcmillan_plus_incidence$date, labels = FALSE)
  text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)
  
  # Label B
  label_function(label_value = "B", label_cex = 4)
  
  
  
  
  
  # Panel C
  counts_transformed <- log(counts_data)
  counts_transformed[ is.infinite(counts_transformed)] <- NA
  # Making corresponding dates NA
  variable_data[ is.na(counts_transformed)] <- NA
  
  # Removing NA-s
  counts_transformed <- counts_transformed[!is.na(counts_transformed)]
  variable_data      <- variable_data[!is.na(variable_data)] 
  
 
  # lm fit
  lm_fit_variable_data <- lm( counts_transformed ~ variable_data) 
  summary(lm_fit_variable_data)
  # lm fit extracts
  a_variable_data <- lm_fit_variable_data$coefficients[2]
  b_variable_data <- lm_fit_variable_data$coefficients[1]
  
  # Summary statistics
  r_variable_data      <- round( x = sqrt(summary(lm_fit_variable_data)$r.squared), digits = 2)
  pvalue_variable_data <- round( x = summary(lm_fit_variable_data)$coefficients[2,4], digits = 2)
  
  # Range for the lm line
  range_x <- c(range(variable_data)[1], range(variable_data)[2])
  range_y <- a_variable_data * range_x + b_variable_data
  
  
  # Plotting x_values vs y_values
  plot(x = variable_data,
       y = counts_transformed,
       # col = "dodgerblue3", 
       col = "darkcyan", 
       lwd = 5,
       pch = 16,
       type = "p",
       main = paste("Zeroes Omitted: Log(Counts) vs ",  variable_name, " (", variable_unit, ")", sep ="" ),
       # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
       ylim = c( min(c(counts_transformed, range_y)), max(c(counts_transformed, range_y)) ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = "Log(Counts)",
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )
  
  
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
         fill = c("darkcyan", "darkorange2"),   
         pt.cex = c(4, 2),
         # pch = c(19, 20),  
         cex = 1.5 ) 
  
  legend(x = "topleft",
         inset= c(0.00, 0.025), 
         legend = c( as.expression(bquote(r ~ " = " ~ .(r_variable_data))),
                     as.expression(bquote(p ~ "- value < " ~ .(pvalue_variable_data)))), 
         cex = 1.5,
         pt.cex = 0.0001,
         box.col=0, 
         bty="n")
  
  
  # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
  # Creating positions where to put vertical bars.
  x_indexes_to_display <-  seq( from  =  min(variable_data), to  = max(variable_data),  
                                by = (range(variable_data)[2] - range(variable_data)[1])/20 )
  x_indexes_to_display <- round(x = x_indexes_to_display, digits = 3)
  # Actual lab elements
  x_tlab <- x_indexes_to_display
  # Actual lab labels
  x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
  # x_lablist  <- merged_dalecarlia_mcmillan_plus_incidence$date
  axis(1, at = x_tlab, labels = FALSE)
  # axis(1, at = merged_dalecarlia_mcmillan_plus_incidence$date, labels = FALSE)
  text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)
  
  # Label C
  label_function(label_value = "C", label_cex = 4)
  
  
  
  dev.off()
  

  # Exporting Corresponding label text.
  sink( paste( "R_Output/file07_final_data_figure_production_water_dalecarlia_mcmillan_",   variable_name, "_no_zeroes.txt", sep ="") ) 
  cat( "The summaries after zero counts are omitted of the A) reported incidence, B) treatment plant ", variable_name, " (",variable_unit,") together with C) Log(Counts) vs treatment plant ",  variable_name, " (", variable_unit, ").",
         "The panel C also contains the linear regression fitted line of Log(Counts) vs treatment plant ",  variable_name, ".",
         "The corresponding linear regression statistics (correlation estimate and p-value for the statistical test of slope to be zero) are also provided.", sep = "") 
  sink()
  
  
  
  # End of -> for ( current_index in c(2:dim(variables_list_dalecarlia_mcmillan_adjusted_frame)[1]) )
}  










# Fix 2022.10.18. ask
# The subset of the dataset without zeroes2

# Looping to plot -> variables_list_river_adjusted

for ( current_index in c(2:dim(variables_list_river_adjusted_frame)[1]) )
{
  
  # Debugging step
  # current_index <- 3
  
  # getting summaries of interest
  counts_data <- merged_river_plus_incidence$Counts
  
  variable_data      <- merged_river_plus_incidence[,variables_list_river_adjusted_frame$var_code[current_index] ]
  variable_name      <- variables_list_river_adjusted_frame$var_name[current_index]
  variable_unit      <- variables_list_river_adjusted_frame$var_unit[current_index]
  var_table_position <- variables_list_river_adjusted_frame$var_table_position[current_index]
  
  variable_data_min <- min(variable_data)
  variable_data_max <- max(variable_data)
  
  
  
  # Fix 2022.10.11.
  # Generating pdf output.
  # pdf( paste( "Plots/file07_final_data_figure_production_water_river_", variable_name, "_no_zeroes2.pdf", sep = ""), height = 6, width = 16.75 )
  png( paste( "Plots/file07_final_data_figure_production_water_river_", variable_name, "_no_zeroes2.png", sep = ""), height = 6, width = 16.75, res = 600, units="in" )
  
  # Defining layout
  # Matrix first
  layout_matrix <- matrix( 1, nrow = 6, ncol = 7, byrow = FALSE)
  
  # Cells definition (column 1)
  layout_matrix[c(1:3),   c(1:3)] <- 1
  layout_matrix[c(4:6),   c(1:3)] <- 2
  
  # Cells definition (column 2)
  layout_matrix[c(1:6),   c(4:5)] <- 3
  layout_matrix[c(1:6),   c(6:7)] <- 4
  
  # Setting layaout
  layout(layout_matrix)
  
  
  
  # Panel A
  # Plotting x_values vs y_values
  plot(x = merged_river_plus_incidence$date,
       y = counts_data,
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
        y = counts_data,
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
       y = variable_data,
       # col = "darkblue", 
       col = "dodgerblue3", 
       lwd = 3,
       pch = 16,
       type = "l",
       main = paste( variable_name, sep ="" ),
       # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
       # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = paste( variable_unit, sep ="" ),
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )
  
  lines(x = merged_river_plus_incidence$date,
        y = variable_data,
        # col = "darkblue", 
        col = "dodgerblue3", 
        lwd = 5,
        pch = 16,
        type = "p",
        main = paste( variable_name, sep ="" ),
        # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
        # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
        #xlim = c( - 0.1, 1.1 ),
        #ylim = c( - 0.1, 1.1 ),
        xlab = "",
        ylab = paste( variable_unit, sep ="" ),
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
  counts_transformed <- counts_data

  # Re-coding as 1
  counts_transformed[ counts_transformed >=1 ] <- 1
  
  
  # Plotting x_values vs y_values
  plot(x = variable_data,
       y = counts_transformed,
       col = "darkcyan", 
       lwd = 5,
       pch = 16,
       type = "p",
       main = paste("Binomial Fit (O vs 1 and up)\nvs ",  variable_name, " (", variable_unit, ")", sep ="" ),
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
  
  # glm fit
  glm_fit_current <- glm( counts_transformed ~ variable_data, family = binomial) 
  summary(glm_fit_current)
  
  # Prediction
  variable_data_prediction     <- data.frame( variable_data = seq( variable_data_min, variable_data_max, len = 500 ) )
  counts_transformed_predicted <- predict(glm_fit_current, newdata = variable_data_prediction , type="response")
  
  
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
         fill = c("darkcyan", "darkorange2"),   
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
  
  # Label C
  label_function(label_value = "C", label_cex = 4)
  
  

  
  
  # Panel D
  counts_transformed <- log(counts_data)
  counts_transformed[ is.infinite(counts_transformed)] <- NA
  # Making corresponding dates NA
  variable_data[ is.na(counts_transformed)] <- NA
  
  # Removing NA-s
  counts_transformed <- counts_transformed[!is.na(counts_transformed)]
  variable_data      <- variable_data[!is.na(variable_data)] 
  
  
  # lm fit
  lm_fit_variable_data <- lm( counts_transformed ~ variable_data ) 
  summary(lm_fit_variable_data)
  # lm fit extracts
  a_variable_data <- lm_fit_variable_data$coefficients[2]
  b_variable_data <- lm_fit_variable_data$coefficients[1]
  
  # Summary statistics
  r_variable_data      <- round( x = sqrt(summary(lm_fit_variable_data)$r.squared), digits = 2)
  pvalue_variable_data <- round( x = summary(lm_fit_variable_data)$coefficients[2,4], digits = 2)
  
  # Range for the lm line
  range_x <- c(variable_data_min, variable_data_max)
  range_y <- a_variable_data * range_x + b_variable_data
  
  
  # Plotting x_values vs y_values
  plot(x = variable_data,
       y = counts_transformed,
       # col = "dodgerblue3", 
       col = "darkcyan", 
       lwd = 5,
       pch = 16,
       type = "p",
       main = paste("Zeroes Omitted\nLog(Counts) vs ",  variable_name, " (", variable_unit, ")", sep ="" ),
       xlim = c( variable_data_min, variable_data_max  ),
       ylim = c( min(c(counts_transformed, range_y)), max(c(counts_transformed, range_y)) ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = "Log(Counts)",
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )
  
  
  # Adding the corresponding regression fit.
  lines(x = range_x, 
        y = range_y, 
        type = "l",
        col="darkorange2", 
        lwd = 5, 
        lty = 2)
  
  legend(x = var_table_position,
         inset= c(0.05, 0.05), 
         legend = c( "Data Points", "LM Fit"), 
         col = "black", 
         fill = c("darkcyan", "darkorange2"),   
         pt.cex = c(4, 2),
         # pch = c(19, 20),  
         cex = 1.5 ) 
  
  legend(x = "topleft",
         inset= c(0.00, 0.025), 
         legend = c( as.expression(bquote(r ~ " = " ~ .(r_variable_data))),
                     as.expression(bquote(p ~ "- value < " ~ .(pvalue_variable_data)))), 
         cex = 1.5,
         pt.cex = 0.0001,
         box.col=0, 
         bty="n")
  
  
  # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
  # Creating positions where to put vertical bars.
  x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                                by = (variable_data_max - variable_data_min)/20 )
  x_indexes_to_display <- round(x = x_indexes_to_display, digits = 3)
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
  
  
  
  
  
  
  
    
  
  dev.off()
  
  # Exporting Corresponding label text.
  sink( paste( "R_Output/file07_final_data_figure_production_water_river_",   variable_name, "_no_zeroes2.txt", sep ="") ) 
  cat( "The summaries of the A) reported incidence and B) river ", variable_name, " (",variable_unit,"). The logistic model fit for zero (coded as 0) vs non-zero (code as 1) counts is provided in panel C. ", 
          "Counts on the log scale i.e. Log(Counts) vs river ",  variable_name, " (", variable_unit, ") are provided in panel C. ",
         "The panel C also contains the linear regression fitted line of Log(Counts) vs river ",  variable_name, ". ",
         "The corresponding linear regression statistics (correlation estimate and p-value for the statistical test of slope to be zero) are also provided.", sep = "") 
  sink()
  
  
  
  
  # End of -> for ( current_index in c(2:dim(variables_list_river_adjusted_frame)[1]) )
}  































# Fix 2022.10.17. ask
# The subset of the dataset without zeroes2

# Looping to plot -> variables_list_dalecarlia_mcmillan_adjusted

for ( current_index in c(2:dim(variables_list_dalecarlia_mcmillan_adjusted_frame)[1]) )
{
  
  # Debugging step
  # current_index <- 3
  
  # getting summaries of interest
  counts_data <- merged_dalecarlia_mcmillan_plus_incidence$Counts
  
  variable_data      <- merged_dalecarlia_mcmillan_plus_incidence[,variables_list_dalecarlia_mcmillan_adjusted_frame$var_code[current_index] ]
  variable_name      <- variables_list_dalecarlia_mcmillan_adjusted_frame$var_name[current_index]
  variable_unit      <- variables_list_dalecarlia_mcmillan_adjusted_frame$var_unit[current_index]
  var_table_position <- variables_list_dalecarlia_mcmillan_adjusted_frame$var_table_position[current_index]
  
  
  variable_data_min <- min(variable_data)
  variable_data_max <- max(variable_data)
  
  
  
  # Fix 2022.10.11.
  # Generating pdf output.
  # pdf( paste( "Plots/file07_final_data_figure_production_water_dalecarlia_mcmillan_", variable_name, "_no_zeroes2.pdf", sep = ""), height = 6, width = 16.75 )
  png( paste( "Plots/file07_final_data_figure_production_water_dalecarlia_mcmillan_", variable_name, "_no_zeroes2.png", sep = ""), height = 6, width = 16.75, res = 600, units="in" )
  
  # Defining layout
  # Matrix first
  layout_matrix <- matrix( 1, nrow = 6, ncol = 7, byrow = FALSE)
  
  # Cells definition (column 1)
  layout_matrix[c(1:3),   c(1:3)] <- 1
  layout_matrix[c(4:6),   c(1:3)] <- 2
  
  # Cells definition (column 2)
  layout_matrix[c(1:6),   c(4:5)] <- 3
  layout_matrix[c(1:6),   c(6:7)] <- 4
  
  # Setting layaout
  layout(layout_matrix)
  
  
  
  # Panel A
  # Plotting x_values vs y_values
  plot(x = merged_dalecarlia_mcmillan_plus_incidence$date,
       y = counts_data,
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
  lines(x = merged_dalecarlia_mcmillan_plus_incidence$date,
        y = counts_data,
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
  x_indexes_to_display <-  seq( from  =  min(merged_dalecarlia_mcmillan_plus_incidence$date), to  = max(merged_dalecarlia_mcmillan_plus_incidence$date),  by = 180 )
  # Actual lab elements
  x_tlab <- x_indexes_to_display
  # Actual lab labels
  x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
  # x_lablist  <- merged_dalecarlia_mcmillan_plus_incidence$date
  axis(1, at = x_tlab, labels = FALSE)
  # axis(1, at = merged_dalecarlia_mcmillan_plus_incidence$date, labels = FALSE)
  text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)
  
  # Label A
  label_function(label_value = "A", label_cex = 4)
  
  
  
  
  # Panel B
  # Plotting x_values vs y_values
  plot(x = merged_dalecarlia_mcmillan_plus_incidence$date,
       y = variable_data,
       # col = "darkblue", 
       col = "dodgerblue3", 
       lwd = 3,
       pch = 16,
       type = "l",
       main = paste( variable_name, sep ="" ),
       # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
       # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = paste( variable_unit, sep ="" ),
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )
  
  lines(x = merged_dalecarlia_mcmillan_plus_incidence$date,
        y = variable_data,
        # col = "darkblue", 
        col = "dodgerblue3", 
        lwd = 5,
        pch = 16,
        type = "p",
        main = paste( variable_name, sep ="" ),
        # xlim = c( x_min_value_current - 0.1 * x_range_current, x_max_value_current + 0.1 * x_range_current  ),
        # ylim = c( y_min_value_current - 0.1 * y_range_current, y_max_value_current + 0.1 * y_range_current  ),
        #xlim = c( - 0.1, 1.1 ),
        #ylim = c( - 0.1, 1.1 ),
        xlab = "",
        ylab = paste( variable_unit, sep ="" ),
        xaxt='n',
        cex = 1.55,
        cex.axis = 1.55,
        cex.lab = 1.55,
        cex.main = 1.55,
        cex.sub = 1.55
  )
  
  
  # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
  # Creating positions where to put vertical bars.
  x_indexes_to_display <-  seq( from  =  min(merged_dalecarlia_mcmillan_plus_incidence$date), to  = max(merged_dalecarlia_mcmillan_plus_incidence$date),  by = 180 )
  # Actual lab elements
  x_tlab <- x_indexes_to_display
  # Actual lab labels
  x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
  # x_lablist  <- merged_dalecarlia_mcmillan_plus_incidence$date
  axis(1, at = x_tlab, labels = FALSE)
  # axis(1, at = merged_dalecarlia_mcmillan_plus_incidence$date, labels = FALSE)
  text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)
  
  # Label B
  label_function(label_value = "B", label_cex = 4)
  
  
  
  
  
  # Panel C
  counts_transformed <- counts_data
  
  # Re-coding as 1
  counts_transformed[ counts_transformed >=1 ] <- 1
  
  
  # Plotting x_values vs y_values
  plot(x = variable_data,
       y = counts_transformed,
       col = "darkcyan", 
       lwd = 5,
       pch = 16,
       type = "p",
       main = paste("Binomial Fit (O vs 1 and up)\nvs ",  variable_name, " (", variable_unit, ")", sep ="" ),
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
  
  # glm fit
  glm_fit_current <- glm( counts_transformed ~ variable_data, family = binomial) 
  summary(glm_fit_current)
  
  # Prediction
  variable_data_prediction     <- data.frame( variable_data = seq( variable_data_min, variable_data_max, len = 500 ) )
  counts_transformed_predicted <- predict(glm_fit_current, newdata = variable_data_prediction , type="response")
  
  
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
         fill = c("darkcyan", "darkorange2"),   
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
  # x_lablist  <- merged_dalecarlia_mcmillan_plus_incidence$date
  axis(1, at = x_tlab, labels = FALSE)
  # axis(1, at = merged_dalecarlia_mcmillan_plus_incidence$date, labels = FALSE)
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
  
  # Label C
  label_function(label_value = "C", label_cex = 4)
  
  
  
  
  
  # Panel D
  counts_transformed <- log(counts_data)
  counts_transformed[ is.infinite(counts_transformed)] <- NA
  # Making corresponding dates NA
  variable_data[ is.na(counts_transformed)] <- NA
  
  # Removing NA-s
  counts_transformed <- counts_transformed[!is.na(counts_transformed)]
  variable_data      <- variable_data[!is.na(variable_data)] 
  
  
  # lm fit
  lm_fit_variable_data <- lm( counts_transformed ~ variable_data ) 
  summary(lm_fit_variable_data)
  # lm fit extracts
  a_variable_data <- lm_fit_variable_data$coefficients[2]
  b_variable_data <- lm_fit_variable_data$coefficients[1]
  
  # Summary statistics
  r_variable_data      <- round( x = sqrt(summary(lm_fit_variable_data)$r.squared), digits = 2)
  pvalue_variable_data <- round( x = summary(lm_fit_variable_data)$coefficients[2,4], digits = 2)
  
  # Range for the lm line
  range_x <- c(variable_data_min, variable_data_max)
  range_y <- a_variable_data * range_x + b_variable_data
  
  
  # Plotting x_values vs y_values
  plot(x = variable_data,
       y = counts_transformed,
       # col = "dodgerblue3", 
       col = "darkcyan", 
       lwd = 5,
       pch = 16,
       type = "p",
       main = paste("Zeroes Omitted\nLog(Counts) vs ",  variable_name, "(", variable_unit, ")", sep ="" ),
       xlim = c( variable_data_min, variable_data_max  ),
       ylim = c( min(c(counts_transformed, range_y)), max(c(counts_transformed, range_y)) ),
       #xlim = c( - 0.1, 1.1 ),
       #ylim = c( - 0.1, 1.1 ),
       xlab = "",
       ylab = "Log(Counts)",
       xaxt='n',
       cex = 1.55,
       cex.axis = 1.55,
       cex.lab = 1.55,
       cex.main = 1.55,
       cex.sub = 1.55
  )
  
  
  # Adding the corresponding regression fit.
  lines(x = range_x, 
        y = range_y, 
        type = "l",
        col="darkorange2", 
        lwd = 5, 
        lty = 2)
  
  legend(x = var_table_position,
         inset= c(0.05, 0.05), 
         legend = c( "Data Points", "LM Fit"), 
         col = "black", 
         fill = c("darkcyan", "darkorange2"),   
         pt.cex = c(4, 2),
         # pch = c(19, 20),  
         cex = 1.5 ) 
  
  legend(x = "topleft",
         inset= c(0.00, 0.025), 
         legend = c( as.expression(bquote(r ~ " = " ~ .(r_variable_data))),
                     as.expression(bquote(p ~ "- value < " ~ .(pvalue_variable_data)))), 
         cex = 1.5,
         pt.cex = 0.0001,
         box.col=0, 
         bty="n")
  
  
  # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
  # Creating positions where to put vertical bars.
  x_indexes_to_display <-  seq( from  =  variable_data_min, to  = variable_data_max,  
                                by = (variable_data_max - variable_data_min)/20 )
  x_indexes_to_display <- round(x = x_indexes_to_display, digits = 3)
  # Actual lab elements
  x_tlab <- x_indexes_to_display
  # Actual lab labels
  x_lablist  <- as.character(substr( x = x_indexes_to_display, start = 1, stop = 7))
  # x_lablist  <- merged_dalecarlia_mcmillan_plus_incidence$date
  axis(1, at = x_tlab, labels = FALSE)
  # axis(1, at = merged_dalecarlia_mcmillan_plus_incidence$date, labels = FALSE)
  text(x = x_tlab, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex = 1.25)
  
  # Label D
  label_function(label_value = "D", label_cex = 4)
  
  
  
  
  
  
  dev.off()
  
  # Exporting Corresponding label text.
  sink( paste( "R_Output/file07_final_data_figure_production_water_dalecarlia_mcmillan_",   variable_name, "_no_zeroes2.txt", sep ="") ) 
  cat( "The summaries of the A) reported incidence and B) treatment plant ", variable_name, " (",variable_unit,"). The logistic model fit for zero (coded as 0) vs non-zero (code as 1) counts is provided in panel C. ", 
         "Counts on the log scale i.e. Log(Counts) vs treatment plant ",  variable_name, " (", variable_unit, ") are provided in panel C. ",
         "The panel C also contains the linear regression fitted line of Log(Counts) vs river ",  variable_name, ". ",
         "The corresponding linear regression statistics (correlation estimate and p-value for the statistical test of slope to be zero) are also provided.", sep = "") 
  sink()
  
  
  
  
  # End of -> for ( current_index in c(2:dim(variables_list_river_adjusted_frame)[1]) )
}  














































