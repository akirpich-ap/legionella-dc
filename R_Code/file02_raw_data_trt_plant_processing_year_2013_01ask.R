# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.11.01. ask
rm(list=ls(all=TRUE))


# Setting the correct working directory.
# Debugging step to run on local machine instead instead of the code right above used for HiPer Gator.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Weppelman Legionella"

# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()



# Reading 2013
# -------------------------------------------------------------------------------------
# Path
year_2013_list_path  <- "Data_new/Ref19_data/2013 Annual Water Quality Report.csv"

# List of values
year_2013_list <- scan(file = year_2013_list_path, what = "list", sep='\n')

# Working with table 1 -> i.e. Potomac river table 1
year_2013_river_table01_names <-  c("MONTH",
                                    "pH",
                                    "ALKALINITY",
                                    "CONDUCTIVITY",
                                    "DISSOLVED SOLIDS",
                                    "SUSPENDED SOLIDS",
                                    "TOTAL SOLIDS",
                                    "TEMPERATURE",
                                    "TOTAL HARDNESS",
                                    "TOTAL ORGANIC CARBON",
                                    "TURBIDITY",
                                    "TOTAL AMMONIA - N",
                                    "HEXAVALENT CHROMIUM",
                                    "BROMIDE",
                                    "CHLORIDE",
                                    "FLUORIDE",
                                    "IODIDE",
                                    "NITRATE - N",
                                    "NITRITE - N",
                                    "ORTHOPHOSPHATE - PO4",
                                    "PERCHLORATE",
                                    "SULFATE",
                                    "ALGAE COUNT",
                                    "TOTAL COLIFORM",
                                    "E. COLI",
                                    "GIARDIA",
                                    "CRYPTOSPORIDIUM" )


# Reading
year_2013_river_table01 <- data.frame( read.table(text = year_2013_list[31:42], sep = ",") )[,-c(17)]
dim(year_2013_river_table01)
length(year_2013_river_table01_names)
names(year_2013_river_table01) <- year_2013_river_table01_names
year_2013_river_table01
# Saving all years raw
year_2013_river_table01_path <- paste0("R_Data/year_2013_river_table01.RData")
save( year_2013_river_table01, file = year_2013_river_table01_path )


# Working with table 2 -> i.e. Potomac river table 2
year_2013_river_table02_names <-  c("MONTH",
                                    "ALUMINUM",
                                    "ANTIMONY",
                                    "ARSENIC",
                                    "BARIUM",
                                    "BERYLLIUM",
                                    "CADMIUM",
                                    "CALCIUM",
                                    "CHROMIUM",
                                    "COBALT",
                                    "COPPER",
                                    "IRON",
                                    "LEAD",
                                    "LITHIUM",
                                    "MAGNESIUM",
                                    "MANGANESE",
                                    "MOLYBDENUM",
                                    "NICKEL",
                                    "SELENIUM",
                                    "SILVER",
                                    "SODIUM",
                                    "STRONTIUM",
                                    "THALLIUM",
                                    "THORIUM",
                                    "URANIUM",
                                    "VANADIUM",
                                    "ZINC" )

year_2013_river_table02 <- data.frame( read.table(text = year_2013_list[52:63], sep = ",") )[,-c(19)]
dim(year_2013_river_table02)
length(year_2013_river_table02_names)
names(year_2013_river_table02) <- year_2013_river_table02_names
year_2013_river_table02
# Saving all years raw
year_2013_river_table02_path <- paste0("R_Data/year_2013_river_table02.RData")
save( year_2013_river_table02, file = year_2013_river_table02_path )




# Working with table 3 -> i.e. Dalecarlia table 1
year_2013_dalecarlia_table01_names <-  c("MONTH",
                                         "TOTAL AMMONIA - N",
                                         "BROMIDE",
                                         "CHLORATE",
                                         "CHLORIDE",
                                         "FLUORIDE",
                                         "IODIDE",
                                         "NITRATE - N",
                                         "NITRITE - N",
                                         "ORTHOPHOSPHATE - PO4",
                                         "PERCHLORATE",
                                         "SULFATE",
                                         "ALUMINUM",
                                         "ANTIMONY",
                                         "ARSENIC",
                                         "BARIUM",
                                         "BERYLLIUM",
                                         "CADMIUM",
                                         "CALCIUM",
                                         "CHROMIUM",
                                         "COBALT",
                                         "COPPER",
                                         "IRON",
                                         "LEAD",
                                         "LITHIUM",
                                         "MAGNESIUM",
                                         "MANGANESE",
                                         "MERCURY",
                                         "MOLYBDENUM",
                                         "NICKEL",
                                         "SELENIUM",
                                         "SILVER",
                                         "SODIUM",
                                         "STRONTIUM",
                                         "THALLIUM",
                                         "THORIUM",
                                         "URANIUM",
                                         "VANADIUM",
                                         "ZINC" )

year_2013_dalecarlia_table01 <- data.frame( read.table(text = year_2013_list[80:91], sep = ",") )
dim(year_2013_dalecarlia_table01)
length(year_2013_dalecarlia_table01_names)
names(year_2013_dalecarlia_table01) <- year_2013_dalecarlia_table01_names
year_2013_dalecarlia_table01

# Fixing columns 
year_2013_dalecarlia_table01$`NITRATE - N`[4] <- 1.3
year_2013_dalecarlia_table01$`ORTHOPHOSPHATE - PO4`[4] <- 2.4
year_2013_dalecarlia_table01$CHROMIUM[4] <- "1.0"
year_2013_dalecarlia_table01$LITHIUM[4] <- 2.0
year_2013_dalecarlia_table01$NICKEL[4] <- 1.8
year_2013_dalecarlia_table01$SELENIUM[4] <- "0.6"
year_2013_dalecarlia_table01$ZINC[4] <- "0.8"

year_2013_dalecarlia_table01


# Saving all years raw
year_2013_dalecarlia_table01_path <- paste0("R_Data/year_2013_dalecarlia_table01.RData")
save( year_2013_dalecarlia_table01, file = year_2013_dalecarlia_table01_path )



# Working with table 4 -> i.e. McMillan table 1
year_2013_mcmillan_table01_names <-  year_2013_dalecarlia_table01_names

year_2013_mcmillan_table01 <- data.frame( read.table(text = year_2013_list[93:104], sep = ",") )
dim(year_2013_mcmillan_table01)
length(year_2013_mcmillan_table01_names)
names(year_2013_mcmillan_table01) <- year_2013_mcmillan_table01_names
year_2013_mcmillan_table01

# Fixing columns 
year_2013_mcmillan_table01$`NITRATE - N`[12] <- 2.0
year_2013_mcmillan_table01$`ORTHOPHOSPHATE - PO4`[12] <- 2.4
year_2013_mcmillan_table01$COPPER[12] <- 5.2
year_2013_mcmillan_table01$LITHIUM[12] <- 2.0
year_2013_mcmillan_table01$NICKEL[12] <- 1.6
year_2013_mcmillan_table01$ZINC[12] <- "1.1"

year_2013_mcmillan_table01

# Saving all years raw
year_2013_mcmillan_table01_path <- paste0("R_Data/year_2013_mcmillan_table01.RData")
save( year_2013_mcmillan_table01, file = year_2013_mcmillan_table01_path )




# Working with table 5 -> i.e. Dalecarlia table 2
year_2013_dalecarlia_table02_names <-  c("MONTH",
                                         "pH",
                                         "ALKALINITY",
                                         "CONDUCTIVITY",
                                         "TEMPERATURE",
                                         "CHLORINE",
                                         "TOTAL HARDNESS",
                                         "TOTAL ORGANIC CARBON",
                                         "TOTAL DISSOLVED SOLIDS",
                                         "TOTAL SUSPENDED SOLIDS",
                                         "TURBIDITY (Average)*",
                                         "TOTAL COLIFORM (% positive)",
                                         "E. COLI (% positive)",
                                         "ALGAE COUNT",
                                         "HETEROTROPHIC PLATE COUNT",
                                         "DIBROMOACETIC ACID",
                                         "DICHLOROACETIC ACID",
                                         "MONOBROMOACETIC ACID",
                                         "MONOCHLOROACETIC ACID",
                                         "TRICHLOROACETIC ACID",
                                         "TOTAL HALOACETIC ACIDS",
                                         "BROMOCHLOROACETIC ACID",
                                         "CHLOROFORM",
                                         "BROMODICHLOROMETHANE",
                                         "CHLORODIBROMOMETHANE",
                                         "BROMOFORM",
                                         "TOTAL TRIHALOMETHANES",
                                         "BENZENE",
                                         "BROMOBENZENE",
                                         "BROMOCHLOROMETHANE",
                                         "BROMOMETHANE",
                                         "tert-BUTYLBENZENE",
                                         "sec-BUTYLBENZENE",
                                         "n-BUTYLBENZENE",
                                         "CARBON TETRACHLORIDE",
                                         "CHLOROBENZENE",
                                         "CHLOROETHANE",
                                         "CHLOROMETHANE",
                                         "2-CHLOROTOLUENE",
                                         "4-CHLOROTOLUENE",
                                         "DIBROMOMETHANE",
                                         "1,3-DICHLOROBENZENE",
                                         "1,4-DICHLOROBENZENE" )

year_2013_dalecarlia_table02 <- data.frame( read.table(text = year_2013_list[129:140], sep = ",") )
dim(year_2013_dalecarlia_table02)
length(year_2013_dalecarlia_table02_names)
names(year_2013_dalecarlia_table02) <- year_2013_dalecarlia_table02_names
year_2013_dalecarlia_table02

# Fixing columns 
year_2013_dalecarlia_table02$pH[4] <- 7.7
year_2013_dalecarlia_table02$CHLORINE[4] <- "3.0"
year_2013_dalecarlia_table02$CHLORINE <- as.numeric(year_2013_dalecarlia_table02$CHLORINE)
year_2013_dalecarlia_table02$`TOTAL ORGANIC CARBON`[4] <- "1.5"
year_2013_dalecarlia_table02$`TOTAL ORGANIC CARBON` <- as.numeric(year_2013_dalecarlia_table02$`TOTAL ORGANIC CARBON`)

year_2013_dalecarlia_table02$`TURBIDITY (Average)*`[4] <- "0.03"
year_2013_dalecarlia_table02$`TURBIDITY (Average)*` <- as.numeric(year_2013_dalecarlia_table02$`TURBIDITY (Average)*`)
year_2013_dalecarlia_table02$CHLORODIBROMOMETHANE[4] <- 4.4


year_2013_dalecarlia_table02

# Saving all years raw
year_2013_dalecarlia_table02_path <- paste0("R_Data/year_2013_dalecarlia_table02.RData")
save( year_2013_dalecarlia_table02, file = year_2013_dalecarlia_table02_path )




# Working with table 6 -> i.e. McMillan table 2
year_2013_mcmillan_table02_names <-  year_2013_dalecarlia_table02_names

year_2013_mcmillan_table02 <- data.frame( read.table(text = year_2013_list[142:153], sep = ",") )
dim(year_2013_mcmillan_table02)
length(year_2013_mcmillan_table02_names)
names(year_2013_mcmillan_table02) <- year_2013_mcmillan_table02_names
year_2013_mcmillan_table02


# Fixing columns 
year_2013_mcmillan_table02$pH[12] <- 7.7
year_2013_mcmillan_table02$CHLORINE[12] <- "3.7"
year_2013_mcmillan_table02$CHLORINE <- as.numeric(year_2013_mcmillan_table02$CHLORINE)
year_2013_mcmillan_table02$`TOTAL ORGANIC CARBON`[12] <- "2.0"
year_2013_mcmillan_table02$`TOTAL ORGANIC CARBON` <- as.numeric(year_2013_mcmillan_table02$`TOTAL ORGANIC CARBON`)

year_2013_mcmillan_table02$`TURBIDITY (Average)*`[12] <- "0.03"
year_2013_mcmillan_table02$`TURBIDITY (Average)*` <- as.numeric(year_2013_mcmillan_table02$`TURBIDITY (Average)*`)
year_2013_mcmillan_table02$BROMODICHLOROMETHANE[12] <- 8.2
year_2013_mcmillan_table02$CHLORODIBROMOMETHANE[12] <- 2.0

year_2013_mcmillan_table02

# Saving all years raw
year_2013_mcmillan_table02_path <- paste0("R_Data/year_2013_mcmillan_table02.RData")
save( year_2013_mcmillan_table02, file = year_2013_mcmillan_table02_path )























