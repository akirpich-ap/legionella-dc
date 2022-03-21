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



# Reading 2014
# -------------------------------------------------------------------------------------
# Path
year_2014_list_path  <- "Data_new/Ref19_data/2014 Annual Water Quality Report.csv"

# List of values
year_2014_list <- scan(file = year_2014_list_path, what = "list", sep='\n')

# Working with table 1 -> i.e. Potomac river table 1
year_2014_river_table01_names <-  c("MONTH",
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
year_2014_river_table01 <- data.frame( read.table(text = year_2014_list[21:32], sep = ",") )[,-c(17,28,29)]
dim(year_2014_river_table01)
length(year_2014_river_table01_names)
names(year_2014_river_table01) <- year_2014_river_table01_names
year_2014_river_table01
# Saving all years raw
year_2014_river_table01_path <- paste0("R_Data/year_2014_river_table01.RData")
save( year_2014_river_table01, file = year_2014_river_table01_path )


# Working with table 2 -> i.e. Potomac river table 2
year_2014_river_table02_names <-  c("MONTH",
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
                                    "POTASSIUM",
                                    "SELENIUM",
                                    "SILVER",
                                    "SODIUM",
                                    "STRONTIUM",
                                    "THALLIUM",
                                    "THORIUM",
                                    "URANIUM",
                                    "VANADIUM",
                                    "ZINC" )

year_2014_river_table02 <- data.frame( read.table(text = year_2014_list[42:53], sep = ",") )[,-c(15)]
dim(year_2014_river_table02)
length(year_2014_river_table02_names)
names(year_2014_river_table02) <- year_2014_river_table02_names
year_2014_river_table02
# Saving all years raw
year_2014_river_table02_path <- paste0("R_Data/year_2014_river_table02.RData")
save( year_2014_river_table02, file = year_2014_river_table02_path )




# Working with table 3 -> i.e. Dalecarlia table 1
year_2014_dalecarlia_table01_names <-  c("MONTH",
                                         "TOTAL AMMONIA - N",
                                         "BROMIDE",
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
                                         "POTASSIUM",
                                         "SELENIUM",
                                         "SILVER",
                                         "SODIUM",
                                         "STRONTIUM",
                                         "THALLIUM",
                                         "THORIUM",
                                         "URANIUM",
                                         "VANADIUM",
                                         "ZINC" )

year_2014_dalecarlia_table01 <- data.frame( read.table(text = year_2014_list[71:82], sep = ",") )
dim(year_2014_dalecarlia_table01)
length(year_2014_dalecarlia_table01_names)
names(year_2014_dalecarlia_table01) <- year_2014_dalecarlia_table01_names
year_2014_dalecarlia_table01

# Fixing columns 
year_2014_dalecarlia_table01$`NITRATE - N`[4] <- 1.7
year_2014_dalecarlia_table01$`ORTHOPHOSPHATE - PO4`[4] <- 2.4
year_2014_dalecarlia_table01$CHROMIUM[4] <- "0.6"
year_2014_dalecarlia_table01$LITHIUM[4] <- 1.5
year_2014_dalecarlia_table01$NICKEL[4] <- 1.4
year_2014_dalecarlia_table01$POTASSIUM[4] <- 2.2
year_2014_dalecarlia_table01$VANADIUM[4] <- "0.3"
year_2014_dalecarlia_table01$ZINC[4] <- "0.7"

year_2014_dalecarlia_table01


# Saving all years raw
year_2014_dalecarlia_table01_path <- paste0("R_Data/year_2014_dalecarlia_table01.RData")
save( year_2014_dalecarlia_table01, file = year_2014_dalecarlia_table01_path )



# Working with table 4 -> i.e. McMillan table 1
year_2014_mcmillan_table01_names <-  year_2014_dalecarlia_table01_names

year_2014_mcmillan_table01 <- data.frame( read.table(text = year_2014_list[84:95], sep = ",") )
dim(year_2014_mcmillan_table01)
length(year_2014_mcmillan_table01_names)
names(year_2014_mcmillan_table01) <- year_2014_mcmillan_table01_names
year_2014_mcmillan_table01

# Fixing columns 
year_2014_mcmillan_table01$`NITRATE - N`[12] <- 1.8
year_2014_mcmillan_table01$`ORTHOPHOSPHATE - PO4`[12] <- 2.4
year_2014_mcmillan_table01$COPPER[12] <- 5.7
year_2014_mcmillan_table01$LITHIUM[12] <- 1.9
year_2014_mcmillan_table01$MOLYBDENUM[12] <- "0.7"
year_2014_mcmillan_table01$ZINC[12] <- "1.0"

year_2014_mcmillan_table01

# Saving all years raw
year_2014_mcmillan_table01_path <- paste0("R_Data/year_2014_mcmillan_table01.RData")
save( year_2014_mcmillan_table01, file = year_2014_mcmillan_table01_path )




# Working with table 5 -> i.e. Dalecarlia table 2
year_2014_dalecarlia_table02_names <-  c("MONTH",
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

year_2014_dalecarlia_table02 <- data.frame( read.table(text = year_2014_list[119:130], sep = ",") )[, -c(21)]
dim(year_2014_dalecarlia_table02)
length(year_2014_dalecarlia_table02_names)
names(year_2014_dalecarlia_table02) <- year_2014_dalecarlia_table02_names
year_2014_dalecarlia_table02

# Fixing columns 
year_2014_dalecarlia_table02$pH[4] <- 7.7
year_2014_dalecarlia_table02$CHLORINE[4] <- "3.2"
year_2014_dalecarlia_table02$CHLORINE <- as.numeric(year_2014_dalecarlia_table02$CHLORINE)
year_2014_dalecarlia_table02$`TOTAL ORGANIC CARBON`[4] <- "1.3"
year_2014_dalecarlia_table02$`TOTAL ORGANIC CARBON` <- as.numeric(year_2014_dalecarlia_table02$`TOTAL ORGANIC CARBON`)

year_2014_dalecarlia_table02$`TURBIDITY (Average)*`[4] <- "0.04"
year_2014_dalecarlia_table02$`TURBIDITY (Average)*` <- as.numeric(year_2014_dalecarlia_table02$`TURBIDITY (Average)*`)

year_2014_dalecarlia_table02$CHLOROFORM[4] <- "19.6"
year_2014_dalecarlia_table02$CHLOROFORM <- as.numeric(year_2014_dalecarlia_table02$CHLOROFORM)

year_2014_dalecarlia_table02$BROMODICHLOROMETHANE[4] <- 8.6
year_2014_dalecarlia_table02$BROMODICHLOROMETHANE <- as.numeric(year_2014_dalecarlia_table02$BROMODICHLOROMETHANE)

year_2014_dalecarlia_table02$CHLORODIBROMOMETHANE[4] <- 2.2

year_2014_dalecarlia_table02

# Saving all years raw
year_2014_dalecarlia_table02_path <- paste0("R_Data/year_2014_dalecarlia_table02.RData")
save( year_2014_dalecarlia_table02, file = year_2014_dalecarlia_table02_path )




# Working with table 6 -> i.e. McMillan table 2
year_2014_mcmillan_table02_names <-  year_2014_dalecarlia_table02_names

year_2014_mcmillan_table02 <- data.frame( read.table(text = year_2014_list[132:143], sep = ",") )[, -c(44)]
dim(year_2014_mcmillan_table02)
length(year_2014_mcmillan_table02_names)
names(year_2014_mcmillan_table02) <- year_2014_mcmillan_table02_names
year_2014_mcmillan_table02


# Fixing columns 
year_2014_mcmillan_table02$pH[12] <- 7.7
year_2014_mcmillan_table02$CHLORINE[12] <- "3.7"
year_2014_mcmillan_table02$CHLORINE <- as.numeric(year_2014_mcmillan_table02$CHLORINE)
year_2014_mcmillan_table02$`TOTAL ORGANIC CARBON`[12] <- "1.7"
year_2014_mcmillan_table02$`TOTAL ORGANIC CARBON` <- as.numeric(year_2014_mcmillan_table02$`TOTAL ORGANIC CARBON`)

year_2014_mcmillan_table02$`TURBIDITY (Average)*`[12] <- "0.02"
year_2014_mcmillan_table02$`TURBIDITY (Average)*` <- as.numeric(year_2014_mcmillan_table02$`TURBIDITY (Average)*`)

year_2014_mcmillan_table02$BROMODICHLOROMETHANE[12] <- "14.1"
year_2014_mcmillan_table02$BROMODICHLOROMETHANE <- as.numeric(year_2014_mcmillan_table02$BROMODICHLOROMETHANE)

year_2014_mcmillan_table02$CHLORODIBROMOMETHANE[12] <- 8.0

year_2014_mcmillan_table02$BROMOFORM[12] <- 1.7

year_2014_mcmillan_table02

# Saving all years raw
year_2014_mcmillan_table02_path <- paste0("R_Data/year_2014_mcmillan_table02.RData")
save( year_2014_mcmillan_table02, file = year_2014_mcmillan_table02_path )























