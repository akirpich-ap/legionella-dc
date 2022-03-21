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



# Reading 2017
# -------------------------------------------------------------------------------------
# Path
year_2017_list_path  <- "Data_new/Ref19_data/2017 Annual Water Quality Report.csv"

# List of values
year_2017_list <- scan(file = year_2017_list_path, what = "list", sep='\n')

# Working with table 1 -> i.e. Potomac river table 1
year_2017_river_table01_names <-  c("MONTH",
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
                                    "NITRATE - N",
                                    "NITRITE - N",
                                    "ORTHOPHOSPHATE - PO4",
                                    "PERCHLORATE",
                                    "SULFATE",
                                    "TOTAL COLIFORM",
                                    "E. COLI",
                                    "GIARDIA - Great Falls Intake",
                                    "CRYPTOSPORIDIUM - Great Falls Intake",
                                    "GIARDIA - Little Falls Intake",
                                    "CRYPTOSPORIDIUM - Little Falls Intake" )


# Reading
year_2017_river_table01 <- data.frame( read.table(text = year_2017_list[31:42], sep = ",") )
dim(year_2017_river_table01)
length(year_2017_river_table01_names)
names(year_2017_river_table01) <- year_2017_river_table01_names
year_2017_river_table01
# Saving all years raw
year_2017_river_table01_path <- paste0("R_Data/year_2017_river_table01.RData")
save( year_2017_river_table01, file = year_2017_river_table01_path )


# Working with table 2 -> i.e. Potomac river table 2
year_2017_river_table02_names <-  c("MONTH",
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
                                    "ZINC" )

year_2017_river_table02 <- data.frame( read.table(text = year_2017_list[45:56], sep = ",") )
dim(year_2017_river_table02)
length(year_2017_river_table02_names)
names(year_2017_river_table02) <- year_2017_river_table02_names
year_2017_river_table02
# Saving all years raw
year_2017_river_table02_path <- paste0("R_Data/year_2017_river_table02.RData")
save( year_2017_river_table02, file = year_2017_river_table02_path )




# Working with table 3 -> i.e. Dalecarlia table 1
year_2017_dalecarlia_table01_names <-  c("MONTH",
                                         "TOTAL AMMONIA - N",
                                         "BROMIDE",
                                         "CHLORIDE",
                                         "FLUORIDE",
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

year_2017_dalecarlia_table01 <- data.frame( read.table(text = year_2017_list[71:82], sep = ",") )
dim(year_2017_dalecarlia_table01)
length(year_2017_dalecarlia_table01_names)
names(year_2017_dalecarlia_table01) <- year_2017_dalecarlia_table01_names
year_2017_dalecarlia_table01

# Saving all years raw
year_2017_dalecarlia_table01_path <- paste0("R_Data/year_2017_dalecarlia_table01.RData")
save( year_2017_dalecarlia_table01, file = year_2017_dalecarlia_table01_path )



# Working with table 4 -> i.e. McMillan table 1
year_2017_mcmillan_table01_names <-  year_2017_dalecarlia_table01_names

year_2017_mcmillan_table01 <- data.frame( read.table(text = year_2017_list[84:95], sep = ",") )
dim(year_2017_mcmillan_table01)
length(year_2017_mcmillan_table01_names)
names(year_2017_mcmillan_table01) <- year_2017_mcmillan_table01_names
year_2017_mcmillan_table01

# Saving all years raw
year_2017_mcmillan_table01_path <- paste0("R_Data/year_2017_mcmillan_table01.RData")
save( year_2017_mcmillan_table01, file = year_2017_mcmillan_table01_path )




# Working with table 5 -> i.e. Dalecarlia table 2
year_2017_dalecarlia_table02_names <-  c("MONTH",
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

# Removing column 20
year_2017_dalecarlia_table02 <- data.frame( read.table(text = year_2017_list[121:132], sep = ",") )[,-c(20)]
dim(year_2017_dalecarlia_table02)
length(year_2017_dalecarlia_table02_names)
names(year_2017_dalecarlia_table02) <- year_2017_dalecarlia_table02_names
year_2017_dalecarlia_table02

# Fixing CHLOROFORM and BROMODICHLOROMETHANE
backup <- year_2017_dalecarlia_table02$CHLOROFORM[6:10]
year_2017_dalecarlia_table02$CHLOROFORM[6:10]         <- c("26.6", "45.2", "24.6", "30.6", "26.2")
year_2017_dalecarlia_table02$BROMODICHLOROMETHANE[6:10] <- c("11.8", "16.0", "14.0", "14.1", "19.1")

year_2017_dalecarlia_table02

# Saving all years raw
year_2017_dalecarlia_table02_path <- paste0("R_Data/year_2017_dalecarlia_table02.RData")
save( year_2017_dalecarlia_table02, file = year_2017_dalecarlia_table02_path )




# Working with table 6 -> i.e. McMillan table 2
year_2017_mcmillan_table02_names <-  year_2017_dalecarlia_table02_names

year_2017_mcmillan_table02 <- data.frame( read.table(text = year_2017_list[134:145], sep = ",") )[,-c(20)]
dim(year_2017_mcmillan_table02)
length(year_2017_mcmillan_table02_names)
names(year_2017_mcmillan_table02) <- year_2017_mcmillan_table02_names
year_2017_mcmillan_table02

# Fixing CHLOROFORM and BROMODICHLOROMETHANE
backup <- year_2017_mcmillan_table02$CHLOROFORM[6:10]
year_2017_mcmillan_table02$CHLOROFORM[6:7]  <- c("29.5", "53.5")
year_2017_mcmillan_table02$CHLOROFORM[9:10] <- c("20.4", "27.5")

year_2017_mcmillan_table02$BROMODICHLOROMETHANE[6:7]  <- c("12.8", "17.4")
year_2017_mcmillan_table02$BROMODICHLOROMETHANE[9:10] <- c("16.7", "19.1")

year_2017_mcmillan_table02


# Saving all years raw
year_2017_mcmillan_table02_path <- paste0("R_Data/year_2017_mcmillan_table02.RData")
save( year_2017_mcmillan_table02, file = year_2017_mcmillan_table02_path )























