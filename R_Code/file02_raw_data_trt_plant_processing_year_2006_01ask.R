# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.10.27. ask
rm(list=ls(all=TRUE))


# Setting the correct working directory.
# Debugging step to run on local machine instead instead of the code right above used for HiPer Gator.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Weppelman Legionella"

# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()



# Reading 2006
# -------------------------------------------------------------------------------------
# Path
year_2006_list_path  <- "Data_new/Ref19_data/2006.csv"

# List of values
year_2006_list <- scan(file = year_2006_list_path, what = "list", sep='\n')

# Working with table 1 -> i.e. Potomac river table 1
year_2006_river_table01_names <-  c("MONTH",
                                    "pH",
                                    "ALKALINITY",
                                    "CONDUCTIVITY",
                                    "DISSOLVED SOLIDS",
                                    "SUSPENDED SOLIDS",
                                    "TEMPERATURE",
                                    "TOTAL HARDNESS",
                                    "TOTAL ORG. CARBON",
                                    "TOTAL SOLIDS",
                                    "TURBIDITY",
                                    "TOTAL AMMONIA",
                                    "BROMIDE",
                                    "CHLORIDE",
                                    "FLUORIDE",
                                    "IODIDE",
                                    "NITRATE",
                                    "NITRITE",
                                    "ORTHOPHOSPHATE as PO4",
                                    "PERCHLORATE",
                                    "SULFATE",
                                    "ALGAE COUNT",
                                    "TOTAL COLIFORM",
                                    "E. COLI",
                                    "GIARDIA",
                                    "CRYPTOSPORIDIUM",
                                    "VIRUS" )


# Column 24,29 is empty and is removed
year_2006_river_table01 <- data.frame( read.table(text = year_2006_list[29:43], sep = ",") )[,-c(24,29)]
dim(year_2006_river_table01)
length(year_2006_river_table01_names)
names(year_2006_river_table01) <- year_2006_river_table01_names
year_2006_river_table01
# Saving all years raw
year_2006_river_table01_path <- paste0("R_Data/year_2006_river_table01.RData")
save( year_2006_river_table01, file = year_2006_river_table01_path )


# Working with table 2 -> i.e. Potomac river table 2
year_2006_river_table02_names <-  c("MONTH",
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

year_2006_river_table02 <- data.frame( read.table(text = year_2006_list[53:67], sep = ",") )
dim(year_2006_river_table02)
length(year_2006_river_table02_names)
names(year_2006_river_table02) <- year_2006_river_table02_names
year_2006_river_table02
# Saving all years raw
year_2006_river_table02_path <- paste0("R_Data/year_2006_river_table02.RData")
save( year_2006_river_table02, file = year_2006_river_table02_path )




# Working with table 3 -> i.e. Dalecarlia table 1
year_2006_dalecarlia_table01_names <-  c("MONTH",
                                         "TOTAL AMMONIA",
                                         "BROMIDE",
                                         "CHLORIDE",
                                         "FLUORIDE",
                                         "IODIDE",
                                         "NITRATE",
                                         "NITRITE",
                                         "ORTHOPHOSPHATE as PO4",
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

year_2006_dalecarlia_table01 <- data.frame( read.table(text = year_2006_list[85:99], sep = ",") )
dim(year_2006_dalecarlia_table01)
length(year_2006_dalecarlia_table01_names)
names(year_2006_dalecarlia_table01) <- year_2006_dalecarlia_table01_names
year_2006_dalecarlia_table01
# Saving all years raw
year_2006_dalecarlia_table01_path <- paste0("R_Data/year_2006_dalecarlia_table01.RData")
save( year_2006_dalecarlia_table01, file = year_2006_dalecarlia_table01_path )



# Working with table 4 -> i.e. McMillan table 1
year_2006_mcmillan_table01_names <-  year_2006_dalecarlia_table01_names

year_2006_mcmillan_table01 <- data.frame( read.table(text = year_2006_list[101:115], sep = ",") )
dim(year_2006_mcmillan_table01)
length(year_2006_mcmillan_table01_names)
names(year_2006_mcmillan_table01) <- year_2006_mcmillan_table01_names
year_2006_mcmillan_table01
# Saving all years raw
year_2006_mcmillan_table01_path <- paste0("R_Data/year_2006_mcmillan_table01.RData")
save( year_2006_mcmillan_table01, file = year_2006_mcmillan_table01_path )




# Working with table 5 -> i.e. Dalecarlia table 2
year_2006_dalecarlia_table02_names <-  c("MONTH",
                                         "pH",
                                         "ALKALINITY",
                                         "CONDUCTIVITY",
                                         "TEMPERATURE",
                                         "TOTAL CHLORINE",
                                         "TOTAL HARDNESS",
                                         "TOTAL ORG. CARBON",
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


year_2006_dalecarlia_table02 <- data.frame( read.table(text = year_2006_list[137:151], sep = ",") )
dim(year_2006_dalecarlia_table02)
length(year_2006_dalecarlia_table02_names)
names(year_2006_dalecarlia_table02) <- year_2006_dalecarlia_table02_names
year_2006_dalecarlia_table02
# Saving all years raw
year_2006_dalecarlia_table02_path <- paste0("R_Data/year_2006_dalecarlia_table02.RData")
save( year_2006_dalecarlia_table02, file = year_2006_dalecarlia_table02_path )





# Working with table 6 -> i.e. McMillan table 2
year_2006_mcmillan_table02_names <-  year_2006_dalecarlia_table02_names


year_2006_mcmillan_table02 <- data.frame( read.table(text = year_2006_list[153:167], sep = ",") )
dim(year_2006_mcmillan_table02)
length(year_2006_mcmillan_table02_names)
names(year_2006_mcmillan_table02) <- year_2006_mcmillan_table02_names
year_2006_mcmillan_table02
# Saving all years raw
year_2006_mcmillan_table02_path <- paste0("R_Data/year_2006_mcmillan_table02.RData")
save( year_2006_mcmillan_table02, file = year_2006_mcmillan_table02_path )























