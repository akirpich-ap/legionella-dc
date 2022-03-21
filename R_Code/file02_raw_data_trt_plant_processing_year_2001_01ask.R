# Alexander Kirpich
# Georgia State University
# akirpich@gsu.edu

# 2021.10.25. ask
rm(list=ls(all=TRUE))


# Setting the correct working directory.
# Debugging step to run on local machine instead instead of the code right above used for HiPer Gator.
work_directory_path  <- "C:/Users/akirpich/Google Drive/2021 Kirpich-Weppelman Legionella"

# Setting up the working directory.
setwd(work_directory_path)
# Extra check
getwd()



# Reading 2001
# -------------------------------------------------------------------------------------
# Path
year_2001_list_path <- "Data_new/Ref19_data/2001.csv"



# List of values
year_2001_list <- scan(file = year_2001_list_path, what = "list", sep='\n')

# Working with table 1 -> i.e. Potomac river table 1
year_2001_river_table01_names <-  c("MONTH",
                                    "pH",
                                    "ALKALINITY",
                                    "ANIONIC SURFACTANTS",
                                    "CONDUCTIVITY",
                                    "DISSOLVED SOLIDS",
                                    "SUSPENDED SOLIDS",
                                    "TEMPERATURE",
                                    "TOTAL HARDNESS",
                                    "TOTAL ORG. CARBON",
                                    "TURBIDITY",
                                    "TOTAL AMMONIA",
                                    "BROMIDE",
                                    "CHLORIDE",
                                    "FLUORIDE",
                                    "NITRATE",
                                    "NITRITE",
                                    "PHOSPHATE",
                                    "SILICATE",
                                    "SULFATE",
                                    "TOTAL COLIFORM",
                                    "E. COLI",
                                    "ALGAE COUNT",
                                    "CRYPTOSPORIDIUM",
                                    "ALUMINUM",
                                    "ANTIMONY" )


# Column 26 is empty and is removed
year_2001_river_table01 <- data.frame( read.table(text = year_2001_list[27:41], sep = ",") )[,-26]
dim(year_2001_river_table01)
length(year_2001_river_table01_names)
names(year_2001_river_table01) <- year_2001_river_table01_names
year_2001_river_table01
# Saving all years raw
year_2001_river_table01_path <- paste0("R_Data/year_2001_river_table01.RData")
save( year_2001_river_table01, file = year_2001_river_table01_path )


# Working with table 2 -> i.e. Potomac river table 2
year_2001_river_table02_names <-  c("MONTH",
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

year_2001_river_table02 <- data.frame( read.table(text = year_2001_list[54:68], sep = ",") )
dim(year_2001_river_table02)
length(year_2001_river_table02_names)
names(year_2001_river_table02) <- year_2001_river_table02_names
year_2001_river_table02
# Saving all years raw
year_2001_river_table02_path <- paste0("R_Data/year_2001_river_table02.RData")
save( year_2001_river_table02, file = year_2001_river_table02_path )




# Working with table 3 -> i.e. Dalecarlia table 1
year_2001_dalecarlia_table01_names <-  c("MONTH",
                                    "TOTAL AMMONIA",
                                    "BROMIDE",
                                    "CHLORIDE",
                                    "FLUORIDE",
                                    "NITRATE",
                                    "NITRITE",
                                    "PHOSPHATE",
                                    "SILICATE",
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

year_2001_dalecarlia_table01 <- data.frame( read.table(text = year_2001_list[86:100], sep = ",") )
dim(year_2001_dalecarlia_table01)
length(year_2001_dalecarlia_table01_names)
names(year_2001_dalecarlia_table01) <- year_2001_dalecarlia_table01_names
year_2001_dalecarlia_table01
# Saving all years raw
year_2001_dalecarlia_table01_path <- paste0("R_Data/year_2001_dalecarlia_table01.RData")
save( year_2001_dalecarlia_table01, file = year_2001_dalecarlia_table01_path )



# Working with table 4 -> i.e. McMillan table 1
year_2001_mcmillan_table01_names <-  c("MONTH",
                                         "TOTAL AMMONIA",
                                         "BROMIDE",
                                         "CHLORIDE",
                                         "FLUORIDE",
                                         "NITRATE",
                                         "NITRITE",
                                         "PHOSPHATE",
                                         "SILICATE",
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

year_2001_mcmillan_table01 <- data.frame( read.table(text = year_2001_list[102:116], sep = ",") )
dim(year_2001_mcmillan_table01)
length(year_2001_mcmillan_table01_names)
names(year_2001_mcmillan_table01) <- year_2001_mcmillan_table01_names
year_2001_mcmillan_table01
# Saving all years raw
year_2001_mcmillan_table01_path <- paste0("R_Data/year_2001_mcmillan_table01.RData")
save( year_2001_mcmillan_table01, file = year_2001_mcmillan_table01_path )




# Working with table 5 -> i.e. Dalecarlia table 2
year_2001_dalecarlia_table02_names <-  c("MONTH",
                                         "pH",
                                         "ALKALINITY",
                                         "ANIONIC SURFACTANTS",
                                         "CONDUCTIVITY",
                                         "TEMPERATURE",
                                         "TOTAL CHLORINE",
                                         "TOTAL HARDNESS",
                                         "TOTAL ORG. CARBON",
                                         "TOTAL DISSOLVED SOLIDS",
                                         "TOTAL SUSPENDED SOLIDS",
                                         "TURBIDITY*",
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
                                         "1,4-DICHLOROBENZENE",
                                         "1,2-DICHLOROBENZENE",
                                         "DICHLORODIFLUOROMETHANE" )


year_2001_dalecarlia_table02 <- data.frame( read.table(text = year_2001_list[136:150], sep = ",") )
dim(year_2001_dalecarlia_table02)
length(year_2001_dalecarlia_table02_names)
names(year_2001_dalecarlia_table02) <- year_2001_dalecarlia_table02_names
year_2001_dalecarlia_table02
# Saving all years raw
year_2001_dalecarlia_table02_path <- paste0("R_Data/year_2001_dalecarlia_table02.RData")
save( year_2001_dalecarlia_table02, file = year_2001_dalecarlia_table02_path )




# Working with table 6 -> i.e. McMillan table 2
year_2001_mcmillan_table02_names <-  c("MONTH",
                                       "pH",
                                       "ALKALINITY",
                                       "ANIONIC SURFACTANTS",
                                       "CONDUCTIVITY",
                                       "TEMPERATURE",
                                       "TOTAL CHLORINE",
                                       "TOTAL HARDNESS",
                                       "TOTAL ORG. CARBON",
                                       "TOTAL DISSOLVED SOLIDS",
                                       "TOTAL SUSPENDED SOLIDS",
                                       "TURBIDITY*",
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
                                       "1,4-DICHLOROBENZENE",
                                       "1,2-DICHLOROBENZENE",
                                       "DICHLORODIFLUOROMETHANE" )


year_2001_mcmillan_table02 <- data.frame( read.table(text = year_2001_list[152:166], sep = ",") )
dim(year_2001_mcmillan_table02)
length(year_2001_mcmillan_table02_names)
names(year_2001_mcmillan_table02) <- year_2001_mcmillan_table02_names
year_2001_mcmillan_table02
# Saving all years raw
year_2001_mcmillan_table02_path <- paste0("R_Data/year_2001_mcmillan_table02.RData")
save( year_2001_mcmillan_table02, file = year_2001_mcmillan_table02_path )















