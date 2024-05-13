# Installing and loading the rtry package ----

install.packages('rtry')
library(rtry)
packageVersion('rtry')
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)


install.packages(c("data.table", "dplyr", "tidyr", "jsonlite", "curl"))

packageVersion("rtry")

# Importing and exploring dataset ----
## For TRYdata1 ----
TRYdata <- rtry_import("~/OneDrive - University of Exeter/Desktop/O3-/TRY DataFile/29791.txt")
View(TRYdata)
head(TRYdata)


## Group the input data based on TraitID and TraitName ----
TRYdata_explore_trait <- rtry_explore(TRYdata, TraitID, TraitName)
View(TRYdata_explore_trait)


## Group the input data based on AccSpeciesID, AccSpeciesName, TraitID and TraitName----
## Note: For TraitID == "NA", meaning that entry is an ancillary data----
TRYdata_explore_species <- rtry_explore(TRYdata, AccSpeciesID, AccSpeciesName, TraitID, TraitName)
View(TRYdata_explore_species)


## Group the input data based on DataID, DataName, TraitID and TraitName----
## Then sort the output by TraitID using the sortBy argument----
TRYdata_explore_anc <- rtry_explore(TRYdata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)
View(TRYdata_explore_anc)


# Keeping columns and rows ----
workdata <- rtry_select_col(TRYdata, ObsDataID, ObservationID, AccSpeciesID, AccSpeciesName, ValueKindName, TraitID, TraitName, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, UnitName, OrigObsDataID, ErrorRisk, Comment)
View(workdata)

workdata_explore_anc <- rtry_explore(workdata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)
View(workdata_explore_anc)


workdata <- rtry_select_row(workdata, TraitID > 0 | DataID %in% c(59, 60, 61, 6601, 327, 413, 1961, 113))
View(workdata)

workdata_explore_anc <- rtry_explore(workdata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)
View(workdata_explore_anc)


# It is recommended to backup the data at some stages of the preprocessing, best before excluding according to different attributes.

workdata_unexcluded <- workdata


# Georeference coding to exclude data with only lat and long ----
## Filter according to latitude ----
## First, obtain only the observations that contain the Latitude (DataID 59) information, i.e. geo-referenced observations, using the function rtry_select_row.----
# Select only the geo-referenced observations, i.e. with DataID 59 Latitude
# Set getAncillary to TRUE to obtain (keep) all traits and ancillary data

workdata <- rtry_select_row(workdata, DataID %in% 59, getAncillary = TRUE)

# Select the rows that contain DataID 59, i.e. latitude information
# Then explore the unique values of the StdValue within the selected data

tmp_unfiltered <- rtry_select_row(workdata, DataID %in% 59)
tmp_unfiltered <- rtry_explore(tmp_unfiltered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
View(tmp_unfiltered)

# # # # # # # # # # # # #
# Exclude observations using latitude information
# Criteria
# 1. DataID equals to 59
# 2. StdValue NA

workdata <- rtry_exclude(workdata, (DataID %in% 59) & (is.na(StdValue)), baseOn = ObservationID)
View(workdata)

# Select the rows where DataID is 59 (Latitude)
# Then explore the unique values of the StdValue within the selected data
# Sort the exploration by StdValue

tmp_filtered <- rtry_select_row(workdata, DataID %in% 59)
tmp_filtered <- rtry_explore(tmp_filtered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
View(tmp_filtered)


#Filter according to longitude
#A similar procedure will be performed for longitude (DataID 60). To ensure the all the observations within the workdata contains the longitude information, use the rtry_select_row function.



# Select only the geo-referenced observations with DataID 60 Longitude
# Set getAncillary to TRUE to obtain (keep) all traits and ancillary data
workdata <- rtry_select_row(workdata, DataID %in% 60, getAncillary = TRUE)


# Select the rows that contain DataID 60, i.e. longitude information
# Then explore the unique values of the StdValue within the selected data
tmp_unfiltered <- rtry_select_row(workdata, DataID %in% 60)
tmp_unfiltered <- rtry_explore(tmp_unfiltered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
View(tmp_unfiltered)


# Exclude observations using longitude information
# Criteria
# 1. DataID equals to 60
# 2. StdValue smaller than 10 or larger than 60 or NA
workdata <- rtry_exclude(workdata, (DataID %in% 60) & (is.na(StdValue)), baseOn = ObservationID)

# Select the rows where DataID is 60 (Longitude)
# Then explore the unique values of the StdValue within the selected data
# Sort the exploration by StdValue
tmp_filtered <- rtry_select_row(workdata, DataID %in% 60)
tmp_filtered <- rtry_explore(tmp_filtered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
View(tmp_filtered)



# Remove duplicates
workdata <- rtry_remove_dup(workdata)


#transform to wide table
# Exclude
# 1. All entries with "" in TraitID
# 2. Potential categorical traits that don't have a StdValue
# 3. Traits that have not yet been standardized in TRY
# Then select the relevant columns for transformation
# Note: The complete.cases() is used to ensure the cases are complete,
#       i.e. have no missing values
num_traits <- rtry_select_row(workdata, complete.cases(TraitID) & complete.cases(StdValue))

num_traits <- rtry_select_col(num_traits, ObservationID, AccSpeciesID, AccSpeciesName, TraitID, TraitName, StdValue, UnitName)

# Extract the unique value of latitude (DataID 59) and longitude (DataID 60) together with the corresponding ObservationID
workdata_georef <- rtry_select_anc(workdata, 59, 60)

# To merge the extracted ancillary data with the numerical traits
# Merge the relevant data frames based on the ObservationID using rtry_join_left (left join)
num_traits_georef <- rtry_join_left(num_traits, workdata_georef, baseOn = ObservationID)

View(num_traits_georef)


# Perform wide table transformation on TraitID, TraitName and UnitName
# With cell values to be the mean values calculated for StdValue
num_traits_georef_wider <- rtry_trans_wider(num_traits_georef, names_from = c(TraitID, TraitName, UnitName), values_from = c(StdValue), values_fn = list(StdValue = mean))
View(num_traits_georef_wider)


#export data

# Export the data into a CSV file
output_file = file.path(tempdir(), "workdata_wider_traits.csv")
rtry_export(num_traits_georef_wider, output_file)



# Export the data into a CSV file
output_file = file.path(tempdir(), "workdata_traits.csv")
rtry_export(num_traits_georef, output_file)




#-------------------------------------------------------------------



# Importing and exploring dataset ----
## For TRYdata1 ----
TRYdata2 <- rtry_import("~/OneDrive - University of Exeter/Desktop/O3-/TRY DataFile/32833.txt")
View(TRYdata2)
head(TRYdata2)


## Group the input data based on TraitID and TraitName ----
TRYdata_explore_trait2 <- rtry_explore(TRYdata2, TraitID, TraitName)
View(TRYdata_explore_trait2)


## Group the input data based on AccSpeciesID, AccSpeciesName, TraitID and TraitName----
## Note: For TraitID == "NA", meaning that entry is an ancillary data----
TRYdata_explore_species2 <- rtry_explore(TRYdata2, AccSpeciesID, AccSpeciesName, TraitID, TraitName)
View(TRYdata_explore_species2)


## Group the input data based on DataID, DataName, TraitID and TraitName----
## Then sort the output by TraitID using the sortBy argument----
TRYdata_explore_anc2 <- rtry_explore(TRYdata2, DataID, DataName, TraitID, TraitName, sortBy = TraitID)
View(TRYdata_explore_anc2)


# Keeping columns and rows ----
workdata2 <- rtry_select_col(TRYdata2, ObsDataID, ObservationID, AccSpeciesID, AccSpeciesName, ValueKindName, TraitID, TraitName, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, UnitName, OrigObsDataID, ErrorRisk, Comment)
View(workdata2)

workdata_explore_anc2 <- rtry_explore(workdata2, DataID, DataName, TraitID, TraitName, sortBy = TraitID)
View(workdata_explore_anc2)


workdata2 <- rtry_select_row(workdata2, TraitID > 0 | DataID %in% c(59, 60, 61, 6601, 327, 413, 1961, 113))
View(workdata2)

workdata_explore_anc2 <- rtry_explore(workdata2, DataID, DataName, TraitID, TraitName, sortBy = TraitID)
View(workdata_explore_anc2)


# It is recommended to backup the data at some stages of the preprocessing, best before excluding according to different attributes.

workdata_unexcluded2 <- workdata2


# Georeference coding to exclude data with only lat and long ----
## Filter according to latitude ----
## First, obtain only the observations that contain the Latitude (DataID 59) information, i.e. geo-referenced observations, using the function rtry_select_row.----
# Select only the geo-referenced observations, i.e. with DataID 59 Latitude
# Set getAncillary to TRUE to obtain (keep) all traits and ancillary data

workdata2 <- rtry_select_row(workdata2, DataID %in% 59, getAncillary = TRUE)

# Select the rows that contain DataID 59, i.e. latitude information
# Then explore the unique values of the StdValue within the selected data

tmp_unfiltered2 <- rtry_select_row(workdata2, DataID %in% 59)
tmp_unfiltered2 <- rtry_explore(tmp_unfiltered2, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
View(tmp_unfiltered2)

# # # # # # # # # # # # #
# Exclude observations using latitude information
# Criteria
# 1. DataID equals to 59
# 2. StdValue NA

workdata2 <- rtry_exclude(workdata2, (DataID %in% 59) & (is.na(StdValue)), baseOn = ObservationID)
View(workdata2)

# Select the rows where DataID is 59 (Latitude)
# Then explore the unique values of the StdValue within the selected data
# Sort the exploration by StdValue

tmp_filtered2 <- rtry_select_row(workdata2, DataID %in% 59)
tmp_filtered2 <- rtry_explore(tmp_filtered2, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
View(tmp_filtered2)


#Filter according to longitude
#A similar procedure will be performed for longitude (DataID 60). To ensure the all the observations within the workdata contains the longitude information, use the rtry_select_row function.



# Select only the geo-referenced observations with DataID 60 Longitude
# Set getAncillary to TRUE to obtain (keep) all traits and ancillary data
workdata2 <- rtry_select_row(workdata2, DataID %in% 60, getAncillary = TRUE)


# Select the rows that contain DataID 60, i.e. longitude information
# Then explore the unique values of the StdValue within the selected data
tmp_unfiltered2 <- rtry_select_row(workdata2, DataID %in% 60)
tmp_unfiltered2 <- rtry_explore(tmp_unfiltered2, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
View(tmp_unfiltered2)


# Exclude observations using longitude information
# Criteria
# 1. DataID equals to 60
# 2. StdValue smaller than 10 or larger than 60 or NA
workdata2 <- rtry_exclude(workdata2, (DataID %in% 60) & (is.na(StdValue)), baseOn = ObservationID)

# Select the rows where DataID is 60 (Longitude)
# Then explore the unique values of the StdValue within the selected data
# Sort the exploration by StdValue
tmp_filtered2 <- rtry_select_row(workdata2, DataID %in% 60)
tmp_filtered2 <- rtry_explore(tmp_filtered2, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
View(tmp_filtered2)



# Remove duplicates
workdata2 <- rtry_remove_dup(workdata2)


#transform to wide table
# Exclude
# 1. All entries with "" in TraitID
# 2. Potential categorical traits that don't have a StdValue
# 3. Traits that have not yet been standardized in TRY
# Then select the relevant columns for transformation
# Note: The complete.cases() is used to ensure the cases are complete,
#       i.e. have no missing values
num_traits2 <- rtry_select_row(workdata2, complete.cases(TraitID) & complete.cases(StdValue))

num_traits2 <- rtry_select_col(num_traits2, ObservationID, AccSpeciesID, AccSpeciesName, TraitID, TraitName, StdValue, UnitName)

# Extract the unique value of latitude (DataID 59) and longitude (DataID 60) together with the corresponding ObservationID
workdata_georef2 <- rtry_select_anc(workdata2, 59, 60)

# To merge the extracted ancillary data with the numerical traits
# Merge the relevant data frames based on the ObservationID using rtry_join_left (left join)
num_traits_georef2 <- rtry_join_left(num_traits2, workdata_georef2, baseOn = ObservationID)

View(num_traits_georef2)


# Perform wide table transformation on TraitID, TraitName and UnitName
# With cell values to be the mean values calculated for StdValue
num_traits_georef_wider2 <- rtry_trans_wider(num_traits_georef2, names_from = c(TraitID, TraitName, UnitName), values_from = c(StdValue), values_fn = list(StdValue = mean))
View(num_traits_georef_wider2)


#export data

# Export the data into a CSV file
output_file = file.path(tempdir(), "workdata_wider_traits2.csv")
rtry_export(num_traits_georef_wider2, output_file)



#export data

# Export the data into a CSV file
output_file = file.path(tempdir(), "workdata_traits2.csv")
rtry_export(num_traits_georef2, output_file)
