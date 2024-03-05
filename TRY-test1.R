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
TRYdata1 <- rtry_import("~/OneDrive - University of Exeter/Desktop/O3-/TRY DataFile/24428.txt")
View(TRYdata1)
head(TRYdata1)


TRYdata1_explore_trait <- rtry_explore(TRYdata1, TraitID, TraitName)
View(TRYdata1_explore_trait)


TRYdata1_explore_species <- rtry_explore(TRYdata1, AccSpeciesID, AccSpeciesName, TraitID, TraitName)
View(TRYdata1_explore_species)


### Group the input data based on DataID, DataName, TraitID and TraitName || ## and sort the output by TraitID using the sortBy argument ----

TRYdata1_explore_anc <- rtry_explore(TRYdata1, DataID, DataName, TraitID, TraitName, sortBy = TraitID)
View(TRYdata1_explore_anc)

## For TRYdata2 ----

TRYdata2 <- rtry_import("~/OneDrive - University of Exeter/Desktop/O3-/TRY DataFile/24427.txt")

TRYdata2_explore_trait <- rtry_explore(TRYdata2, TraitID, TraitName)
View(TRYdata2_explore_trait)


TRYdata2_explore_species <- rtry_explore(TRYdata2, AccSpeciesID, AccSpeciesName, TraitID, TraitName)
View(TRYdata2_explore_species)


### Group the input data based on DataID, DataName, TraitID and TraitName || ## and sort the output by TraitID using the sortBy argument ----

TRYdata2_explore_anc <- rtry_explore(TRYdata2, DataID, DataName, TraitID, TraitName, sortBy = TraitID)
View(TRYdata2_explore_anc)




# Binding imported data ----
TRYdata <- rtry_bind_row(TRYdata1, TRYdata2)
View(TRYdata)

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
workdata_explore_anc <- rtry_explore(workdata, DataID, DataName, TraitID, TraitName, sortBy = TraitID)


workdata <- rtry_select_row(workdata, TraitID > 0 | DataID %in% c(59, 60, 61, 6601, 327, 413, 1961, 113))
View(workdata)

workdata_unexcluded <- workdata


# Select the rows where DataID is 413, i.e. the data containing the plant development status
tmp_unfiltered <- rtry_select_row(workdata, DataID %in% 413)

# Then explore the unique values of the OrigValueStr within the selected data
tmp_unfiltered <- rtry_explore(tmp_unfiltered, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = OrigValueStr)
View(tmp_unfiltered)


         #### I will stop the excluding bits here because I first want to get the georeferenced before anything else###


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
workdata12 <- rtry_exclude(workdata, (DataID %in% 59) & (is.na(StdValue)), baseOn = ObservationID)
View(workdata12)
tmp_unfiltered12 <- rtry_select_row(workdata12, DataID %in% 59)
tmp_unfiltered12 <- rtry_explore(tmp_unfiltered12, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment, sortBy = StdValue)
View(tmp_unfiltered12)


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
