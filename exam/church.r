#This R script documents the process after webscraping and initial processing
#(HTML to csv) in Python.

setwd("/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Output") #setting working directory to where files to be read are

#loading packages required to work
library(data.table) # I tried to work with data tables as much as possible for faster processing
library(dplyr)
library(psych) # to create summary statistics with the function describe

#### Importing and cleaning the data files ####

# Reading data files
all_data_files <-
  list.files(path = "/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Output",
             pattern = "data")  # all_data_files creates a vector with all the filenames in the Output folder
data_list <-
  lapply(all_data_files, fread)  # Importing all the data files into a list

# To get the cced_id, I first read all the file names into a datatable and then
# created the cced_function to attach cced_id to the correct item in the
# data_list (which hqs all the data files)

data_list2 <-
  rbindlist(lapply(all_data_files, function(x)
    fread(
      cmd = paste("grep RDIST", x),
      header = FALSE,
      sep = ","
    )[, cced_id := x])) #Ignore warnings because they don't affect results

cced_function <- function(list1, datatable1) {
  data_list8 <- data.table() #create an empty datatable
  
  for (i in 1:length(list1)) {
    #list1 is the list with all data files as items
    e = list1[[i]]  #read i^{th} element of list1 into e
    r = cbind(e, datatable1[i]) #add a column containing the cced_id to the i^{th} element of list1
    data_list8 <-
      rbind(data_list8, r) # Create a datatable by binding the rows of each iteration
  }
  data_list8 # making sure the datatable gets saved after each iteration
}

# Create final data using list and data table with cced_ids
final_data <-
  cced_function(data_list, data_list2)  # using cced_function to create a datatable (final_data) that contains cced_id as a column and the other information in other columns
check_if_all_files <-
  final_data %>% group_by(cced_id) %>% summarise(n()) # checking that the function worked properly, should have the same information as data_list
View(check_if_all_files) #to check that the cced_function bound each item of the list to the right element from the datatable

# Clean data files by dropping entries without a person_id, cleaning the entries in Names, Type, cced_id and changing the cced_id to an integer
final_data <-
  final_data[PersonID != 0][, Names := gsub("              ", "", Names)][, Type := gsub("              ", "", Type)][, cced_id := gsub("data", "", cced_id)][, cced_id := gsub(".csv", "", cced_id)][, cced_id := as.integer(cced_id)]

#### Importing and cleaning the location files ####

all_location_files <-
  list.files(path = "/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/Output",
             pattern = "Location")
location_list <-
  lapply(all_location_files, fread, header = TRUE) # read all location files into a list, location_list

# Location files have no column names and the column names are saved into row 1. Using a function
# to shift row 1's values into the column name
header_true <- function(data.table) {
  names(data.table) <- as.character(unlist(data.table[1, ]))
  data.table[-1, ]
}

# Use function to define column names for all location files
location_list2 <- lapply(location_list, header_true)

# Removing all columns that are not required further (Contains, in locations, etc.)
c1 = c("County",
       "Diocese (Jurisdiction)",
       "Diocese (Geographic)",
       "parish")
location_list3 <- lapply(location_list2, select, contains(c1))

# Creating cced_ids for location files, similar to that for data files above
location_table <-
  rbindlist(lapply(all_location_files, function(x)
    fread(
      cmd = paste("grep RDIST", x),
      header = FALSE,
      sep = ","
    )[, cced_id := x]))

cced_function_location <- function(list1, datatable1) {
  location_list8 <- data.table()
  for (i in 1:length(list1)) {
    e = list1[[i]]
    r = cbind(e, datatable1[i])
    location_list8 <-
      rbind(location_list8, r , fill = TRUE) # fill = T because there exist observations with no county value which were causing problems
  }
  location_list8
}

final_location <-
  cced_function_location(location_list3, location_table) # Creates file with cced_id as a column

# Cleaning the names
n1 <-
  c("Diocese_Jurisdiction",
    "Diocese_Geographic",
    "Parish",
    "cced_id",
    "County") #define colnames, make sure that the columns are ordered in the right order
final_location1 <- setnames(final_location, n1)

# Cleaning observations in County, the Dioceses, Parish and cced_id
final_location2 <-
  final_location1[, County := gsub(":", "", County)][, Diocese_Jurisdiction := gsub(":      ", "", Diocese_Jurisdiction)][, Diocese_Geographic := gsub(":      ", "", Diocese_Geographic)][, Parish := sub(".*: ", "", Parish)][, cced_id := gsub("Location", "", cced_id)][, cced_id := gsub(".csv", "", cced_id)][, cced_id := as.integer(cced_id)]

#### Merging data and location files to get final dataset and cleaning it ####
final <-
  merge.data.table(final_location2, final_data) # 693225 observations of 11 variables

final2 <-
  final %>% filter(Year >= 1540 &
                     Year <= 1835) # final2 has 683845 observations

write.csv(
  final2,
  "/Users/BRAJENDRA2/Courses/Data_Analytics/Submission/MB_modified/Data/final2.csv",
  row.names = FALSE
)

#### Optional code ####
# Creating summary statistics for final2 

summarystat_final2 <- describe(final2, fast = T)
summarystat_final2
write_excel_csv(summarystat_final2, "summarystat_final2.csv")

#remove all elements except final2 before continuing with analysis

rm(list = setdiff(ls(), "final2")) 
ls()
