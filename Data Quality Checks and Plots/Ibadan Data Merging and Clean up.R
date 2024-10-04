
# Install and load necessary packages
install.packages("haven")
install.packages("dplyr")
library(haven)
library(dplyr)

rm(list = ls())

# Function to define custom ggplot theme
map_theme <- function() {
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align = 0.5,
        legend.title = element_text(size = 18, colour = 'black'),
        legend.text = element_text(size = 18, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

# Define the drive and directory paths
Drive <- gsub("Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
directory_path <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/new_field_data")

# List of shapefiles in the directory
city_osunwardshp <- list.files(path = directory_path, pattern = "\\.dta$", full.names = TRUE)

# Load required libraries
library(sf)
library(ggplot2)
library(ggrepel)

# Iterate over each shapefile
for (city in city_osunwardshp) {
  
  # Read shapefile
  index <- read_dta(city)
  

  # Extract variable names and labels
  variable_names <- names(index)
  variable_labels <- sapply(index, attr, which = "label")
  
  # Create a data dictionary
  data_dictionary <- data.frame(
    Variable = variable_names,
    Label = variable_labels,
    stringsAsFactors = FALSE
  )
  
  # Print the data dictionary
  print(data_dictionary)
  
  # Optionally, save the data dictio
  
}

write.csv(data_dictionary, "data_dictionary.csv", row.names = FALSE)















# Read the .dta file
file_path <- "/Users/macbookpro/Downloads/IB Wet season Women survey_270724.dta"  # Replace with the path to your .dta file
data <- read_dta(file_path)
datasummary <- data%>%
  group_by(sn)%>%
summarise(count=n())


# Read the .dta file
file_pathhh <- "/Users/macbookpro/Downloads/IB Wet season household data_edited_270724.dta"  # Replace with the path to your .dta file
datahh <- read_dta(file_pathhh)
datasummaryhh <- datahh%>%
  group_by(sn)%>%
  summarise(count=n())


# Read the .dta file
file_pathmen <- "/Users/macbookpro/Downloads/IB Wet season  Men survey_270724.dta"  # Replace with the path to your .dta file
datamen <- read_dta(file_pathmen)
datasummarymen <- datamen%>%
  group_by(sn)%>%
  summarise(count=n())

missinghhib <- anti_join(datahh,data, by ="sn") 


missingmenib <- anti_join(datahh,datamen, by ="sn") %>%

  missingmenibn <- missingmenib %>%
  
      filter(!hl4 == 2)

write_csv(missingmenib,"missing_men_records_overall_from_hh_data.csv")

write_csv(missingmenibn,"missing_men_records_hl4_is_male_from_hh_data.csv")

# Read the .dta file
file_pathrdt <- "/Users/macbookpro/Downloads/IB Wet season household members RDT.dta"  # Replace with the path to your .dta file
datardt <- read_dta(file_pathrdt)
datasummaryrdt <- datardt%>%
  group_by(sn)%>%
  summarise(count=n())

data$sn <- as.numeric(data$sn)

wide_data <- pivot_wider(data, names_from = sn, values_from = Value)

wide_data <- pivot_wider(
  datardt,
  names_from = 4,
  values_from = c(2:19)  # Spread both Value1 and Value2 columns
)%>%
  select(-2:-25)

write_dta(wide_data,"data_dictionary.dta")


write_csv(wide_data,"wide_rdt_data.csv")

write_csv(missinghhib,"missing_women_records_from_hh_data.csv")


# Read the .dta file
file_pathrdtw <- "/Users/macbookpro/Downloads/wide_data_rdt.dta"  # Replace with the path to your .dta file
datardtw <- read_dta(file_pathrdtw)

# Extract variable names and labels
variable_names <- names(wide_data)
variable_labels <- sapply(wide_data, attr, which = "label")

# Create a data dictionary
data_dictionary <- data.frame(
  Variable <- variable_names,
  Label <- variable_labels,
  stringsAsFactors = FALSE
)



# Create a data dictionary
data_dictionary <- data.frame(variable_names)

label <- data.frame(variable_labels)

# Print the data dictionary
print(data_dictionary)

# Optionally, save the data dictionary to a CSV file
write.csv(data_dictionary, "data_dictionary.csv", row.names = FALSE)






# Read the .dta file
file_path <- "/Users/macbookpro/Downloads/KN Wet season women malaria hx.dta"  # Replace with the path to your .dta file
data <- read_dta(file_path)

# Extract variable names and labels
variable_names <- names(datahh)
variable_labels <- sapply(datahh, attr, which = "label")

# Create a data dictionary
data_dictionary <- data.frame(
  Variable = variable_names,
  Label = variable_labels,
  stringsAsFactors = FALSE
)

# Print the data dictionary
print(data_dictionary)

# Optionally, save the data dictionary to a CSV file
write.csv(data_dictionary, "maldata_dictionary.csv", row.names = FALSE)




# Extract variable names
variable_names <- names(wide_data)

# Extract labels, replacing NULL with an empty string
variable_labels <- sapply(wide_data, function(x) attr(x, "label"))
variable_labels[is.null(variable_labels)] <- ""  # Handle NULL by replacing with empty string

# Alternatively, to avoid the error, ensure all elements are of length 1
variable_labels <- sapply(wide_data, function(x) {
  lbl <- attr(x, "label")
  if (is.null(lbl)) "" else lbl
})

# Create a data dictionary
data_dictionary <- data.frame(
  Variable = variable_names,
  Label = variable_labels,
  stringsAsFactors = FALSE
)

# View the data dictionary
print(data_dictionary)

write.csv(data_dictionary, "rdt_data_dictionary.csv", row.names = FALSE)






#data dictionary for missing hh data

# Extract variable names
variable_names <- names(datahh)

# Extract labels, replacing NULL with an empty string
variable_labels <- sapply(datahh, function(x) attr(x, "label"))
variable_labels[is.null(variable_labels)] <- ""  # Handle NULL by replacing with empty string

# Alternatively, to avoid the error, ensure all elements are of length 1
variable_labels <- sapply(datahh, function(x) {
  lbl <- attr(x, "label")
  if (is.null(lbl)) "" else lbl
})

# Create a data dictionary
data_dictionary <- data.frame(
  Variable = variable_names,
  #Label = variable_labels,
  stringsAsFactors = FALSE
)

# Create a data dictionary
data_dictionary_ <- data.frame(
  #Variable = variable_names,
  Label = variable_labels,
  stringsAsFactors = FALSE
)

# View the data dictionary
print(variable_labels)

write.csv(variable_labels, "data_dictionary_missing_hh.csv", row.names = FALSE)



labelling_missing_data <- merged_data %>%
  mutate(across(123:324, as.character)) %>%  # Convert all to character
  rowwise() %>%  # Operate row by row
  mutate(
    across(
      123:324, 
      ~ ifelse(all(is.na(c_across(123:324))), "No Women Data Available", .)
    )
  ) %>%
  ungroup()

as.character(merged_data[, 123:326])
na_rows <- apply(merged_data[, 123:326], 1, function(row) all(is.na(row)))

# Replace NAs with "No Women Data Available" in the identified rows
merged_data[na_rows, 123:326] <- "No Women Data Available"

library(dplyr)

# Rename columns in each dataframe with a prefix based on the dataframe name
datahh <- datahh %>% rename_with(~ paste0("hh_data_", .), -sn)
data <- data %>% rename_with(~ paste0("women_data_", .), -sn)
datamen <- datamen %>% rename_with(~ paste0("men_data_", .), -sn)
datardtw <- datardtw %>% rename_with(~ paste0("rdt_wide_data_", .), -sn)

# Perform the joins
merged_data <- datahh %>%
  left_join(data, by = "sn") %>%
  left_join(datamen, by = "sn") %>%
  left_join(datardtw, by = "sn")

na_rows <- apply(merged_data[, 123:324], 1, function(row) all(is.na(row)))

# Replace NAs with "Missing Data" in the identified rows
merged_data[na_rows, 123:126] <- "Missing Data in Women Questionnaire"


na_rows_men <- apply(merged_data[, 325:629], 1, function(row) all(is.na(row)))

# Replace NAs with "Missing Data" in the identified rows
merged_data[na_rows_men, 325:328] <- "Missing Data in Men Questionnaire"

na_rows_rdt <- apply(merged_data[, 630:821], 1, function(row) all(is.na(row)))

# Replace NAs with "Missing Data" in the identified rows
merged_data[na_rows_rdt, 642:653] <- "Missing Data in RDT Questionnaire"

write.csv(merged_data, "Merged_data_women_men_rdt_Ibadan.csv", row.names = FALSE)



# Extract variable names and labels
variable_names <- names(merged_data)
variable_labels <- sapply(merged_data, attr, which = "label")



# Create a data dictionary
data_dictionary <- data.frame(
 Variable = variable_names,
  Label = variable_labels,
  stringsAsFactors = FALSE
)




# Print the data dictionary
print(variable_labels)

# Optionally, save the data dictionary to a CSV file
write.csv(variable_labels, "maldata_dictionary.csv", row.names = FALSE)





# Temporarily change the number of rows displayed
options(max.print = 300)

# Print the data frame
print(variable_labels)

# Optionally, reset to default
options(max.print = 1000)  # You can set this to a value that suits your usual needs




datardtmismatch <- datardt %>%
  filter(!redcap_repeat_instance == hl1)

write.csv(datardtmismatch, "RDT_SN_LN_MISMATCH.csv", row.names = FALSE)



 selecteddffemale<- merged_data %>%
  select(1,146,150,158,159,316,318,642:653,666:677,690:701,750:761, 774:785, 798:809)

 selecteddfmale <- merged_data %>%
  select(1, 348,352,360,361,621,623, 642:653,666:677,690:701,750:761,  774:785, 798:809)



dbs_code_rdt <- datardt %>%
  tidyr::separate(newdbs_code , into = c("newsn", "ln"), sep = "/") 


femaledata <- selecteddffemale %>%
  tidyr::separate(women_data_q704 , into = c("newsn", "ln"), sep = "/") %>%
  tidyr::separate(rdt_wide_data_newdbs_code_2 , into = c("newsnrdt", "lnrdt"), sep = "/") 

male_data <- selecteddfmale  %>%
  tidyr::separate(men_data_q704 , into = c("newsn", "ln"), sep = "/") %>%
  tidyr::separate(rdt_wide_data_newdbs_code_1 , into = c("newsnrdt", "lnrdt"), sep = "/") 



not_matching_f_dbsln_ln <- femaledata%>%
  filter(!lnrdt == women_data_ln)%>%
  select(1:8,10,22,34,47,71)

not_matching_f_ln <- femaledata%>%
  filter(!ln == women_data_ln)


not_matching_f_rdt <- femaledata%>%
  filter(!women_data_q702 == rdt_wide_data_q302_2)%>%
  select(1:8,10,22,34,47,71)

female_age_mismatch <- not_matching_f_rdt %>%
  filter(!women_data_q201b == rdt_wide_data_hl6_2)


female_age_match <- not_matching_f_rdt %>%
  filter(women_data_q201b == rdt_wide_data_hl6_2)


notmatchingfemales <- not_matching_f_dbsln_ln %>%
select(,-10:-20 )%>%
  select(, -12:-22, -24:-34 ) %>%
  select(, -14:-24 ) 


not_matching_m_dbsln_ln <- male_data%>%
  filter(!rdt_wide_data_q300i_1 == ln)%>%
  select(,-10:-20 )%>%
  select(, -11:-21, -23:-33 ) %>%
  select(, -13:-23 ) 
  


  
  not_matching_m__dbsln_ln <- male_data%>%
  filter(!men_data_ln == ln)%>%
  select(,-10:-20 )%>%
  select(, -11:-21, -23:-33 ) %>%
  select(, -13:-23 ) 



  
  not_matching_m_rdt <- male_data%>%
    filter(!men_data_q702 == rdt_wide_data_q302_1)%>%
    select(,-10:-20 )%>%
    select(, -11:-21, -23:-33 ) %>%
    select(, -13:-23 )

  
  dobmismatch_rdt <- not_matching_m_rdt %>%
    filter(!men_data_q201b == rdt_wide_data_hl6_1)
  
  dobmatch_rdt <- not_matching_m_rdt %>%
    filter(men_data_q201b == rdt_wide_data_hl6_1)
  
  
  not_matching_m_dbsln_ln <- male_data%>%
    filter(!rdt_wide_data_q300i_1 == ln)%>%
    select(,-10:-20 )%>%
    select(, -11:-21, -23:-33 ) %>%
    select(, -13:-23 ) 
  
  
  
  
  not_matching_m__dbsln_ln <- male_data%>%
    filter(!men_data_ln == ln)%>%
    select(,-10:-20 )%>%
    select(, -11:-21, -23:-33 ) %>%
    select(, -13:-23 ) 
  
  
  
  
  not_matching_m_rdt <- male_data%>%
    filter(!men_data_q702 == rdt_wide_data_q302_1)%>%
    select(,-10:-20 )%>%
    select(, -11:-21, -23:-33 ) %>%
    select(, -13:-23 )
  
  
  dobmismatch_rdt <- not_matching_m_rdt %>%
    filter(!men_data_q201b == rdt_wide_data_hl6_1)
  
  
  plot(merged_data$men_data_q201b)
  
  plot(merged_data$women_data_q201b)
  
  library(openxlsx)
  
  # List of dataframes with sheet names
  df_list <- list("Not Matching LN- Females" = not_matching_f_dbsln_ln, "Not Matching RDTs -Female" = not_matching_f_rdt, "Date of Birth Mismatch - Female" = female_age_mismatch, "Date of Birth Match - Female" = female_age_match, "Not Matching LN- Males" = not_matching_m_dbsln_ln, "Not Matching RDTs -Male" = not_matching_m_rdt, "Date of Birth Mismatch - Male" = dobmismatch_rdt, "Date of Birth Match - Male" = dobmatch_rdt)
  
  # Create a workbook
  wb <- createWorkbook()
  
  # Add dataframes to workbook sheets
  for (sheet_name in names(df_list)) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, df_list[[sheet_name]])
  }
  
  # Save the workbook
  saveWorkbook(wb, "Ibadan QA_sheets.xlsx", overwrite = TRUE)
  
  
  

view(not_matching_m_rdt)




# Check for missing values across all columns---
missing_values <- data.frame(colSums(is.na(datardt)))
View(missing_values)



# Check for missing values across all columns---
missing_values_w <- data.frame(colSums(is.na(data)))
View(missing_values_w)


# Check for missing values across all columns---
missing_values_m <- data.frame(colSums(is.na(datamen)))
View(missing_values_m)

proportion_missing_values <- missing_values / nrow(df1)
View(proportion_missing_values)






library(randomForest)

# Fit Random Forest model
rf_model <- randomForest(malaria ~ ., data = merged_data, importance = TRUE)

# Variable importance
importance(rf_model)
