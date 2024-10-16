rm(list=ls())

library(readxl)


# read the contents of this xlsx file from url, save it as Publcations.xlsx: https://www.spezialitätenliste.ch/File.axd?file=Publications.xlsx
# save the file in the working directory
download.file("https://www.spezialitätenliste.ch/File.axd?file=Publications.xlsx", "Publications.xlsx", mode = "wb")


#set the working directory to the location of the file
setwd("C:/Users/Dante Salvador/Documents/RStudio Github/SwissSL")

#import the file as a dataframe called Publications
Publications <- read_excel("Publications.xlsx")

options(scipen = 999)

#setup tidying dataframe
meds <- as.data.frame(Publications$Bezeichnung)
colnames(meds) <- "Description"
meds$price <- Publications[[9]]
meds$price <- as.numeric(meds$price)
meds$'Rec-ID' <- Publications$'Rec-ID'
meds$Substanzen <- Publications$Substanzen

#count number of commas in meds$description and add it to a new column
meds$number_of_commas <- sapply(strsplit(as.character(meds$Description), ","), length)

# Create a function to split the description into three parts
split_description <- function(description) {
  parts <- unlist(strsplit(as.character(description), ","))
  
  # Ensure there are at least three parts
  if (length(parts) < 3) {
    return(c(NA, NA, NA))
  }
  
  # Extract the first, middle, and last parts
  first <- parts[1]
  middle <- paste(parts[2:(length(parts) - 1)], collapse = ",")
  last <- parts[length(parts)]
  
  return(c(first, middle, last))
}

# Apply the function to the Description column
split_results <- t(sapply(meds$Description, split_description))

# Combine the results with the original dataframe
meds <- cbind(meds, split_results)

# Rename the new columns if desired
colnames(meds)[(ncol(meds) - 2):ncol(meds)] <- c("Description_First", "Description_Middle", "Description_Last")

# make a new column that counts how many separate numeric terms are in meds$Description_Last
# Create a function to count numeric terms
count_numeric_terms <- function(description) {
  # Use regular expression to find numeric terms
  matches <- regmatches(description, gregexpr("\\b\\d+\\.?\\d*\\b", description))
  
  # Return the number of numeric terms found
  return(length(unlist(matches)))
}

# Apply the function to the Description_Last column
meds$Numeric_Count_last <- sapply(meds$Description_Last, count_numeric_terms)

#table of numeric_count
table(meds$Numeric_Count_last)

#subset meds where numeric_count is 1
meds_sub_num1 <- subset(meds, Numeric_Count_last == 1)
library(tidyr)
library(dplyr)
library(stringr)


  # Create two new columns: col3 for the number and col4 for the terms after the number
  meds_sub_num1 <- meds_sub_num1 %>%
    mutate(
      Last3 = str_extract(Description_Last, "\\d+(\\.\\d+)?$"),  # Extract the last number
      Last4 = str_trim(str_remove(Description_Last, ".*\\d+(\\.\\d+)?\\s*"))  # Remove everything up to the last number and trim whitespace
    )
  
  #subset meds where numeric_count is 2
  meds_sub_num2 <- subset(meds, Numeric_Count_last == 2)
  
  # Use tidyr::extract to split the Description_Last column into four columns
  meds_sub_num2 <- meds_sub_num2 %>%
    extract(Description_Last, 
            into = c("Last1", "Last2", "Last3", "Last4"), 
            regex = "(\\d+\\.?\\d*)\\s*([^0-9]+)\\s*(\\d+\\.?\\d*)\\s*(.*)", 
            remove = FALSE, 
            convert = TRUE)
  
  
  meds_sub_num1$Last3 <- as.numeric(meds_sub_num1$Last3)
  
  #merge meds_sub_num1 and meds_sub_num2
  merged_meds <- full_join(meds_sub_num1, meds_sub_num2, by = c())
  
  
# count the number of forward slashes in merged_meds$Description_Middle, and put it in new column
merged_meds$number_of_slashes <- sapply(strsplit(as.character(merged_meds$Description_Middle), "/"), length)
  
table(merged_meds$number_of_slashes)
  
  #subset merged_meds where number_of_slashes is 1 or 2
  merged_meds_sub_slash1 <- subset(merged_meds, number_of_slashes == 1)
  merged_meds_sub_slash2 <- subset(merged_meds, number_of_slashes == 2)
  
  library(dplyr)
  library(stringr)

  ### SLASH 1
    # Extract the last term (unit) and the number immediately before it
    merged_meds_sub_slash1 <- merged_meds_sub_slash1 %>%
      mutate(unit = word(Description_Middle, -1),  # Extract the last term (unit)
             number_before_unit = str_extract(Description_Middle, "\\d+\\.?\\d*(?=\\s*\\w+$)"))  # Extract the number before the last term
    
    table(!is.na(merged_meds_sub_slash1$number_before_unit))
    merged_meds_sub_slash1$number_before_unit <- as.numeric(merged_meds_sub_slash1$number_before_unit)
    
    #count numeric terms in middle description
    merged_meds_sub_slash1$Numeric_Count_middle <- sapply(merged_meds_sub_slash1$Description_Middle, count_numeric_terms)
    table(merged_meds_sub_slash1$Numeric_Count_middle)
    
    
    
    #if numeric_count_middle == 1, extract the numeric term in Description_Middle
    merged_meds_sub_slash1$Middle3 <- str_extract(merged_meds_sub_slash1$Description_Middle, "\\d+\\.?\\d*")
    
    
    
    #if numeric_count_middle == 0, replace Middle3 with 1
    merged_meds_sub_slash1$Middle3[merged_meds_sub_slash1$Numeric_Count_middle ==0] <- 1
    
    table(is.na(merged_meds_sub_slash1$Middle3))
    
    # if number_before_unit, Last1 and Last3 are NA, replace it with 1,
    merged_meds_sub_slash1$number_before_unit[is.na(merged_meds_sub_slash1$number_before_unit)] <- 1
    merged_meds_sub_slash1$Last1[is.na(merged_meds_sub_slash1$Last1)] <- 1
    merged_meds_sub_slash1$Last3[is.na(merged_meds_sub_slash1$Last3)] <- 1
    merged_meds_sub_slash1$unit_cost <- merged_meds_sub_slash1$price / (merged_meds_sub_slash1$Last3 * merged_meds_sub_slash1$Last1) / merged_meds_sub_slash1$number_before_unit
    
    class(merged_meds_sub_slash1$price)
    
    # if number_before_unit == Middle3, replace Middle3 with 1
    merged_meds_sub_slash1$Middle3[merged_meds_sub_slash1$number_before_unit == merged_meds_sub_slash1$Middle3] <- 1
    merged_meds_sub_slash1$Middle3 <- as.numeric(merged_meds_sub_slash1$Middle3)
    
    # calculate unitcost by price / (Last3 * Last1) / number_before_unit / Middle3
    merged_meds_sub_slash1$unit_cost <- merged_meds_sub_slash1$price / (merged_meds_sub_slash1$Last3 * merged_meds_sub_slash1$Last1) / merged_meds_sub_slash1$number_before_unit / merged_meds_sub_slash1$Middle3
    
    
    
    #table of unitcost
    table(!is.na(merged_meds_sub_slash1$unit_cost))
    
    table(merged_meds_sub_slash1$Numeric_Count_middle)
    merged_meds_sub_slash1$Middle3[merged_meds_sub_slash1$Numeric_Count_middle ==0] <- 1
    merged_meds_sub_slash1$Middle3[merged_meds_sub_slash1$Numeric_Count_middle ==1] <- 1
    
    
    
    
    
    
    # concatenate the string in merged_meds_sub_slash2$Description_Middle before the forward slash. 
    # Put in new column called MiddleA and the string after the slash and put in new column called MiddleB
    merged_meds_sub_slash2 <- merged_meds_sub_slash2 %>%
      mutate(MiddleA = word(Description_Middle, 1, sep = "/"),  # Extract the first term before the slash
             MiddleB = word(Description_Middle, 2, sep = "/"))  # Extract the second term after the slash
    
    
    
    
    
    table(!is.na(merged_meds_sub_slash1$number_before_unit))
    
    #count numeric terms in middle description
    merged_meds_sub_slash1$Numeric_Count_middle <- sapply(merged_meds_sub_slash1$Description_Middle, count_numeric_terms)
    table(merged_meds_sub_slash1$Numeric_Count_middle)
    
    merged_meds_sub_slash1$number_before_unit <- as.numeric(merged_meds_sub_slash1$number_before_unit)
    
    #if numeric_count_middle == 1, extract the numeric term in Description_Middle
    merged_meds_sub_slash1$Middle3 <- str_extract(merged_meds_sub_slash1$Description_Middle, "\\d+\\.?\\d*")
    
    
    
    #if numeric_count_middle == 0, replace Middle3 with 1
    merged_meds_sub_slash1$Middle3[merged_meds_sub_slash1$Numeric_Count_middle ==0] <- 1
    
    table(is.na(merged_meds_sub_slash1$Middle3))
    
    # if number_before_unit, Last1 and Last3 are NA, replace it with 1,
    merged_meds_sub_slash1$number_before_unit[is.na(merged_meds_sub_slash1$number_before_unit)] <- 1
    merged_meds_sub_slash1$Last1[is.na(merged_meds_sub_slash1$Last1)] <- 1
    merged_meds_sub_slash1$Last3[is.na(merged_meds_sub_slash1$Last3)] <- 1
    merged_meds_sub_slash1$unit_cost <- merged_meds_sub_slash1$price / (merged_meds_sub_slash1$Last3 * merged_meds_sub_slash1$Last1) / merged_meds_sub_slash1$number_before_unit
    
    # if number_before_unit == Middle3, replace Middle3 with 1
    merged_meds_sub_slash1$Middle3[merged_meds_sub_slash1$number_before_unit == merged_meds_sub_slash1$Middle3] <- 1
    merged_meds_sub_slash1$Middle3 <- as.numeric(merged_meds_sub_slash1$Middle3)
    
    # calculate unitcost by price / (Last3 * Last1) / number_before_unit / Middle3
    merged_meds_sub_slash1$unit_cost <- merged_meds_sub_slash1$price / (merged_meds_sub_slash1$Last3 * merged_meds_sub_slash1$Last1) / merged_meds_sub_slash1$number_before_unit / merged_meds_sub_slash1$Middle3

  ###SLASH 2
    
    #table of unitcost
    table(!is.na(merged_meds_sub_slash1$unit_cost))
    
    table(merged_meds_sub_slash1$Numeric_Count_middle)
    merged_meds_sub_slash1$Middle3[merged_meds_sub_slash1$Numeric_Count_middle ==0] <- 1
    merged_meds_sub_slash1$Middle3[merged_meds_sub_slash1$Numeric_Count_middle ==1] <- 1
    
    # Create new column number_comma that counts number of commas in merged_meds_sub_slash2$Substanzen
    merged_meds_sub_slash2$number_substances <- sapply(strsplit(as.character(merged_meds_sub_slash2$Substanzen), ","), length)
    
    table(merged_meds_sub_slash2$number_substances)
    
    # Count numeric terms
    merged_meds_sub_slash2$Numeric_Count_middle <- sapply(merged_meds_sub_slash2$Description_Middle, count_numeric_terms)
    table(merged_meds_sub_slash2$Numeric_Count_middle)
    
    #if numeric_count_middle == 1, extract the numeric term in Description_Middle
    merged_meds_sub_slash2$Middle1 <- str_extract(merged_meds_sub_slash2$Description_Middle, "\\d+\\.?\\d*")
    
    # create a new variable single if number_substances == 1
    merged_meds_sub_slash2$single <- merged_meds_sub_slash2$number_substances == 1
    
    table(merged_meds_sub_slash2$single)
    
    # create subsets of merged_meds_sub_slash2 where single is TRUE and FALSE
    merged_meds_sub_slash2_single <- subset(merged_meds_sub_slash2, single == TRUE)
    merged_meds_sub_slash2_multiple <- subset(merged_meds_sub_slash2, single == FALSE)
      
     ### SINGLE
      # Extract the last word of MiddleA and put it in Middle2
      merged_meds_sub_slash2_single <- merged_meds_sub_slash2_single %>%
        mutate(Middle2 = word(MiddleA, -1))  # Extract the last word

      # Extract the number from MiddleA and put it in Middle1
      merged_meds_sub_slash2_single <- merged_meds_sub_slash2_single %>%
        mutate(Middle1 = as.numeric(str_extract(MiddleA, "\\d+\\.?\\d*")))

      # How many numeric terms are in the string MiddleB? put it in MiddleB_n. For example "140mg" has 1 numeric term, 140 mg has 1 numeric term
      merged_meds_sub_slash2_single <- merged_meds_sub_slash2_single %>%
        mutate(MiddleB_n = str_count(MiddleB, "\\d+\\.?\\d*"))
      
      # if MiddleB_n == 0 or MiddleB_n == 2, code Middle3 as 1
      merged_meds_sub_slash2_single$Middle3[merged_meds_sub_slash2_single$MiddleB_n == 0 | merged_meds_sub_slash2_single$MiddleB_n == 2] <- 1
        
      # if the string MiddleB has no numeric character, code Middle3 as 1
      merged_meds_sub_slash2_single <- merged_meds_sub_slash2_single %>%
        mutate(Middle3 = ifelse(!str_detect(MiddleB, "\\d"), 1, NA))
      
      # if merged_meds_sub_slash2_single$MiddleB contains the substring "ml", encode "ml" in Middle4
      merged_meds_sub_slash2_single <- merged_meds_sub_slash2_single %>%
        mutate(Middle4 = ifelse(grepl("ml", MiddleB, ignore.case = TRUE), "ml", NA))
      
      # Extract the number from MiddleB into Middle3 when Middle3 is NA
      merged_meds_sub_slash2_single <- merged_meds_sub_slash2_single %>%
        mutate(Middle3 = ifelse(is.na(Middle3), 
                                as.numeric(str_extract(MiddleB, "\\d+\\.?\\d*")), 
                                Middle3))
      # Recode Middle3 to 1 if MiddleB_n is 2
      merged_meds_sub_slash2_single <- merged_meds_sub_slash2_single %>%
        mutate(Middle3 = ifelse(MiddleB_n == 2, 1, Middle3))
     
      # Extract non-numeric characters from MiddleB and put it in Middle4 if Middle4 is NA
      merged_meds_sub_slash2_single <- merged_meds_sub_slash2_single %>%
        mutate(Middle4 = ifelse(is.na(Middle4), 
                                str_extract(MiddleB, "[^0-9\\.]+"),  # Extract non-numeric characters
                                Middle4))
      #table of middle4
      table(merged_meds_sub_slash2_single$Middle4)

      # Rearranging columns in the data frame
      merged_meds_sub_slash2_single <- merged_meds_sub_slash2_single %>%
        select(
          everything(),  # Select all columns
          Middle1, Middle2, Middle3, Middle4,  # Move Middle columns to 3rd to 6th positions
          Last1, Last2, Last3, Last4           # Move Last columns to 7th to 10th positions
        ) %>%
        select(
          -c(Middle1, Middle2, Middle3, Middle4, Last1, Last2, Last3, Last4), # Remove duplicated middle and last columns
          everything()  # Select remaining columns again
        ) %>%
        relocate(Middle1, .after = 2) %>% # Move Middle1 to 3rd position
        relocate(Middle2, .after = 3) %>% # Move Middle2 to 4th position
        relocate(Middle3, .after = 4) %>% # Move Middle3 to 5th position
        relocate(Middle4, .after = 5) %>% # Move Middle4 to 6th position
        relocate(Last1, .after = 6) %>%    # Move Last1 to 7th position
        relocate(Last2, .after = 7) %>%    # Move Last2 to 8th position
        relocate(Last3, .after = 8) %>%    # Move Last3 to 9th position
        relocate(Last4, .after = 9)         # Move Last4 to 10th position
      
      # calculate unitcost by price / Last3 / Middle1 / Middle3
      merged_meds_sub_slash2_single$unit_cost <- merged_meds_sub_slash2_single$price / merged_meds_sub_slash2_single$Last3 / merged_meds_sub_slash2_single$Middle1 / merged_meds_sub_slash2_single$Middle3

    ### MULTIPLE
      # Extract the number from MiddleA and put it in Middle1
      merged_meds_sub_slash2_multiple <- merged_meds_sub_slash2_multiple %>%
        mutate(Middle1 = as.numeric(str_extract(MiddleA, "\\d+\\.?\\d*")))
      
      # Apply the logic
      merged_meds_sub_slash2_multiple <- merged_meds_sub_slash2_multiple %>%
        mutate(
          # Step 1: Extract substring from the start until the last numeric character for Middle1
          Middle1e = str_extract(MiddleA, "^.*\\d"),  # Extract everything until the last numeric character
          
          # Step 2: Extract the remaining characters after the last numeric character for Middle2
          Middle2 = str_trim(str_replace(MiddleA, "^.*\\d", ""))  # Remove the numeric part and capture remaining text
        )
      
      # Apply the logic for MiddleB
      merged_meds_sub_slash2_multiple <- merged_meds_sub_slash2_multiple %>%
        mutate(
          # Step 1: Extract substring from the start until the last numeric character for Middle1
          Middle3 = str_extract(MiddleB, "^.*\\d"),  # Extract everything until the last numeric character
          
          # Step 2: Extract the remaining characters after the last numeric character for Middle2
          Middle4 = str_trim(str_replace(MiddleB, "^.*\\d", ""))  # Remove the numeric part and capture remaining text
        )
      
      merged_meds_sub_slash2_multiple$Middle3 <- as.numeric(merged_meds_sub_slash2_multiple$Middle3)
      
      # Update Middle2 to copy Middle4 if Middle2 is only spaces or NA
      merged_meds_sub_slash2_multiple <- merged_meds_sub_slash2_multiple %>%
        mutate(
          Middle2 = ifelse(is.na(Middle2) | trimws(Middle2) == "", Middle4, Middle2)  # Copy Middle4 to Middle2 if Middle2 is NA or empty
        )
      
      # Create a new variable 'vitamin' based on the presence of the substring "vitamin" or "Vitamin"
      merged_meds_sub_slash2_multiple <- merged_meds_sub_slash2_multiple %>%
        mutate(
          vitamin = str_detect(Substanzen, regex("vitamin", ignore_case = TRUE))  # Check for substring "vitamin" or "Vitamin"
        )
      
      # how many vitamin == TRUE
      table(merged_meds_sub_slash2_multiple$vitamin)
      
      # Update Middle2 and Middle4 based on the vitamin variable
      merged_meds_sub_slash2_multiple <- merged_meds_sub_slash2_multiple %>%
        mutate(
          Middle2 = ifelse(vitamin, "mg", Middle2),  # Set Middle2 to "mg" if vitamin is TRUE
          Middle4 = ifelse(vitamin, "UI", Middle4)   # Set Middle4 to "UI" if vitamin is TRUE
        )
      
      # Replace Middle2 and Middle4 with "mg" if they are spaces or NA
      merged_meds_sub_slash2_multiple <- merged_meds_sub_slash2_multiple %>%
        mutate(
          Middle2 = ifelse(is.na(Middle2) | trimws(Middle2) == "", "mg", Middle2),  # Replace with "mg" if Middle2 is NA or empty
          Middle4 = ifelse(is.na(Middle4) | trimws(Middle4) == "", "mg", Middle4)   # Replace with "mg" if Middle4 is NA or empty
        )
   
      # Rearranging columns in the data frame
      merged_meds_sub_slash2_multiple <- merged_meds_sub_slash2_multiple %>%
        select(
          everything(),  # Select all columns
          Middle1, Middle2, Middle3, Middle4,  # Move Middle columns to 3rd to 6th positions
          Last1, Last2, Last3, Last4           # Move Last columns to 7th to 10th positions
        ) %>%
        select(
          -c(Middle1, Middle2, Middle3, Middle4, Last1, Last2, Last3, Last4), # Remove duplicated middle and last columns
          everything()  # Select remaining columns again
        ) %>%
        relocate(Middle1, .after = 2) %>% # Move Middle1 to 3rd position
        relocate(Middle2, .after = 3) %>% # Move Middle2 to 4th position
        relocate(Middle3, .after = 4) %>% # Move Middle3 to 5th position
        relocate(Middle4, .after = 5) %>% # Move Middle4 to 6th position
        relocate(Last1, .after = 6) %>%    # Move Last1 to 7th position
        relocate(Last2, .after = 7) %>%    # Move Last2 to 8th position
        relocate(Last3, .after = 8) %>%    # Move Last3 to 9th position
        relocate(Last4, .after = 9)         # Move Last4 to 10th position
      
      #if Last1 == NA, replace it with 1
      merged_meds_sub_slash2_multiple$Last1[is.na(merged_meds_sub_slash2_multiple$Last1)] <- 1
      
      # calculate unitcost by price / Last3 / Last1 / Middle1 
      merged_meds_sub_slash2_multiple$unit_cost <- merged_meds_sub_slash2_multiple$price / merged_meds_sub_slash2_multiple$Last3 / merged_meds_sub_slash2_multiple$Last1 /merged_meds_sub_slash2_multiple$Middle1
      
   # join merged_meds_sub_slash2_single and merge merged_meds_sub_slash2_multiple
   merged_meds_sub_slash2_merged <- full_join(merged_meds_sub_slash2_single, merged_meds_sub_slash2_multiple, by = c())

   # join merged_meds_sub_slash1 and merged_meds_sub_slash2_merged
   overall <- full_join(merged_meds_sub_slash1, merged_meds_sub_slash2_merged, by = c())
   
   # remove functions and dataframes except overall and publications
   rm(list=ls()[!ls() %in% c("overall", "Publications")])
   
   #rename Publications$Bezeichnung to Publications$Description
   colnames(Publications)[2] <- "Description"
   
   #how many unique entries are in Publications$Bezeichnung
   length(unique(Publications$'Rec-ID'))
 
   
   # append overall$unit_cost to Publications dataframe. at rows in Publications that do not have a corresponding row in overall, unit_cost will be NA
   Publications <- merge(Publications, overall[, c('Rec-ID', "unit_cost")], by = 'Rec-ID', all.x = TRUE)
   
   # as numeric 
   Publications$Gammennummer <- as.numeric(Publications$Gammennummer)
   
   # scatterplot of Gammennummer and unit_cost
   plot(Publications$Gammennummer, Publications$unit_cost)
   plot(Publications$'Rec-ID', Publications$unit_cost)
   
  

   
   