#Data analysis script

library(tidyverse)
library(readr)
library(readxl)
library(writexl)
library(janitor)

#Import raw data from files
final_results_ChatGPT_raw <- readRDS("Results/ChatGPT/final_results_ChatGPT.rds")
summary_final_claude_raw <- readRDS("Results/Claude/summary_final_claude.rds")%>%
  bind_rows()
summary_final_gemini_raw <- read_csv("Results/Gemini/summary_output_gemini.csv")
human_results_raw <- read_excel("data-raw/Data abstraction Excel reviewer 1 and 2.xlsx", 
                                sheet = "Reviewer 2")%>%
  clean_names()


#Clean data by renaming with suffix and removing unneccesary columns
final_results_ChatGPT_clean <- final_results_ChatGPT_raw %>%
  rename_with(~ paste0(.x, "_ChatGPT"), -filename)%>%
  select(-text_ChatGPT, -raw_json_ChatGPT)

summary_final_claude_clean <- summary_final_claude_raw %>%
  rename_with(~ paste0(.x, "_claude"), -filename)

summary_final_gemini_clean <- summary_final_gemini_raw %>%
  rename_with(~ paste0(.x, "_gemini"), -filename)

human_results_clean<-human_results_raw%>%
  #Language variables--split into multiple columns
  mutate(languages_used_R=str_detect(what_programming_language_s_was_were_used, "R"))%>%
  mutate(languages_used_SAS=str_detect(what_programming_language_s_was_were_used, "SAS"))%>%
  mutate(languages_used_Stata=str_detect(what_programming_language_s_was_were_used, "STATA"))%>%
  mutate(languages_used_SPSS=str_detect(what_programming_language_s_was_were_used, "SPSS"))%>%
  mutate(languages_used_Python=str_detect(what_programming_language_s_was_were_used, "Python"))%>%
  mutate(languages_used_SQL=str_detect(what_programming_language_s_was_were_used, "SQL"))%>%
  mutate(languages_used_Julia=str_detect(what_programming_language_s_was_were_used, "Julia"))%>%
  mutate(languages_used_Matlab=str_detect(what_programming_language_s_was_were_used, "Matlab"))%>%
  
  #Files used--split into multiple columns
  mutate(files_used_Core=str_detect(what_files_were_used, "Core"))%>%
  mutate(files_used_Institution=str_detect(what_files_were_used, "Institution"))%>%
  mutate(files_used_PhysicianSupplier=str_detect(what_files_were_used, "Physician/Supplier"))%>%
  mutate(files_used_Hospital=str_detect(what_files_were_used	, "Hospital"))%>%
  mutate(files_used_Transplant=str_detect(what_files_were_used	, "Transplant"))%>%
  mutate(files_used_PartD=str_detect(what_files_were_used, "Part D"))%>%
  mutate(files_used_CROWNWeb=str_detect(what_files_were_used, "CROWNWeb"))%>%
  
  #Components extracted--split into multiple columns
  mutate(component_basicdemographics=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Basic demographics"))%>%
  mutate(component_icd9=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "ICD9 code"))%>%
  mutate(component_icd10=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "ICD10 code"))%>%
  mutate(component_cptcode=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "CPT code"))%>%
  mutate(component_medicationD=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Medications"))%>%
  mutate(component_transplant=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Transplant-specific information"))%>%
  mutate(component_costs=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Costs"))%>%
  mutate(component_residential=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Residential location"))%>%
  
  #Selecting out variables that are not in the LLM data sets
  select(-timestamp, -changes_made, -comments_16, -comments_26, -confirmed_by_vh,
         -what_files_were_used, -what_programming_language_s_was_were_used, -which_crosswalks_were_used)%>%
  
  #Rename variables for merging purposes
  rename(filename=filepath)%>%
  rename_with(~ paste0(.x, "_human"), -filename)%>%
  
  #Convert all Yes/No into logical
  mutate(across(where(~ all(.x %in% c("Yes", "No", NA))), 
                ~ case_when(
                  .x == "Yes" ~ TRUE,
                  .x == "No"  ~ FALSE,
                  TRUE ~ NA
                )))%>%
  
  #Make sure all filenames end in .pdf
  mutate(filename = if_else(
    str_ends(filename, "\\.pdf"),
    filename,
    paste0(filename, ".pdf")
  ))

#Ensure filenames match before merging
setdiff(human_results_clean$filename, final_results_ChatGPT_clean$filename)
setdiff(final_results_ChatGPT_clean$filename, human_results_clean$filename)

setdiff(human_results_clean$filename, summary_final_claude_clean$filename)
setdiff(summary_final_claude_clean$filename, human_results_clean$filename)

setdiff(human_results_clean$filename, summary_final_gemini_clean$filename)
setdiff(summary_final_gemini_clean$filename, human_results_clean$filename)

#Gemini unable to parse a single PDF despite multiple attempts

#Merge into a large mega_file
df_wide<-human_results_clean%>%
  left_join(final_results_ChatGPT_clean, by = join_by(filename))%>%
  left_join(summary_final_claude_clean, by = join_by(filename))%>%
  left_join(summary_final_gemini_clean, by = join_by(filename))%>%
  filter(!is.na(filename))
  
df_wide<-df_wide%>%
  #Resort with filename first, then all variable names in alphabetical order
  select(filename, sort(setdiff(names(.), "filename")))

#write
  