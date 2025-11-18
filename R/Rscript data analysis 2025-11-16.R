library(tidyverse)
library(readr)
library(readxl)
library(janitor)

final_results_ChatGPT_raw <- readRDS("Results/ChatGPT/final_results_ChatGPT.rds")
summary_final_claude_raw <- readRDS("Results/Claude/summary_final_claude.rds")%>%
  bind_rows()
summary_final_gemini_raw <- read_csv("Results/Gemini/summary_output_gemini.csv")

human_results_raw <- read_excel("data-raw/Data abstraction Excel reviewer 1 and 2.xlsx", 
                                sheet = "Reviewer 2")%>%
  clean_names()



final_results_ChatGPT_clean <- final_results_ChatGPT_raw %>%
  rename_with(~ paste0(.x, "_ChatGPT"), -filename)

summary_final_claude_clean <- summary_final_claude_raw %>%
  rename_with(~ paste0(.x, "_claude"), -filename)

summary_final_gemini_clean <- summary_final_gemini_raw %>%
  rename_with(~ paste0(.x, "_gemini"), -filename)

human_results_clean<-human_results_raw%>%
  
  #Language variables
  mutate(languages_used_R=str_detect(what_programming_language_s_was_were_used, "R"))%>%
  mutate(languages_used_SAS=str_detect(what_programming_language_s_was_were_used, "SAS"))%>%
  mutate(languages_used_Stata=str_detect(what_programming_language_s_was_were_used, "STATA"))%>%
  mutate(languages_used_SPSS=str_detect(what_programming_language_s_was_were_used, "SPSS"))%>%
  mutate(languages_used_Python=str_detect(what_programming_language_s_was_were_used, "Python"))%>%
  mutate(languages_used_SQL=str_detect(what_programming_language_s_was_were_used, "SQL"))%>%
  mutate(languages_used_Julia=str_detect(what_programming_language_s_was_were_used, "Julia"))%>%
  mutate(languages_used_Matlab=str_detect(what_programming_language_s_was_were_used, "Matlab"))%>%
  
  
  #Files used
  mutate(files_used_Core=str_detect(what_files_were_used, "Core"))%>%
  mutate(files_used_Institution=str_detect(what_files_were_used, "Institution"))%>%
  mutate(files_used_PhysicianSupplier=str_detect(what_files_were_used, "Physician/Supplier"))%>%
  mutate(files_used_Hospital=str_detect(what_files_were_used	, "Hospital"))%>%
  mutate(files_used_Transplant=str_detect(what_files_were_used	, "Transplant"))%>%
  mutate(files_used_PartD=str_detect(what_files_were_used, "Part D"))%>%
  mutate(files_used_CROWNWeb=str_detect(what_files_were_used, "CROWNWeb"))%>%
  
  #Components extracted
  mutate(component_basicdemographics=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Basic demographics"))%>%
  mutate(component_icd9=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "ICD9 code"))%>%
  mutate(component_icd10=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "ICD10 code"))%>%
  mutate(component_cptcode=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "CPT code"))%>%
  mutate(component_medicationD=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Medications"))%>%
  mutate(component_transplant=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Transplant-specific information"))%>%
  mutate(component_costs=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Costs"))%>%
  mutate(component_residential=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Residential location"))
  