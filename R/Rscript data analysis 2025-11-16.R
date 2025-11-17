library(tidyverse)
library(readr)
library(readxl)
library(janitor)

final_results_ChatGPT_raw <- readRDS("C:/Users/katta/OneDrive - Montefiore Medicine/Ellmer project/final_results_ChatGPT.rds")
summary_final_claude_raw <- readRDS("C:/Users/katta/OneDrive - Montefiore Medicine/Ellmer project/summary_final_claude.rds")%>%
  bind_rows()
summary_final_gemini_raw <- read_csv("summary_final_gemini.csv")

human_results_raw <- read_excel("C:/Users/katta/OneDrive - Montefiore Medicine/Desktop backup 2025-2-20/Anagha project/Anagha summer project form files/Anagha summer project form 2025-10-29.xlsx", 
                                                    sheet = "Vagish Hemmige summer project f")%>%
  clean_names()



final_results_ChatGPT_clean <- final_results_ChatGPT_raw %>%
  rename_with(~ paste0(.x, "_ChatGPT"), -filename)

summary_final_claude_clean <- summary_final_claude_raw %>%
  rename_with(~ paste0(.x, "_claude"), -filename)

summary_final_gemini_clean <- summary_final_gemini_raw %>%
  rename_with(~ paste0(.x, "_gemini"), -filename)

human_results_clean<-human_results_raw%>%
  
  #Language variables
  mutate(language_r=str_detect(what_programming_language_s_was_were_used, "R"))%>%
  mutate(language_sas=str_detect(what_programming_language_s_was_were_used, "SAS"))%>%
  mutate(language_stata=str_detect(what_programming_language_s_was_were_used, "STATA"))%>%
  mutate(language_spss=str_detect(what_programming_language_s_was_were_used, "SPSS"))%>%
  
  #Files used
  mutate(file_core=str_detect(what_files_were_used, "Core"))%>%
  mutate(file_institution=str_detect(what_files_were_used, "Institution"))%>%
  mutate(file_ps=str_detect(what_files_were_used, "Physician/Supplier"))%>%
  mutate(file_hospital=str_detect(what_files_were_used	, "Hospital"))%>%
  mutate(file_transplant=str_detect(what_files_were_used	, "Transplant"))%>%
  mutate(file_partd=str_detect(what_files_were_used, "Part D"))%>%
  mutate(file_CROWNWeb=str_detect(what_files_were_used, "CROWNWeb"))%>%
  
  #Components extracted
  mutate(component_basicdemographics=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Basic demographics"))%>%
  mutate(component_icd9=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "ICD9 code"))%>%
  mutate(component_icd10=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "ICD10 code"))%>%
  mutate(component_cptcode=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "CPT code"))%>%
  mutate(component_medicationD=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Medications"))%>%
  mutate(component_transplant=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Transplant-specific information"))%>%
  mutate(component_costs=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Costs"))%>%
  mutate(component_residential=str_detect(which_pieces_of_data_were_abstracted_from_the_files, "Residential location"))
  