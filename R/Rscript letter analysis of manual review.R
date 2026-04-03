

# ---- Load libraries ----

library(tidyverse)
library(janitor)
library(readxl)
library(gtsummary)
library(gt)


# ---- Import data ----

Anagha_summer_project_form_2025_raw <- read_excel("data-raw/Data abstraction Excel reviewer 1 and 2.xlsx", sheet = "Reviewer 2")

# ---- Clean data ----

Anagha_summer_project_form_clean <- Anagha_summer_project_form_2025_raw %>%
  clean_names() %>%
  filter(confirmed_by_vh == "Yes")

Anagha_summer_project_form_clean %>%
  count(is_this_paper_a_new_data_analysis == "Yes")
Anagha_summer_project_form_clean <- Anagha_summer_project_form_clean %>%
  filter(is_this_paper_a_new_data_analysis == "Yes")

Anagha_summer_project_form_clean %>%
  count(is_this_a_transplant_related_study == "No")
Anagha_summer_project_form_clean %>%
  count(is_this_a_transplant_related_study == "Yes")

Anagha_summer_project_form_final <- Anagha_summer_project_form_clean %>%
  
  #Language variables
  mutate(language_r = str_detect(what_programming_language_s_was_were_used, "R")) %>%
  mutate(language_sas = str_detect(what_programming_language_s_was_were_used, "SAS")) %>%
  mutate(language_stata = str_detect(what_programming_language_s_was_were_used, "STATA")) %>%
  mutate(language_spss = str_detect(what_programming_language_s_was_were_used, "SPSS")) %>%
  
  #Files used
  mutate(file_core = str_detect(what_files_were_used, "Core")) %>%
  mutate(file_institution = str_detect(what_files_were_used, "Institution")) %>%
  mutate(file_ps = str_detect(what_files_were_used, "Physician/Supplier")) %>%
  mutate(file_hospital = str_detect(what_files_were_used	, "Hospital")) %>%
  mutate(file_transplant = str_detect(what_files_were_used	, "Transplant")) %>%
  mutate(file_partd = str_detect(what_files_were_used, "Part D")) %>%
  mutate(file_CROWNWeb = str_detect(what_files_were_used, "CROWNWeb")) %>%
  
  #Components extracted
  mutate(
    component_basicdemographics = str_detect(
      which_pieces_of_data_were_abstracted_from_the_files,
      "Basic demographics"
    )
  ) %>%
  mutate(
    component_icd9 = str_detect(
      which_pieces_of_data_were_abstracted_from_the_files,
      "ICD9 code"
    )
  ) %>%
  mutate(
    component_icd10 = str_detect(
      which_pieces_of_data_were_abstracted_from_the_files,
      "ICD10 code"
    )
  ) %>%
  mutate(
    component_cptcode = str_detect(
      which_pieces_of_data_were_abstracted_from_the_files,
      "CPT code"
    )
  ) %>%
  mutate(
    component_medicationD = str_detect(
      which_pieces_of_data_were_abstracted_from_the_files,
      "Medications"
    )
  ) %>%
  mutate(
    component_transplant = str_detect(
      which_pieces_of_data_were_abstracted_from_the_files,
      "Transplant-specific information"
    )
  ) %>%
  mutate(component_costs = str_detect(
    which_pieces_of_data_were_abstracted_from_the_files,
    "Costs"
  )) %>%
  mutate(
    component_residential = str_detect(
      which_pieces_of_data_were_abstracted_from_the_files,
      "Residential location"
    )
  )

#Split the final data set into non-transplant and transplant components
Anagha_summer_project_form_split <- Anagha_summer_project_form_final %>%
group_by(is_this_a_transplant_related_study) %>%
  group_split() %>%
  set_names(c("not_transplant", "transplant"))


# ---- Analysis and table creation ----


#Save the overall table as a document
Anagha_summer_project_form_final %>%
  select(
    -filepath,
    -timestamp,
    -authors,
    -title,
    -journal,
    -year,
    -what_programming_language_s_was_were_used,-what_files_were_used
  ) %>%
  tbl_summary() %>%
  as_gt() %>%
  gtsave(filename = "Letter analysis/Letter analysis Table 1 overall raw.html")

Anagha_summer_project_form_final %>%
  select(
    -timestamp,-filepath,-authors,-title,-journal,-year,-what_programming_language_s_was_were_used,-what_files_were_used,-which_type_of_transplant_study_is_this
  ) %>%
  tbl_summary() %>%
  as_gt() %>%
  gtsave(filename = "Letter analysis/Letter analysis Table 1 overall raw.html")

#Create variable label list
label_list = list(
  is_this_paper_a_new_data_analysis ~ "New data analysis",
  which_type_of_transplant_study_is_this ~ "Type of transplant study",
  did_the_authors_state_what_programming_language_they_used ~ "Programming language stated",
  did_the_authors_explicitly_state_what_files_they_used ~ "USRDS files explicitly stated",
  which_pieces_of_data_were_abstracted_from_the_files ~ "Data elements abstracted",
  does_the_choice_of_files_codes_seem_appropriate_for_the_research_question ~ "Files/codes appropriate for research question",
  comments_16 ~ "Comments on file/code choice",
  did_the_authors_share_the_code_they_used ~ "Authors shared code",
  was_the_study_funded ~ "Study funded",
  who_funded_the_study ~ "Study funder",
  would_the_major_data_tasks_needed_for_this_study_be_covered_by_currently_proposed_usrds_r_package ~ "Covered by proposed usRds package",
  which_tasks_would_not_be_covered ~ "Tasks not covered by usRds",
  were_any_crosswalks_used ~ "Any crosswalks used",
  which_crosswalks_were_used ~ "Crosswalks used",
  confirmed_by_vh ~ "Confirmed by VH",
  changes_made ~ "Changes made after review",
  comments_26 ~ "Reviewer comments",
  language_r ~ "R used",
  language_sas ~ "SAS used",
  language_stata ~ "Stata used",
  language_spss ~ "SPSS used",
  file_core ~ "Core file used",
  file_institution ~ "Institution file used",
  file_ps ~ "Physician/Supplier file used",
  file_hospital ~ "Hospital file used",
  file_transplant ~ "Transplant file used",
  file_partd ~ "Part D file used",
  file_CROWNWeb ~ "CROWNWeb file used",
  component_basicdemographics ~ "Basic demographics abstracted",
  component_icd9 ~ "ICD-9 codes abstracted",
  component_icd10 ~ "ICD-10 codes abstracted",
  component_cptcode ~ "CPT codes abstracted",
  component_medicationD ~ "Part D medication data abstracted",
  component_transplant ~ "Transplant-specific data abstracted",
  component_costs ~ "Cost data abstracted",
  component_residential ~ "Residential location abstracted"
)




Anagha_summer_project_form_final %>%
  select(
    -filepath,
    -timestamp,
    -authors,
    -title,
    -journal,
    -year,
    -what_programming_language_s_was_were_used,-what_files_were_used
  ) %>%
  tbl_summary(by = is_this_a_transplant_related_study,
              label=label_list
              ) %>%
  
  add_variable_group_header(
    header = "Data components abstracted",
    variables = c(
      component_basicdemographics,
      component_icd9,
      component_icd10,
      component_cptcode,
      component_medicationD,
      component_transplant,
      component_costs,
      component_residential
    )
  )%>%
  
  add_variable_group_header(
    header = "Files used",
    variables = c(
      file_core,
      file_institution,
      file_ps,
      file_hospital,
      file_transplant,
      file_partd,
      file_CROWNWeb
    
    )
  )%>%
  
  add_variable_group_header(
    header = "Files used",
    variables = c(
      language_r,
      language_sas,
      language_stata,
      language_spss
      
    )
  )%>%
  
  as_gt() %>%
  gtsave(filename = "Letter analysis/Letter analysis Table 2 stratified by transplant.html")

Anagha_summer_project_form_split[["transplant"]] %>%
  select(
    -filepath,
    -timestamp,
    -authors,
    -title,
    -journal,
    -year,
    -what_programming_language_s_was_were_used,
    -what_files_were_used
  ) %>%
  tbl_summary(by = which_type_of_transplant_study_is_this) %>%
  as_gt() %>%
  gtsave(filename = "Letter analysis/Letter analysis Table 3 stratified by transplant study type.html")
