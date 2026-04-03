

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
  tbl_summary(by = is_this_a_transplant_related_study) %>%
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
    -what_programming_language_s_was_were_used,-what_files_were_used
  ) %>%
  tbl_summary(by = which_type_of_transplant_study_is_this) %>%
  as_gt() %>%
  gtsave(filename = "Letter analysis/Letter analysis Table 3 stratified by transplant study type.html")
