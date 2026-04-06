



# ---- Import data ----

Anagha_summer_project_form_2025_raw <- read_excel("data-raw/Data abstraction Excel reviewer 1 and 2.xlsx", sheet = "Reviewer 2")

# ---- Clean data ----

Anagha_summer_project_form_clean <- Anagha_summer_project_form_2025_raw %>%
  clean_names()%>%
  filter(confirmed_by_vh == "Yes")

#Create flowchart of STROBE diagram
Anagha_summer_project_form_clean_fc<-Anagha_summer_project_form_clean%>%
  as_fc(label = "Initial papers from Pubmed search")%>%
  fc_filter(is_this_paper_a_new_data_analysis == "Yes",
            label="New patient-level data analysis",
            show_exc = TRUE)%>%
  
  fc_split(is_this_a_transplant_related_study,
           label=c("Not transplant-related", "Transplant-related"))

fc_draw(Anagha_summer_project_form_clean_fc)%>%
  fc_export("Letter analysis/flowchart.png")

#Create final dataset for analysis
Anagha_summer_project_form_final <- Anagha_summer_project_form_clean_fc$data %>%
  
  #Language variables
  mutate(languages_used_R=str_detect(what_programming_language_s_was_were_used, "R"))%>%
  mutate(languages_used_SAS=str_detect(what_programming_language_s_was_were_used, "SAS"))%>%
  mutate(languages_used_Stata=str_detect(what_programming_language_s_was_were_used, "STATA"))%>%
  mutate(languages_used_SPSS=str_detect(what_programming_language_s_was_were_used, "SPSS"))%>%
  
  #Files used
  mutate(files_used_Core=str_detect(what_files_were_used, "Core"))%>%
  mutate(files_used_Institution=str_detect(what_files_were_used, "Institution"))%>%
  mutate(files_used_PhysicianSupplier=str_detect(what_files_were_used, "Physician/Supplier"))%>%
  mutate(files_used_Hospital=str_detect(what_files_were_used	, "Hospital"))%>%
  mutate(files_used_Transplant=str_detect(what_files_were_used	, "Transplant"))%>%
  mutate(files_used_PartD=str_detect(what_files_were_used, "Part D"))%>%
  mutate(files_used_CROWNWeb=str_detect(what_files_were_used, "CROWNWeb"))%>%
  
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
    is_this_a_transplant_related_study,
    
    did_the_authors_state_what_programming_language_they_used,
    languages_used_R,
    languages_used_SAS,
    languages_used_Stata,
    languages_used_SPSS,
    
    did_the_authors_explicitly_state_what_files_they_used,
    
    does_the_choice_of_files_codes_seem_appropriate_for_the_research_question,
    files_used_Core,
    files_used_Institution,
    files_used_PhysicianSupplier,
    files_used_Hospital,
    files_used_Transplant,
    files_used_PartD,
    files_used_CROWNWeb,

    component_basicdemographics,
    component_icd9,
    component_icd10,
    component_cptcode,
    component_medicationD,
    component_transplant,
    component_costs,
    component_residential,

    were_any_crosswalks_used,
            
    did_the_authors_share_the_code_they_used,
    was_the_study_funded,
    would_the_major_data_tasks_needed_for_this_study_be_covered_by_currently_proposed_usrds_r_package


  ) %>%
  
  tbl_summary(by = is_this_a_transplant_related_study,
              label=label_list,
              missing = "no"
  ) %>%

  modify_spanning_header(all_stat_cols() ~ "**Transplant-related study?**")%>%
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
      files_used_Core,
      files_used_Institution,
      files_used_PhysicianSupplier,
      files_used_Hospital,
      files_used_Transplant,
      files_used_PartD,
      files_used_CROWNWeb,
      
    )
  )%>%
  
  add_variable_group_header(
    header = "Programming language used",
    variables = c(
      languages_used_R,
      languages_used_SAS,
      languages_used_Stata,
      languages_used_SPSS,
      
    )
  )%>%
  modify_footnote_body(
    footnote = "Categories in this section are not mutually exclusive; percentages may not sum to 100%.",
    columns = label,
    rows = label %in% c(
      "Data components abstracted",
      "Files used",
      "Programming language used"
    )
  ) %>%
  
  as_gt() %>%
  gtsave(filename = "Letter analysis/Letter analysis Table 2 stratified by transplant.html")

Anagha_summer_project_form_split[["transplant"]] %>%
  
  
  select(
    which_type_of_transplant_study_is_this,
    
    did_the_authors_state_what_programming_language_they_used,
    languages_used_R,
    languages_used_SAS,
    languages_used_Stata,
    languages_used_SPSS,
    
    did_the_authors_explicitly_state_what_files_they_used,
    
    does_the_choice_of_files_codes_seem_appropriate_for_the_research_question,
    files_used_Core,
    files_used_Institution,
    files_used_PhysicianSupplier,
    files_used_Hospital,
    files_used_Transplant,
    files_used_PartD,
    files_used_CROWNWeb,
    
    component_basicdemographics,
    component_icd9,
    component_icd10,
    component_cptcode,
    component_medicationD,
    component_transplant,
    component_costs,
    component_residential,
    
    were_any_crosswalks_used,
    
    did_the_authors_share_the_code_they_used,
    was_the_study_funded,
    would_the_major_data_tasks_needed_for_this_study_be_covered_by_currently_proposed_usrds_r_package
    
    
  )%>%
  

  tbl_summary(by = which_type_of_transplant_study_is_this,
              label=label_list,
              missing = "no") %>%
  
    modify_spanning_header(all_stat_cols() ~ "**Type of transplant study**")%>%
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
        files_used_Core,
        files_used_Institution,
        files_used_PhysicianSupplier,
        files_used_Hospital,
        files_used_Transplant,
        files_used_PartD,
        files_used_CROWNWeb,
        
      )
    )%>%
    
    add_variable_group_header(
      header = "Programming language used",
      variables = c(
        languages_used_R,
        languages_used_SAS,
        languages_used_Stata,
        languages_used_SPSS,
        
      )
    )%>%
  modify_footnote_body(
    footnote = "Categories in this section are not mutually exclusive; percentages may not sum to 100%.",
    columns = label,
    rows = label %in% c(
      "Data components abstracted",
      "Files used",
      "Programming language used"
    )
  ) %>%
    
  as_gt() %>%
  gtsave(filename = "Letter analysis/Letter analysis Table 3 stratified by transplant study type.html")
