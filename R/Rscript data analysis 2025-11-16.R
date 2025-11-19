#Data analysis script

library(tidyverse)
library(readr)
library(readxl)
library(writexl)
library(janitor)
library(irr)
library(gtsummary)
library(gt)
library(officer)

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


#----------------------------------------------------------------
# Make sure all variables are logical format
#----------------------------------------------------------------

df_wide<-df_wide%>%
  #Resort with filename first, then all variable names in alphabetical order
  select(filename, sort(setdiff(names(.), "filename")))

# Function that safely normalizes to logical
convert_to_logical <- function(x) {
  if (is.logical(x)) return(x)
  
  x_clean <- trimws(tolower(as.character(x)))
  
  case_when(
    x_clean %in% c("true", "yes", "1")  ~ TRUE,
    x_clean %in% c("false", "no", "0") ~ FALSE,
    TRUE ~ NA
  )
}

# Identify all LLM + human columns that should be logical
logical_cols <- names(df_wide)[
  grepl("_ChatGPT$|_claude$|_gemini$|_human$", names(df_wide))
]

# Apply the conversion
df_wide <- df_wide %>%
  mutate(across(all_of(logical_cols), convert_to_logical))

#Save intermediate step
write_xlsx(df_wide, "Results/Cleaned and merged data.xlsx")  


#How well can LLMs differentiate new data analyses from papers that are not new data analyses?
df_wide %>% count(is_this_paper_a_new_data_analysis_human,
             is_this_paper_a_new_data_analysis_ChatGPT)

df_wide %>% count(is_this_paper_a_new_data_analysis_human,
                  is_this_paper_a_new_data_analysis_claude)

df_wide %>% count(is_this_paper_a_new_data_analysis_human,
                  is_this_paper_a_new_data_analysis_gemini)

#When limiting to true new analyses, how well can LLMs spot transplant-related papers?
df_wide%>%filter(is_this_paper_a_new_data_analysis_human==TRUE)%>%
  count(is_this_a_transplant_related_study_human,
        is_this_a_transplant_related_study_ChatGPT)
  
df_wide%>%filter(is_this_paper_a_new_data_analysis_human==TRUE)%>%
  count(is_this_a_transplant_related_study_human,
        is_this_a_transplant_related_study_claude)

df_wide%>%filter(is_this_paper_a_new_data_analysis_human==TRUE)%>%
  count(is_this_a_transplant_related_study_human,
        is_this_a_transplant_related_study_gemini)


#Create a table 2
#----------------------------------------------------------------
# 1. Filter to human-positive dataset
#----------------------------------------------------------------
df_filt <- df_wide %>%
  filter(
    is_this_paper_a_new_data_analysis_human == TRUE,
    is_this_a_transplant_related_study_human == TRUE
  )

#----------------------------------------------------------------
# 2. Identify all human-coded boolean variables
#----------------------------------------------------------------
human_vars <- names(df_filt)[grepl("_human$", names(df_filt))]

#----------------------------------------------------------------
# 3. Compute kappa helper
#----------------------------------------------------------------
compute_kappa <- function(hvar, mvar) {
  if (!(mvar %in% names(df_filt))) return(NA_real_)
  
  df_k <- df_filt %>%
    select(h = all_of(hvar), m = all_of(mvar)) %>%
    drop_na()
  
  if (nrow(df_k) == 0) return(NA_real_)
  
  kappa2(df_k, weight = "unweighted")$value
}

#----------------------------------------------------------------
# 4. Compute full kappa table
#----------------------------------------------------------------
kappa_results <- map_dfr(human_vars, function(h) {
  base <- sub("_human$", "", h)
  
  tibble(
    variable = base,
    ChatGPT = compute_kappa(h, paste0(base, "_ChatGPT")),
    Claude  = compute_kappa(h, paste0(base, "_claude")),
    Gemini  = compute_kappa(h, paste0(base, "_gemini"))
  )
})

#----------------------------------------------------------------
# 5. Make a clean gt table
#----------------------------------------------------------------
kappa_table <- kappa_results %>%
  gt() %>%
  fmt_number(
    columns = c(ChatGPT, Claude, Gemini),
    decimals = 3
  ) %>%
  tab_header(
    title = "Cohen’s Kappa: LLM vs Human Across All Variables",
    subtitle = "Filtered to human new-data-analysis = TRUE and transplant-related = TRUE"
  )

kappa_table
gtsave(
  data = kappa_table,
  filename = "Results/kappa_table.docx"
)


#Create a table 3
#------------------------------------------------------------
# 1. Identify human-coded variables
#------------------------------------------------------------
human_vars <- names(df_filt)[grepl("_human$", names(df_filt))]

#------------------------------------------------------------
# 2. Function to compute numerators, denominators, percentages
#------------------------------------------------------------
get_breakdown <- function(hvar) {
  
  base <- sub("_human$", "", hvar)
  chat <- paste0(base, "_ChatGPT")
  cla  <- paste0(base, "_claude")
  gem  <- paste0(base, "_gemini")
  
  model_vars <- c(chat, cla, gem)
  model_vars <- model_vars[model_vars %in% names(df_filt)]
  if (length(model_vars) == 0) return(NULL)
  
  df_filt %>%
    select(human = all_of(hvar), all_of(model_vars)) %>%
    mutate(human = as.logical(human)) %>%
    filter(!is.na(human)) %>%
    group_by(human) %>%
    summarise(
      chat_num = if (chat %in% names(.)) sum(!!sym(chat), na.rm = TRUE) else NA_real_,
      cla_num  = if (cla  %in% names(.)) sum(!!sym(cla),  na.rm = TRUE) else NA_real_,
      gem_num  = if (gem  %in% names(.)) sum(!!sym(gem),  na.rm = TRUE) else NA_real_,
      
      den      = n(),
      
      chat_pct = chat_num / den,
      cla_pct  = cla_num / den,
      gem_pct  = gem_num / den,
      
      .groups = "drop"
    ) %>%
    mutate(
      variable = base,
      chat_nd  = paste0(chat_num, "/", den),
      cla_nd   = paste0(cla_num, "/", den),
      gem_nd   = paste0(gem_num, "/", den)
    ) %>%
    select(variable, human,
           chat_nd, chat_pct,
           cla_nd, cla_pct,
           gem_nd, gem_pct)
}

#------------------------------------------------------------
# 3. Build combined dataset
#------------------------------------------------------------
breakdown_table <- map_dfr(human_vars, get_breakdown)

#------------------------------------------------------------
# 4. Produce formatted gt table with spanners and combined N/D
#------------------------------------------------------------
breakdown_gt <- breakdown_table %>%
  gt(
    groupname_col = "variable",
    rowname_col = "human"
  ) %>%
  fmt_percent(
    columns = c(chat_pct, cla_pct, gem_pct),
    decimals = 1
  ) %>%
  cols_label(
    human = md("**Human**"),
    chat_nd = "Num/Den",
    chat_pct = "%",
    cla_nd = "Num/Den",
    cla_pct = "%",
    gem_nd = "Num/Den",
    gem_pct = "%"
  ) %>%
  tab_spanner(
    label = md("**ChatGPT**"),
    columns = c(chat_nd, chat_pct)
  ) %>%
  tab_spanner(
    label = md("**Claude**"),
    columns = c(cla_nd, cla_pct)
  ) %>%
  tab_spanner(
    label = md("**Gemini**"),
    columns = c(gem_nd, gem_pct)
  ) %>%
  tab_options(
    row_group.as_column = TRUE,
    row_group.font.weight = "bold",
    table.font.size = px(13),
    heading.align = "left"
  )

breakdown_gt
gtsave(
  data = breakdown_gt,
  filename = "Results/breakdown_gt.docx"
)