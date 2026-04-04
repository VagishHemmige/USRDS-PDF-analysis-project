#Data analysis script for NKF.  This will run the full 170+ PDFs


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
# 1. Filter to human-positive dataset (no filtering for the NKF abstract)
#----------------------------------------------------------------
df_filt <- df_wide

#----------------------------------------------------------------
# 2. Identify all human-coded boolean variables
#----------------------------------------------------------------
human_vars <- names(df_filt)[grepl("_human$", names(df_filt))]


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
  filename = "Results/kappa_table_NKF.docx"
)


#Create a table 3
#------------------------------------------------------------
# 1. Identify human-coded variables
#------------------------------------------------------------
human_vars <- names(df_filt)[grepl("_human$", names(df_filt))]


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
  filename = "Results/breakdown_gt_NKF.docx"
)