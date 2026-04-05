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





#---- Filter to human-positive dataset----

df_filt_full <- df_wide



#---- Compute full kappa table----

kappa_results_full <- purrr::map_dfr(human_vars, function(h) {
  base <- sub("_human$", "", h)
  
  tibble::tibble(
    variable = base,
    ChatGPT = compute_kappa(h, paste0(base, "_ChatGPT"), df_filt_full),
    Claude  = compute_kappa(h, paste0(base, "_claude"),  df_filt_full),
    Gemini  = compute_kappa(h, paste0(base, "_gemini"),  df_filt_full)
  )
})


#---- Make a clean gt table----

kappa_table_full <- kappa_results_full %>%
  gt() %>%
  fmt_number(
    columns = c(ChatGPT, Claude, Gemini),
    decimals = 3
  ) %>%
  
  gt::fmt_missing(
    columns = everything(),
    missing_text = "-"
  )  %>%

  
  tab_header(
    title = "Cohen’s Kappa: LLM vs Human Across All Variables",
  )

kappa_table_full


gtsave(
  data = kappa_table_full,
  filename = "Results/kappa_table_full.svg"
)


#Create a table with raw proportions


#---- Build combined dataset----
breakdown_table_full <- map_dfr(human_vars, get_breakdown, df_filt = df_filt_full)


#---- Produce formatted gt table with spanners and combined N/D

breakdown_gt_full <- breakdown_table_full %>%
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

breakdown_gt_full
gtsave(
  data = breakdown_gt_full,
  filename = "Results/breakdown_gt_full.svg"
)