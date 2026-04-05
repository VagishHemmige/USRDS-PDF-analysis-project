#Data analysis script for ATC




#---- Filter to human-positive dataset and transplant only----

df_filt_atc <- df_wide %>%
  filter(
    is_this_paper_a_new_data_analysis_human == TRUE,
    is_this_a_transplant_related_study_human == TRUE
  )



#---- Compute full kappa table----

kappa_results_atc <- purrr::map_dfr(human_vars, function(h) {
  base <- sub("_human$", "", h)
  
  tibble::tibble(
    variable = base,
    ChatGPT = compute_kappa(h, paste0(base, "_ChatGPT"), df_filt_atc),
    Claude  = compute_kappa(h, paste0(base, "_claude"),  df_filt_atc),
    Gemini  = compute_kappa(h, paste0(base, "_gemini"),  df_filt_atc)
  )
})


#---- Make a clean gt table----

kappa_table_atc <- kappa_results_atc %>%
  gt() %>%
  fmt_number(
    columns = c(ChatGPT, Claude, Gemini),
    decimals = 3
  ) %>%
  tab_header(
    title = "Cohen’s Kappa: LLM vs Human Across All Variables",
    subtitle = "Filtered to human new-data-analysis = TRUE and transplant-related = TRUE"
  )

kappa_table_atc
gtsave(
  data = kappa_table_atc,
  filename = "Results/kappa_table_atc.svg"
)


#Create a table with raw proportions


#---- Build combined dataset----
breakdown_table_atc <- map_dfr(human_vars, get_breakdown, df_filt = df_filt_atc)


#---- Produce formatted gt table with spanners and combined N/D

breakdown_gt_atc <- breakdown_table_atc %>%
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

breakdown_gt_atc
gtsave(
  data = breakdown_gt_atc,
  filename = "Results/breakdown_gt_atc.svg"
)