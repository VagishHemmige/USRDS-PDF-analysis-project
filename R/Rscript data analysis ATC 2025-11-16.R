#Data analysis script for ATC




#---- Filter to human-positive dataset and transplant only----

df_filt_atc <- df_wide %>%
  filter(
    is_this_paper_a_new_data_analysis_human == TRUE,
    is_this_a_transplant_related_study_human == TRUE
  )


#---- Table of times to run analysis----



timing_gt<-timing_table %>%
  gt() %>%
  fmt_number(
    columns = `Time (minutes)`,
    decimals = 0
  )%>%
  tab_header(
    title = "Runtime of LLMs Used in the Analysis",
    subtitle = "Wall-clock time in minutes to process the full dataset"
  )

gt::gtsave(
  data = timing_gt,
  filename = "ATC 2026 abstract/timing_table_atc.png"
)
saveRDS(timing_gt, "ATC 2026 abstract/timing_table_atc.rds")

#---- Compute full kappa table----

kappa_results_atc <- purrr::map_dfr(human_vars, function(h) {
  base <- sub("_human$", "", h)
  
  tibble::tibble(
    variable = base,
    ChatGPT = compute_kappa(h, paste0(base, "_ChatGPT"), df_filt_atc),
    Claude  = compute_kappa(h, paste0(base, "_claude"),  df_filt_atc),
    Gemini  = compute_kappa(h, paste0(base, "_gemini"),  df_filt_atc)
  )%>%
    mutate(
    across(c(ChatGPT, Claude, Gemini), ~ ifelse(is.nan(.x), NA_real_, .x))
  )
})%>%
  filter(!variable %in% c(
    "which_pieces_of_data_were_abstracted_from_the_files",
    "which_tasks_would_not_be_covered",
    "which_type_of_transplant_study_is_this",
    "who_funded_the_study",
    "year",
    "journal",
    "title",
    "authors",
    "languages_used_SQL",
    "languages_used_Matlab",
    "languages_used_Julia"
  ))


#---- Make a clean gt table----

kappa_table_atc <- kappa_results_atc %>%
  add_label_for_gt() %>%
  gt(rowname_col = "label") %>%
  gt::cols_hide(columns = variable) %>%
  fmt_number(
    columns = c(ChatGPT, Claude, Gemini),
    decimals = 3
  ) %>%
  cols_align_decimal(
    columns = c(ChatGPT, Claude, Gemini)
  ) %>%
  tab_header(
    title = "Cohen’s κ: LLM vs Human Across All Variables",
    subtitle = "Dataset of 44 transplant-related papers"
  ) %>%
  gt::tab_stubhead(label = "Variable") %>%
  sub_missing(
    columns = c(ChatGPT, Claude, Gemini),
    missing_text = "-"
  )%>%
  gt::tab_row_group(
    label = "Programming languages",
    rows = grepl("^languages_", variable)
  ) %>%
  gt::tab_row_group(
    label = "Files used",
    rows = grepl("^files_", variable)
  )%>%
  gt::tab_row_group(
    label = "Components of USRDS used",
    rows = grepl("^component_", variable)
  ) %>%
  gt::tab_stub_indent(
    rows = grepl("^(languages_|files_|component_)", variable),
    indent = 4
  )%>%
  tab_footnote(
    footnote = "κ not estimable, usually due to insufficient variation or no usable paired observations.",
    locations = cells_body(columns = ChatGPT, rows = is.na(ChatGPT)),
    placement="right"
  ) %>%
  tab_footnote(
    footnote = "κ not estimable, usually due to insufficient variation or no usable paired observations.",
    locations = cells_body(columns = Claude, rows = is.na(Claude)),
    placement="right"
  ) %>%
  tab_footnote(
    footnote = "κ not estimable, usually due to insufficient variation or no usable paired observations.",
    locations = cells_body(columns = Gemini, rows = is.na(Gemini)),
    placement="right"
  )%>%
  data_color(
    columns = c(ChatGPT, Claude, Gemini),
    fn = kappa_fill
  )

kappa_table_atc
gtsave(
  data = kappa_table_atc,
  filename = "ATC 2026 abstract/kappa_table_atc.png"
)
saveRDS(kappa_table_atc, "ATC 2026 abstract/kappa_table_atc.rds")

#Create legend to supplement kappa table
kappa_legend_gt<-create_kappa_legend_gt()
gtsave(
  data = kappa_legend_gt,
  filename = "ATC 2026 abstract/kappa_table_legend_atc.png"
)
saveRDS(kappa_legend_gt, "ATC 2026 abstract/kappa_legend_gt.rds")




#Create a table with raw proportions


#---- Build combined dataset----
breakdown_table_atc <- map_dfr(human_vars, get_breakdown, df_filt = df_filt_atc)


#---- Produce formatted gt table with spanners and combined N/D

breakdown_gt_atc <- breakdown_table_atc %>%
  add_label_for_gt() %>%
  dplyr::select(-variable) %>%
  gt(
    groupname_col = "label",
    rowname_col = "human"
  ) %>%
  fmt_percent(
    columns = c(chat_pct, cla_pct, gem_pct),
    decimals = 1
  ) %>%
  cols_label(
    chat_nd = "Num/Den",
    chat_pct = "%",
    cla_nd = "Num/Den",
    cla_pct = "%",
    gem_nd = "Num/Den",
    gem_pct = "%"
  ) %>%
  tab_stubhead(label = md("**Human**")) %>%
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
  filename = "ATC 2026 abstract/breakdown_gt_atc.png"
)
saveRDS(breakdown_gt_atc, "ATC 2026 abstract/breakdown_gt_atc.rds")

#---- Heatmaps ----

file_vars_atc <- names(label_list)[str_detect(names(label_list), "^files_")]

files_res_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = file_vars_atc,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Files used heatmap",
  reorder = FALSE
)

files_res_atc
ggsave(
  plot = files_res_atc,
  filename = "ATC 2026 abstract/files_res_atc.png"
)




languages_vars_atc <- names(label_list)[str_detect(names(label_list), "^languages_")]

languages_res_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = languages_vars_atc,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Languages used heatmap",
  reorder = FALSE
)

languages_res_atc
ggsave(
  plot = languages_res_atc,
  filename = "ATC 2026 abstract/languages_res_atc.png"
)




component_vars_atc <- names(label_list)[str_detect(names(label_list), "^component_")]

component_res_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = component_vars_atc,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Components used heatmap",
  reorder = FALSE
)

component_res_atc
ggsave(
  plot = component_res_atc,
  filename = "ATC 2026 abstract/component_res_atc.png"
)



other_vars_atc <- setdiff(
  setdiff(
    names(label_list),
    c(file_vars_atc, languages_vars_atc, component_vars_atc)
  ),
  "did_the_authors_explicitly_state_what_files_they_used"
)

other_res_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = other_vars_atc,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Other study features heatmap",
  reorder = FALSE
)

other_res_atc

ggsave(
  filename = "ATC 2026 abstract/other_res_atc.png",
  plot = other_res_atc,
)

# ============================================================
# Poster heatmap for ATC abstract
# Combined heatmap + separate horizontal legend
# ============================================================


#--------------------------------------------------------------
# 1. Wrap long variable labels for poster readability
#--------------------------------------------------------------

label_list_poster <- purrr::map(
  label_list,
  ~ stringr::str_wrap(.x, width = 20)
)


#--------------------------------------------------------------
# 2. Poster-specific themes
#--------------------------------------------------------------

# Larger angled variable labels at the bottom of every panel
poster_x_axis_theme <- theme(
  axis.text.x = element_text(
    size = 20,
    angle = 55,
    hjust = 1,
    vjust = 1,
    lineheight = 0.90
  ),
  plot.title = element_text(
    size = 24,
    face = "bold",
    hjust = 0.5
  ),
  plot.margin = margin(
    t = 7,
    r = 5.5,
    b = 12,
    l = 5.5
  )
)

# First panel:
# retain paper labels and y-axis title, but suppress legend
first_panel_theme <- theme(
  legend.position = "none",
  axis.text.y = element_text(
    size = 11
  ),
  axis.title.y = element_text(
    size = 24,
    face = "bold"
  )
)

# Subsequent panels:
# remove repeated y-axis information and suppress legend
later_panel_theme <- theme(
  legend.position = "none",
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank()
)


#--------------------------------------------------------------
# 3. Build the four heatmap sections
#--------------------------------------------------------------

components_poster_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = component_vars_atc,
  paper_id_col = "filename",
  label_list = label_list_poster,
  title = "Analytic components",
  subtitle = NULL,
  reorder = FALSE,
  show_paper_ids = TRUE,
  left_margin = 10
) +
  first_panel_theme +
  poster_x_axis_theme


files_poster_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = file_vars_atc,
  paper_id_col = "filename",
  label_list = label_list_poster,
  title = "USRDS files used",
  subtitle = NULL,
  reorder = FALSE,
  show_paper_ids = FALSE,
  left_margin = 0.5
) +
  later_panel_theme +
  poster_x_axis_theme


languages_poster_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = languages_vars_atc,
  paper_id_col = "filename",
  label_list = label_list_poster,
  title = "Programming languages",
  subtitle = NULL,
  reorder = FALSE,
  show_paper_ids = FALSE,
  left_margin = 0.5
) +
  later_panel_theme +
  poster_x_axis_theme


other_poster_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = other_vars_atc,
  paper_id_col = "filename",
  label_list = label_list_poster,
  title = "Other study features",
  subtitle = NULL,
  reorder = FALSE,
  show_paper_ids = FALSE,
  left_margin = 0.5
) +
  later_panel_theme +
  poster_x_axis_theme


#--------------------------------------------------------------
# 4. Allocate panel widths based on number of variables
#--------------------------------------------------------------

section_widths <- c(
  length(component_vars_atc),
  length(file_vars_atc),
  length(languages_vars_atc),
  length(other_vars_atc)
)


#--------------------------------------------------------------
# 5. Assemble the combined poster heatmap
#    No embedded legend
#--------------------------------------------------------------

combined_heatmap_atc <- wrap_plots(
  components_poster_atc,
  files_poster_atc,
  languages_poster_atc,
  other_poster_atc,
  nrow = 1,
  widths = section_widths
) +
  plot_annotation(
    title = "Agreement between LLM extraction and human review",
    subtitle = paste(
      "Each tile indicates how many of the three LLMs matched",
      "the human-reviewed value across 44 transplant-related papers"
    ),
    theme = theme(
      plot.title = element_text(
        size = 30,
        face = "bold"
      ),
      plot.subtitle = element_text(
        size = 24
      )
    )
  )

combined_heatmap_atc


#--------------------------------------------------------------
# 6. Create a separate horizontal legend
#
# Drawn directly as a standalone figure so that it can be
# positioned and resized independently in PowerPoint.
#--------------------------------------------------------------

legend_df_atc <- tibble::tibble(
  n_models_correct = factor(
    c("0", "1", "2", "3"),
    levels = c("0", "1", "2", "3")
  ),
  y = 1
)

heatmap_legend_atc <- ggplot(
  legend_df_atc,
  aes(
    x = n_models_correct,
    y = y,
    fill = n_models_correct
  )
) +
  geom_tile(
    width = 0.85,
    height = 0.75,
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "0" = "#b2182b",
      "1" = "#ef8a62",
      "2" = "#fddbc7",
      "3" = "#d1e5f0"
    ),
    drop = FALSE
  ) +
  scale_x_discrete(
    labels = c(
      "0" = "0",
      "1" = "1",
      "2" = "2",
      "3" = "3"
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "Models matching human review",
    x = NULL,
    y = NULL
  ) +
  theme_void(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 8)
    ),
    axis.text.x = element_text(
      size = 17,
      margin = margin(t = 5)
    ),
    plot.margin = margin(
      t = 6,
      r = 6,
      b = 12,
      l = 6
    )
  )

heatmap_legend_atc


#--------------------------------------------------------------
# 7. Save combined heatmap
#--------------------------------------------------------------

ggsave(
  filename = "ATC 2026 abstract/combined_heatmap_atc.svg",
  plot = combined_heatmap_atc,
  width = 34,
  height = 15,
  units = "in"
)

ggsave(
  filename = "ATC 2026 abstract/combined_heatmap_atc.png",
  plot = combined_heatmap_atc,
  width = 34,
  height = 15,
  units = "in",
  dpi = 300
)


#--------------------------------------------------------------
# 8. Save separate horizontal legend
#--------------------------------------------------------------

ggsave(
  filename = "ATC 2026 abstract/combined_heatmap_legend_atc.svg",
  plot = heatmap_legend_atc,
  width = 7,
  height = 1.8,
  units = "in"
)

ggsave(
  filename = "ATC 2026 abstract/combined_heatmap_legend_atc.png",
  plot = heatmap_legend_atc,
  width = 7,
  height = 1.8,
  units = "in",
  dpi = 300
)