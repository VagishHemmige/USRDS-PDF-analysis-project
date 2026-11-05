# LLM extraction methods diagram

`draw.py` generates `../llm-extraction-methods.svg`; the PNG beside it is what
`quarto/analysis_methods.qmd` embeds. Adapted from the ESRD cause-classification
diagram in [Dialysis-facility-density-USRDS](https://github.com/VagishHemmige/Dialysis-facility-density-USRDS/tree/main/images/esrd-cause-classification).

## Regenerate

From the repository root:

```sh
python3 images/llm-extraction/draw.py
rsvg-convert images/llm-extraction-methods.svg -o images/llm-extraction-methods.png
```

SVG generation uses Python 3 standard libraries only, with no network access.
PNG export used `rsvg-convert` and the DejaVu Sans font; Inkscape also works.
The website consumes the committed PNG; its build does not run Python.
Commit the script, assets and regenerated outputs together after visual inspection.

## Provenance and scope

This is a methods illustration, not an analysis output. It reads no papers, no
reviewer data and no model outputs, and it shows no results. The Y/N cells in the
dataset tables are illustrative and identical across cards.

The values shown are typed into `draw.py` (see `SOURCES` there), taken from commit
`1f8e386`; the scripts have since been renamed, and are listed here by their current names:

- model identifiers and text sent: `R/extract_gpt.R`,
  `R/extract_claude.R`, `R/extract_gemini.R`, the scripts
  that wrote the outputs read by `R/data_cleaning.R`;
- schema fields (44, with group counts): `type_summary` in `R/abstract_schema.R`;
- human sheet read: `R/data_cleaning.R`;
- papers used for kappa: `df_filt_full` in `R/llm_data_full.R`.

Update them by hand if any of those change. The models were not sent the same
text (GPT: all pages, cut 10% per rate-limit retry; Claude and Gemini: first 50
pages); the figure lists each model's input without highlighting the difference,
which is described in the methods text.

## Logo assets

Copied unchanged from
`Dialysis-facility-density-USRDS/images/esrd-cause-classification/assets/` and
embedded as data URIs in the generated SVG. No logo is downloaded during
regeneration. Brand marks belong to their respective owners; use here identifies
software and services and does not imply endorsement.

| Asset | Source |
| --- | --- |
| `r.svg` | [Simple Icons R icon](https://github.com/simple-icons/simple-icons/blob/develop/icons/r.svg) |
| `ellmer.png` | [ellmer package sticker](https://github.com/tidyverse/ellmer/blob/main/man/figures/logo.png) |
| `openai.svg` | [Lobe Icons: openai.svg](https://github.com/lobehub/lobe-icons/blob/master/packages/static-svg/icons/openai.svg) |
| `claude.svg` | [Lobe Icons: claude-color.svg](https://github.com/lobehub/lobe-icons/blob/master/packages/static-svg/icons/claude-color.svg) |
| `gemini.svg` | [Lobe Icons: gemini-color.svg](https://github.com/lobehub/lobe-icons/blob/master/packages/static-svg/icons/gemini-color.svg) |

As in the source repository, SVG dimensions were normalized to 64 and
`currentColor` replaced with `#18334e` where present.
