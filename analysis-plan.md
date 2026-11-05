# Analysis plan: LLM extraction of study characteristics from USRDS-based papers

Status: draft, written retrospectively. The extraction, manual review and analyses were carried out before this plan was written. It records the choices the study made, so the decider can confirm or change them, and marks those not yet settled as **Open:**. Nothing here has been confirmed by the decider yet.

## Scope and reporting framework

This document is the authoritative specification of the study's scientific objectives, paper selection, reference standard, extraction, measures and statistical methods. `ARCHITECTURE.md` documents the implementation, `AGENTS.md` the working conventions, and GitHub Issues the open decisions and implementation status. Neither file supersedes this plan.

The study has two strands, which share the paper sample and the manual review:

1. **LLM validation:** how well three LLMs reproduce dual manual abstraction of study characteristics.
2. **Reporting practices:** what the manual review shows about how USRDS-based papers report their methods (the letter).

**Open:** the reporting guideline and the plan's section structure. For the validation strand, TRIPOD-LLM [@gallifant2025tripodllm] is the candidate; the plugin's templates do not cover this design, so the sections below follow the topics of the existing methods page until a structure is agreed. The letter strand may need no formal guideline. Decision issue not yet opened.

## 1. Objectives

- Measure agreement between each of three LLMs and the human reference standard, field by field, for study characteristics of USRDS-based papers: whether the paper is a new analysis, whether it is transplant-related, programming languages, USRDS files, data components, crosswalks, funding and code sharing.
- Describe the reporting practices of USRDS-based papers, overall and by transplant relevance and transplant study type.

**Open:** which fields are primary for the validation, and which are secondary or exploratory. Decision issue not yet opened.

## 2. Paper selection

As described on the methods page: a PubMed search in June 2022 for "United States Renal Data System" and "USRDS", with a convenience sample of the 175 most recently published papers. Full texts were obtained as PDFs.

**Open:** the exact search string, date run and any filters, to be recorded so the search can be repeated; and how the 175 sampled papers reconcile with the number of papers in the flowchart and the analysis tables. Decision issue not yet opened.

## 3. Reference standard

Each paper was reviewed independently by two reviewers using the abstraction form shown on the methods page.

**Open:** which human data are the reference standard. The LLM comparison reads the "Reviewer 2" sheet of the abstraction spreadsheet without filtering on its confirmation column; the letter reads the same sheet and keeps only confirmed rows. The plan should state whether the reference is one reviewer's sheet, a confirmed or adjudicated set, and how disagreements between the reviewers were resolved, and whether inter-rater agreement is reported. Decision issue not yet opened.

**Open:** whether the human coding rules match the prompt's rules. The prompt allows only explicitly stated programming languages, crosswalks and USRDS files, never inferred ones. The plan should state whether the reviewers followed the same rule, item by item. Decision issue not yet opened.

**Open:** how to treat the item on coverage by the `usRds` package, which is the maintainer's own package: disclose it, or exclude it from the accuracy metrics. Decision issue not yet opened.

## 4. LLM extraction

- **Models:** OpenAI `gpt-5.1`, Anthropic `claude-sonnet-4-5` and Google `gemini-2.5-flash`, called through the `ellmer` R package with structured output, one call per paper, with the same prompt and a 44-field schema.
- **Prompt rules:** details count only when stated in the paper; programming languages, crosswalks and USRDS files are never inferred; if USRDS or Medicare ESRD program data are not mentioned, the paper is coded as not a new analysis and its file and data-component fields as FALSE.
- **Text sent:** text extracted from each PDF with `pdftools`. GPT was sent all pages up to 240,000 characters, cut by 10% on each rate-limit retry down to 5,000 characters; Claude and Gemini were sent the first 50 pages up to 240,000 characters.

**Open:** whether the difference in text sent is accepted and disclosed, or the extraction repeated with the same input for every model; and whether the characters actually sent per paper can be recovered. Decision issue not yet opened.

**Open:** model parameters (temperature and others), whether model identifiers are pinned snapshots or aliases, run dates, and whether runs are repeated. Decision issue not yet opened.

**Open:** how failed, empty or unparsed model responses are counted (excluded, FALSE or missing). Decision issue not yet opened.

## 5. Statistical analysis

- **Agreement:** Cohen's κ for each field, each model against the human reference, on papers the humans coded as new USRDS analyses; repeated for the subset the humans coded as transplant-related.
- **Classification of study type:** cross-tabulations of each model against the humans for new analysis (all papers) and transplant relevance (new analyses).
- **Breakdown:** the share of papers each model codes TRUE, by the human answer.
- **Reporting practices:** descriptive tables of the manual review, overall, by transplant relevance and by transplant study type.

**Open:** how κ is reported for fields where one rater has no variation (not estimable rather than 0), and whether confidence intervals, prevalence-robust measures (PABAK, Gwet's AC1) or sensitivity, specificity, positive and negative predictive values with confidence intervals are added. Decision issue not yet opened.

**Open:** whether run time is reported, given that wall-clock time includes rate-limit waits, or tokens and cost are reported instead. Decision issue not yet opened.

## 6. Limitations to address in reporting

- The plan was written after the analysis.
- The models did not all receive the same text.
- The `usRds` coverage item concerns the maintainer's own package.

## 7. Reporting map

**Open:** to be completed once the reporting guideline is chosen. Decision issue not yet opened.

## References

::: {#refs}
:::
