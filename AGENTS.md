# AGENTS.md

Guidance for everyone working on this repository: the maintainer, collaborators, and any AI agents working for them. It covers how to work here: conventions, data handling, and checks.

Four places divide the documentation:

| Where | Covers |
|---|---|
| `analysis-plan.md` | Study design and statistics: what the analysis is meant to do |
| `ARCHITECTURE.md` | Code structure: scripts, data flow, and the objects passed between them |
| `AGENTS.md` (this file) | How to work in the repository: rules, conventions, and checks |
| [GitHub Issues](https://github.com/VagishHemmige/USRDS-PDF-analysis-project/issues) | Open scientific decisions (decision issues), gaps between code and plan, and known defects |

Read the three files before making changes. Each covers its own topic and does not repeat the others.

## The project

The project evaluates whether large language models (LLMs) can extract study characteristics (study type, transplant relevance, programming language, USRDS files and data components used, and related items) from published papers that use United States Renal Data System (USRDS) data, compared with dual manual review. It also reports the reporting practices found by manual review. The data are published papers, not patient records. The code and methods are published as a Quarto website.

Outputs feed several products: the reporting-practices letter, the LLM validation manuscript, and conference abstracts and posters (the kidney conference abstract and the ATC 2026 abstract). Their tables and figures are in `tables/<strand>/` and `figures/<strand>/`, with `manual` (manual review only), `llm_full` (all papers, humans versus LLMs) and `llm_transplant` (transplant papers, humans versus LLMs) as the folders.

- **`analysis-plan.md` is the source of truth for design.** Code implements the plan; it does not redefine it. If code needs to depart from the plan, open a decision issue, or comment on the existing one, for the decider. Once the decider has settled it (in the issue, or relayed there in their exact words), the plan change and the code can go in the same pull request, which closes the issue.
- **Decider.** Scientific decisions are made by Vagish Hemmige. Anyone may open or contribute to a decision issue; only the decider settles it (see "Working with agents").
- **This is an analysis project, not an R package.** Do not add DESCRIPTION, NAMESPACE, roxygen, or pkgdown infrastructure, or apply CRAN conventions.
- **Default branch:** `master`. Changes from agents reach it only through pull requests.

## Data sources

| Source | Accessed through | Package documentation | Package maintained by | Access | What AI agents may do |
|---|---|---|---|---|---|
| Full-text PDFs of the sampled papers (PubMed search) | `pdftools::pdf_text()` | [pdftools](https://docs.ropensci.org/pdftools/) | External | Copyrighted; kept in `data-raw/`, not in the repository | Refer to papers by PMID or citation. Do not upload PDFs or their extracted text to any outside service, or reproduce substantial passages |
| Manual abstraction spreadsheet (two reviewers) | `readxl::read_excel()` | [readxl](https://readxl.tidyverse.org/) | External | Internal; kept in `data-raw/`, not in the repository | Report aggregate findings, not reviewer-by-reviewer content or reviewer identities, unless the decider asks |
| OpenAI, Anthropic and Google model APIs | `ellmer::chat_openai()`, `chat_anthropic()`, `chat_google_gemini()` | [ellmer](https://ellmer.tidyverse.org/) | External | Paid API; keys in the user's `.Renviron` | No calls without the decider's explicit approval for that specific run, including a single test paper |
| Saved model outputs | `readRDS()`, `readr::read_csv()` | | | Kept in `Results/`, not in the repository | Read when provided; they are research records, not disposable caches |

**For every package:** read its documentation before writing code for a source, including known issues. Behavior that sets which papers are included, how an answer is coded, or a denominator is checked in the documentation, not assumed from a function's name.

**For external packages:** work around problems here. Wrap the function in `R/functions.R` with a comment explaining the problem and linking any upstream issue. Reporting a bug upstream is an external communication; the person directing the work decides whether to file it.

**USRDS definitions** (files, data components, analysis types) are checked against the USRDS Researcher's Guide for the relevant release and cited, never inferred from a variable's name.

## Exploration and definitive analysis

Exploration is welcome on branches or forks. Experimental scripts need not be sourced from `R/main.R` or have Quarto pages, and their findings are labelled preliminary. A script becomes part of the definitive analysis when it is sourced from `R/main.R`; from then on every rule below applies to it.

## R scripts

- **Running the analysis.** Run `R/main.R` from the project root in a fresh session. It sources the other scripts in dependency order; `ARCHITECTURE.md` describes what each does. It never calls a model: it reads the saved runs listed in `model_runs` and stops with a message if one is missing. The extraction scripts call paid APIs and run only when `run_extraction` is set to `TRUE` in `R/setup.R`.
- **One job per script.** When you add a definitive script, source it from `main.R` at the right point and update `ARCHITECTURE.md` in the same pull request.
- **Header comment.** Each script begins with what it does, which objects it needs from earlier scripts, and what it produces.
- **Constants and libraries** live in `R/setup.R`, including `run_extraction` and the `model_runs` record. Other scripts do not call `library()` or hard-code values a reader might want to change. (The extraction scripts still load their own packages; see `ARCHITECTURE.md`.)
- **Helper functions** go in `R/functions.R`. A function takes its inputs as arguments and does not read global objects.
- **No side effects outside the project.** No `setwd()`, absolute paths, or writes outside the project.
- **Saved model outputs are records.** Do not overwrite a saved run. A new extraction is a new run, saved with the prompt and schema, model identifier, text limits and run date that produced it, and added to `model_runs`. Runs saved before `model_runs` existed are marked `legacy`.

## Chunk labels

Scripts are divided into labelled sections so Quarto pages can show the code without copying it:

```r
## ---- <step-name>
```

- Every logical step has a label. A label covers the code from its line to the next label.
- Labels are kebab-case, describe the step, and are **unique across all scripts**; a duplicate silently picks up the wrong code.
- Search the `.qmd` files before renaming or deleting a label.
- Labels are comments and never change what the code does.

## Quarto website

The site documents the code. It does not run the analysis.

- `_quarto.yml`, `index.qmd`, `404.qmd` (the page GitHub Pages shows for a missing address) and `styles.css` sit at the root; `quarto/` has one page per definitive script, plus the Background, methods, About and project-document pages. The `custom-callout` extension in `_extensions/` provides the `Rcode`, `Results` and `Pending` callouts, and the `issue-comments` extension lets readers file comments as GitHub issues.
- **Visual cues.** Each kind of page has one sidebar icon, coloured by `styles.css`, and pages mark the same kind of content with the callout of that colour: background `book` (blue); study design and methods `diagram-3` (orange); R code `laptop` (pink, `Rcode` 💻); results `clipboard-data` (green, `Results` 📊); project documents `file-earmark-ruled` (grey). Give every new page the icon of its kind.
- **Provisional content.** `quarto/_pending.lua` highlights in yellow every paragraph that starts with **Decision pending:** or, in the plan, **Open:**, and any `[text]{.pending}`, and adds a ⏳ `Pending` callout to pages that have a mark. Mark only what an open decision could change, and remove the mark when the decision is settled.
- **Project documents.** `analysis-plan.md`, `ARCHITECTURE.md`, `AGENTS.md` and `REPOSITORY-BLUEPRINT.md` are shown on the site by pages in `quarto/` that only include the root file, which stays the one source: edit the root file, not the page.
- Each code page loads its script with `knitr::read_chunk("../R/<script>.R")` in a hidden chunk and shows each labelled section with `#| eval: false`, with prose explaining what the section does and why. Pages link to the script on GitHub.
- **Never evaluate analysis code in the site.** Only the `read_chunk()` imports, the Architecture page's chunk that reads `ARCHITECTURE.md`, site helpers that read code or text, and results pages that read saved outputs run.
- The site renders to `docs/`, which is not committed; `.github/workflows/publish.yml` renders it on every pull request as a check and publishes it to the `gh-pages` branch on every push to `master`. Wait for one publish run to finish before merging the next pull request. When you add a script, add its page and sidebar entry in the same pull request.
- **Prose must match the code.** If a change alters what a section does, update its explanation in the same pull request.
- **Figures drawn in Python** keep their generator next to their assets (`images/<name>/draw.py`) and commit the generated image; the site does not run Python.

## Outputs

- Tables are saved by the analysis scripts to `tables/<strand>/` and figures to `figures/<strand>/`, and both are tracked: they are what the website reads. Hand-edited documents (such as `ATC 2026 abstract/Tables.docx`) stay out of these folders.
- Results pages read the saved `gt` tables (`.rds`). They must be re-saved when a `gt` update can no longer print them; compare the `.html` tables before and after to confirm only formatting changed.
- Published figures need alt text.
- `README.Rmd` is the source of `README.md`; edit the `.Rmd` and re-knit.

## Credentials and paid APIs

- Never commit keys, tokens or `.Renviron`. Agents never read, print or copy them; if a key appears in code or history, say so without repeating it.
- Ask before any model API call. Before a paid batch, make one call with the exact package version, model and settings, and check the result before running the rest. Which model or service version to use is the decider's choice; an agent proposes, with the cost if known, and does not switch it on its own.

## Coding conventions

- **Paper flow** uses the `flowchart` package, so the flow diagram and the analysed set of papers come from the same code. Every inclusion or exclusion goes through `fc_filter()`, in the plan's order.
- **Joins** use `join_by()` with an explicit `relationship` where possible. After every join of model outputs to the human data, check that rows did not multiply, and decide deliberately what happens to unmatched papers.
- **Code that turns model output into reported numbers** lives in named functions in `R/functions.R`, tested on synthetic data with a known answer.
- **Package APIs:** read the help page and NEWS for the installed version before calling a package; don't write calls from memory.
- **Style:** readable for a clinician-analyst who learned R from online guides: pipes and dplyr/tidyr verbs, `case_when()`, loops over names or named lists rather than the apply family. Tidyverse layout; style changes in their own commits.

## Checks for any change

Check the items that apply before opening a pull request.

- **Plan:** does the change implement the plan as written, and does it leave every open decision open?
- **Paper selection and reference standard:** do the PubMed search, dates, and inclusion and exclusion rules match the plan, and do counts reconcile across the methods text, flowchart and tables? Which reviewer sheet or adjudicated set does each analysis read? Do the human coding rules match the prompt rules (for example, stated versus inferred data files)? Are subjective or author-conflicted items (such as coverage by the maintainer's `usRds` package) flagged for disclosure or excluded from accuracy metrics?
- **LLM extraction:** does the prompt and schema wording match what is actually sent (full text versus abstract), including gating rules that force fields to FALSE? Does every model receive the same text, and is truncation logged per paper? Are model identifiers, parameters, run dates and repeat runs recorded? How are failed, empty or unparsed responses counted?
- **Merging and agreement:** are joins free of duplicates and row multiplication, with counts before and after each join? Are denominators and subsets filtered on the reference standard, not on the outcome evaluated? Is κ reported as not estimable, not 0, when one rater has no variation, and are confidence intervals or prevalence-robust measures reported where positives are few? Is wall-clock time confounded by rate-limit waits?
- **Reported numbers:** do tables, figures and site prose say what the code actually computes, and does the letter's text match its tables?
- **Reporting and definitions:** does the validation write-up follow its reporting guideline? Are USRDS files, analysis types and clinical categories checked against the Researcher's Guide, NIDDK or PubMed, never invented?
- **Structure:** is `ARCHITECTURE.md` current; are chunk labels unique and found by their pages; does every definitive script have a page and sidebar entry; does any site chunk run analysis code?
- **Execution:** does `main.R` run in a fresh session with relative paths, without calling a model; are any saved outputs or tables stale relative to the code?

## Working with agents

- **Autonomy.** The person directing an agent sets its working style (review only, step by step, or hands-off) in their own instructions, and may add stricter rules; those take precedence. Without a style, an agent works step by step. At most, an agent may edit code and documentation, add scripts and tests, run code and render the site on synthetic data, and commit to a branch and open a pull request.
- **Decisions.** Anyone, including an agent, may add options and evidence to a decision issue. The decider states the choice in the issue. Agents implement decisions; they never choose among options on their own, in the issue, the plan, or the code.
- **Every agent, under any autonomy:** never merges a pull request; follows the data-source rules above; asks before running the full pipeline or making any model API call; shows a person the exact text before posting or sending anything as them, unless that person's own instructions waive this.
- **Pull requests opened by an agent** are focused. Each says which plan section or issue it addresses, what was checked, and whether by running code on synthetic data or by reading only. Synthetic results are never described as results.
- **References:** cite the source documentation and literature used, in `references.bib`.
