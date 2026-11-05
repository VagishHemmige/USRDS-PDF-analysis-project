"""Render the LLM extraction methods diagram with Python standard libraries only.

Three cloud models are each called once per paper through ellmer and each returns
its own dataset; R merges each with the human abstraction by filename and computes
Cohen's kappa per field. This is a methods illustration: it shows no results.

The values below are typed in. They were taken from the repository at commit
1f8e386 (see SOURCES); update them by hand if the extraction scripts change.

Usage, from the repository root: python3 images/llm-extraction/draw.py
writes images/llm-extraction-methods.svg (or the path given as the first argument).
"""
from pathlib import Path
from html import escape
import base64
import sys

HERE = Path(__file__).resolve().parent
OUTPUT = Path(sys.argv[1]) if len(sys.argv) > 1 else HERE.parent / 'llm-extraction-methods.svg'

# Logos: the committed asset files from Dialysis-facility-density-USRDS,
# images/esrd-cause-classification/assets/, embedded unchanged (see that README for sources).
ASSET_DIR = HERE / 'assets'

# Where each value came from (commit 1f8e386).
SOURCES = {
    'models and text sent': ['R/extract_gpt.R', 'R/extract_claude.R',
                             'R/extract_gemini.R'],
    'schema fields': 'R/abstract_schema.R (type_summary)',
    'human sheet': 'R/data_cleaning.R',
    'kappa papers': 'R/llm_data_full.R (df_filt_full)',
}

# Label, short name, logo, model identifier and text sent. The difference in text sent
# (GPT: all pages, cut on retry) is disclosed in the methods text, not highlighted here.
MODELS = [
    ('OpenAI · GPT', 'GPT', 'openai', 'gpt-5.1',
     ['all pages, ≤240,000 characters', 'cut 10% per rate-limit retry,', 'down to 5,000 at least']),
    ('Anthropic · Claude', 'Claude', 'claude', 'claude-sonnet-4-5',
     ['first 50 pages, ≤240,000 characters']),
    ('Google · Gemini', 'Gemini', 'gemini', 'gemini-2.5-flash',
     ['first 50 pages, ≤240,000 characters']),
]

# Schema: total fields, and the groups the figure lists (name prefix, label, count).
N_FIELDS = 44
GROUPS = [
    ('languages_used_', 'languages used', 8),
    ('files_used_', 'USRDS files used', 7),
    ('component_', 'data components', 8),
    ('crosswalk_used_', 'crosswalks used', 3),
]
HUMAN_SHEET = 'Reviewer 2'

INK = '#18334e'
MUTED = '#5b6b7d'
SEND = '#316bbb'
RETURN = '#168577'
BEZEL = '#2b3e52'
PORT = '#d9e4ef'
PDF_RED = '#c8372d'
HUMAN = '#d9a273'

ROW_TOP, ROW_STEP, ROW_H = 318, 156, 140
W, H = 1600, 1120
FOOTER_Y = 1005

p = []


# ---- drawing helpers --------------------------------------------------------

def box(x, y, w, h, fill='#fff', stroke='#c5d3e2', r=14, sw=2):
    p.append(f'<rect x="{x}" y="{y}" width="{w}" height="{h}" rx="{r}" '
             f'fill="{fill}" stroke="{stroke}" stroke-width="{sw}"/>')


def t(x, y, s, size=16, b=False, c=INK, anchor='start', mono=False, italic=False):
    family = ' font-family="DejaVu Sans Mono, monospace"' if mono else ''
    style = ' font-style="italic"' if italic else ''
    p.append(f'<text x="{x}" y="{y}" font-size="{size}" font-weight="{700 if b else 400}" '
             f'fill="{c}" text-anchor="{anchor}"{family}{style}>{escape(s)}</text>')


def arrow(d, ret=False):
    c = RETURN if ret else SEND
    p.append(f'<path d="{d}" fill="none" stroke="{c}" stroke-width="3" '
             f'marker-end="url(#{"ret" if ret else "send"})"/>')


def flow(d, dashed=False):
    dash = ' stroke-dasharray="5 4"' if dashed else ''
    p.append(f'<path d="{d}" fill="none" stroke="{MUTED}" stroke-width="2.5"{dash} '
             f'marker-end="url(#flow)"/>')


def line(d):
    p.append(f'<path d="{d}" fill="none" stroke="{MUTED}" stroke-width="2.5"/>')


def logo(name, x, y, w=40, h=40):
    ext = 'png' if name == 'ellmer' else 'svg'
    raw = (ASSET_DIR / f'{name}.{ext}').read_bytes()
    mime = 'image/png' if ext == 'png' else 'image/svg+xml'
    p.append(f'<image x="{x}" y="{y}" width="{w}" height="{h}" '
             f'href="data:{mime};base64,{base64.b64encode(raw).decode()}"/>')


def row_top(i):
    return ROW_TOP + i * ROW_STEP


def row_mid(i):
    return row_top(i) + ROW_H // 2


# ---- parts of the figure ----------------------------------------------------

def pdf_page(x, y, w=50, h=64, fold=14, tag=True):
    """A generic document page with a folded corner and a PDF tag (not a brand mark)."""
    p.append(f'<path d="M {x} {y} H {x + w - fold} L {x + w} {y + fold} V {y + h} H {x} Z" '
             f'fill="#fff" stroke="{MUTED}" stroke-width="1.8" stroke-linejoin="round"/>')
    p.append(f'<path d="M {x + w - fold} {y} V {y + fold} H {x + w}" fill="#e8edf3" '
             f'stroke="{MUTED}" stroke-width="1.8" stroke-linejoin="round"/>')
    for k in range(3):
        p.append(f'<rect x="{x + 7}" y="{y + 18 + k * 7}" width="{w - 14 - (8 if k == 2 else 0)}" '
                 f'height="2.4" rx="1" fill="#c5d0dc"/>')
    if tag:
        box(x - 7, y + h - 22, 36, 16, PDF_RED, PDF_RED, 3, 0)
        t(x + 11, y + h - 9.5, 'PDF', 11, True, '#fff', 'middle')


def inputs():
    box(20, 170, 262, 170, '#fff8e4', '#e3d5af')
    # A small stack of papers, back to front.
    for k in range(3):
        pdf_page(194 + k * 11, 212 + k * 9, tag=(k == 2))
    t(38, 204, 'Paper PDFs', 21, True)
    t(38, 230, 'One PDF per', 15)
    t(38, 250, 'paper, from the', 15)
    t(38, 270, 'PubMed search', 15)
    t(38, 302, 'Kept locally; not in', 14, c=MUTED)
    t(38, 321, 'the repository', 14, c=MUTED)

    box(20, 360, 262, 330, '#fff8e4', '#e3d5af')
    t(38, 394, 'Shared prompt', 21, True)
    t(38, 418, 'and schema', 21, True)
    t(38, 446, 'The same for every model:', 15)
    t(38, 472, f'{N_FIELDS} fields, including', 14)
    for i, (_, label, n) in enumerate(GROUPS):
        t(48, 498 + i * 22, f'· {n} {label}', 14)
    t(48, 586, '· two gates: new analysis,', 14)
    t(60, 606, 'transplant study', 14)
    t(38, 638, 'Stated, not inferred; no USRDS', 13, c=MUTED)
    t(38, 656, 'mention → gated fields FALSE', 13, c=MUTED)
    t(38, 676, 'R/abstract_schema.R', 12, c=MUTED, mono=True)

    arrow('M 282 245 H 336')
    arrow('M 282 520 H 290 V 268 H 336')


def frames():
    box(298, 108, 979, 864, BEZEL, BEZEL, 22)
    box(310, 120, 955, 840, '#f1f6fc', '#f1f6fc', 8)
    p.append(f'<circle cx="787" cy="114" r="3" fill="{MUTED}"/>')
    for x, y, h in [(298, 228, 60), (1265, 340, 450)]:
        box(x, y, 12, h, PORT, PORT, 3, 0)
    t(1135, 337, 'over the internet', 12, c=MUTED, anchor='middle')
    t(335, 157, 'ANALYST’S COMPUTER', 22, True)
    box(325, 170, 560, 775, '#fff', '#9db9d5')
    logo('r', 340, 178, 44, 34)
    t(394, 205, 'R session', 22, True)
    box(1290, 265, 295, 535, '#eef8f3', '#a2cdb9')
    t(1306, 292, 'CLOUD · ONE CALL PER PAPER', 16, True)


def text_and_ellmer():
    box(340, 225, 525, 66, '#fff8e4', '#dcc88e', 8)
    t(356, 251, 'Text from each PDF (pdftools)', 16, True)
    t(356, 274, 'with the shared prompt, sent to each model in turn', 14)
    arrow('M 865 258 H 896')

    box(900, 225, 105, 720, '#fff8e4', '#dbc483', 14)
    logo('ellmer', 911, 470, 83, 92)
    t(952, 585, 'ellmer', 21, True, anchor='middle')
    for i, s in enumerate(['structured', 'output;', 'one call', 'per model', 'per paper']):
        t(952, 610 + i * 19, s, 13, c=MUTED, anchor='middle')


def calls():
    for i, (label, _, mark, model, sent) in enumerate(MODELS):
        top, x, w = row_top(i) + 10, 1306, 266
        box(x, top, w, 142)
        logo(mark, x + w - 50, top + 12, 36, 36)
        t(x + 14, top + 28, label, 16, True)
        t(x + 14, top + 52, model, 13, mono=True)
        t(x + 14, top + 76, 'Text sent:', 12, True, MUTED)
        for k, s in enumerate(sent):
            t(x + 14, top + 96 + k * 19, s, 12.5)
        arrow(f'M 1005 {row_mid(i) - 12} H 1302')
        arrow(f'M 1306 {row_mid(i) + 12} H 1009', ret=True)
        arrow(f'M 900 {row_mid(i) + 12} H 869', ret=True)


# Illustrative cells for the dataset cards: rows are papers, columns are schema
# variables, values Y/N. Not results; the same in every card.
CARD_COLUMNS = ['languages_used_R', 'files_used_Core']
CARD_ROWS = [('p001.pdf', 'Y', 'Y'), ('p002.pdf', 'N', 'Y'), ('p003.pdf', 'N', 'N')]
CARD_X, CARD_W = 588, 277


def table_card(top, title, border, note):
    """A dataset drawn as a table: one row per paper, one column per variable."""
    x = CARD_X
    cols = [x + 12, x + 68, x + 168]   # left edge of each column
    widths = [52, 96, 92]
    box(x, top, CARD_W, ROW_H, '#fff', border, 10, 2.4 if border != '#a0cebf' else 2)
    t(x + 14, top + 24, title, 15, True)
    for c, name in zip(cols, ['filename'] + CARD_COLUMNS):
        t(c, top + 46, name, 10, True, MUTED, mono=True)
    p.append(f'<path d="M {x + 8} {top + 52} H {x + CARD_W - 8}" stroke="#c5d3e2" stroke-width="1.2"/>')
    for k, (fname, *cells) in enumerate(CARD_ROWS):
        y = top + 68 + k * 18
        if k % 2 == 0:
            box(x + 8, y - 13, CARD_W - 16, 17, '#f3f7fb', '#f3f7fb', 3, 0)
        t(cols[0], y, fname, 10.5, mono=True)
        for c, w, v in zip(cols[1:], widths[1:], cells):
            t(c + w / 2, y, v, 11, True, RETURN if v == 'Y' else MUTED, 'middle', mono=True)
    t(x + 14, top + 128, note, 11.5, c=MUTED)


def dataset_cards():
    more = f'+ {N_FIELDS - len(CARD_COLUMNS)} more variables ({N_FIELDS} in all)'
    for i, (_, short, *_rest) in enumerate(MODELS):
        table_card(row_top(i), f'{short} dataset', '#a0cebf', more)
    table_card(row_top(len(MODELS)), 'Human dataset · reference', HUMAN,
               f'two reviewers; “{HUMAN_SHEET}” sheet read')


def comparison():
    # All four datasets join into the merge by filename.
    n = len(MODELS) + 1
    for i in range(n):
        line(f'M {CARD_X} {row_mid(i)} H 577')
    line(f'M 577 {row_mid(0)} V {row_mid(n - 1)}')
    flow('M 577 460 H 574')

    box(340, 400, 230, 120, '#fff', INK, 10)
    t(355, 428, 'Merge by filename', 17, True)
    for k, s in enumerate(['Human row plus each', 'model’s answers,', 'field by field']):
        t(355, 452 + k * 19, s, 13)
    flow('M 455 520 V 556')

    box(340, 562, 230, 150, '#eef8f4', '#a0cebf', 10)
    t(355, 590, 'Agreement per field', 17, True)
    for k, s in enumerate(['Cohen’s κ, each model', 'against the human', 'answers, on papers',
                           'humans coded as new', 'USRDS analyses']):
        t(355, 612 + k * 19, s, 13)
    flow('M 455 712 V 748', dashed=True)
    box(340, 754, 230, 60, '#f8fafc', '#c5d3e2', 10)
    t(355, 779, 'κ tables and heatmaps', 14, True)
    t(355, 800, 'for abstracts and manuscript', 13, c=MUTED)


def footer():
    box(20, FOOTER_Y, 1560, 100, '#f8fafc', '#d7e0e9')
    arrow(f'M 40 {FOOTER_Y + 29} H 86')
    t(100, FOOTER_Y + 35, 'Request: paper text + prompt', 15)
    arrow(f'M 40 {FOOTER_Y + 67} H 86', ret=True)
    t(100, FOOTER_Y + 73, 'Reply, as structured fields', 15)
    t(380, FOOTER_Y + 35, 'A methods illustration: it shows no results; the Y/N cells in the datasets are illustrative. '
      'Model identifiers, text sent and fields are as set in the R scripts.', 13, c=MUTED)
    t(380, FOOTER_Y + 56, 'Extraction scripts: extract_gpt.R, extract_claude.R, '
      'extract_gemini.R.', 13, c=MUTED)


def main():
    box(0, 0, W, H, '#fff', '#fff', 0)
    t(40, 55, 'Extracting study characteristics with R and ellmer', 34, True)
    t(40, 90, 'Three cloud models each read every paper and return their own dataset; '
      'R compares each with human abstraction.', 19)
    frames()
    inputs()
    text_and_ellmer()
    calls()
    dataset_cards()
    comparison()
    footer()

    svg = (f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" '
           f'viewBox="0 0 {W} {H}"><defs>')
    for n, c in [('send', SEND), ('ret', RETURN), ('flow', MUTED)]:
        svg += (f'<marker id="{n}" markerWidth="8" markerHeight="8" refX="7" refY="4" '
                f'orient="auto"><path d="M0 0 L8 4 L0 8" fill="{c}"/></marker>')
    svg += '</defs><g font-family="DejaVu Sans, sans-serif">' + ''.join(p) + '</g></svg>'
    OUTPUT.write_text(svg, encoding='utf-8')


if __name__ == '__main__':
    main()
