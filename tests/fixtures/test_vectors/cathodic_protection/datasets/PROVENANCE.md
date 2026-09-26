# Provenance: DNV-RP-B401 / DNV-RP-F103 table fixtures

These CSVs are byte-for-byte copies of the dataset files held in the private
`vamseeachanta/llm-wiki` repository. They are the test oracles for
`src/digitalmodel/cathodic_protection/b401_tables.py` and `f103_tables.py`
(issue #2207). Do not edit them here; fix the wiki dataset and re-copy.

| Item | Value |
|---|---|
| Copy date | 2026-09-25 |
| Copied from | `llm-wiki/wikis/engineering-standards/wiki/datasets/` (sibling checkout `C:\ws\llm-wiki`) |
| Copy check | `filecmp.cmp(..., shallow=False)` identical for all 11 files |

## DNV-RP-B401 (`dnv-rp-b401/2005-with-2008-amendments/`)

- Source directory: `wikis/engineering-standards/wiki/datasets/dnv-rp-b401/2005-with-2008-amendments/`
- Wiki page: `wikis/engineering-standards/wiki/standards/dnv-rp-b401.md`,
  frontmatter `code_id: dnv-rp-b401`, `publisher: DNV`, `revision: "2011"`
  (October 2010 edition, printed 2011; the 2005-with-2008-amendments PDF is
  listed as the second `source_pdf`).
- Licence note (copied from the wiki frontmatter):
  `license_status: licensed-local-reference-derived-notes`. The page adds:
  "local licensed reference; raw PDF remains off-repo and this page contains
  derived notes plus selected short attributed excerpts only."
- Extraction status recorded on the wiki page: `provisional-unverified`
  (text-layer extraction, source pages 23-24). The values used by the
  modules were cross-checked against the hand-derived spot values in
  `docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md`.

| File | Table |
|---|---|
| `...-table-001.csv` | Table 10-1 initial and final design current densities by depth band and climate |
| `...-table-002.csv` | Table 10-2 mean design current densities by depth band and climate |
| `...-table-003.csv` | Table 10-3 mean design current densities for reinforcing steel |
| `...-table-004.csv` | Table 10-4 paint coating breakdown constants a and b |
| `...-table-005.csv` | Table 10-6 anode electrochemical capacity and closed circuit potential |
| `...-table-006.csv` | Table 10-5 anode compositional limits (not consumed by the modules) |
| `...-table-007.csv` | Table 10-7 anode resistance formulae (text; formulae stay in `dnv_rp_b401.py`) |
| `...-table-008.csv` | Table 10-8 anode utilisation factors |

## DNV-RP-F103 (`dnv-rp-f103/2010/`)

- Source directory: `wikis/engineering-standards/wiki/datasets/dnv-rp-f103/2010/`
- Wiki page: `wikis/engineering-standards/wiki/standards/dnv-rp-f103.md`,
  frontmatter `code_id: dnv-rp-f103`, `publisher: DNV`, `revision: "2010"`,
  `parse_status: verified-reference-summary`.
- Licence note (copied from the wiki frontmatter):
  `license_status: "licensed-derived-reference; raw PDFs remain off-repo"`.

| File | Table |
|---|---|
| `dnv-rp-f103-2010-table-001.csv` | Table 5-1 design mean current density by exposure and internal fluid temperature |
| `dnv-rp-f103-2010-table-002.csv` | Table A.1 linepipe coating constants a and b (printed x100) |
| `dnv-rp-f103-2010-table-003.csv` | Table A.2 field-joint coating constants a and b (printed x100) |

## Format notes

- UTF-8, CRLF line endings, no BOM. Headers contain multi-line quoted cells
  and typographic quotes; parse with Python `csv` (it handles the quoting).
- Table 10-6 prints capacities with thousands separators ("2,000").
- Table 10-4 prints `b` cells as `b = 0.10` and `a` in the column headers.
- Tables A.1 and A.2 end with a `Note:` row explaining the x100 scaling.
