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

---

# Provenance: 2017 / 2021 B401 and 2019 F103 table fixtures (#2208)

Unlike the 2005/2010 fixtures above, these CSVs are **hand transcriptions**
from the text layer of the licensed PDFs (`pdftotext`, hand-verified against
the page images 2026-09-26). They hold table numbers, captions and numeric
cells only; no clause prose. The canonical llm-wiki pages
(`dnv-rp-b401-2017.md`, `dnv-rp-b401-2021.md`, `dnv-rp-f103-2019.md`) are
created in parallel with the same frontmatter; when the wiki gains dataset
CSVs for these editions, replace these files with byte-for-byte copies and
record the copy check here.

| Item | Value |
|---|---|
| Transcription date | 2026-09-26 |
| Method | `pdftotext` text layer, hand-verified 2026-09-26 |
| Source PDFs (licensed, off-repo, on `ace-linux-1`) | `DNVGL_RP_B401_(2017)_Cathodic_Protection_Design.pdf` (private standards archive, path withheld); `DNV_RP_B401_(2021)_Cathodic_Protection_Design.pdf` (private standards archive, path withheld); `DNVGL_RP_F103_(2019)_*.pdf` (private standards archive, path withheld) (September 2019) and `DNV_RP_F103_(2019_amended_2021)_*.pdf` (private standards archive, path withheld) |
| Licence note | licensed local reference; raw PDFs remain off-repo; per-user watermark lines are not reproduced |

## DNVGL-RP-B401 June 2017 (`dnv-rp-b401/2017-06/`)

- Wiki page: `wikis/engineering-standards/wiki/standards/dnv-rp-b401-2017.md`,
  frontmatter `code_id: dnv-rp-b401`, `publisher: DNV`, `revision: "2017-06"`.
- Design tables moved to Appendix A. Values of A-1, A-2, A-3, A-4, A-6 and
  A-8 are identical to the 2010 Tables 10-1, 10-2, 10-3, 10-4, 10-6 and 10-8.
  A-5 (composition) and A-7 (resistance formulae) are not transcribed.

| File | Table |
|---|---|
| `dnv-rp-b401-2017-06-table-a-1.csv` | Table A-1 initial and final design current densities |
| `dnv-rp-b401-2017-06-table-a-2.csv` | Table A-2 mean design current densities |
| `dnv-rp-b401-2017-06-table-a-3.csv` | Table A-3 mean design current densities for reinforcing steel |
| `dnv-rp-b401-2017-06-table-a-4.csv` | Table A-4 paint coating breakdown constants a and b (categories I-III) |
| `dnv-rp-b401-2017-06-table-a-6.csv` | Table A-6 anode capacity and closed circuit potential (ambient temperature) |
| `dnv-rp-b401-2017-06-table-a-8.csv` | Table A-8 anode utilisation factors |

## DNV-RP-B401 May 2021 (`dnv-rp-b401/2021-05/`)

- Wiki page: `wikis/engineering-standards/wiki/standards/dnv-rp-b401-2021.md`,
  frontmatter `code_id: dnv-rp-b401`, `publisher: DNV`, `revision: "2021-05"`.
- Design tables in Sec. 8. 8-1, 8-2, 8-3 and 8-8 equal the 2017 values.
  8-4 adds category IV (a = 0.02; b = 0.008 for 0-30 m, 0.005 for >30 m).
  8-6 is keyed by anode surface temperature. 8-5 and 8-7 not transcribed.

| File | Table |
|---|---|
| `dnv-rp-b401-2021-05-table-8-1.csv` | Table 8-1 initial and final design current densities |
| `dnv-rp-b401-2021-05-table-8-2.csv` | Table 8-2 mean design current densities |
| `dnv-rp-b401-2021-05-table-8-3.csv` | Table 8-3 mean design current densities for reinforcing steel (footnote 1 kept as the last row) |
| `dnv-rp-b401-2021-05-table-8-4.csv` | Table 8-4 paint coating breakdown constants a and b (categories I-IV) |
| `dnv-rp-b401-2021-05-table-8-6.csv` | Table 8-6 anode closed circuit potential and capacity by anode surface temperature (footnote 1 kept as the last row) |
| `dnv-rp-b401-2021-05-table-8-8.csv` | Table 8-8 anode utilization factors |

## DNVGL-RP-F103 September 2019, amended May 2021 (`dnv-rp-f103/2019-09/`)

- Wiki page: `wikis/engineering-standards/wiki/standards/dnv-rp-f103-2019.md`,
  frontmatter `code_id: dnv-rp-f103`, `publisher: DNV`, `revision: "2019-09"`.
- The September 2019 print republishes the July 2016 edition with unchanged
  content; the May 2021 amendment is editorial (DNV naming, FJC system
  names). Numbers were checked in both prints.

| File | Table |
|---|---|
| `dnv-rp-f103-2019-09-table-6-2.csv` | Table 6-2 design mean current density by exposure and internal fluid temperature (five bands) |
| `dnv-rp-f103-2019-09-table-6-3.csv` | Table 6-3 anode design values by anode surface temperature (equal to B401 2021 Table 8-6) |
| `dnv-rp-f103-2019-09-table-a-1.csv` | Table A-1 linepipe coating constants a and b (printed directly, not x100) |
| `dnv-rp-f103-2019-09-table-a-2.csv` | Table A-2 field-joint coating constants a and b with the DNVGL-RP-F102 (2011) ids (September 2019 names) |

## Format and layout notes (2017 / 2021 / 2019 files)

- UTF-8, no BOM, LF as stored in the repository (same as the 2005/2010 files
  in the git index); caption row first, then header row(s), then data;
  footnotes, where kept, are the last row and start with `1)`, `*)` or `*`.
- Cells the print merges across rows are repeated in every row they cover
  (Table A-1 coating name / CDS / temperature; Table A-2 temperatures),
  except the Zn seawater cell of Table 8-6 / 6-3 (-1.030 V, 780 Ah/kg),
  which the print sets once across the `<=30` and `> 30 to 50` rows and
  which is left blank in the second row to preserve that layout (the module
  reads it as valid for both rows).
- Table 8-4 / A-4 print `a` in the column headers and `b` cells as
  `b = 0.10`, like the 2010 file. Table 8-6 keeps the thousands separators of
  the print (`2,000`); Table 6-3 prints `2000`. The Unicode minus (U+2212)
  of the Table 6-3 print is normalised to ASCII `-`.
- Table A-2 (2019) rows that print one a/b pair for two infill options are
  one CSV row with the infill cell `none or 4E(2) ...`; the two "3A FBE" rows
  and the two "NA" (5C(1) / 5C(2)) rows are distinct because their infill
  differs. The 2C(1) infill cell reads `on top 2B(1)` as printed.
