# Metocean Data

This domain contains metocean design and operating condition data extracted from standards and project sources.

Structure follows the raw → processed → results pipeline used across the repository.

- raw/: Original sources and direct table extractions from PDFs
- processed/: Cleaned, standardized CSVs with consistent headers and units
- results/: Summaries, plots, and analysis deliverables

## Source: API RP 2MET (Jan 2021)

- Title: API RP 2MET — Derivation of Metocean Design & Operating Conditions (Jan 2021)
- File: `raw/sources/API_RP_2MET_Derivation_of_Metocean_Design_Operating_Conditions_Jan2021.pdf`
- Provenance: Provided by user (`acma-projects/B1522/data/...`)
- License: Refer to API/Publisher terms; redistribution may be restricted

### Extraction Plan

- Tables extracted using `tabula` (Java backend) into `raw/extracted/api_rp_2met_jan2021/`
- Outputs: `table_###.csv` for each detected table and a combined Excel workbook
- Metadata: `metadata.json` summarizing pages, table sizes, and column headers

### How To Extract (local)

Requirements:
- Java 8+ (tested with OpenJDK 21)
- Python 3.10+
- Packages: `tabula-py`, `pandas`

Steps:
1) Ensure the source PDF is present at:
   `data/metocean/raw/sources/API_RP_2MET_Derivation_of_Metocean_Design_Operating_Conditions_Jan2021.pdf`
2) Run the extractor:
   `python scripts/metocean/extract_api_rp_2met.py`
3) Find outputs under:
   `data/metocean/raw/extracted/api_rp_2met_jan2021/`

### Next Steps

- Promote key cleaned tables to `processed/` with standardized schemas
- Add a data dictionary and unit references
- Generate plots for design sea states and return period summaries in `results/`

## Processed Schema (standardized)

The cleaning pipeline consolidates extracted tables into three standardized datasets:

- Waves: `data/metocean/processed/api_rp_2met_waves.csv`
  - Columns: `region`, `location`, `return_period_years`, `probability_exceedance`, `hs_m`, `tp_s`, `tz_s`, `direction_deg`, `source_table`
- Wind: `data/metocean/processed/api_rp_2met_wind.csv`
  - Columns: `region`, `location`, `return_period_years`, `probability_exceedance`, `u10_ms`, `direction_deg`, `height_m`, `source_table`
- Current: `data/metocean/processed/api_rp_2met_current.csv`
  - Columns: `region`, `location`, `return_period_years`, `probability_exceedance`, `speed_ms`, `direction_deg`, `depth_m`, `source_table`

Data dictionary: `data/metocean/processed/api_rp_2met_dictionary.json`

Run the cleaner after extraction:
`python scripts/metocean/clean_api_rp_2met.py`
