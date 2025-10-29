# South East Asia project -1 (Hengyi PMB CALM Buoy)

This folder packages a reference CALM buoy design extracted from the Hengyi PMB Single Point Mooring project. The dataset is intended as an example project for the digital model workspace and follows the CALM buoy documentation issued in 2017.

## Source Documents

- HYBN-01DD-3.15.6-84011SP-SP02-1001-0 — Specification for Hose Design Basis (Rev 0, 25 Apr 2017)
- HYBN-01DD-3.15.6-84011NA-DP03-2001-0 — CALM Buoy In-Place Motion (Rev 0, 12 Jun 2017)
- HYBN-01DD-3.15.6-84011NA-DP03-2002-0 — CALM Buoy Mooring Analysis (Rev 0, 12 Jun 2017)

Values copied into the CSV files maintain the units from the originating tables. Notes fields reference the table or section where each figure was obtained.

## File Inventory

| File | Description |
| --- | --- |
| `metadata.csv` | Project identification, location, and document references. |
| `buoy_geometry.csv` | Geometric particulars for the CALM buoy body and manifold. |
| `mooring_lines.csv` | Studless R3 mooring leg template including pretension, MBS, and operational safety factors. |
| `hawser_system.csv` | Hawser arrangement used for tanker connection with capacities converted to kN and ABS safety margins. |
| `environmental_conditions.csv` | Operational (1-yr) and survival (100-yr) environmental design inputs. |
| `water_levels.csv` | Tide and water level design envelope relative to chart datum. |
| `marine_growth.csv` | Marine growth allowances by depth band. |
| `temperature_profiles.csv` | Air and seawater temperature statistics for the site. |
| `hose_configuration.csv` | PLEM manifold geometry, float data, and hose content properties. |
| `performance_summary.csv` | Coupled analysis results for operating and survival cases including governing safety factors. |
| `vessel_design.csv` | 300k DWT design tanker particulars (full load & ballast). |
| `document_register.csv` | Normalized extract of the SALM/CALM document tracker for quick lookup. |
| `data_requests.csv` | Outstanding technical queries (TQs) and their impact on dataset completeness. |

## Usage Notes

- Operational cases correspond to 1-year return-period metocean with tanker connected; survival cases correspond to 100-year metocean with tanker disconnected.
- Mooring line tensions and hawser loads are taken from dynamic OrcaFlex simulations at the minimum design water depth (32.72 m). Survival cases report the governing maximum line forces for intact and damaged configurations with the tanker absent.
- Safety factors recorded in the mooring, hawser, and performance CSVs reference ABS code checks reported in the mooring analysis (Tables 6.6–6.14).
- Connection point offsets that were reported in millimetres have been converted to metres for consistency with the repository convention, and manifold azimuths now use the finer 0.1° values from the PLEM layout table.
- The `document_register.csv` file originates from the OrcaFlex data master tracker; `data_requests.csv` highlights unresolved SALM/CALM TQs (2019–2020) that explain missing properties such as detailed mass distribution, updated metocean scatter tables, and material datasets.

## Basis of Design Template

The provided `Ref/BOD_template_0.1.docx` is a boilerplate outline (Introduction, CAD modelling, loads/BCs, fatigue, recommendations) with no Hengyi-specific content. Use it as the framework to capture the CALM buoy basis once the required inputs in `data_requests.csv` are fulfilled.
