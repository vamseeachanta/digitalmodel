# Vessel Data — Ecosystem Source Inventory

Map of every vessel/rig data source found across the ecosystem (2026-06-19) and
how they relate, so the data stops fragmenting. **Decision: worldenergydata is
the single source of truth for collection; digitalmodel consumes it** (see
`README.md` → "Source of truth").

## Sources

| Source | Location | Count | Real? | Extracted from | Role now |
|---|---|---|---|---|---|
| **WED vessel_fleet (curated)** | `worldenergydata/data/modules/vessel_fleet/curated/` | **2,268 rigs + 17 constr.** | ✅ | BSEE pickle + web scrape + **PDF** + XLS | **SOURCE OF TRUTH** (consumed via `wed_adapter`) |
| Frontier Deepwater fleet | ace share `/frontierdeepwater/.../vessel-fleet/` (+2010/2011 snapshots) | ~350 heavy-lift + pipelay | ✅ | brochure digitization | candidate to ingest into WED |
| ACMA B1535 MSIV | ace share `/acma-projects/B1535/data/vessels/msiv/` | 30+ | ✅ | vessel brochures | candidate to ingest into WED |
| ACMA `_hulls` RAOs | ace share `/acma-projects/_hulls/` | Q4000, Uncle John, SDP3500, Aframax | ✅ | design XLS (**real RAOs**) | high-value for diffraction RAO layer |
| OCIMF Coef.xlsx | ace share `/acma-codes/OCIMF/` | tanker/gas coeffs | ✅ | digitized standard | OCIMF coefficient layer |
| llm-wiki OCIMF MEG4 annex | `llm-wiki/.../datasets/ocimf-meg4-annex-a/` | tanker coeff tables | ✅ | PDF table digitization | OCIMF coefficient layer |
| **dm vessel_db (this dir)** | `digitalmodel/data/vessels/` | 41 + RAO proxies + crane | ✅ | agent web research | engineering layers WED lacks |
| GTM synthetic JSONs | `digitalmodel/examples/demos/gtm/data/` | 4 classes | ❌ synthetic | — | demo fixtures only |
| orcaflex template CSVs | `digitalmodel/docs/.../components/vessels/` | 14 class rows | ❌ idealized | — | orcaflex templates |
| aceengineer-website rig sample | `aceengineer-website/.../rig-fleet-summary.csv` | 16 | ✅ | sample | website demo |
| workspace-hub GTM contractor matrix | `workspace-hub/docs/gtm/outreach/` | 26 contractor×vessel | ✅ | public web research | GTM outreach (not engineering) |

## The PDF-extracted database

The "vessels extracted from PDF" lives in **worldenergydata's `vessel_fleet`**:
`collectors/spec_pdf_collector.py` + `parsers/pdf.py` (pdfplumber) parse
contractor spec-sheet PDFs into the curated fleet. The ace-share Frontier
Deepwater and ACMA B1535 CSVs are additional brochure-digitized sets not yet
folded into WED.

## Consolidation status

- ✅ digitalmodel consumes the WED curated fleet via `marine_ops.vessel_db.wed_adapter`
  (installation crane vessels merged into `installation_vessels()`, WED preferred).
- ✅ Off-repo real-RAO adapter `marine_ops.vessel_db.hulls_adapter`
  (`VESSEL_RAO_LIBRARY_PATH`): parses real model-test/computed RAO workbooks into
  `RAOData` at runtime — **data stays off-repo** (project-confidential), nothing
  committed. Validated against a real semisubmersible workbook (heave RAO≈1.0 at
  long period, head seas). digitalmodel#852.
- ⬜ Ingest off-repo brochure-digitized heavy-lift + pipelay fleet (~350) into WED — worldenergydata#504.
- ⬜ Ingest off-repo MSIV brochure dataset (~30) into WED — worldenergydata#505.
- ⬜ Diffraction-setup consumer (particulars + gyradii → mass/inertia → OrcaWave/AQWA) — digitalmodel#853.
- ⬜ Retire/redirect the synthetic GTM JSONs + orcaflex template CSVs to the consolidated store.

> Confidentiality: real RAOs and brochure-digitized fleets are project data and
> are **not** committed to this public repo. Adapters resolve them off-repo via
> env vars (`VESSEL_RAO_LIBRARY_PATH`, `WED_VESSEL_FLEET_PATH`/`WORLDENERGYDATA_ROOT`).
