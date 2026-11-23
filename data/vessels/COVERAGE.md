# Vessel Database — Coverage Report

Generated 2026-06-18 from the `vessel-db-research` agent workflow (22 agents) + one targeted re-collection. See `README.md` / `SCHEMA.yaml` for the contract.

**85 records across 4 scopes × 5 layers.** Every numeric field is a cited value, an `estimated:<basis>` string, or `gap` — no bare invented numbers (one fabrication caught + remediated, see below).

## Status matrix

| Scope | Layer | Records | Plausible | Citations | Note |
|---|---|---:|---|---|---|
| install | particulars | 10 | ✅ | thin | fabrication caught → remediated |
| install | rao | 11 | ✅ | solid |  |
| install | crane_deck | 11 | ✅ | solid |  |
| floating | particulars | 11 | ✅ | solid |  |
| floating | rao | 11 | ✅ | solid |  |
| support | particulars | 8 | ✅ | solid |  |
| support | metocean | 7 | ✅ | solid |  |
| tanker | particulars | 12 | ✅ | solid |  |
| tanker | rao | 4 | ✅ | thin |  |

## What's solid

- **Particulars** (install/floating/support/tanker): real named vessels with cited principal dimensions + displacement. Strongest layer.
- **Crane & deck** (install): SWL-vs-radius from operator brochure PDFs (Heerema, Saipem, Boskalis, Subsea7) — `solid`.
- **RAOs**: benchmark/proxy datasets (NREL OC4 semi & OC3 spar, OrcaWave L01/L02, ITTC S175, KCS, KVLCC2, DTMB 5415, Wigley, a box-form lay-barge AQWA study) — `solid` after re-collection.
- **Metocean** (support): regional design Hs/Tp/wind/current for North Sea, GoM, West Africa, Brazil, NW Australia — `solid`.

## Known gaps (expected — these are mostly proprietary)

- **Radii of gyration (kxx/kyy/kzz)** — almost never public; carried as `estimated:<relation>` or `gap`. Needed for diffraction mass setup → estimate per documented relation or get from client.
- **Named-vessel RAOs** for commercial crane/installation vessels — proprietary to owners; we use proxy hulls instead.
- **Paid feeds** (Clarksons, IHS/S&P Petrodata, Enverus, Bassoe) deliberately not chased — see each `sources/<scope>.json` `paywalled_gaps`.
- **install/particulars** earlier carried fabricated IMO identifiers; the verifier flagged them, they did not survive into the schema, and dimensional data is intact. See that file's `remediation` field.

## Per-dataset coverage notes

### install / particulars
All principal dimensions (LOA, beam, draft, displacement, GT) are real, citable values from Wikipedia, builder/operator pages, and Ship-Technology. LBP is almost never published for these specialised offshore vessels - gap throughout except DLV 2000 (waterline length given). Depth published only for Thialf, Pioneering Spirit, plus keel-to-pedestal heights for Saipem 7000/Castorone. Block coefficient not published for any (semi-subs have no meaningful Cb) - gap throughout. Mass properties (LCG/VCG/TCG, displacement-condition breakdowns) are confidential proprietary ballast/design data and are N…

### install / rao
RAOs for named commercial crane/installation vessels (Sleipnir, Thialf, Saipem 7000, derrick/pipelay barges) are proprietary and NOT public. Collected proxies instead: NREL OC4-DeepCwind semi (best SSCV-topology proxy) + OC3-Hywind spar (geometry confirmed against NREL-maintained OpenFAST input files since docs.nrel.gov PDFs were unfetchable); turn-key OrcaWave L02 (OC4 semi) and L01 (generic monohull) open RAO datasets; monohull seakeeping benchmarks DTMB 5415/5512, S175, KCS, KVLCC2, Wigley; one open-access box-form pipelay lay-barge AQWA study (closest barge proxy); Capytaine/NEMOH BEM exam…

### install / crane_deck
All records are real named installation/construction vessels with crane and deck data sourced from public web pages (Ship-Technology project pages, operator/manufacturer news, Wikipedia) and, where summarized by search, the operators' official PDF spec sheets (Heerema HMC equipment folders, Saipem datasheets, Allseas/Boskalis vessel-spec PDFs). LIMITATION: the official brochure PDFs (Heerema, Saipem, Allseas, Boskalis) are binary/compressed and could NOT be parsed by the fetch tool, so PDF-only numbers were taken from the search engine's extraction of those PDFs or from corroborating HTML page…

### floating / particulars
11 real named floating units captured across all required sub-types: 3 FPSOs (Terra Nova, SeaRose, Noble Seillean), 1 FDPSO (Azurite), 3 drillships (Yavuz, Fatih, Deepwater Conqueror), 1 production semisubmersible (Thunder Horse PDQ), 2 spars (Perdido, Genesis), 1 FSRU (Independence). LAYER-1 principal particulars (LOA, beam, often depth + one draft + displacement/tonnage) are well-sourced from public Wikipedia infoboxes and one OEM brochure. CRITICAL DATA-AVAILABILITY REALITY: (1) LBP is essentially never published in public sources for these units — universally a gap, which in turn blocks co…

### floating / rao
Layer 3 (motion RAOs). Real named-vessel/named-field RAO datasets are almost never public — operators treat hull hydrodynamic databases (AQWA/WAMIT .lis, OrcaFlex vessel-type RAOs) as proprietary. The honest, citable answer for this layer is the published BENCHMARK / model-test community datasets, which is what these records capture. Most are floating-offshore-wind floaters (OC3 spar, OC4/OC5 DeepCwind semisub) and academic seakeeping hull forms (S175, KCS, DTC, Wigley) — directly transferable RAO methodology to FPSO/FPU/drillship/FSRU hulls, but only the DeepCwind semisub and OTRC FPSO are fl…

### support / metocean
Regional metocean / environmental design criteria for support-vessel operability limits (AHTS, PSV, CTV, SOV/W2W). Records are structured per region. STRONGEST DATA (full extreme-value tables with explicit return-period columns, extracted from public PDFs via pdftotext): (1) North Sea / Buchan Deep from the publicly published Hywind Buchan Deep Metocean Design Basis (marine.gov.scot), and (2) Gulf of Mexico West and West-Central regions from API Bulletin 2INT-MET (free copy on law.resource.org). These give omni-directional Hs, Hmax, Tp, 1-hr/10-min/3-s wind at 10 m, and surface/profile current…

### tanker / particulars
12 real named/standard-class vessels collected across all 5 SCOPE types: VLCC (Almi 320k DSME, Almi 315.5k HSHI, Alaska-class), Suezmax (Almi 158k DSME, GSI Greenway 158k LNG dual-fuel), Aframax (Almi 115k DSME, MV Prestige), LNG carriers (Mozah Q-Max, Hoegh Esperanza, standard 174k membrane class), LPG/VLGC (LPGC Ayame, standard 84k VLGC). LAYER 1 (principal particulars) is well-covered with cited values for LOA, beam, draft, deadweight, GT, cargo capacity from public Wikipedia pages, VesselFinder/Marine Insight news, and the Almi Tankers 2018 brochure PDF (extracted via pdftotext: LOA, beam,…

## Next steps to make this analysis-ready

1. **Promote `raw/` → `processed/`**: flatten to per-scope CSV/parquet matching `SCHEMA.yaml`, dedup, attach a gyradii-estimation pass (documented relations) so diffraction setups have mass props.
2. **Wire into `hull_library` catalog + `marine_ops/installation`** as the backing dataset (currently code-complete, data-empty).
3. **Crane curves → installation envelope**: the brochure SWL-vs-radius points feed `crane_tip_motion` / `go_no_go`.
4. **File a dedicated issue + branch** (we're currently on `feat/821-...`); land via PR per repo gate.
