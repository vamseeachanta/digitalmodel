# Project → Domain / Client Map of CAD-bearing Folders

**Epic:** [#1004](https://github.com/vamseeachanta/digitalmodel/issues/1004) · **Issue:** [#1007](https://github.com/vamseeachanta/digitalmodel/issues/1007) (3/5) · depends on the inventory ([#1005](https://github.com/vamseeachanta/digitalmodel/issues/1005))
**Source:** `cad-file-manifest.csv.gz` + folder-structure probes on ace-linux-1

> **Privacy note — this repository is PUBLIC.** Client / operator / field identities and any personal names are
> **de-identified** here (coded `O`, `E1`, `V`, etc.). The code → real-identity key is kept **only** in the private
> ace-linux-1 context (not committed). Internal ACE project numbers that do not, on their own, reveal an end client
> are retained for traceability.

---

## How to read "CAD density" and "canonical"

- **CAD density** = how much *modeling/drawing* substance lives there, weighted toward **curated native CAD**
  (SolidWorks/Inventor parts & assemblies, drawing sets) rather than analysis I/O. An OrcaFlex-only folder is
  "analysis-dense", not "CAD-dense".
- **Canonical?** = whether to treat the folder as the live source (✅), an archival duplicate to ignore (🗄️ dedup),
  or a quarantine/backlog pile (⚠️).

---

## A. The two masses that dominate the share

| Code | Folder (abstracted) | Domain | Client | Files | CAD character | Canonical? |
|---|---|---|---|--:|---|---|
| **PA** | `…/knowledge_skills/projects/ri/00_inbox/` | Risers / subsea equipment **drawings** | Internal — a single ex-staff member's **personal hard-drive & flush-drive backups** consolidated into one inbox | **416,283** | Inventor + AutoCAD; massively duplicated, un-sorted ("Backup Old Hard drive", "256G-flushdrive", "2tb", "Risers DWGs", "Load King Drawings") | ⚠️ **Quarantine/backlog** — *not* a model source; treat as a data-hygiene problem |
| **CR** | `digitalmodel/docs/domain/subsea-risers/riser-eng-job/` | Subsea **risers** (equipment models) | Internal engineering library | **36,787** (≈28.8k SolidWorks) | The single richest **curated SolidWorks** pile on the share | ⚠️ **Loose working dir** — present in the repo checkout but **NOT git-tracked**; canonical-_ish_ but uncommitted |

These two folders are ~86% of all matches. **Everything below is the genuinely curated, project-scoped CAD** —
small in file count, high in value.

---

## B. Curated engineering projects (the real reuse value)

Ranked by curated-CAD density × reuse potential.

| Rank | Code | Project (abstracted) | Domain | Client (de-id) | CAD/analysis character | Files | Canonical? |
|--:|---|---|---|---|---|--:|---|
| 1 | **P1** | `…/misc/projects/2100_*_slor_design` | Subsea risers — **SLOR** (single-line offset riser) design | Deepwater operator (de-id) | Heavy SolidWorks part/assembly modeling | ~26,120 | ✅ live |
| 2 | **P2** | `…/drilling/projects/3824_*_containment_riser_analysis` | Drilling / **containment-riser** analysis | Major operator (de-id) | Mixed CAD + OrcaFlex; large drawing set | ~9,251 | ✅ live |
| 3 | **F** | `*_fdas_*` family (tensioner-cart FE, airgap, support, animation) | **Drilling-riser tensioner** equipment & FE | Private deepwater **appraisal program** (de-id) | SolidWorks equipment + FE; the epic's "FDAS tensioner" assemblies | ~1,100 | ✅ live (private) |
| 4 | **L1** | `acma-projects/B15xx` (LNG terminal) | **LNG terminal / structural** (Ansys FEA) | LNG terminal owner (de-id) | SolidWorks + FEA; structural fabrication | ~480 (+3,230 archived twin) | ✅ live (`acma-projects`); 🗄️ dedup `.preexisting` |
| 5 | **O** | `…/drilling/projects/{31057,31098,...}` | Drilling-riser & subsea-structure analysis | Several international operators (de-id) | Per-job riser analysis; some CAD | ~1,400 | ✅ live |
| 6 | **S1** | `seanation/0122_ct_drilling/reference/design` | **Coiled-tubing drilling** design | CT-drilling client (de-id) | ~700 SolidWorks design refs | 225 live (+481 archived) | ✅ live (`seanation`); 🗄️ dedup `.preexisting` |
| 7 | **E1** | `epc-partner/‹field›` | Deepwater field development (EPC support) | EPC contractor / deepwater dev (de-id) | OrcaFlex-dominant (`general/engg`, modular YAML) | 317 (archive only) | ⚠️ `.preexisting` only — **kept as canonical** (no live twin) |
| 8 | **V** | `rock-oil-field/s7/{…}` | **VIV / riser-fatigue** (Shear7) | Multiple operators (de-id, 4+ fields) | Analysis-dense (Shear7), little native CAD | 424 (archive only) | ⚠️ `.preexisting` only — **kept** (no live twin) |
| 9 | **D1** | `eng-partner/models/‹field›`, `62092_sesa` | Deepwater riser/field analysis | Partner engineering firm (de-id) | OrcaFlex + Abaqus models | 18 live (+502 archived) | ✅ live (`eng-partner`); 🗄️ dedup `.preexisting` |
| 10 | — | `client_projects/energy_*` | Mixed: pipeline-installation, intervention riser, VIV | Several consulting clients | OrcaFlex + some SolidWorks (`ecs/quotes`) | 815 live (+4,522 archived) | ✅ live; 🗄️ dedup `.preexisting` |

---

## C. Not project CAD (exclude from reuse analysis)

| Folder | Why excluded |
|---|---|
| `OGManufacturing/.venv/...` | 1,115 files = Python `site-packages` (scipy test `.dat`/`.nc`) — dependency noise |
| `WEC-Sim`, `openfast`, `MoorDyn`, `MoorPy`, `capytaine`, `gmsh` | Open-source simulation toolkits (incl. the 10 NetCDF `.nc`), not in-house CAD |
| `gdrive` | Mirrored Google-Drive corpus (mixed back-office) |
| `O&G-Standards` | Standards/reference `.dat`, not modeling |
| `data/`, `build/`, `aceengineer-admin` | Archives / admin / build artifacts |

---

## D. Active vs `.preexisting-before-repo-move-*` — settled per folder

| Live folder | Archived twin | Decision |
|---|---|---|
| `client_projects` (815) | `client_projects.preexisting…` (4,522) | **Live = canonical**, archive = dedup 🗄️ |
| `acma-projects` (474) | `acma-projects.preexisting…` (3,230) | **Live = canonical**, archive = dedup 🗄️ |
| `eng-partner` (18) | `eng-partner.preexisting…` (502) | **Live = canonical** (but thin — archive holds most history; spot-check before deleting) |
| `seanation` (225) | `seanation.preexisting…` (481) | **Live = canonical**, archive = dedup 🗄️ |
| — (no live) | `epc-partner.preexisting…` (317) | **Archive IS canonical** — keep |
| — (no live) | `rock-oil-field.preexisting…` (424) | **Archive IS canonical** — keep |

> Caveat: dedup here is by **folder identity**, not content hash. `eng-partner` live (18) ≪ archive (502) suggests the
> "live" folder may be a partial re-clone, not a full superset — a per-file hash reconciliation is recommended
> before any archive is deleted. Flagged for follow-up; out of scope for discovery.

---

## E. Where the reusable CAD value concentrates (feeds #1009)

1. **Subsea-riser equipment modeling (SolidWorks)** — `CR` (riser-eng-job) + `P1` (SLOR) + `F` (FDAS tensioners)
   are the same *domain*: deepwater riser hardware (tensioners, clamps, joints, frames). This is the **densest,
   most reusable CAD domain** and the natural target for parametric/library automation.
2. **Riser & mooring *analysis* (OrcaFlex)** — `P2`, `E1`, `V`, `D1`, `client_projects` — analysis-dense, already
   matched by digitalmodel's `solvers/orcaflex`. Reuse via the existing Python surface, not CAD modeling.
3. **The `PA` personal hoard** is a **liability and an opportunity**: 416k duplicated files = a de-dup / sort /
   BOM-extract automation target, but contains no unique modeling value beyond what's already in `CR`/`P1`/`F`.

See [`sample-deepdive.md`](./sample-deepdive.md) (#1008) for structure of representative `CR`/`F`/`P1` assemblies,
and [`automation-opportunity-report.md`](./automation-opportunity-report.md) (#1009) for the ROI ranking.
