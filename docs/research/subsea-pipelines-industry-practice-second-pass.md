# Subsea pipeline industry practice: second pass

Checked 2026-11-13. Companion to the first-pass survey; no new engineering analysis.
Only extracted facts and source links are retained. Read means repository readback for S1/S2; other sources require a public page opened in this pass. Unread sources do not establish coverage.

## 1. Contractor matrix updates

### Verified

| Contractor: vessel | Technique | Extracted facts | Basis and limits |
|---|---|---|---|
| Allseas: Solitaire | S-lay [S2] | 1,050 t total installed tension capacity [S1]; 2 to 60 in outside diameter [S1]; DP-3 [S1]; achieved record 2,775 m [S2], not an operating limit | 2026 filename edition; publication date not established [S1]. Dataset: demonstration reference facts only; not an engineering operating envelope. |
| Subsea7: Seven Borealis | S-lay; J-lay | Dynamic tension: S-lay 600 t; J-lay 937 t. S-lay pipe range 4.5–46 in; J-lay 4–24 in. | May 2025 sheet, pipelaying system, pages 1–2 [S3](https://www.subsea7.com/content/dam/subsea7-corporate2018/Datasheets/Vessel/2025/Seven_Borealis-300dpi.pdf.downloadasset.pdf); published water depths: S-lay 20–3,000 m; J-lay 3,000 m. |
| TechnipFMC: Deep Energy | Reel-lay | Rigid pipe; flexible pipe; umbilicals. | Published water depth up to 3,000 m [S4](https://www.technipfmc.com/en/what-we-do/fleet); undated fleet page, checked in this pass. |

No new Solitaire fetch. The JSON provides the upper diameter bound; the single specification-sheet readback anchor supplies the lower bound. Retrieval instants and digests are copied from the [provenance manifest](../../assets/data/vessel_specs_provenance.json) [S1][S2]. The [JSON row](../../assets/data/vessel_specs_pipelay.json) and [README Table 2](../../assets/data/vessel_specs_README.md) are the readback basis. Source links: [sheet](https://www.allseas.com/sites/d10p.allseas.com/files/2026-02/solitaire-vessel-specs-2026_0.pdf), [fleet page](https://www.allseas.com/en/who-we-are/our-fleet/solitaire).

### Unverified or not reached

| Item | Residual figures or missing evidence | Reason |
|---|---|---|
| Allseas: Solitaire | three 350 t tensioners; 300 m length *(unverified)* | Absent from the repository readback anchors. Both occur in legacy raw records, classified as source leads only by the vessel README; neither is promoted. |

## 2. Tool matrix updates

### Verified

| Tool | Extracted facts | Source |
|---|---|---|
| SAGE Profile | Finite-element route optimisation, lay simulation, free-span evaluation, on-bottom stress, buckling and expansion analysis. | [S5](https://www.sage-profile.com/sage-profile/) |
| ABAQUS | Finite-element installation and pipe-soil interaction modelling; residual tension and operating pressure/temperature loads. | Vendor case study [S6](https://blog.3ds.com/brands/simulia/hcl-tech-masters-subsea-pipeline-design-simulia/); reported use, without independent benchmark verification. |

## 3. JIP landscape updates

### Verified

| JIP | Scope | Source |
|---|---|---|

No verified additions; SAFEBUCK remains supported by first-pass section 6.

### Unverified or not reached

| JIP | Status | Reason |
|---|---|---|
| HOTPIPE | located_not_read [S7](https://doi.org/10.1115/OMAE2005-67523) | DOI page inaccessible through the browsing tool; no scope claim is taken from search summaries. The separate OnePetro SAFEBUCK/F110 abstract also returned a cache miss. |
| POSIT | not reached | Searches for the exact name with pipe-soil interaction, pipeline and joint-industry-project terms did not locate a public page naming this JIP and its scope. No coverage is inferred from related projects. |

## 4. Papers

ISOPE/OPT papers deferred under the approved line-budget rule: the evidence validator and status table take priority. The first pass already meets the five-paper count; venue breadth remains open. OTC papers do not count toward this criterion. No standard text, proprietary deliverables, licensed analysis, trade-journal sweep, line-pipe mills, CP or pre-commissioning follow-up is included.

## 5. Acceptance status

First-pass evidence: [survey](subsea-pipelines-industry-practice.md), sections 3 (papers), 4 (contractors), 1 (tools), 6 (JIPs), 2 (methods). P ids retain the first-pass checked date; S metadata is in the adjacent sources file.

| Criterion | Required items | Covered items with evidence ids | Disposition | Note |
|---|---|---|---|---|
| 5+ recent OMAE/ISOPE/OPT papers summarized | OMAE, ISOPE, OPT | P1 [P1]; P2 [P2]; P3 [P3]; P4 [P4]; P5 [P5] | met | Five OMAE2024 abstracts; ISOPE/OPT breadth remains open. |
| Contractor + vessel capability matrix (Subsea7, Allseas, Saipem, TechnipFMC, McDermott, Heerema) | Subsea7, Allseas, Saipem, TechnipFMC, McDermott, Heerema | Saipem [P21]; McDermott [P22]; Heerema [P23]; Subsea7 [P24]; Allseas [S1][S2]; TechnipFMC [S4] | met | Six contractors; first-pass section 4 plus second-pass verified rows. All six required names have read evidence. |
| Tool capability matrix (OrcaFlex, SAGE Profile, ABAQUS, StableLines) | OrcaFlex, SAGE Profile, ABAQUS, StableLines | StableLines [P8]; OrcaFlex [P26][P27][P25]; SAGE Profile [S5]; ABAQUS [S6] | met | Four required tools; capability statements establish no usage share. |
| JIP landscape (SAFEBUCK, HOTPIPE, POSIT) documented | SAFEBUCK, HOTPIPE, POSIT | SAFEBUCK [P17][P19] | partly met | HOTPIPE unread; POSIT not reached. |
| Method convergence/divergence flagged | Converging, Diverging or unresolved | Converging [P3]; Diverging or unresolved [P1] | met | Both lists in first-pass section 2. |
