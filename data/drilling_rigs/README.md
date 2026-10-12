# External ultra-deepwater rig records

- Other contractors: zero verified rig rows.
- Semisubmersibles: zero verified rows. No semisubmersible specification or contract was verified in this pass.
- "Only two 20K rigs worldwide" is not proven by this note: it verifies that these two have 20K stacks, not that no other rig does. Other contractors' rigs were not verified (see Coverage).
- 15K coverage is partial by construction. Most modern ultra-deepwater floaters carry 15,000 psi stacks; this note verifies nine Transocean drillships only. It must not be read as a census of the 15K fleet.
- The gap between the first 20K-era contract (Titan, 463,000) and the latest 20K fixtures (Atlas, 635,000-650,000) is as published; no trend or forecast is implied.

This REAL-EXTERNAL file is a transcription of the [research note on the issue](https://github.com/vamseeachanta/digitalmodel/issues/899#issuecomment-6113191084), checked 2026-10-11. It retains the note's 9 rig rows and 18 contract rows separately from other holdings. The file uses `data/drilling_rigs/` because the proposed `config/...` folder was unspecified. No model consumes this file.

| Block | Layout |
|---|---|
| Header and limits | Schema version, data class, entry source, checked date and coverage limits |
| Sources | S1–S7: type, title, printed URL, published wording and checked date |
| Rigs | Each sourced field wraps `value`, `source_key`, `as_published_on`, optional `unit` and quoted `as_published` wording; `generation` is null |
| Contracts | Printed rig, customer, location, status and date strings; dayrate, source keys, report date, footnote marker and earlier-report values |
| Secondary reports | S6 deployment account, marked secondary |
| Narrative items | S7 award account, outside the rig rows |
| Not verified | Coverage gaps and secondary generation labels; no record references these statements |

Table: record layout.

Rig values retain the source's published-date wording; spec-sheet dates are file dates, not printed revision dates. Hookload carries S1 and S2. Contract `as_published_on` is the ISO report date of the first source key (S1 when present, S2 for S2-only rows). Earlier values have their own S2 date. `not_disclosed` preserves the note's undisclosed dayrate; null contract values preserve printed hyphens. The Conqueror drilling depth remains null with `not extracted (unverified)` status. Atlas's printed metric figure remains in `as_published`, with `sic: true`; no conversion is applied. Same-wording cells carry the note's attribution to Poseidon.

| Key | Type | Title | Published | URL |
|---|---|---|---|---|
| S1 | Primary, securities filing (8-K Ex. 99.2) | Transocean Ltd. Provides Quarterly Fleet Status Report | 2026-08-05 | https://www.sec.gov/Archives/edgar/data/0001451505/000145150526000058/rig-20260805xex99d2.htm |
| S2 | Primary, securities filing (8-K Ex. 99.2) | Transocean Ltd. Provides Quarterly Fleet Status Report | 2026-05-04 | https://www.sec.gov/Archives/edgar/data/0001451505/000145150526000041/rig-20260504xex99d2.htm |
| S3 | Primary, contractor spec sheet | Deepwater Atlas rig specification | file dated 2024-07-12 | https://deepwater.com/documents/rigspecs/deepwater%20atlas.pdf |
| S4 | Primary, contractor spec sheet | Deepwater Titan rig specification | file dated 2023-12-14 | https://deepwater.com/documents/rigspecs/deepwater%20titan.pdf |
| S5 | Primary, contractor spec sheets | Deepwater Poseidon / Pontus / Conqueror / Proteus / Thalassa / Invictus / Asgard rig specifications | Conqueror file dated 2026-02-02; Invictus 2024-07-12; others undated | https://deepwater.com/documents/rigspecs/deepwater%20poseidon.pdf |
| S6 | Secondary, trade press | Transocean Deploys World's First Subsea 20,000 psi Blowout Preventer (Ocean News) | 2024-07-10 | https://oceannews.com/news/energy/transocean-deploys-world-s-first-subsea-20-000-psi-blowout-preventer |
| S7 | Primary, contractor report | Valaris Fleet Status Report | 2026-08-05 | https://s23.q4cdn.com/956522167/files/doc_financials/2026/q2/08052026-Fleet-Status-Report_FINAL.pdf |

Table: source list as recorded in the research note.

Footnote 1 in both reports covers dayrate changes from cost escalation or de-escalation; that explains the small S2-to-S1 movements. The Atlas gap between Jul-27 and Jun-28 has no published contract in S1.

S5 prints only the Poseidon URL and remarks “same path pattern, rig name substituted”; no additional URL is constructed. Source pages were not opened for this transcription.

| Issue task or gap | Disposition |
|---|---|
| Marketed supply by year | Not delivered; not researched in this pass |
| Fleet-level utilization | Not delivered; not researched in this pass |
| Drilling-versus-intervention split and intervention-vessel counts | Not delivered; intervention-vessel counts were not researched in this pass |
| Other contractors and semisubmersibles | Zero verified rig rows in the note; not delivered |
| Unverified items | Retained only in `not_verified`; not model inputs |
| Model integration | Left for later |

Table: remaining issue scope.
