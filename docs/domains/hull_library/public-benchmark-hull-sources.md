# Public benchmark hull sources

public benchmark hull source register. Source: [research note on the issue, checked 2026-10-11](https://github.com/vamseeachanta/digitalmodel/issues/1839#issuecomment-6111980881). No geometry was downloaded.

**Verified** = geometry location and the quoted particulars were read on a page or repository listing opened during this check.
**Partial** = the files or pages were confirmed, but some particulars could not be read from a primary source (flagged "unverified" in the cell).
**Unverified** = from memory or a search-result summary only; do not seed the registry from these rows without a second check.

"Terms" quotes the source. Where a source says nothing, the cell says "terms not stated", which is not the same as permission: treat those as cite-and-reference, do not redistribute the files.

| Id | Hull | Grade | Geometry available |
| --- | --- | --- | --- |
| S1 | KCS, KRISO container ship | verified | yes |
| S2 | KVLCC2, KRISO tanker | verified | yes |
| S3 | DTMB 5415, naval combatant | verified | yes |
| S4 | JBC, Japan Bulk Carrier | verified | yes |
| S5 | ONR Tumblehome (ONRT), naval surface combatant | verified | yes |
| S6 | DTC, Duisburg Test Case, 14 000 TEU container ship | partial | yes |
| S7 | Series 60, Cb = 0.6 | partial | no |
| S8 | Wigley hull (parabolic, analytic) | unverified | no |
| S9 | S-175 container ship (ITTC seakeeping comparative study) | unverified | no |
| O1 | DeepCwind / OC4 Phase II semi-submersible (three offset columns plus centre column) | verified | yes |
| O2 | OC3-Hywind spar | partial | yes |
| O3 | UMaine VolturnUS-S semi-submersible for the IEA 15 MW reference turbine | verified | yes |
| O4 | ISSC tension-leg platform (four columns, four pontoons) | verified | dimensions only |
| O5 | Rectangular barges and FPSO with internal tanks (WAMIT standard tests) | verified | dimensions only |
| O6 | Truncated circular cylinder, and cylinder with moonpool | verified | yes |
| O7 | NEMOH verification set (cylinder, two bodies, non-symmetrical body) | partial | yes |
| O8 | Reference Model 3 (RM3) two-body point absorber: float and spar/plate | partial | yes |
| O9 | ITI Energy barge and MIT/NREL TLP (5 MW baseline platforms) | partial | no |

Grades follow the Counts section. O2 retains its conflicting Status cell in the YAML and is listed in `meta.note_inconsistencies`. Geometry availability describes the source note; it does not establish reuse permission. "dimensions only" identifies the manual-based cases whose demonstration files have restrictive terms.

Suggested use for the registry: seed from the Apache-2.0 rows first (O1, O2, O3, O6), reference the workshop hulls by URL and checksum rather than vendoring the files, and rebuild the box and TLP cases from published dimensions.
