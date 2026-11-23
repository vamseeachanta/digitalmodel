# Public vessel specification reference

This document describes only the `vessel_specs_*` files.
Refs [issue 1773](https://github.com/vamseeachanta/digitalmodel/issues/1773).
The arrays at repo-root `assets/data/` contain five HLV rows and four pipelay
rows read from owner/operator sources on 2026-10-11 and 2026-11-14 (UTC).
These are partial demonstration references, not qualified operating envelopes.
They neither replace the vessel fleet collection nor certify current capabilities.
Existing representative-class GTM data and demo scripts are unchanged.

## Fields and units

Every schema property documents its meaning and unit with `description` and
`x-unit`. All listed fields are required; an unknown technical value is `null`.
The schemas forbid undeclared fields. Values are transcribed without estimates.

| Field                                    | Unit                | Meaning                                                                                      |
| ---------------------------------------- | ------------------- | -------------------------------------------------------------------------------------------- |
| `vessel_name`                            | dimensionless       | Publicly marketed vessel name                                                                |
| `owner`                                  | dimensionless       | Fleet owner/operator brand on the source, not legal ownership certification                  |
| `source_url`                             | dimensionless       | Primary retrieved public HTTPS source                                                        |
| `retrieved_utc`                          | UTC                 | RFC3339 retrieval instant of that primary source                                             |
| `field_sources`                          | dimensionless       | Map of every non-null fact field to a retrieved manifest URL                                 |
| `dp_class`                               | dimensionless       | Normalised IMO equipment-class vocabulary: DP-1, DP-2, DP-3 or null                          |
| `dp_notation_source`                     | dimensionless       | Normalised class and printed page locator or null; source link in `field_sources`            |
| `crane_capacity_tonnes`                  | metric tonnes       | Headline crane lifting capacity, with the stated basis; not topsides-lifting-system capacity |
| `crane_capacity_basis`                   | dimensionless       | `combined` vessel crane capacity or `individual` crane capacity                              |
| `deck_area_m2`                           | square metres       | Published usable deck area, never hull length multiplied by breadth                          |
| `bollard_pull_tonnes`                    | metric tonnes-force | Published bollard pull                                                                       |
| `lay_methods`                            | dimensionless       | Explicitly stated S-lay/J-lay/reel-lay methods; unknown is null                              |
| `tensioner_capacity_tonnes`              | metric tonnes-force | Published total installed capacity; optional tensioners excluded                             |
| `max_pipe_diameter_inches`               | inches              | Maximum outside diameter in the published equipment range                                    |
| `water_depth_min_m`, `water_depth_max_m` | metres              | Explicit operating bounds for the stated methods; not vessel draught or achieved records     |
| `record_water_depth_m`                   | metres              | Achieved pipelay record, kept separate from operating limits                                 |

Table 1. Schema field meanings; technical fields apply only to their respective dataset.

## Source readback and provenance

`vessel_specs_provenance.json` records nineteen retrieved documents: nine fleet pages,
five specification PDFs and five robots policies. It records retrieval instants,
SHA-256 digests, byte counts, final URLs, observed terms and access status.
The downloads remain outside Git; no owner brochure or page is redistributed.
No reuse licence was stated in the retrieved documents. Only factual
specifications are extracted; unknown rights and document vintage are disclosed
in the manifest. Nothing establishes engineering-use qualification.

Requests used one connection, a descriptive User-Agent, a 30-second timeout,
one retry maximum and a three-second same-site pause. The initial two robots policies
allowed the requested pages. No site refused access on 2026-10-11.
The 2026-11-14 UTC batch retained three further robots policies allowing the selected
URLs; three access stops are recorded below. Two policies advertise crawl delays:

- Jan De Nul: 10 seconds advertised; 4.221, 3.909 seconds between consecutive
  manifest retrieval instants (robots policy, fleet page, specification sheet).
- DEME: 30 seconds advertised; 3.948, 3.666 seconds between consecutive
  manifest retrieval instants (robots policy, Orion page, Gulliver page).

The unchanged helper applied its fixed three-second same-site pause. The batch
did not meet either advertised crawl delay; robots permission for the URLs does
not establish compliance with those delays. The intervals above describe recorded
observations, not measured request-start spacing.
Follow-up: `scripts/fetch_vessel_specs.py` must honor advertised crawl delays
before further collection. Historical observations are retained without alteration.

The committed `scripts/fetch_vessel_specs.py` reads source URLs and human
statements from the manifest and emits the same manifest structure, with new
retrieval observations and a product token and version only in its User-Agent.
Run it once into a new empty directory **outside the checkout**:

```sh
python scripts/fetch_vessel_specs.py "$VESSEL_SOURCE_OUTPUT"
```

Set `VESSEL_SOURCE_OUTPUT` to an external data directory. Numbered `.document`
files correspond to the manifest's source order; PDF text is extracted with
`pdftotext -layout` when installed, otherwise extraction is reported as skipped.
The helper stops a site on robots disallow, HTTP refusal or challenge, without
workarounds; redirects also stop access pending policy review. It refuses
existing output and never overwrites documents. Tests inject an offline
transport; the command is never run by tests. Documents and extracted text
are retained outside Git and shall not be committed to this public repository.
Factual transcription remains a separate readback with a page locator.

| Vessel            | Observed specification                                                                           | Source locator                                                                                                                                                                                          |
| ----------------- | ------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Thialf            | 14,200 metric tonnes combined crane lifting capacity                                             | [Heerema fleet page](https://www.heerema.com/heerema-marine-contractors/fleet/thialf), Main data / Lift Capacity; tandem basis in June 2020 sheet, page 2                                               |
| Thialf            | DP-3                                                                                             | [June 2020 operator sheet](https://www.heerema.com/hubfs/HMC%20Equipment%20Folder%20-%20Thialf.pdf), printed page 3                                                                                     |
| Sleipnir          | 20,000 metric tonnes combined; two 10,000-tonne cranes                                           | [Heerema fleet page](https://www.heerema.com/heerema-marine-contractors/fleet/sleipnir), Main data and vessel description                                                                               |
| Solitaire         | 1,050 t tension capacity; 2–60 inches outside diameter; DP-3                                     | [2026 operator sheet](https://www.allseas.com/sites/d10p.allseas.com/files/2026-02/solitaire-vessel-specs-2026_0.pdf), printed page 09                                                                  |
| Solitaire         | S-lay; achieved depth 2,775 m                                                                    | [Operator fleet page](https://www.allseas.com/en/who-we-are/our-fleet/solitaire), Setting the pipelay standard                                                                                          |
| Lorelay           | 165 t installed tension capacity; 2–41 inches outside diameter; DP-3                             | [2026 operator sheet](https://www.allseas.com/sites/d10p.allseas.com/files/2026-02/lorelay-vessel-specs-2026.pdf), printed page 11                                                                      |
| Pioneering Spirit | 2,000 t tension capacity; 6–68 inches outside diameter; DP-3                                     | [2026 operator sheet](https://www.allseas.com/sites/d10p.allseas.com/files/2026-02/pioneering-spirit-vessel-specs-2026.pdf), printed page 08                                                            |
| Pioneering Spirit | S-lay                                                                                            | [Operator fleet page](https://www.allseas.com/en/who-we-are/our-fleet/pioneering-spirit), Unmatched pipelay capacity                                                                                    |
| Les Alizés        | Name and operator                                                                                | [Fleet page](https://www.jandenul.com/fleet/heavy-lift-vessels), Les Alizés entry and site identity                                                                                                     |
| Les Alizés        | Individual crane 5,000 t at 34.7 m radius; deck 9,300 m²; DP-2                                   | [2026 operator sheet](https://www.jandenul.com/sites/default/files/public/Les%20Aliz%C3%A9s%20%28EN%29_2026_0.pdf), PDF page 2 (unnumbered), Technical specifications / Classification and Crane system |
| Orion             | Individual main crane 5,000 ton as labelled; deck 8,000 m²; DP-3; name and operator              | [Fleet page](https://www.deme-group.com/technologies/orion), Facts / Crane, Operational conditions, Power & propulsion; page title and site identity                                                    |
| Gulliver          | Combined tandem crane capacity 4,000 t; name and operator                                        | [Fleet page](https://www.deme-group.com/technologies/gulliver), Facts / Lifting capacity; page title and site identity                                                                                  |
| Castorone         | Maximum pipe diameter 48 inches without coating (60 inches including coating); name and operator | [Fleet page](https://www.saipem.com/en/solutions-energy-transition/fleet-and-yards/castorone), Pipelaying capacity; page title and site identity                                                        |

Table 2. Readback anchors; exact URLs and per-source retrieval instants appear in JSON.

Lorelay's optional fourth 100 t tensioner is excluded from its installed total.
The Pioneering Spirit sheet describes an approximate achieved depth; it does
not establish a precise operating limit and is not converted into one.
The Heerema headline lift ratings are not radius-dependent crane load charts.
Thialf's positioning class is normalised to the IMO equipment-class
vocabulary as DP-3; `dp_notation_source` retains only that value and its page locator.
Neither field asserts the vessel's current class status: the sheet is dated
June 2020. The live fleet page corroborates the lift rating.
All three pipe ranges explicitly state outside diameter (`OD`), in inches;
none of those printed lines adds a coating qualifier. Their upper bounds
match the maximum-outside-diameter field definition; 60, 41 and 68 are confirmed.
Castorone's JSON value is 48 inches without coating; the separate 60-inch limit
includes coating and is excluded from the pipe-diameter field.
The source calls the uncoated limit "pipe size" rather than explicitly stating
outside diameter; the field retains that source terminology limitation.
The page describes a future J-lay tower, so no active lay method is inferred.
Its tensioner capacity, positioning class and depth bounds remain null.
Les Alizés and Orion have no numbered printed-page positioning locator, so
`dp_notation_source` remains null. Gulliver's deck area and DP class remain null.
Gulliver is listed by both DEME and Jan De Nul in the retrieved fleet pages;
`owner` retains the DEME source brand without establishing exclusive ownership.
Orion's capacity is transcribed without conversion from the source's `ton` label;
the metric-tonne interpretation is assumed, not independently established.

Related legacy research was found in
`data/vessels/raw/install__crane_deck.json` and `install__particulars.json`.
Those records contain unparsed brochure citations and flagged identifiers.
They were used as source leads only; none of their numbers were copied.
The research issue links in issue 1773 now point to unrelated hydrodynamics work.

## Gaps and remaining work

- Thialf and Sleipnir: usable deck area and bollard pull were not established;
  both remain null. Sleipnir's DP class and source notation remain null.
- All four pipelay vessels: quantitative operating water-depth bounds remain
  null. An achieved depth or vessel draught cannot establish those bounds.
- Lorelay: a named lay method and numerical achieved depth were not established
  in the retrieved documents; both remain null.
- Pioneering Spirit: an exact achieved depth was not established; the approximate
  sheet statement is excluded.
- Batch two retained the first four sourceable rows in the approved alternating
  selection: Castorone, Les Alizés, Orion, Gulliver. The projected fifth row would
  exceed the collection cap and was omitted. That 400-line cap applied to the
  collected batch, not the full pull request including regression tests.
  Access stops were not retried or bypassed.
- DB50 (pipelay): refused; McDermott robots policy returned HTTP 403.
- DB30 (pipelay): refused; collection stopped for McDermott after HTTP 403.
- Seven Borealis (pipelay): refused; Subsea7 robots policy returned HTTP 403.
- Bokalift 2 (hlv): refused; Boskalis fleet page triggered the helper's access-challenge detector.
- Bokalift 1 (hlv): refused; collection stopped for Boskalis after the challenge.
- Svanen (hlv): batch cap; fifth row omitted under the 400-line limit.
- DB50 (hlv): refused; McDermott robots policy returned HTTP 403.
- Remaining HLV scope: Pioneering Spirit lifting systems, Boskalis, Van Oord,
  McDermott and further Jan De Nul, DEME and other vessels require source readback.
- Remaining pipelay scope: McDermott DB30, DB50, DB101 and other vessels remain
  unsourced in these arrays. No complete vessel roster was supplied.
- GTM demo wiring remains outside this change.
- Issue 1773 remains open; demo scripts must not assume a complete fleet.
- Extracted facts with source links are retained; no specification sheets,
  page images, extracted text, or quoted or paraphrased passages are copied.
- Merge sequencing against issue blockers and robots-policy decisions remain
  with the owner.

## Offline validation

With the repository's Python environment and existing jsonschema dependency:

```sh
PYTHONPATH=src uv run python -m digitalmodel.data_systems.vessel_specs assets/data
PYTHONPATH=src uv run python -m pytest tests/data_systems/test_vessel_specs.py
```

The validator checks schemas, finite positive numbers, case-insensitive unique
names, valid HTTPS URLs and UTC retrieval dates, ordered depth bounds, capacity
basis and exact per-field links to successfully retrieved manifest sources.
JSON duplicate keys and non-finite constants are rejected during parsing.
Offline checks cannot prove source authenticity or factual transcription;
the source readback above remains necessary.
