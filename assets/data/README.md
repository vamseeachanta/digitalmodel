# Public vessel specification reference

Refs [issue 1773](https://github.com/vamseeachanta/digitalmodel/issues/1773).
The arrays at repo-root `assets/data/` contain two HLV rows and three pipelay
rows read from owner/operator pages and specification sheets during this run.
These are partial demonstration references, not qualified operating envelopes.
They neither replace the vessel fleet collection nor certify current capabilities.
Existing representative-class GTM data and demo scripts are unchanged.

## Fields and units

Every schema property documents its meaning and unit with `description` and
`x-unit`. All listed fields are required; an unknown technical value is `null`.
The schemas forbid undeclared fields. Values are transcribed without estimates.

| Field | Unit | Meaning |
| --- | --- | --- |
| `vessel_name` | dimensionless | Publicly marketed vessel name |
| `owner` | dimensionless | Fleet owner/operator brand on the source, not legal ownership certification |
| `source_url` | dimensionless | Primary retrieved public HTTPS source |
| `retrieved_utc` | UTC | RFC3339 retrieval instant of that primary source |
| `field_sources` | dimensionless | Map of every non-null fact field to a retrieved manifest URL |
| `dp_class` | dimensionless | Explicit source DP class, DP1/DP2/DP3 hyphenated; NMD Class III preserved verbatim |
| `crane_capacity_tonnes` | metric tonnes | Headline crane lifting capacity, with the stated basis; not topsides-lifting-system capacity |
| `crane_capacity_basis` | dimensionless | `combined` vessel crane capacity or `individual` crane capacity |
| `deck_area_m2` | square metres | Published usable deck area, never hull length multiplied by breadth |
| `bollard_pull_tonnes` | metric tonnes-force | Published bollard pull |
| `lay_methods` | dimensionless | Explicitly stated S-lay/J-lay/reel-lay methods; unknown is null |
| `tensioner_capacity_tonnes` | metric tonnes-force | Published total installed capacity; optional tensioners excluded |
| `max_pipe_diameter_inches` | inches | Maximum outside diameter in the published equipment range |
| `water_depth_min_m`, `water_depth_max_m` | metres | Explicit operating bounds for the stated methods; not vessel draught or achieved records |
| `record_water_depth_m` | metres | Achieved pipelay record, kept separate from operating limits |

Table 1. Schema field meanings; technical fields apply only to their respective dataset.

## Source readback and provenance

`vessel_specs_provenance.json` records eleven retrieved documents: five fleet pages,
four specification PDFs and two robots policies. It records retrieval instants,
SHA-256 digests, byte counts, final URLs, observed terms and access status.
The downloads remain outside Git; no owner brochure or page is redistributed.
No reuse licence was stated in the retrieved documents. Only factual
specifications are extracted; unknown rights and document vintage are disclosed
in the manifest. Nothing establishes engineering-use qualification.

Requests used one connection, a descriptive User-Agent, a 30-second timeout,
one retry maximum and a three-second same-site pause. Both robots policies
allowed the requested pages. No site refused access during this run.

| Vessel | Observed specification | Source locator |
| --- | --- | --- |
| Thialf | 14,200 metric tonnes combined crane lifting capacity | Heerema fleet page, Main data / Lift Capacity; tandem basis in June 2020 sheet, page 2 |
| Thialf | NMD Class III dynamic positioning | June 2020 operator sheet, page 3, Dynamic positioning system |
| Sleipnir | 20,000 metric tonnes combined; two 10,000-tonne cranes | Heerema fleet page, Main data and vessel description |
| Solitaire | 1,050 t tension capacity; 60 inch maximum OD; DP3 | Linked 2026 operator sheet, printed page 09, Specifications |
| Solitaire | S-lay; achieved depth 2,775 m | Operator fleet page, Setting the pipelay standard |
| Lorelay | 165 t installed tension capacity; 41 inch maximum OD; DP3 | Linked 2026 operator sheet, printed page 11, Specifications |
| Pioneering Spirit | 2,000 t tension capacity; 68 inch maximum OD; DP3 | Linked 2026 operator sheet, printed page 08, Specifications |
| Pioneering Spirit | S-lay | Operator fleet page, Unmatched pipelay capacity |

Table 2. Readback anchors; exact URLs and per-source retrieval instants appear in JSON.

Lorelay's optional fourth 100 t tensioner is excluded from its installed total.
The Pioneering Spirit sheet describes an approximate achieved depth; it does
not establish a precise operating limit and is not converted into one.
The Heerema headline lift ratings are not radius-dependent crane load charts.
Thialf DP notation is from a June 2020 sheet; current class status is not
established by that document. The live fleet page corroborates the lift rating.

Related legacy research was found in
`data/vessels/raw/install__crane_deck.json` and `install__particulars.json`.
Those records contain unparsed brochure citations and flagged identifiers.
They were used as source leads only; none of their numbers were copied.
The research issue links in the task now point to unrelated hydrodynamics work.

## Gaps and remaining work

- Thialf and Sleipnir: usable deck area and bollard pull were not established;
  both remain null. Sleipnir's DP class remains null. Thialf's published NMD
  Class III is preserved without asserting equivalence to an IMO DP class.
- All three pipelay vessels: quantitative operating water-depth bounds remain
  null. An achieved depth or vessel draught cannot establish those bounds.
- Lorelay: a named lay method and numerical achieved depth were not established
  in the retrieved documents; both remain null.
- Pioneering Spirit: an exact achieved depth was not established; the approximate
  sheet statement is excluded.
- Remaining HLV scope: Pioneering Spirit lifting systems, Boskalis, Van Oord,
  Jan De Nul, McDermott, DEME and other vessels require separate source readback.
- Remaining pipelay scope: McDermott DB30, DB50, DB101 and other vessels remain
  unsourced in these arrays. No complete vessel roster was supplied.
- GTM demo wiring remains outside this change.

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
