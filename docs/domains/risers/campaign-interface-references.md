# Riser campaign interface references

The [public interface inventory](campaign-interface-references.json) records the
return surface available for a private consumer comparison. It does not establish
which outputs an external campaign consumes. External consumer completeness,
private readback and campaign qualification are **NOT_EVALUATED**. No private
repository was accessed and no private sidecar entry has been assigned.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-19 | Record the public extraction surface and the evidence required to resolve the private consumer gap in issue #2531. |

## Scope and public source basis

This inventory describes public source revision
`6476218e8f5af3fa7d16c3dafc60b6a9084c6e6c`. Source paths and symbol names in
the inventory locate the implementation; they are not private evidence locators.

| Surface | Public implementation | Inventory coverage |
| --- | --- | --- |
| Drilling extraction | [RiserCampaignAdapter.extract](../../../src/digitalmodel/drilling_riser/campaign.py) | Static response and end-tension maps, ring elevation, optional modes; dynamic governing responses, series statistics, sample count and W5 delegation |
| Open-water extraction | [RiserCampaignAdapter._extract_open_water](../../../src/digitalmodel/drilling_riser/campaign.py) | Static end tensions, base moment, stress, frame elevation and optional modes; dynamic extremes, series statistics, sample count and W5 delegation |
| Hang-off extraction | [hang_off_summary and extract_hang_off](../../../src/digitalmodel/drilling_riser/global_model/w5_channels.py) | Separate static and dynamic summary maps plus W5 delegation; the adapter returns before its drilling modes, series and fatigue additions |
| Response maps and units | [static_responses, governing_responses, end_effective_tensions, open_water_end_tensions, open_water_static_responses and open_water_governing_responses](../../../src/digitalmodel/drilling_riser/global_model/orcaflex_run.py) | Explicit producer keys, dictionary expansions and units as implemented |
| Nested W5 output | [extract, extract_open_water, extract_hang_off and UNITS](../../../src/digitalmodel/drilling_riser/global_model/w5_channels.py) | The top-level `w5` field is recorded; nested points, range graphs, events and fatigue channels require a later field inventory |
| External calculation contracts | [load_calc_contracts](../../../src/digitalmodel/drilling_riser/calc_contracts.py) | Existing loader scans registry and source pages, selects mappings with both `rsu_id` and `calc`, rejects duplicate pairs and adds `source_page`; this is not a campaign consumer inventory |

Issue #2531 defines the private reference gap. The inventory introduces no engineering
equations, coefficients, conversion factors or acceptance limits. Units describe
the existing implementation contract, including its explicit scalar conversions
and W5 `UNITS` mapping; they do not verify solver settings or private consumer
units. Raw dynamic series are summarized by the adapter's `_stats`; the scalar
summary and the series can use different force units.

Open-water dispatch precedes hang-off dispatch. For the remaining cases,
`spec.hang_off is not None` selects hang-off; `analysis == "statics"` selects
static extraction and the other path selects dynamic extraction. This records
the current code paths, not validation of arbitrary analysis labels. Modal
output is conditional on a truthy `params.modal_modes` in the drilling and
open-water static paths. A truthy `params.fatigue` adds nested W5 fatigue output
only in their dynamic paths. Each branch in the JSON lists its own fields and
the producer maps it expands. Optional fields are not promised on every result.

## Private evidence contract

The following is a preparation checklist for an authorized private sidecar, not
an assertion that such a sidecar exists. Keep private values, names, locations,
consumer code and source documents in the authorized data repository. Allocate
stable opaque identifiers there for the sidecar entry, campaign-derived lessons
and consumer evidence. Identifiers must not encode private names or locations.
Publish only the approved opaque identifiers, their reference revisions and
the resulting evidence status. A public identifier is a pointer, not proof of
authorization, readback or engineering qualification.

| Required private fields | Evidence to retain and read back privately |
| --- | --- |
| `entry_id`, `entry_revision` | Stable opaque sidecar identity and substantive revision; changes to evidence create a new revision |
| `dataset_id`, `dataset_revision`, `dataset_digest`, `digest_scope` | Stable dataset identity, exact source revision, content digest and the bytes or canonical representation covered by the digest |
| `lesson_id`, `lesson_revision`, `lesson_digest` | Stable campaign-derived lesson identity and evidence revision; record established absence explicitly rather than inventing a lesson |
| `consumer_evidence_id`, `consumer_evidence_revision`, `consumer_evidence_digest` | Opaque identity and revision of the retained field-by-field consumer inventory and its evidence |
| `adapter_revision`, `consumer_revision` | Exact public producer and private consumer code revisions used for the comparison |
| `branch`, `field_path`, `consumption_status` | Each applicable public field, nested field or conditional output matched to the consumer, explicitly unused or unresolved; also retain consumer-required fields absent from the producer |
| `units` | Producer and consumer units, reference frame, sign convention and any conversion evidence for each consumed field; do not infer units solely from suffixes |
| `derived_from` | Source relationships linking datasets, lessons, consumer evidence and campaign results by stable identity and revision |
| `target_solver_version` | Exact intended solver version, supported by retained evidence rather than inferred from an adapter import |
| `intended_use`, `qualification_status`, `qualification_evidence` | Intended use, governing criteria revision, applicable checks, their evidence and limitations; missing checks remain NOT_EVALUATED |
| `readback_revision`, `readback_digest`, `readback_status` | Revision and digest independently read from the authorized repository, comparison outcome and unresolved discrepancies |

`source_page` from the existing calculation-contract loader is a provenance
filename, not an opaque public reference. The loader does not require the
dataset revisions, content digests, lesson relationships, solver version or
intended-use qualification above. Finding a contract or passing an offline test
therefore cannot close this evidence gap. Loader behavior is unchanged.

## Readback and publication procedure

1. In the authorized repository, enumerate the actual consumers and their code
   revisions. Compare each applicable producer field and condition with each
   consumer input. Expand nested W5, modal and fatigue structures there before
   claiming complete coverage. Include extra required consumer inputs as gaps.
2. Retain the required private evidence above. Resolve units and relationships
   from source evidence. Record unknown evidence explicitly; do not fill it from
   assumptions or from the public field names.
3. Read the retained entries back from the authorized repository. Match stable
   identifiers, revisions, digests and source relationships against the intended
   records. Retain the readback receipt privately. A mismatch leaves the affected
   reference unresolved.
4. After authorized readback, replace the public null reference slots with the
   approved opaque identifiers and reference revisions in a subsequent change.
   Link each applicable branch and campaign-derived lesson to its evidence;
   update the pending-state tests to require that evidence. Do not copy private
   values, names, paths, source documents or raw private digests into this file.
5. Evaluate consumer completeness and intended-use qualification separately.
   Successful reference readback establishes reference integrity only; it does
   not establish solver qualification or that all required checks passed.

## Verification and limits

The offline documentation tests compare the independently transcribed producer
maps with parsed public source, including dictionary expansions, adapter-added
fields, fixed series keys and statistics leaves. Solver source is read as text;
no licensed solver is imported, started or queried. Additional tests compile
only the adapter extraction methods after removing their imports and injecting
symbolic dependencies. These exercise each dispatch branch with optional modes
and fatigue enabled and disabled, checking the resulting keys against the
inventory. They do not execute response calculations or solver interfaces.
These tests protect the documented interface from drift. They do not supply
private consumer evidence.

The null sidecar, consumer and lesson references in the JSON mean **not yet
established**. They do not mean that a consumer or a lesson is absent. Private
evidence and access authorization have not been supplied for this change. Until
the required readback is established, `external_consumer_completeness`,
`private_readback` and `campaign_qualification` remain `NOT_EVALUATED`.

Later slices cover nested W5 and modal field inventories, authorized sidecar
creation and readback, actual consumer coverage, and intended-use qualification.
No engineering acceptance result is asserted by this documentation.
