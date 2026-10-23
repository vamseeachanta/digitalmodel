# Generator standards citation metadata

## Summary

The modular generator names standards without selecting governing editions or
qualifying clause applicability. The [structured inventory](generator-standards.json)
covers the four locations identified in issue #2516 and the associated generated
inputs. Every engineering attribution remains `NOT_EVALUATED`. Public catalogue
metadata resolves some publication identities; it does not qualify the existing
coefficients, default inputs or generated models. Numerical behaviour is unchanged.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-19 | Inventory the standards-labelled inputs and evidence gaps so that publication metadata cannot be mistaken for engineering qualification. |
| 2 | 2026-10-19 | Record the separate uncited strength fallback, confirm the catalogue title wording and extend the inventory checks for portable paths and citation syntax. |

## Publication identities and edition boundaries

| Code | Title and evidence | Edition evidence | Governing locator and limitation |
| --- | --- | --- | --- |
| DNV-ST-F101 | Submarine pipeline systems; [issuer product page](https://www.dnv.com/energy/standards-guidelines/dnv-st-f101-submarine-pipeline-systems/), checked 2026-10-19 | Product page: 2021-08, amended 2021-12 | Governing edition, clause number and clause title are unresolved. The API material-strength basis does not establish a DNV design strength. |
| DNV-OS-E302 | Full title unresolved; the attempted issuer-page addresses returned HTTP 404 | No verified edition | The chain relation, grade coefficients, applicability bounds and any successor designation remain unresolved. No clause is inferred from the number. |
| API RP 2SK | Design and Analysis of Stationkeeping Systems for Floating Offshore Structures; [API 2025 publications catalogue](https://www.api.org/-/media/Files/Publications/2025-Catalog/02_Exploration-and-Production_2025.pdf), printed page 6, RP 2SK entry, checked 2026-10-19 | Catalogue: 4th edition, February 2024 | The schema shorthand `API 2SK` selects neither an edition nor a grade clause. Catalogue metadata does not establish grade equivalence between editions or with DNV-OS-E302. |
| API Spec 5L | Full publication title unverified in this assessment; repository strength metadata says API 5L | Repository: 46th edition; 9.3.1/Table 6 and 9.3.2/Table 7, SI | These are repository locators, with clause titles unverified. The attempted issuer announcement returned HTTP 200 with a page headed “404 Page Not Found”. Product level and governing edition are not selected by the pipeline schema. |

Table 1. Publication metadata and limitations. No licensed clause text or tabulated
engineering value is reproduced. An unsuccessful lookup supports only an access
limitation; the inventory records its address and response status.

The API product-level references originate in
[`materials/grades.py`](../../../src/digitalmodel/materials/grades.py), `_api5l`
and the strength-source strings. That module distinguishes product levels and
canonical SI strengths from legacy snapshots. Its `smys_kn_m2` unit view is
consumed by the builder. Neither conversion nor matching a grade label establishes
product certification, DNV applicability or equivalence between standard editions.
A qualified comparison across editions requires the selected editions and their
applicable clauses; none is asserted here.

## Use-site inventory

The JSON file is an assessment artifact, not a runtime `Citation` sidecar. Its
`status` fields describe evidence qualification; they do not change execution.

| Inventory entry | Consumer and quantities | Basis and unresolved limitation |
| --- | --- | --- |
| `pipe-material` | [`linetype_builder.py`](../../../src/digitalmodel/solvers/orcaflex/modular_generator/builders/linetype_builder.py), `MATERIAL_PROPERTIES`: density, modulus, Poisson ratio and yield input | Yield follows the canonical materials unit view. Other entries are modelling defaults without a qualified standard clause. Unknown material strings fall back to the default grade. Separately, `LineTypeBuilder.build` uses `material_props.get("DNVSTF101Fy", 450e3)` when the yield key is absent: an uncited strength fallback in kN/m2 with no qualified standard basis. |
| `pipe-code-inputs` | `LineTypeBuilder._build_line_type_entry`: every emitted `DNVSTF101*` key | Includes the simplified strain limit, material/safety factors, strength inputs and zero/null entries. No governing clause or solver sentinel interpretation is established. Generating these fields is not an evaluated code check. |
| `chain-defaults` | [`mooring_router.py`](../../../src/digitalmodel/solvers/orcaflex/modular_generator/routers/mooring_router.py), `CHAIN_DATABASE`: breaking-load coefficient, mass, stiffness, drag and added mass | The code attributes only the breaking-load relation to a standard. The remaining defaults and nearby curated-library examples are not independent standard evidence. |
| `chain-breaking-load` | `MooringRouter._chain_linetype`: diameter and grade coefficient to computed breaking load, then `AllowableTension`; explicit breaking-load override takes precedence | The existing relation can be denoted symbolically as `MBL = c_grade d² (a − b d)`. The edition and clause for all coefficients and diameter bounds are unresolved. No numerical coefficient is qualified or reproduced here. Assignment to `AllowableTension` applies no cited design factor. |
| `chain-grade-label` | [`schema/mooring.py`](../../../src/digitalmodel/solvers/orcaflex/modular_generator/schema/mooring.py), `ChainGrade` enum | Grade labels do not demonstrate certification or that both named publications contain every enum member. |
| `pipeline-grade-label` | [`schema/pipeline.py`](../../../src/digitalmodel/solvers/orcaflex/modular_generator/schema/pipeline.py), `Pipeline.material` | Free text; no edition, product level or API product qualification is validated. |

Table 2. Standards-labelled uses and adjacent defaults. The symbolic relation is
transcribed from the router solely to identify the unresolved method. It is not a
sourced design equation. No new engineering equation, coefficient or acceptance
limit is introduced.

## Citation sidecars at consumer boundaries

The assessment reads the following consumers without starting or importing a
licensed solver:

- `MooringRouter.route` returns generic line-type dictionaries. It carries no
  per-quantity citation metadata into `GenericModel`.
- `ModularModelGenerator._generate` serializes builder output as include YAML,
  parameters and a master file. It emits no `Citation` evidence artifact.
- `ModularModelGenerator._generate_with_overrides` also accepts replacement
  templates. `GenerationResult` records files, resolved variables and warnings,
  without binding derived quantities to citations.
- `writers/basefile.py::write_variation_model` writes variation YAML referring to
  a base model; the base model's standards provenance is not established there.

The repository's [`Citation`](../../../src/digitalmodel/citations/schema.py)
requires `code_id`, `publisher`, `revision`, `section` and `wiki_path`.
`validate_citation` checks live wiki existence and agreement of the code,
publisher and revision, and explicitly does not validate the section locator.
A constructed object or a matching wiki header would therefore not resolve the
missing clause title or establish engineering applicability.

Before a standards-qualified output is claimed, the derived quantities need
selected editions, applicable clause titles, authorized evidence, and citations
preserved through routing and serialization into a separate evidence artifact.
Custom-template and base-model provenance require the same assessment. Unresolved
quantities need an explicit unevaluated result at that boundary. These requirements
are recorded in the inventory; runtime enforcement and sidecar emission are not
implemented by this documentation change. No fabricated wiki path or placeholder
`Citation` is constructed to imply successful resolution.

## Validation and remaining work

`tests/docs/test_generator_standards_inventory.py` checks the four issue locations,
resolves inventory symbols through the Python AST, and compares the recorded DNV
fields and material/chain property keys with the current source. It also checks
catalogue metadata separately from unresolved governing locators and records the
missing sidecars at each assessed boundary. These are evidence-coverage tests,
not numerical engineering validation or a licensed-text comparison.

Later work requires authorized source qualification of the unresolved titles,
editions, clause titles and coefficients, followed by an explicit design-input
contract and tested sidecar propagation. In particular, the simplified strain
limit, zero/null DNV inputs and direct breaking-load assignment need qualification
before generated inputs can support an engineering acceptance claim. The
maintainer must select the applicable edition and intended use; catalogue currency
alone cannot make that selection.
