# Pending sweep catalog

The [catalog](../../registry/sweeps.yaml) records four proposed sweep families.
Every descriptor remains `pending`, including a fully populated proposal.
Structural validation establishes neither execution eligibility nor acceptance
by an external preflight. No cases or batch manifests are generated.

## Descriptor contract

`schema_version: 1` and a nonempty `families` list are required. Each family has
a unique `id`, `status: pending`, a workflow identifier, named axes with explicit
units, output roles, a consumer role and public prerequisite issue references.
Workflow identifiers describe intended use; registration is not inferred.

`vessel_id`, `base_input`, `grid` and `batch_size` may be null only when the
`unresolved` mapping supplies a nonempty reason for each null field. Resolved
fields cannot retain an unresolved reason. A supplied batch size is a positive
integer excluding booleans; it does not approve run duration or concurrency.

`variants.source` retains the existing `factorial`, `range`, `csv` and
`yaml_matrix` vocabulary. A supplied `grid` is a nonempty mapping reserved for
that source's existing variants payload. Source-specific grid semantics and
physical units are not validated here. Axis units describe a proposed basis;
they neither convert inputs nor establish a solver's unit convention.

`validate_catalog(mapping)` in `digitalmodel.workflows.sweep_catalog` accepts
an already parsed mapping and returns `pending_family_ids` in catalog order
and `prerequisite_counts` keyed by family ID. Malformed descriptors raise
`ValueError`; input mappings are preserved. The function performs no file,
network or execution operations. YAML parsing belongs to the caller.

## Next slices

Reviewed base inputs, rights and vessel eligibility will be established first.
Pure expansion helpers will then be extracted from the existing parametric
workflow without importing its dispatch path. The actual external manifest
schema will be obtained before a deterministic generator is implemented.
A separately authorized preflight will establish end-to-end acceptance.

The catalog prerequisites identify the batch workflow and consumer (#1554,
#1556), convergence and threading (#612, #1555), damping provenance (#1538),
and the coupled model (#1528). The full acceptance criteria of #1557 remain open.
