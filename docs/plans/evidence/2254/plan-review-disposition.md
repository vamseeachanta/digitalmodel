# Plan review disposition (2026-10-09)

Frozen packet review by Claude returned CHANGES-REQUIRED. Packet verification passed before disposition. Applicable findings are resolved through these implementation criteria:

- The synthetic box is 20 × 4 × 3 m at 1.5 m draft. Beam writes and write-flag restoration shall raise ValueError; V shall remain 120 m³, B_wl 4 m, and full result serialization and hashes shall remain unchanged.
- All seven ClippedHull array fields, including artificial face masks, will be isolated and immutable; the container will be frozen. Mesh public geometry, digest and scale metadata will be read-only properties. Private attribute replacement and hostile native-pointer writes are outside the public-API contract.
- Quantity has no arithmetic, unit-conversion or magnitude API. The review's derived-array examples do not apply. Quantity array inputs will be snapshotted; list inputs/reads will be copied defensively, preserving the existing half-breadth nested-list shape. Repository consumers use np.asarray for that grid.
- Mesh hash encoding will remain identical, with canonical float64 vertices and int64 faces. A pre-fix digest comparator will be tested. Hashing will occur once over immutable snapshots. Changing the hash schema is outside this fix.
- Fixed-size numeric arrays will be supported; object arrays will be refused rather than serializing pointer bytes. Non-contiguous inputs will be serialized in C order. Bytes snapshots add construction-time allocation; million-face memory qualification remains open.
- Parent issue https://github.com/vamseeachanta/digitalmodel/issues/2239 supplies tracking. The label decision:architecture is explicitly required by the owner. The R02 public setflags reproduction is quoted in the regression test's beam mutation path.
- Fixture factories will keep defensive, mutable authoring outputs with no shared state; a two-call isolation regression will be added.
- T2 review will combine Claude packet review and Codex local defect analysis. Tests will run with the isolated uv environment against tests/naval_architecture, with the three named closed-form tests also selected explicitly. Red results will precede implementation.

Existing file-size limits, post-clip area policy, scale evidence and unresolved citation registry dependencies remain follow-on work. No physical machine identifiers or measured/client data will be introduced.

## Inline final plan review

The second Claude packet was invalidated by adding tests while review ran; its verdict is advisory only. Its applicable findings are resolved in the final tests: pinned digest, metadata rebinding, object refusal and independent factories were added before implementation (five failing tests, one passing factory test). Non-contiguous and empty numeric snapshots are also covered. R02 reproduction: `mesh.vertices.setflags(write=True); mesh.vertices[:, 1] *= 2`, from https://github.com/vamseeachanta/digitalmodel/pull/2254#issuecomment-6083028919.

Codex inline defect analysis accepts the revised plan against the actual APIs: ClippedHull has precisely seven array fields; Quantity has no derived arithmetic API; the box is centred at x=0; dry/fully submerged clips are refused. Public buffer and attribute mutation are the governing boundary. The empty-array test will check shape and write-flag rejection without indexing element zero.

Closed-form node IDs will be tests/naval_architecture/test_mesh_hydrostatics.py::test_box_hydrostatics_exact, tests/naval_architecture/test_mesh_hydrostatics.py::test_wigley_independent_tessellations, and tests/naval_architecture/test_friction_scaling.py::test_ittc57_reference_values.
