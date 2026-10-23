# NumPy provenance-container audit

## Summary

The bounded audit for issue #2313 identifies retained numerical data beside
source identities and derived quantities. `AccelerogramRecord` now snapshots
its sample bytes, isolates public array headers, and reconstructs snapshots
after copy or pickle. Other candidates below were inspected statically; their
mutation paths are NOT_EVALUATED by this change. This is not a package-wide
immutability qualification.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| Initial | 2026-10-19 | Inventory provenance-bearing numerical containers and protect accelerogram samples from mutation beside a retained source digest. |

## Boundary and method

Discovery searched `src/digitalmodel` Python source for provenance identities
(`source_digest`, `source_sha256`, `input_hash`, `data_hash`, `source_hash`,
`sha256`) and array/reconstruction paths (`ndarray`, `frombuffer`, `setflags`,
`__reduce__`, `__setstate__`). Candidate definitions and consumers were read
without importing solver integrations. This lexical search can miss dynamic
storage, differently named identities and nested arrays in arbitrary metadata.

The acceptance boundary is ordinary constructor and dataclass APIs, public
array reads and base chains, shallow/deep copy, current pickle protocols, and
legacy default dictionary-state reconstruction. Deliberate access to a record's
raw `__dict__` or `object.__setattr__` is outside this boundary. Explicit array
copies and derived outputs may be writable because they do not retain aliases
to the stored samples. Object-reference arrays are refused rather than encoded
as raw pointer bytes.

## Inventory

Paths in this table are relative to `src/digitalmodel/`.

| Container and location | Retained data and identity | Public and reconstruction paths | Disposition |
|---|---|---|---|
| `AccelerogramRecord`, `seismic/readers/ascii.py` | Time and acceleration arrays; file-byte `source_digest`; sample count, time step and peak acceleration depend on the arrays. | Dataclass fields, caller arrays, `acceleration_g`, copy/deepcopy, pickle and default dictionary state. | Implemented and regression tested in the three slices below. |
| `KinematicResult`, `seismic/response_spectrum/calculations.py` | Record, DataFrame, summary and processing-chain dictionaries; the nested record carries the source digest. | Public mutable fields; default copy/pickle; summary is computed separately from the DataFrame. | NOT_EVALUATED: audit DataFrame/summary consistency separately. Protecting the nested record does not freeze this result. |
| `TriMesh`, `naval_architecture/mesh_validation.py` | Canonical vertices/faces, cached source digest and scale. | Existing immutable byte backing, public ndarray views and `__setstate__`. | Existing protection inspected; additional mutation qualification NOT_EVALUATED here. |
| `ClippedMesh`, `naval_architecture/mesh_clipping.py` | Vertices, faces, cap tags, waterline and plane arrays; derived face areas. | Existing snapshot initialization, defensive public views and constructor reduction. | Existing protection inspected; additional mutation qualification NOT_EVALUATED here. |
| `Quantity` and `HydrostaticsResult`, `naval_architecture/mesh_results.py` | Array-valued quantities beside `input_hash`; quantity and conventions mappings. | Existing snapshot/deepcopy reads and constructor reduction. | Existing protection inspected; additional mutation qualification NOT_EVALUATED here. |
| `DataProvenance` and `TransformationRecord`, `infrastructure/core/provenance.py` and `infrastructure/persistence/provenance.py` | Source hashes and arbitrary metadata/parameters that can contain arrays; no typed retained sample field. | Mutable dictionaries, serialization helpers, default copy/pickle. `compute_hash` hashes ndarray bytes without shape/dtype. | NOT_EVALUATED: define metadata ownership and hash-schema compatibility before changing identity. |
| `ValidationResult` / `ValidationIssue`, `infrastructure/validation/pipeline.py` | Cached data hash, arbitrary issue values and mutable summaries; values can contain arrays. | Pydantic fields and validation-cache references. | NOT_EVALUATED: determine retained aliases and cache ownership with dedicated tests. |
| `RaoRegistry`, `hydrodynamics/hull_library/rao_registry.py` | Persistent JSON lists, reference metadata and content digest in filenames. | Registration serializes incoming arrays with `tolist`; loading verifies content then returns a fresh parsed payload. | No typed ndarray field retained by this registry. Persistent identity and mutable parsed payloads need a separate contract; copy/pickle NOT_EVALUATED. |
| `TableSet`, `subsea/on_bottom_stability/e305_tables.py` | Tuple axes and table values beside `source_sha256` and citation metadata. | Frozen dataclasses and tuple values; no retained ndarray. | NumPy backing/header defect is inapplicable to the numerical tuples. Citation metadata ownership and copy/pickle NOT_EVALUATED. |

## Implemented slices and independent oracles

1. Snapshot constructor inputs into immutable byte backing. Caller aliases,
   noncontiguous views, direct writes and write-flag restoration through every
   public ndarray base cannot alter retained samples.
2. Return fresh ndarray headers over those bytes. Public shape, dtype and stride
   edits, including edits through base chains, do not change future reads or
   derived quantities. Array reads share immutable bytes without copying them.
3. Reconstruct through the constructor for copy/deepcopy and pickle; normalize
   legacy dictionary state on load. Preserve dataclass `replace` and `asdict`
   behavior and independent writable derived arrays.

`tests/seismic/test_record_integrity.py` writes a synthetic SI record. Its
docstring gives the independent hand calculation: four samples, adjacent times
differing by 0.5 s, and the largest acceleration magnitude equal to 2 m/s².
Array values, dtype, sample count, time step, peak acceleration, units and the
digest of the literal file bytes are checked after mutation attempts and
reconstruction. Exact equality is used; no physical acceptance tolerance or
engineering coefficient is introduced. Existing unit-conversion constants and
reader validation limits are unchanged.

The digest still identifies source file bytes. Direct construction or
`dataclasses.replace` can intentionally pair new samples with an existing
digest; neither operation authenticates those samples. The fix prevents later
mutation of the accepted snapshot and does not certify input provenance.

## Sources

- [NumPy ndarray.flags reference, Notes](https://numpy.org/doc/stable/reference/generated/numpy.ndarray.flags.html),
  accessed 2026-10-19: WRITEABLE restoration depends on ultimate buffer ownership.
- [NumPy ndarray reference, buffer parameter and Notes](https://numpy.org/doc/stable/reference/generated/numpy.ndarray.html),
  accessed 2026-10-19: new array headers can use existing buffer storage.
- [Python pickle reference, pickling class instances and reduction protocol](https://docs.python.org/3/library/pickle.html#pickling-class-instances),
  accessed 2026-10-19: default restoration bypasses initialization; reduction and
  state hooks control reconstruction, and copy uses the same protocol.
- Repository implementation context: `naval_architecture/mesh_geometry.py`,
  `_read_only`, and the snapshot/reconstruction methods inventoried above.

## Later slices

Audit the mutable kinematic result and validation cache with independent alias
and reconstruction regressions. Establish a versioned shape/dtype-aware identity
contract before changing generic ndarray hashing. Requalify existing mesh
containers and nested metadata separately; their static inventory entries do
not claim complete mutation coverage.
