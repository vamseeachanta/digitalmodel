**Verdict: CHANGES-REQUIRED**

One defect blocks approval. It is a regression in dataclass compatibility, which is one of the findings the disposition records as fixed. This review did not execute anything. Every finding below is traced from the packet bytes against Python 3.11 `dataclasses`/`copy` semantics, so each repro should be run before the fix is accepted.

## Findings

### F1 (MAJOR): `dataclasses.asdict()` / `astuple()` on `HydrostaticsResult` now raise `TypeError`

- **Where:** `src/digitalmodel/naval_architecture/mesh_hydrostatics.py:642-644` (the fields are stored as `MappingProxyType`).
- **Mechanism:** `dataclasses._asdict_inner` recurses only into dataclasses, lists/tuples, namedtuples and `dict` instances. Anything else goes through `copy.deepcopy`. A `MappingProxyType` is not a `dict`, so it reaches `copy.deepcopy`, then `object.__reduce_ex__(4)`, and fails with `TypeError: cannot pickle 'mappingproxy' object`. Before this patch, `quantities` and `conventions` were plain dicts, so `asdict(result)` worked and recursed into each `Quantity`.
- **Repro:**
  ```python
  from dataclasses import asdict
  r = compute_hydrostatics(TriMesh(*box_mesh(20,4,3), units="m", axes=("forward","port","up")), 1.5)
  asdict(r)   # TypeError after this patch; dict before
  ```
- **Test gap:** `test_array_integrity.py:173-186` checks `asdict`/`replace`/`repr` only on `Quantity`. The "dataclass compatibility" closure in `code-review-disposition.md:10` therefore covers one of the two changed dataclasses. `replace(result, ...)` still works, because `__post_init__` copies the proxy with `dict(...)`.
- **Fix options:**
  - **Recommended:** store an immutable `dict` subclass that blocks the mutating methods (`__setitem__`, `__delitem__`, `update`, `pop`, `popitem`, `clear`, `setdefault`, `__ior__`). It needs its own `__reduce__`, because pickle rebuilds dict subclasses through `SETITEMS`, which would hit the blocked `__setitem__`. `asdict` then rebuilds it with `type(obj)(pairs)`.
  - **Alternative:** keep the proxy, and both document and test that `asdict` is unsupported on `HydrostaticsResult`.
- Either way, add `asdict(result)` and `astuple(result)` to the regression test. Also check callers of `HydrostaticsResult` (W1 and any serialisers) for `asdict` use; the packet cannot show that.

### F2 (MINOR, at the scope boundary): `vars()` exposes the stored arrays of `ClippedHull` and `Quantity`, so their shape and dtype can be changed in place

- **Where:** `mesh_hydrostatics.py:469` and `:617`.
- **Mechanism:** `vars(clipped)["vertices"]` (or `clipped.__dict__[...]`) returns the stored snapshot itself, not a fresh view. The `__dict__` lookup goes through `__getattribute__` and returns a dict, not an ndarray. Assigning `.shape = (-1,)` to that snapshot is allowed, because the write flag does not guard metadata. After that, every later `clipped.vertices` has the changed shape. `Quantity` has the same route through `vars(q)["value"]`.
- **Scope:** this reads a field by its public dataclass name through standard introspection; it does not replace a private attribute. The exclusion at `code-review-disposition.md:15` does not clearly cover it. `TriMesh` does not have this problem in the same form, because its storage is the private `_vertices`, which is excluded.
- **Disposition:** not blocking by itself. Either name `vars()`/`__dict__` explicitly in the out-of-scope list, or store a non-ndarray record (bytes, dtype, shape) and rebuild the array on access.

### F3 (INFO): the `HydrostaticsResult` constructor does not check what it is given

- **Where:** `mesh_hydrostatics.py:642-644`.
- **Mechanism:** `__post_init__` accepts values that are not `Quantity` objects, and quantities whose `input_hash` differs from the result's `input_hash`.
- **Effect:** `replace(result, quantities={**result, "V": Quantity(240.0, "m^3", "computed", result.input_hash)})` produces a new result that keeps the original hash. This is construction of a new object, not aliasing of the old one, so it is not an integrity defect under the stated contract.
- **Note:** the guarantee pinned at `test_array_integrity.py:79` (every quantity carries the result's hash) holds only for objects built by `compute_hydrostatics`. Validating the types and hashes in `__post_init__` would make it hold for every construction path.

## Checked and found sound (within the stated scope)

- **Immutability:** `_read_only` is backed by `frombuffer(bytes)`, so `setflags(write=True)` fails at every level of the base chain. Non-contiguous and empty `(0, 3)` inputs produce C-order snapshots; object dtypes and ndarray subclasses (masked arrays included) are refused.
- **Fresh views:** the views returned by `TriMesh`, `ClippedHull` and `Quantity` keep shape and dtype changes on the view away from the stored array. `bounds` re-snapshots on each call.
- **Copying:**
  - `TriMesh.__setstate__` re-snapshots after `deepcopy`, `pickle` and `copy.copy`.
  - `ClippedHull.__reduce__` and `Quantity.__reduce__` rebuild through `__init__`, so `__post_init__` snapshots again. `object.__reduce_ex__` dispatches to the overridden `__reduce__`, so `deepcopy` is covered as well as `pickle`.
  - `HydrostaticsResult.__reduce__` avoids pickling a mappingproxy.
- **Defensive copies:**
  - Non-array `Quantity` values are deep-copied on the way in and on every read. The `half_breadth` list therefore cannot be changed through `.value` or `to_dict()`.
  - `clip_at_waterline` and `_section` copy before mutating, using `np.array(..., copy=True)` and fancy indexing.
  - The fixture factories return independent arrays.
- **Digest:** the snapshot does not change the bytes being hashed. The pinned digest `b1068466…248d` is consistent with that.
- **Evidence count:** 12 test cases (11 functions, one of them parametrised twice), which matches the "twelve passes" in the disposition.

`★ Insight ─────────────────────────────────────`
- `dataclasses.asdict` is not a generic serialiser. It special-cases exact container families (`dict`, list/tuple, namedtuple) and deep-copies everything else. Swapping a field to a read-only mapping type therefore silently moves it onto the pickle/deepcopy path. A frozen mapping that inherits from `dict` stays on the dict path.
- On NumPy arrays, the write flag guards the data buffer, not the array's metadata. `.shape` and `.dtype` can still be reassigned on a read-only array. That is why fresh views on every access are the right defence, and why any route that reaches the stored object itself (here `vars()`) bypasses it.
- Overriding `__reduce__` on a dataclass makes both `pickle` and `deepcopy` rebuild through `__init__`/`__post_init__`, so the snapshot logic runs again. That is simpler than `__setstate__` because it reuses the constructor's checks.
`─────────────────────────────────────────────────`

**To get to APPROVE:** fix F1 and add `asdict`/`astuple` regressions for `HydrostaticsResult`. Then either fix or explicitly scope out F2, rebuild the packet, and re-review the changed lines plus the `HydrostaticsResult` callers.
