# Array integrity review evidence

Tracking: https://github.com/vamseeachanta/digitalmodel/issues/2239 and https://github.com/vamseeachanta/digitalmodel/pull/2254#issuecomment-6083028919. Synthetic analytic geometry only; no client/measured data or licensed sources are introduced.

The first Claude code packet was invalidated by adding a metadata regression while the review ran. Its CHANGES-REQUIRED findings are advisory and are independently reproduced before correction:

- Public shape mutation changes retained geometry metadata: one failing regression before fresh-view access is added.
- Deepcopy/pickle restore writable owning buffers: two failing round-trip regressions before snapshot reconstruction hooks are added.
- Result dictionaries permit replacement with stale identity: failing regression before copied mapping proxies are added (later replaced by defensive native dictionaries; see r3-inline-review.md).
- Quantity dataclass field compatibility is retained with the public value field, generated repr/asdict/replace and defensive access. Masked/subclass arrays are refused explicitly rather than discarding masks.
- Review-delta red run: five failures and seven passes. Corrected integrity tests: twelve passes with NumPy 1.26.4, Python 3.11.14, pytest 9.1.1. Non-contiguous snapshots and empty (0,3) arrays are included.

Initial merged-code red run: five failures and one passing factory isolation test. Existing geometry accepts write-flag restoration, clipped constructor aliases, Quantity aliases/list aliases, and mesh digest metadata rebinding. The recorded base digest for the 20 × 4 × 3 m box is b1068466fc4d686a7643fc10a40d153bc134de22856cdf7752eb9c7b7386248d, measured on origin/main abdf5ef979bcbe25347bc27d06a10e50c38b9779 before source edits. The digest comparator remains pinned in the regression.

The public contract covers ordinary NumPy/Python access, including metadata mutation, copying and pickle. Direct replacement of private attributes, object.__setattr__ bypass and native-pointer attacks are outside it. Quantity.value returns a fresh ndarray view or defensive copy of a list. ClippedHull returns fresh views of its snapshotted numeric fields; factory outputs remain independent mutable geometry-authoring data. Array copy/serialization incurs construction memory cost; million-face peak-memory qualification remains open.

W1 exposes scalar records, with no retained public NumPy arrays. Its numerical/citation implementation is unchanged. R02 finding 5 (citation registry/wiki and unconfigured resolver disposition), 7 (post-clip physical-face area checks), 9 (station caches and million-face memory evidence), and 10 (file/function limits) remain open under the parent issue. Existing source file/function violations are not claimed closed by this patch.

Codex local adversarial review identifies no remaining public array-alias route after the delta tests. Cross-provider packet verification passed before the r2 verdict was used. The final delta is reviewed in r3-inline-review.md. Earlier packets are retained as advisory scratch evidence only.
