# Free-Span Rectification Design

Designs the **intermediate-support layout** to rectify an over-long subsea pipeline free span, building on the DNV-RP-F105 Level-1 allowable free-span length.

When an as-surveyed span `L_a` exceeds the allowable span `L_allow`, it is split into shorter sub-spans by intermediate supports (grout bags, mattresses, rock berms). The minimum number of equally spaced supports is:

```
n_supports = ceil(L_a / L_allow) − 1
```

giving `n_supports + 1` equal sub-spans of length `L_a / (n_supports + 1)`, each within the allowable limit. No rectification is needed when `L_a ≤ L_allow`.

The allowable span is computed by the same tested `SpanAllowableLength` calculator used by `digitalmodel:free-span-f105` (the governing of the F105 static-deflection and VIV-onset Level-1 limits). The workflow reports the allowable span, the required support count and spacing, the resulting sub-span utilisation, and a top-level `screening_status` — `fail` when the as-built span exceeds the allowable (rectification required), `pass` otherwise.

The example takes a 60 m surveyed span against a ~12.4 m allowable → 4 supports, 12.0 m sub-spans.

Run with `uv run python -m digitalmodel examples/workflows/span-rectification/input.yml`.

Reference: DNV-RP-F105 (free spanning pipelines), Sec. 4.2 (allowable span length).
