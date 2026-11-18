# Supplied-result riser fatigue view

`render_fatigue_view` renders one in-memory `riser_fatigue` result mapping; original inputs are passed separately because the workflow overwrites its settings.
Values remain observations: no solver, fatigue solution or certification runs.
Required life and DFF margin are cross-checked before rendering.
Units follow the workflow fields: damage and DFF margin are dimensionless; life is in years.

```python
from digitalmodel.reporting import Provenance
from digitalmodel.riser_reporting import canonical_digest, render_fatigue_view

inputs = {"design_life_years": 20.0, "dff": 2.0}  # synthetic example only
row = dict(segment_id="segment-a", wave_damage=0.15, viv_damage=0.05,
           total_damage=0.2, fatigue_life_years=100.0, required_life_years=40.0,
           dff_margin=2.5, passes_dff=True)
results = dict(governing_segment="segment-a", governing_dff_margin=2.5,
               screening_status="pass", segments=[row])
metadata = dict(method="Synthetic supplied-result method", assumptions=None,
                references=[dict(identifier="Synthetic reference", title="Synthetic fatigue method", edition="test",
                                 locator="test clause", qualification="unverified")],
                workflow_version="synthetic-v1")
provenance = Provenance(code_edition="synthetic-v1", validated_path="screening")
for kind, identifier, payload in (("spec", "input-record", inputs),
                                 ("workflow_results", "result-record", results),
                                 ("metadata", "method-record", metadata)):
    provenance.add(kind, identifier, digest=canonical_digest(payload))
html = render_fatigue_view(results, inputs, provenance=provenance, **metadata)
```

Digests use `sha256:` followed by SHA-256 of UTF-8 JSON with sorted keys, compact separators, `ensure_ascii=False` and `allow_nan=False`. Bind the complete input and result objects and the four-key metadata object shown above. Exactly one
source per required kind and unique nonempty identifiers are required; no identifier is dereferenced. `validated_path` accepts `screening` or `unvalidated`.
An optional `output_path` writes UTF-8 HTML and returns its path; otherwise HTML is returned.
Assumptions appear in the body; `None` explicitly means the record was not supplied.
Inputs must include finite positive `design_life_years` and `dff` values with a finite positive product.
Every segment's `required_life_years` must match that product, and `dff_margin` must match
`fatigue_life_years / required_life_years`, using relative tolerance `1e-9` and zero absolute tolerance.
Tolerance never overrides verdicts: `passes_dff` must agree with both life at or above required life
and margin at or above one. Mismatches raise before any output is written.
Numeric fields accept real numbers, including JSON-compatible NumPy scalars such as `numpy.float64`,
and reject booleans; canonical digests still require JSON-compatible content.
Each reference requires nonempty `identifier`, `title`, `edition`, `locator` (clause title),
and `qualification` fields.
Reference qualification (`verified` or `unverified`) is caller-supplied, not independently checked. This table-only slice omits revision history, strength/utilisation and code-check sections from the standalone catenary report API, which remains unchanged. Historical-outline
comparison, coordinate plots, sensitivities, atlas adapters and automatic workflow output remain outstanding under #814. Engineering adequacy is not established by rendering.
Damage sums and governing-location ranking are not recalculated.

Focused verification includes the routing contract:

```sh
uv run --no-project --with 'pytest>=7.4.3,<8' --with pytest-asyncio==0.21.1 \
  --with pydantic --with jinja2 --with PyYAML --with 'numpy<2' --with plotly \
  python -m pytest tests/reporting/test_riser_fatigue_view.py \
  tests/reporting/test_reporting_library.py tests/reporting/test_provenance_contract.py \
  tests/docs/test_digitalmodel_routing_contract.py -q -rA -p no:cacheprovider
```
