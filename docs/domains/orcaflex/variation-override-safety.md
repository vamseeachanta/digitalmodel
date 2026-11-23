# Property-only variation overrides

The shared BaseFile writer emits named-object modifications instead of object
creation lists. Artifact validation rejects creation syntax rather than repairing
it. A separate audit compares supplied resolved Standard snapshots, checking the
selected values and every unselected item. Native preservation remains
`NOT_EVALUATED` until independent native exports are supplied and compared.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-22 | Extend nested syntax guards and add artifact and snapshot audits to detect unintended object recreation and detached connections. |
| Correction | 2026-10-25 | Register Wings alongside WaveTrains and Draughts so mapping input cannot bypass structural-operation and name validation. |

## Basis and method

[Orcina, *Variation models*](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Variationmodels.htm),
online help accessed 2026-10-22, distinguishes named-object edits from `New`,
`Deleted` and `Order` operations in **Text data format**. Its opening example
addresses `Environment.WaveTrains` by name; its vessel-type example addresses
`Draughts` by name. **Connections** describes the coordinate and tracking effects
of changing a connection. Issue #2137 records the object-recreation defect.

There are no engineering equations, coefficients or acceptance limits in these
utilities. The acceptance rule is exact structural equality of all unselected
data, plus exact equality of selected data to caller-supplied expected values.
The audit uses type-sensitive equality and preserves list order. It supplies no
floating-point tolerance or physical acceptance criterion.

- `build_variation_document` and `write_variation_model` convert registered
  named collections and nested Name-bearing lists into modification mappings.
  Known nested `WaveTrains`, `Draughts` and `Wings` collections also handle empty
  lists. Their mapping forms reject structural operations, renaming, invalid
  property mappings and duplicate names without changing inputs. Ordinary
  property tables remain lists. Ambiguous mixed named lists fail.
- `validate_variation_document` checks an already parsed artifact without
  normalizing it. `load_variation_document` additionally rejects duplicate YAML
  keys, aliases, and malformed YAML before syntax validation. Native blank
  scalars remain empty text; explicit null remains `None`.
- `TemplateGenerator.generate(..., as_reference=True)` embeds guarded edits
  after `BaseFile` instead of including the raw variation file. The base path
  remains relative to the output. Neither source may alias the destination.
  Later changes to the variation input require regeneration; later changes to
  the base remain inherited. Merged-model generation retains its behavior.
- `validate_resolved_variation` compares independent Standard snapshots supplied
  by the caller. Expected-change keys are tuples of mapping keys and Python list
  indices. Select individual existing scalar or table items, never object names,
  property mappings, or entire object collections. Overlapping selections fail.
  Selections are exact data paths, not OrcaFlex indexed-data expressions.

## Worked structural example

Set `EXAMPLE_OUTPUT` to an empty scratch directory and run this Python example
with the package on the import path. Input strings are synthetic sentinels, not
physical data. This example is intentionally not a loadable native model.

```python
import os
from copy import deepcopy
from pathlib import Path

import yaml

from digitalmodel.solvers.orcaflex.modular_generator.writers import (
    load_variation_document,
    validate_resolved_variation,
    write_variation_model,
)

root = Path(os.environ["EXAMPLE_OUTPUT"])
root.mkdir(parents=True, exist_ok=True)
master = root / "master.yml"
reference = {
    "6DBuoys": [{"Name": "Body", "SelectedField": "old"}],
    "Lines": [{"Name": "Sling", "EndAConnection": "Body"}],
    "Shapes": [{"Name": "Attachment", "Connection": "Body"}],
}
master.write_text(yaml.safe_dump(reference), encoding="utf-8", newline="\n")
original_bytes = master.read_bytes()
variation = root / "changes" / "case.yml"
write_variation_model(
    master, {"6DBuoys": [{"Name": "Body", "SelectedField": "edit"}]}, variation
)
assert load_variation_document(variation) == {
    "BaseFile": "../master.yml",
    "6DBuoys": {"Body": {"SelectedField": "edit"}},
}
assert master.read_bytes() == original_bytes

# Hand-built oracle only: this is not native resolution.
resolved = deepcopy(reference)
resolved["6DBuoys"][0]["SelectedField"] = "edit"
selected = {("6DBuoys", 0, "SelectedField"): "edit"}
validate_resolved_variation(reference, resolved, selected)
detached = deepcopy(resolved)
detached["Lines"][0]["EndAConnection"] = "Fixed"
try:
    validate_resolved_variation(reference, detached, selected)
except ValueError:
    print("Detached connection: rejected")
else:
    raise AssertionError("Detached connection was accepted")
print("Syntax: PASS; snapshot audit: PASS; native preservation: NOT_EVALUATED")
```

The answer follows by hand: remove `Name` from the buoy edit and use `Body` as
the mapping key. Only `SelectedField` changes from `old` to `edit`. The sling
endpoint and attached shape still refer to `Body`; changing either connection
to `Fixed` fails the audit. The source master remains byte-identical. The test
suite executes this example in a temporary directory and asserts its output.

## Native qualification and limits

On an authorized solver installation, load the source master and variation
independently, convert the variation to Standard in memory, and export both
Standard models without saving over either input. Supply those parsed snapshots
and the expected selected values to the audit. Include both an attached line
and an attached object, and compare every exported field. Retain the source
master and its byte digest unchanged. This procedure does not run simulations.

No native model was loaded or resolved for this change. Synthetic tests establish
audit behavior only; they are not native regression evidence. The audit cannot
authenticate snapshot provenance, export completeness, solver version, or the
absence of omitted default data. A residual numerical representation change
fails exact equality and requires investigation rather than an invented tolerance.

Object existence is not checked by the writer. Unnamed nested structures,
unregistered nested mappings and unregistered empty collections require separate
qualification. This is not a repository-wide audit: specialized workflow writers
and hand-authored IncludeFile compositions retain their existing behavior. Physical validity,
statics, dynamics and acceptance remain outside these structural checks.
