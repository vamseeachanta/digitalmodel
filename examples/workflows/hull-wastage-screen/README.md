# Representative-element CSV intake

This synthetic example supplies the same reviewed representative element as
`input.yml`. It is a normalized interchange example. Raw thickness-measurement
reports require a separately qualified sampling and aggregation method.

To use CSV intake, copy `input.yml` and `representative-elements.csv` into a
working directory. Replace the complete `elements` block in the copied YAML with:

```yaml
  elements_csv: representative-elements.csv
```

Keep `rule_family`, source-tagged `limits`, and a fresh `output_dir` explicit.
Invoke the existing engine with the copied input:

```sh
uv run python -m digitalmodel input.yml
```

Paths resolve relative to the configuration directory, then the configuration
file's parent, then the current directory when neither is supplied. Absolute
local file paths are also supported. URLs and network-share syntax are rejected.

Every row represents one element. Column order is unrestricted; names are exact:

| Rule family | Required columns |
| --- | --- |
| `csr` | `id,as_built_mm,measured_mm,uncertainty_mm,corrosion_mm,voluntary_mm` |
| `percentage` | `id,as_built_mm,measured_mm,uncertainty_mm,renewal_mm` |

All numeric columns use millimetres, a dot decimal separator and finite values.
As-built and renewal thicknesses must be positive; other numeric columns must be
nonnegative. Scientific notation is supported. IDs must be nonempty and unique.
Unknown or duplicate headers, missing values, malformed rows and family/header
mismatches fail before any analysis output is created. UTF-8 with an optional BOM
and LF or CRLF line endings is supported. No units or representative averages are
inferred, and no zones are assigned.

The JSON `ingestion` record retains schema version, units, record count and the
SHA-256 digest of the exact imported bytes, without a source path. Input bytes and
the caller's CSV configuration are preserved. Existing output files are retained;
a fresh output directory is required for each analysis.

The synthetic CSR example retains local `MONITOR` and overall `NOT_EVALUATED`.
Pitting qualification, stiffened-panel wastage and class survey acceptance remain
open. CSV intake adds no engineering criteria or default uncertainty.
