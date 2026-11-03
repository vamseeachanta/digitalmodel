# Simulation adapter catalog: CalculiX ASCII

This catalog slice documents the existing CalculiX ASCII result parser for
issue #1013. It covers parsing and reuse of existing results; native solving
is outside this slice. OrcaFlex, AQWA, ANSYS and mesh adapters remain unreviewed.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-26 | Catalog the existing ASCII parser with a synthetic runnable example to establish its reuse boundary. |

## Entry and public API

The [related OCR plan, integration sketch](../plans/2026-06-26-issue-1038-ocr-unlimited-ocr-evaluation.md#5-integration-sketch-conceptual--no-implementation)
references #1013's format/read/convert/license/headless catalog convention.
No simulation matrix existed in the inspected base; this entry makes no
coverage claim for the other adapters.

| Format | Read | Convert | License | Headless | Existing implementation |
| --- | --- | --- | --- | --- | --- |
| CalculiX ASCII `.frd` result fields | Yes, subset below | In-memory dictionaries and scalar summaries; no file converter | [Repository MIT license](../../LICENSE) | Yes; no solver executable needed for parsing | [`CalculiXResultParser`](../../src/digitalmodel/solvers/calculix/result_parser.py) |

Import `CalculiXResultParser` from
`digitalmodel.solvers.calculix.result_parser`, construct it with
`(job_name, work_dir)`, then call `parse_frd_file()`. It reads
`work_dir / (job_name + ".frd")`. Its returned dictionaries use integer node
identifiers as keys:

| Recognized field | Returned key | Values at each node |
| --- | --- | --- |
| `DISP` | `displacements` | `dx`, `dy`, `dz`, computed Euclidean `magnitude` |
| `STRESS` | `stresses` | `sxx`, `syy`, `szz`, `sxy`, `syz`, `szx` |
| `TOSTRAIN` | `strains` | `exx`, `eyy`, `ezz`, `exy`, `eyz`, `ezx`, descending `principal` values |
| `PE` or `PEEQ` | `equivalent_plastic_strain` | Scalar equivalent plastic strain |

The [parser implementation](../../src/digitalmodel/solvers/calculix/result_parser.py)
defines the ASCII layout: a three-character record flag, ten-character integer
node identifier, and twelve-character real fields in the component order above.
It accepts finite values, including Fortran `D`/`d` exponents. Field blocks
start with ` -4`, permit ` -5` descriptors, contain ` -1` node rows and end
with ` -3`. Descriptor labels do not remap component order. `TOSTRAIN` is
total Lagrangian strain, not plastic strain; the parser treats its shear
components as tensor entries when computing principal values.

After parsing, use `get_stress_at_node(node_id)`, `get_max_von_mises()` and
`get_max_displacement()` for the example's extraction. These are public methods
of the [same class](../../src/digitalmodel/solvers/calculix/result_parser.py).
The maximum getters parse lazily if needed. `get_max_stress_component(component)`
returns the maximum absolute component. No result call runs a solver.

## Units, failures and gaps

The caller supplies a consistent unit system. No unit metadata is read or
converted: displacement retains the caller's length unit, stress retains the
caller's stress unit, and strain is dimensionless. The example explicitly
labels its invented results as mm and MPa.

- An absent `.frd` raises `FileNotFoundError`. A missing requested stress node
  raises `KeyError`. Malformed recognized blocks, duplicate nodes, non-finite
  values, truncated records and missing terminators raise `ValueError`.
- Unrecognized field labels are ignored. Absent supported fields yield empty
  dictionaries; maximum displacement and equivalent stress then return zero.
  Consumers must check field presence before interpreting a zero as a result.
- Repeated fields retain only the latest complete block of each field. They
  are not synchronized by step or time; no result history is returned.
- Binary FRD, native result databases and other result formats are unsupported
  by this ASCII entry. There is no binary-format detection guarantee; use known
  ASCII inputs. Geometry/connectivity and arbitrary field labels are not
  extracted here. Separate `.dat` and `.sta` methods exist but are outside this
  entry's example and coverage claim.
- Full real-output compatibility, native solver execution and the other
  adapters need separate review. Reuse this parser for known ASCII fields
  rather than implementing another reader; add format-specific fixtures before
  extending its coverage.

## Runnable synthetic example and checks

[`ascii_adapter.py`](../../examples/calculix/ascii_adapter.py) writes a tiny,
explicitly synthetic FRD string into a temporary folder, calls only the public
parser and prints JSON. The fixture contains result blocks only, not a complete
solver-produced mesh file or a physically solved state. It uses no corpus,
binary fixture, external download or solver probe.

From the repository root, with the runtime dependencies installed and `TMPDIR`
set to a writable temporary location:

```sh
PYTHONPATH=src uv run --no-sources python examples/calculix/ascii_adapter.py
```

The invented node has uniaxial stress `sxx = 120 MPa`, with other stress
components zero. The independent von Mises calculation is
`sqrt(((120-0)^2 + (0-0)^2 + (0-120)^2)/2) = 120 MPa`.
The invented displacement `(3, 4, 12) mm` gives
`sqrt(3^2 + 4^2 + 12^2) = sqrt(169) = 13 mm`.
These hand calculations are the source of all physical example and test values.

[`test_calculix_ascii_catalog_example`](../../tests/solvers/calculix/test_adapter_example.py)
executes the script entry point and compares all extracted components, both
maxima and unit labels with those independent expected values. The parser
selection starts with eight tests in `TestResultParser`; the final selection
contains those same eight tests plus this example test, for nine tests total.
These counts come from the selected pytest runs, not whole-suite coverage.

```sh
PYTHONPATH=src uv run --no-sources python -m pytest \
  tests/solvers/calculix/test_adapter_example.py \
  tests/solvers/calculix/test_fem_chain.py::TestResultParser \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/bt-module"
```
