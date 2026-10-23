# Floating wind worked example: Phase A audit

The worked example currently provides source inputs, dependency gaps, proposed
case families and an unissued report skeleton. The text-only audit makes that
preparation reproducible. **A passing audit establishes document consistency;
engineering results and execution readiness remain NOT_EVALUATED.**

## Inputs, method and answer

Inputs are the two raw models linked in the [inventory](external-files-inventory.md),
their modular masters, the [case worksheet](load-case-table.md), and the
[report skeleton](report-skeleton.html). These repository files remain the
source of each model value; no model data is copied into this implementation.
The module records SHA-256 digests of the exact raw-model bytes inspected.

From the repository root, with the package on the Python path:

```bash
PYTHONPATH=src python -m digitalmodel.floating_wind.worked_example --root .
```

The JSON answer is written to standard output. It can be retained in the
configured scratch directory; it is derived audit evidence, not solver output:

```bash
PYTHONPATH=src python -m digitalmodel.floating_wind.worked_example --root . > "$TMPDIR/fowt-phase-a.json"
```

The audit uses the following method:

- Read raw models as text, without evaluating YAML tags or embedded scripts.
  Record every `FileName`, `ControllerDLL` and `InputFile` occurrence, including
  repeated declarations and their source lines. Header provenance is excluded.
- Search the checkout for case-insensitive basename holdings, excluding Git
  metadata, virtual environments and Python/tool caches. Symlink targets outside
  the checkout are excluded. A holding elsewhere retains unverified compatibility.
- Read each modular master's `includefile` directives and require its exact
  relative target and matching inventory row, source locator and link target.
  A matching basename elsewhere cannot satisfy an include.
- Check that inventory text names each detected dependency and its holdings.
  Require each expected report input once per model and check cells against
  the cited raw-model line, key and value, including stage-duration range ends.
- Require the worksheet's statics, free-decay and operating/parked families,
  marked required inputs and a source disposition. Require exactly one results
  section and its exact four-column header once. Every other row in that section
  must carry the result class and all four cells, with nonempty, nonnumeric
  result placeholders in its three value cells, whether they use data or header
  tags. Marked result rows outside that section are rejected.

The expected answer for the current sources is:

- L02 declares `PythonController.py` twice. The file is held elsewhere;
  compatibility and whether the declarations are active remain unverified.
- K03 references `BladedControllerWrapper.dll`, `libdiscon.dll` and
  `DISCON-UMaineSemi.IN`. The library is held elsewhere; the wrapper and input
  are missing. No substitution is performed.
- The modular includes are held. The proposed case families remain
  NOT_EVALUATED, and `execution_ready` remains `false` even when `errors` is empty.

These answers come from the inventory's model-line citations and the case
worksheet, rather than a numerical simulation. There is no engineering equation,
coefficient or acceptance limit in this audit. Tests use literal synthetic text
and independent source transcriptions; synthetic text is never promoted to a
model input or result.

## Validation and limits

```bash
PYTHONPATH=src python -m pytest tests/floating_wind/test_worked_example.py -p no:cacheprovider
ruff check --no-cache src/digitalmodel/floating_wind/worked_example.py tests/floating_wind/test_worked_example.py
```

The dependency scan is limited to the named keys in these models and masters.
It does not discover transitive dependencies inside controllers, prove library
representation equivalence, validate arbitrary OrcaFlex syntax, establish rights
to redistribute model files, or qualify a controller version. Inventory prose
and technical case-reference suitability still require review. The worksheet's
as-held values and their Markdown citations, including the NPD wind parameters,
are not audited; they require manual comparison with the raw models before use.
The report check covers its required source-cited input cells and every table
row in the results section, not arbitrary HTML.

A nonzero exit indicates missing audit inputs or inconsistent documentation.
Missing runtime dependencies are reported as preparation gaps; they are not
concealed by a successful document audit. No executable launcher is produced.

Later work remains the [preconditions for Phase B](external-files-inventory.md#preconditions-for-phase-b):
select public case definitions and comparison references, freeze inputs and
criteria, resolve controller gaps, and obtain separate authority for licensed
runs. Statics, periods, tensions, motions, tower-base loads and response
post-processing against retained native outputs remain NOT_EVALUATED.
