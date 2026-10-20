# Offline diffraction imports

Numerical, parsing, mesh and report-building helpers can be imported without
requesting a licensed solver interface. Licensed converters remain available
through explicit imports. This change implements the boundary in issue #2526;
it changes no hydrodynamic equations, coefficients or acceptance limits.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-20 | Defer interface imports so offline test collection can run without licensed software. |

## Import contract

Ordinary package imports, direct numerical submodule imports and these helpers
do not import or probe an interface:

```python
from digitalmodel.hydrodynamics.diffraction import MockDataGenerator
from digitalmodel.hydrodynamics.diffraction.analytical_cylinder import wavenumbers
from digitalmodel.hydrodynamics.diffraction.result_extractor import ResultExtractor
from digitalmodel.hydrodynamics.diffraction.solver import build_report_data_from_solver_results
```

`ResultExtractor` loads a converter only after an extraction request locates a
model file. `build_report_data_from_solver_results` stays offline when no
`owr_path` is supplied. `extract_report_data_from_owr` loads the interface when
called, and reports a missing dependency at that point.

Explicit access to `OrcaWaveConverter`, `convert_orcawave_results`,
`OrcaWaveDataExtractor`, `extract_all_rao_data`, `extract_all_added_mass` or
`extract_all_damping` resolves the licensed modules. The package-level
`ORCAWAVE_AVAILABLE` and `TEST_UTILITIES_AVAILABLE` flags and
`result_extractor.CONVERTER_AVAILABLE` also opt into interface detection when
read. Optional solver exports retain a `None` fallback if importing the
dependency raises `ImportError`. Direct converter submodule imports retain
their normal missing-dependency errors. Import errors other than `ImportError`
are propagated. A wildcard import requests licensed exports too; offline callers
should import the named helpers they need. `dir()` lists exports without loading
the interface.

The implementation follows [PEP 562, Module `__getattr__` and `__dir__`
(2017), Specification](https://peps.python.org/pep-0562/#specification).

## Worked example

Inputs are the module and callable pairs in
`examples/demos/diffraction/offline_imports.py`. The method imports each module
and checks the requested export with `callable`; no numerical criterion is
introduced. Run from the repository root with the offline Python dependencies
installed:

```bash
PYTHONPATH=src python examples/demos/diffraction/offline_imports.py
```

Expected answer:

```text
analytical_cylinder.wavenumbers: callable
reverse_parsers.OrcaWaveInputParser: callable
mesh_orientation.orientation_report: callable
solver.report_extractors.build_report_data_from_solver_results: callable
```

## Regression evidence

`tests/hydrodynamics/test_diffraction_import_boundary.py` runs fresh interpreters
with a rejecting import wrapper and finder installed before diffraction is
imported. The wrapper detects ordinary import statements; the finder detects
dynamic imports and discovery requests. Rejection raises a `BaseException`
subclass so an optional-dependency `except ImportError` cannot hide a request.
The expected request list is empty, as required by issue #2526. No interface
substitutes are installed in these offline checks, and collection policy and
numerical cases are unchanged.

The regression runs the complete analytical-cylinder, unit conversion, mesh
orientation, open-boundary, mesh-pipeline, reverse-parser and AH1 parser files.
It also checks report and result-extraction helpers and the worked example.
Run it with:

```bash
PYTHONPATH=src python -m pytest tests/hydrodynamics/test_diffraction_import_boundary.py -q -rA -p no:cacheprovider
```

The draft pull request records the base revision, content digests and node
results for the tested uncommitted tree. Licensed execution remains untested;
existing converter tests use their established test doubles separately from
the offline regression. Independent review and a licensed execution check
remain for the maintainers before merging.
