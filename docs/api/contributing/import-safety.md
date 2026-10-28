# Import safety for computational APIs

Import of reusable computational APIs must not perform output filesystem
mutations. Importing a module or its package initializer must not create output
directories, write reports, generate charts, or start a workflow. This contract
also applies to imports reached indirectly through an atlas response registry.

Resolve input and output locations at an explicit call boundary. Accept
caller-supplied paths, and document any defaults. Avoid fixed drives and
machine-specific directories. Create output directories only when the caller
requests output. An import must remain usable when the default output location
is unavailable or read-only.

Defer plotting dependencies until chart generation is requested. Package
initializers must not use eager wildcard exports that load chart generators as
a side effect of importing unrelated calculations. Apply plot styles, palettes
and warning filters within an explicit call and restore the caller's settings
when that call returns or raises an exception.

## Existing example

The [OCIMF chart module](https://github.com/vamseeachanta/digitalmodel/blob/90b3e40a5638da4e0a13ba76657a9937e1def452/src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py)
accepts `csv_file` in `load_data` and `main`, and `output_dir` in `main`, report
creation and each plotting function. A library call without `output_dir` writes
to `ocimf_charts` below the working directory at call time. The command-line
entry retains the repository-relative `docs/charts/phase2/ocimf` destination.
Importing the chart module creates neither destination.

The [visualization initializer](https://github.com/vamseeachanta/digitalmodel/blob/90b3e40a5638da4e0a13ba76657a9937e1def452/src/digitalmodel/marine_ops/marine_analysis/visualization/__init__.py)
exposes chart submodules lazily. Accessing `ocimf_charts` does not load
`integration_charts`. This example defines the chart boundary; it does not
certify import safety of every ancestor package or every chart module.

## Regression checks

Use a fresh subprocess and an isolated temporary working directory to check
imports. Observe directory creation and write attempts, directory contents,
plotting configuration, warning filters and loaded chart modules. A directory
snapshot alone cannot detect attempted writes elsewhere. Initialize third-party
caches outside the observation boundary when testing a complete import chain;
separately test cold chart imports with plotting dependencies blocked.

Exercise explicit chart generation with temporary input and output paths.
Check the output filenames and report references, and verify that plot settings
are restored after successful and failing chart calls. Use synthetic data only
for routing and state-isolation checks; engineering validation requires its own
sourced inputs and independently calculated expected results.

The [chart generation tests](https://github.com/vamseeachanta/digitalmodel/blob/90b3e40a5638da4e0a13ba76657a9937e1def452/tests/marine_ops/marine_analysis/test_ocimf_charts.py)
and [cold import tests](https://github.com/vamseeachanta/digitalmodel/blob/90b3e40a5638da4e0a13ba76657a9937e1def452/tests/marine_ops/marine_analysis/test_visualization_import_contract.py)
cover these boundaries. The existing atlas tests retain numerical regression
coverage. These checks are focused regressions, rather than a repository-wide
import audit.
