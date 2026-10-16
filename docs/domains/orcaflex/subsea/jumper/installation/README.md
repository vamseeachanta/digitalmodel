# Jumper installation input

The domain specification is converted to `ProjectInputSpec`, emitted as a
modular YAML model, and checked by the repository's solver-free validators.
The retained inputs in `generated/jumper-a` and `generated/jumper-b` each contain
27 line sections. No solver run was made; solver loadability and installation
performance are not established by these checks.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-14 | Retain generated inputs and document generation and validation so the specification-to-input path can be reproduced. |

Table 1. Revision history.

## Generation

Select an existing domain specification as `SPEC`, and a destination outside the
checkout as `OUTPUT`. The existing source specifications remain unchanged.

```bash
PYTHONPATH=src uv run --no-sources python -m \
  digitalmodel.marine_ops.installation.jumper_input \
  generate "$SPEC" "$OUTPUT" --name jumper-a
```

`build_modular_spec` uses the existing jumper calculations to construct line
types, section rows, an assumed M-profile, water and metocean properties, and
simulation controls. `generate_input` replaces descriptive source identifiers
with the supplied neutral name, validates `ProjectInputSpec`, then calls
`ModularModelGenerator.from_spec`. Numerical settings are preserved.

The generated files are `master.yml`, `inputs/parameters.yml`, and
`includes/01_general.yml`, `includes/03_environment.yml`, and
`includes/20_generic_objects.yml`. The two retained models correspond to the
first and second source specifications in lexicographic path order. Their
current section geometry is identical; environmental inputs differ. The
integration test compares regenerated and retained YAML values for both models.

## Validation

```bash
PYTHONPATH=src uv run --no-sources python -m \
  digitalmodel.marine_ops.installation.jumper_input \
  validate docs/domains/orcaflex/subsea/jumper/installation/generated/jumper-a

PYTHONPATH=src uv run --no-sources python -m \
  digitalmodel.marine_ops.installation.jumper_input \
  validate docs/domains/orcaflex/subsea/jumper/installation/generated/jumper-b
```

Each command reports `PASS: repository solver-free input validation; no solver
run` when the checks pass. Generation runs the same checks automatically.
Validation requires parseable master and include YAML, existing includes within
the model directory, a parameters file, no collection collisions, and no
cross-object reference errors. Warnings from the existing format validator do
not establish solver acceptance.

```bash
PYTHONPATH=src uv run --no-sources python -m pytest \
  tests/solvers/orcaflex/modular_generator/test_jumper_input.py \
  -q -rA -p no:cacheprovider --basetemp="$TEST_TEMP"
```

Set `TEST_TEMP` to a dedicated test output directory. The integration test
requires 27 positive-length sections with resolved line types, complete include
references, and matching retained inputs. Fresh subprocesses fail if the solver
binding is imported or looked up, even if an optional import catches the error.
Negative tests reject missing includes, invalid format and escaping include
paths.

## Basis and limits

Existing repository data is used as **assumed, based on previous experience**.
The M-profile orientation, hub clearance, equivalent coating and mesh settings
retain the existing converter's assumptions. Source provenance requires
maintainer confirmation; these inputs are not qualified engineering results.
Vessel and crane dynamics, lift, splash-zone, lowering and landing results
require later analysis. No licensed solver or licence check is invoked by these
commands.
