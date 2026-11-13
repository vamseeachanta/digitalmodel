# Cathodic protection workflows

## Synthetic YAML example

With digitalmodel and its runtime dependencies installed, run from the repository
root (use `PYTHONPATH=src` for a source checkout):

```bash
PYTHONPATH=src uv run python examples/cathodic_protection/run_design.py
PYTHONPATH=src uv run python examples/cathodic_protection/run_design.py --config custom.yml
```

The default is `examples/cathodic_protection/synthetic_design.yml`, resolved beside
the runner regardless of the current directory. An explicit `--config` is resolved
from the caller's directory. The runner calls `design_from_yaml`; all inputs and
unit declarations required by `CPDesignSpec` remain mandatory. It writes no files
and returns exit code 1 for input, YAML, validation or calculation-range errors.

The fixture uses arbitrary arithmetic inputs: 120 m2, 0.05 A/m2, coating fraction
0.2, ten years, 1500 Ah/kg and utilisation 0.8. These are test inputs, not material
or standard values. With the existing 365-day year convention, console output is
1.2 A mean demand, 105120.0 Ah lifetime charge and 87.6 kg required net mass.
Results are displayed to one decimal place; small values can round to zero.

This is single-zone mean-demand and net-mass screening. The printed limitation
requires further protection-potential, anode-resistance, current-output, count,
distribution, interference and installation checks. Multi-zone jacket design,
buried-pipeline soil correction and hull installation qualification are outside
this example's scope.

## Edition study

From the repository root, reproduce the newsletter's F103 pipeline comparison
with public dependencies in one command (Python 3.11 and uv required):

```bash
uv run --no-project --python 3.11 --with . python examples/cathodic_protection/run_edition_study.py
```

After installation in an activated environment, the runner can also be invoked
with `python examples/cathodic_protection/run_edition_study.py`. It imports only
standard-library modules and digitalmodel and reads no fixture or external data
files. It returns exit code 1 when a displayed calculation differs from its
pinned regression value; checks remain active under `python -O`.

The embedded inputs reproduce the public
[pipeline fixture](../../../tests/fixtures/cathodic_protection/workflow_inputs/pipeline.yml),
already checked by the
[engine-adapter edition tests](../../../tests/cathodic_protection/test_engine_adapter.py).
The basis is a non-buried, FBE-coated pipeline: 1,500 m length, 0.3239 m outside
diameter, 0.0159 m wall thickness, 60 C fluid and 30 years of service. Aluminium
bracelets have 25 kg net mass, 0.2 m length, 0.04 m thickness and 0.80 utilisation;
seawater resistivity is 0.30 ohm-m and steel resistivity is 0.2e-6 ohm-m.
Field-joint area and concrete weight coating are absent. Only the edition changes.

| F103 edition | Mean current (A) | Required net mass (kg) | Anodes (count) |
|---|---:|---:|---:|
| 2010 | 1.3279 | 218.111 | 9 |
| 2019 | 5.1514 | 846.119 | 34 |

Table 1. Current module results for the fixed public pipeline; mass governs both counts.

The 2010 and 2019 current-density and FBE coating-breakdown tables differ. These
values are regression checks for this declared case, not an independent design
qualification. The calculation returns its standard, edition and table citations
in `results`; no standards PDF is needed to execute it.

### Selecting inputs.design_data.edition

For `run_cathodic_protection(cfg)` and the YAML engine, place the edition under
`inputs.design_data`, alongside the design life. For example, this selector
fragment will pin the earlier F103 edition when combined with the full pipeline
inputs:

```yaml
inputs:
  calculation_type: DNV_RP_F103
  design_data:
    edition: "2010"
    design_life: 30
```

Quoted years make the edition explicit as a string. Selection depends on the
calculation route:

| Calculation type | Canonical edition values | Default when omitted |
|---|---|---|
| `DNV_RP_F103` | `"2010"`, `"2019"` | `"2019"` |
| `DNV_RP_B401_offshore` | `"2005"`, `"2010"`, `"2017"`, `"2021"` | `"2021"` |

Table 2. Edition selection for the public DNV engine routes.

F103 also accepts `"2021"` as the May 2021 amended print of `"2019"` (same
tables), and `"2016"` as the 2019 republication with a `UserWarning`. B401 accepts
`"2011"` as an alias of its October 2010 edition. Standard-prefixed aliases such
as `"dnv-rp-f103-2010"` and `"dnv-rp-b401-2021"` are accepted; the full alias maps
are in [`_edition.py`](_edition.py). Unsupported edition strings raise
`ValueError`.

The deprecated `DNV_RP_F103_2010` calculation type pins 2010 regardless of the
new default, emits `DeprecationWarning` and rejects an explicitly conflicting
edition. New inputs should use `DNV_RP_F103` with an explicit edition. Functional
APIs such as `design_bracelet_cp` accept the equivalent `edition="2010"` keyword;
omitting that keyword emits `UserWarning` and uses the F103 default. The engine
routes in Table 2 supply their defaults without a missing-edition warning.

## One-at-a-time sensitivity

Run a sweep with the existing B401 engine:

```bash
PYTHONPATH=src python -m digitalmodel.cathodic_protection.sensitivity \
  tests/fixtures/cathodic_protection/workflow_inputs/published_jacket_sweep.yml \
  --mode standard_tables --output results/sensitivity.json \
  --report results/sensitivity.html
```

The YAML names a `base_input` relative to the config file, a `mode`, and a
`parameters` list of `path` and `values` entries. Paths are relative to `inputs`.
Supported paths are `environment.seawater_temperature_C`,
`design_data.design_life` and `environment.seawater_resistivity_ohm_m`.
Each value runs independently against the base input; this is not a Cartesian
product. The supplied config produces 12 rows per mode. Duplicate base values
remain separate cases under their respective parameters.

`run_sweep(config, mode=...)` returns rows containing initial, mean and final
current [A], required net mass [kg], recommended count [anodes], and the engine's
own `adequacy` and `status` mappings. Those flags assess the installed count in
the base input, not the recommended replacement count. Missing, non-finite or
non-applicable sweep inputs raise an error. The base input is copied and never
written. Text outputs use UTF-8 and LF line endings; paths use `pathlib`.

In `published` mode all per-zone effective phase densities are retained.
Temperature is therefore a no-op for the supplied fixture. Temperature acts
only through the climate column of the standard's current-density table:
tropical above 20 degC, subtropical from 12 through 20 degC, temperate from 7
to below 12 degC, and arctic below 7 degC. Selection is stepwise, without
interpolation. The engine does not link seawater temperature to resistivity,
anode capacity or driving voltage. Anode surface temperature is a separate
input and is not varied by this sweep.

`standard_tables` removes the effective densities and their density-source
metadata only from the two zones named in `standard_table_zones`, setting their
depths to the configured 15 m and 57 m. Coated and buried effective densities
and connected well allowances remain unchanged. This mode is a what-if on a
modified input, not a published result. The existing base-input report remains
unchanged outside its added sensitivity section. The section states its source
and review limit, uses three decimal places for current and two for mass, and
shows each status beside its exact integer count criterion and basis. A small
shortfall cannot disappear through one-decimal rendering.

The fixture's recorded public-source provenance and declared assumptions remain
its basis. The sweep does not establish spatial protection or introduce a
temperature correlation beyond climate selection. Sweep numbers are from a
read-only engine run and are not yet reviewed.
