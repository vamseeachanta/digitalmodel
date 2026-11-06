# Cathodic protection workflows

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
