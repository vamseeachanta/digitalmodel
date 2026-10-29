# Wave selector ordering

The environment builder emits wave selectors before their dependent properties
for typed trains, matched raw trains and extra raw trains. Updating a dictionary
value does not move its key: raw input order therefore needs a final ordering
pass after typed values are overlaid. The existing implementation performs this
pass in `EnvironmentBuilder._order_wave_train`.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-22 | Document the existing ordering contract and add an executable raw-input example for regression coverage. |

## Method and sources

[Orcina public help, Text data files, Ordering issues](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles.htm)
(online edition accessed 2026-10-22) states that assignment follows file order,
inactive data cannot be assigned, and activation selectors must precede the
data they enable. The builder puts `Name` and `WaveType` first, then any
`WaveJONSWAPParameters` and `WaveGamma`, before the remaining properties.
Remaining raw properties retain their relative order; keys are not globally
sorted.

[Orcina public help, Environment: Data for JONSWAP and ISSC spectra, Setting the data and Spectral parameters](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Environment%2CDataforJONSWAPandISSCspectra.htm)
(online edition accessed 2026-10-22) explains the JONSWAP modes and the dependency
of peak period on gamma. In partially specified mode, gamma must be assigned
before peak period. Automatic mode calculates gamma, so the builder omits
`WaveGamma` from output while retaining it in the source mapping. An explicit
gamma without a mode selects `Partially specified`.

Typed height, period and direction are authoritative for matched trains. Raw
extra trains retain their values. The builder retains a raw `WaveTp` period
when no `WaveTz` is present; otherwise it writes `WaveTz` and removes stale
`WaveTp` from the matched train. No spectral conversion is performed.

## Worked example

Run the [example script](examples/wave_selector_ordering.py) from the repository
root in an environment with the generator dependencies:

```sh
PYTHONPATH=src uv run python docs/domains/orcaflex/examples/wave_selector_ordering.py
```

The example prints an Environment mapping without starting a solver or writing
model files. All numeric values below are synthetic input tokens, not design
values, coefficients or acceptance limits.

Input raw train, deliberately placing dependents before their selector:

```yaml
WaveHeight: 2
WavePeriod: 8
WaveDirection: 0
WaveType: Airy
Name: matched
```

Typed input selects `airy`, height `2` and period `8`. A second raw train named
`extra` has the same values and lies beyond the typed train count. The method
copies the raw mapping, overlays typed values on the matched train, then orders
selectors for both trains. Independent identity calculation gives matched
height = typed height = `2` and matched period = typed period = `8`; extra
height = raw height = `2` and extra period = raw period = `8`.

The `WaveTrains` portion of the answer is:

```yaml
WaveTrains:
- Name: matched
  WaveType: Airy
  WaveHeight: 2.0
  WavePeriod: 8.0
  WaveDirection: 0.0
- Name: extra
  WaveType: Airy
  WaveHeight: 2
  WavePeriod: 8
  WaveDirection: 0
```

Each dependent assignment follows `WaveType`, so there are zero ordering
violations. This is an exact categorical ordering check. The test suite executes
the example and checks ordered keys and identity values, rather than deriving
expected results from the ordering helper.

## Limits and follow-up

These checks establish generated dictionary and YAML order without importing,
starting or querying a licensed solver. Native acceptance is not evaluated.
Changing wave theory while retaining theory-specific inactive raw properties
is outside this ordering contract; callers must supply compatible raw data.
The existing fully specified JONSWAP overlay behavior is retained; its native
acceptance is not established by these tests. Hash-seed reproducibility is a
separate concern tracked by issue #2092.
