# Riser utilization campaign inputs

## Data provenance

| Input | Source and section | Licence or terms | Date | Version | SHA-256 |
| --- | --- | --- | --- | --- | --- |
| field-development rows (NOT_USED) | No field-development rows used | Not applicable: no rows used. | 2026-10-25 | not applicable | not applicable: no rows used |
| FPSO particulars (ASSUMED) | [New-build FPSO hull technical description, Key Figures](https://www.modec.com/business/service/floater/fpso/hull/m350.html) | Public dimension facts only; original document and images are not redistributed. | 2026-10-25 | Undated public page; retrieved edition | 9cee1d839086e49569be059477419423e18be02dd501430502accf4bc2257c76 |
| semi-submersible particulars (ASSUMED) | [Semisubmersible reference-system definition; report 60601, Section 3, Table 3-1](https://docs.nlr.gov/docs/fy14osti/60601.pdf) | Public dimension facts only; original document and images are not redistributed. | 2026-10-25 | NREL/TP-5000-60601, September 2014 | ccb0f7cdc001cfc41f3a32abc858c5ab0fca2564152b0ed81a0f56781913a1a0 |
| spar particulars (ASSUMED) | [Spar reference-system definition; report 47535, Section 3, Table 3-1](https://docs.nlr.gov/docs/fy10osti/47535.pdf) | Public dimension facts only; original document and images are not redistributed. | 2026-10-25 | NREL/TP-500-47535, May 2010 | ba6922aae6a9869a3b3e6ce440acb4e8529710a5bd62e8d41a0bf19b0ed45a5a |
| TLP particulars (ASSUMED) | [Tension-leg platform model development and loads analysis; report 45891, Section 3.2, Table 2](https://docs.nlr.gov/docs/fy10osti/45891.pdf) | Public dimension facts only; original document and images are not redistributed. | 2026-10-25 | NREL/SR-500-45891, February 2010 | 09d07ddd20b92b2de185fb8befb6f1871efe8ecad7539bbc5c843e5fd0d59e53 |
| floater RAOs (ASSUMED) | [Authored campaign basis, Assumed screening basis and parameter ranges](basis.md#assumed-screening-basis-and-parameter-ranges) | Repository MIT licence; authored synthetic assumptions. | 2026-10-25 | screening-basis-v1 | 7b3c6465bdcaafc995ba3659d16d98c43ffcec77079ed324650995c033ff8d77 |
| metocean (ASSUMED) | [Authored campaign basis, Assumed screening basis and parameter ranges](basis.md#assumed-screening-basis-and-parameter-ranges) | Repository MIT licence; authored synthetic assumptions. | 2026-10-25 | screening-basis-v1 | 7b3c6465bdcaafc995ba3659d16d98c43ffcec77079ed324650995c033ff8d77 |
| riser properties (ASSUMED) | [Authored campaign basis, Assumed screening basis and parameter ranges](basis.md#assumed-screening-basis-and-parameter-ranges) | Repository MIT licence; authored synthetic assumptions. | 2026-10-25 | screening-basis-v1 | 7b3c6465bdcaafc995ba3659d16d98c43ffcec77079ed324650995c033ff8d77 |
| geometry and numerical settings (ASSUMED) | [Authored campaign basis, Assumed screening basis and parameter ranges](basis.md#assumed-screening-basis-and-parameter-ranges) | Repository MIT licence; authored synthetic assumptions. | 2026-10-25 | screening-basis-v1 | 7b3c6465bdcaafc995ba3659d16d98c43ffcec77079ed324650995c033ff8d77 |

Table 1. Source digests identify public response bytes or the authored basis; no source publications are included.

## Summary

This campaign prepares **48 ASSUMED screening cases** and **288 solver input
files**, without a licensed solver. It crosses four floater types, two size
classes, three water depths and two riser configurations. No stress utilization
curve, native equilibrium or operating limit is established by these inputs.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-25 | Define an assumed comparison matrix and reproducible inputs for later licensed analysis. |
| 2 | 2026-10-25 | Place inputs under examples and bind each input group to explicit source terms, versions and digests. |

## Matrix and existing code

| Axis | Selected values | Basis |
| --- | --- | --- |
| Floater | FPSO, semi-submersible, spar, TLP | Parametric concepts; public geometry context below |
| Size | Small, large | ASSUMED scaling; not an operating asset |
| Water depth (m) | 500, 1000, 1500 | ASSUMED comparison grid; not a population distribution |
| Configuration | SCR, lazy-wave | Existing configuration algorithms |
| Radial span change (% of water depth) | 0, -2, +2 | ASSUMED nominal, near and far offsets |
| Regular wave height (m), period (s) | (2, 8), (6, 12) | ASSUMED mild and severe states; no return period |

Table 2. The complete factorial matrix contains 48 geometry cases; each has six environmental inputs.

The generator reuses `riser_configuration_screen.static.scr_offsets` and
`lazy_wave_offsets`. These retain total installed length and fixed buoyancy
material coordinates across offsets. They use the existing general and lazy-wave
catenary kernels. The screening workflow's static stress proxy is deliberately
not reported as dynamic utilization. The existing
`digitalmodel.orcaflex.riser_config.calculate_weight_in_water` supplies submerged
weight, including contents once. Existing `GenericModelBuilder`,
`EnvironmentBuilder`, `PostGenerationValidator` and `OrcaFlexDumper` supply typed
model mapping, native references and YAML serialization. The modular campaign
writer and benchmark catenary writer are precedents; no new catenary solver or
YAML dialect is introduced.

## Assumed screening basis

All engineering inputs, including synthetic RAOs and inherited builder defaults,
are **ASSUMED**. [The authored basis](basis.md) defines every selected range,
quantity, unit, scale factor, RAO arithmetic method and geometry criterion.
Public dimension references provide context only. Field-development database
rows, measured RAOs and site metocean data are not used.

## Validate and generate

From the repository root, with the repository dependencies available:

```bash
PYTHONPATH=src:. uv run python -m scripts.riser_campaign.validate_manifest
PYTHONPATH=src:. uv run python -m scripts.riser_campaign.generate_inputs --output-dir <output-directory>
PYTHONPATH=src:. uv run python -m pytest tests/riser_campaign tests/riser_configuration_screen -q -rA -p no:cacheprovider
```

The output directory is mandatory. The generator validates the entire manifest
and every geometry before writing inputs. The printed table lists every case,
units, assumptions and input count, followed by the number of licensed runs
(always zero). Identical existing files are read and reused without rewriting;
a digest mismatch or symlink is rejected without replacing that file. An
interrupted generation can be resumed with the same manifest, code and output
directory. Changed inputs require a new output directory. No output is deleted.

The output contains exact manifest/schema/basis snapshots, `index.json`, a geometry
JSON file and six self-contained `.yml` models per case. The index records
relative paths, file sizes and SHA-256 digests for every input, geometry and
snapshot. Verify the index against a pinned repository revision and hash before
using it. Geometry JSON uses material arc length from hang-off, local horizontal
position from hang-off, and elevation above the touchdown centreline, in metres;
forces are newtons and angles are degrees. Geometry is an analytical seed, not
native solver output. Only one worked YAML input per configuration and their
hash index are retained here; bulk outputs belong outside the checkout.

## Later licensed analysis and post-processing

The later run will consume the generated index and its exact manifest/schema/basis
snapshots. Each input will be hash-checked, loaded in the licensed solver and
qualified for native static equilibrium before dynamics. Missing native output,
load failures and failed equilibrium checks will remain explicit failed cases.
A zero exit status alone will not qualify a case. RAO origin, phase, direction,
units and draft will require independent review. Native load compatibility has
not been tested by this campaign preparation.

Before utilization can be calculated, the maintainer will specify a material
grade and yield strength, pressure/temperature conditions, corrosion allowance,
stress recovery method, governing design criterion and utilization limit. These
are not silently defaulted by this generator. Regular-wave sensitivity results
will not establish irregular-sea extreme or fatigue performance.

| Output required per case, offset and sea state | Units and retained evidence |
| --- | --- |
| Native input/run record | Case ID, offset ID, sea-state ID, RAO-set ID, input digest, manifest/schema/basis digests, pinned generator revision, criteria version, solver version, solver status, messages and result digest |
| Stress utilization versus material arc length | Arc length (m), component stresses (MPa), criterion/allowable and dimensionless utilization; separate missing strength basis from a true zero |
| Effective tension | Time (s), material arc length (m), effective tension (kN); minimum, maximum and time histories at hang-off, sag, hog and touchdown |
| Touchdown-point measures | Global position (m), arc coordinate (m), excursion range (m), curvature (1/m), bending moment (kN·m), contact force per length (kN/m), gap/penetration (m), and how touchdown was identified |
| Geometry and qualification | Node coordinates (m), unstretched arc lengths (m), equilibrium residuals with their units and limits, stage/time window (s), convergence and mesh evidence |

Table 3. Required later outputs; none is supplied as a licensed result by this preparation.

The later post-processor will join on case, offset and sea-state IDs, verify all
digests and plot utilization against material arc length by floater type, size,
RAO set and water depth. Geometry-screen results will remain distinct from
native stress/tension/contact results. No result archive, utilization curve,
newsletter figure or strength acceptance is delivered here.

## Baseline

None recorded at initial preparation; check results are reported with the change.

## Hooks that could not run

The hook framework is not part of the solver-free generation. File-level checks
are run directly during preparation; repository hooks run at commit time.

## Follow-up

Licensed loading, native equilibrium, dynamic response and utilization criteria
remain required before reporting utilization curves.
