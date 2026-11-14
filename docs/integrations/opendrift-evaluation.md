# OpenDrift — constant-forcing evaluation

As checked 13 Nov 2026: OpenDrift **1.14.7**, tag `v1.14.7`, commit
`4cfbbbf813e0642175f8dff840d721859fb21174`.
The [pinned source](https://github.com/OpenDrift/opendrift/tree/4cfbbbf813e0642175f8dff840d721859fb21174)
is the API basis; the five inspected model/reader files match the installed
distribution byte for byte. OpenDrift provides ocean trajectory, oil-spill and
search-and-rescue models. This slice evaluates OceanDrift surface advection only.

## Licence boundary

The pinned [LICENSE](https://github.com/OpenDrift/opendrift/blob/4cfbbbf813e0642175f8dff840d721859fb21174/LICENSE)
states GPL version 2. The digitalmodel adapter has an MIT header and imports
OpenDrift only when `run_constant_drift` is called. No OpenDrift source is copied
or vendored, and OpenDrift is not declared as a digitalmodel runtime dependency.
Distribution of a combined environment is a user action subject to the applicable
GPL terms; call-time importing does not establish a licensing exemption.

## Installation and dependency resolution

A disposable Python **3.12.3** environment was used with the editable project:

```bash
python -m pip install -e . "opendrift==1.14.7"
```

Resolution retained **NumPy 1.26.4**, **pyproj 3.6.1**, **psutil 5.9.6** and
**xarray 2025.12.0**; `pip check` reported no broken requirements.
OpenDrift 1.14.12 declares `psutil>=7`, conflicting with the project's existing
`psutil==5.9.6`; 1.14.7 was selected without changing project pins or lockfiles.
No licensed solver, credentials, external forcing service or landmask download
is required for this case.

## Adapter mapping

| Adapter input or operation | OpenDrift API |
| --- | --- |
| Frozen `ConstantDriftCase` | Finite coordinates, velocities, nonnegative explicit wind factor, positive duration and dividing time step |
| `advective_displacement_m(case)` | Independent `(current + factor * wind) * duration`, east/north metres |
| `run_constant_drift(case)` | `OceanDrift`, returning final longitude/latitude degrees |
| Constant current and wind | `reader_constant.Reader`, `x/y_sea_water_velocity`, `x/y_wind` |
| One surface release | `seed_elements(number=1, z=0)`, explicit wind/current factors and zero terminal velocity |
| Deterministic forcing | Diffusion, current/wind uncertainty, relative wind, Stokes drift and vertical motion disabled |
| No coastline data | Automatic landmask disabled; constant `land_binary_mask=0` |
| Duration and readback | `run` with equal integration/output steps; `result.isel(trajectory=0, time=-1)` |

Table 1. Adapter mapping to the pinned OpenDrift release.

Naive release times mean UTC; aware times are converted to UTC. Non-datetime
and non-finite inputs are rejected. Incomplete runs and non-finite final
coordinates raise `RuntimeError`; a transitive import failure retains its name.

## Synthetic verification

The release is 4° E, 60° N at 2026-01-01 00:00 UTC. East current is 0.5 m/s,
north wind is 10 m/s, other components are zero, and the supplied wind factor
is 0.02. Duration is 21600 s with a 900 s step. These are synthetic test values,
not measured forcing or operational design data. Hand displacement is 10800 m
east and 4320 m north. WGS84 displacement is read with `pyproj.Geod.inv`.

| Quantity | Hand reference | Backend | Difference | Criterion |
| --- | --- | --- | --- | --- |
| Distance (m) | 11631.956 | 11631.455 | -0.004307 % | Within 1 % |
| Bearing clockwise from north (deg) | 68.198591 | 68.117316 | -0.081275 | Within 0.5 deg |

Table 2. Both criteria are met for the governing six-hour synthetic case.

Final coordinates are 4.193656921° E, 60.038768768° N. The backend uses sequential
geodesic current/wind updates; the hand check assumes planar constant advection.
The new test file passes 51 cases with the backend installed; without it,
50 pass and the single backend case skips. Tests use `-m "not solver"`.

## Deferred work

OpenOil weathering, Leeway search-and-rescue, gridded NetCDF/THREDDS forcing,
real environmental data, plotting and a full operational adapter remain deferred
until a named use case and its validation criteria are established.
