# OC4 Linux test evidence

The complete selected test collection was exercised on Linux aarch64 at
`0324b8f8ada70033add258e7ef28dc778b4d86d2`. The final result is 436 passed,
19 failed, 29 skipped and 6 setup errors. All remaining failures and errors
require the unavailable pinned Gmsh package. Complete execution with that
package remains unverified; this evidence does not establish native acceptance.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| A | 2026-10-26 | Selected test counts, complete failure diagnostics and the dependency limitation are recorded to distinguish exercised software from unverified generation. |

## Scope and environment

Every test file changed by the pull request is included: seven OC4 modules and
`tests/solvers/smoke/test_ci_registration.py`. The selection includes every
pytest path registered by `.github/workflows/gmsh-meshing-tests.yml`, plus the
whole `tests/solvers/gmsh_meshing` directory. No test, threshold or tolerance
was modified. No test was deselected or newly marked as skipped.

Python 3.11.17, pytest 8.4.2, pytest-asyncio 0.21.1, pytest-timeout 2.4.0,
NumPy 1.26.4, PyYAML 6.0.1, SciPy 1.15.3, Click 8.3.1 and Loguru 0.7.3
were used. The final selection also includes pandas 2.3.3, Plotly 5.17.0,
pydantic 2.12.5 and Shapely 2.1.2 for the broader meshing suite's imports.
These direct package versions match `uv.lock`. This is an isolated test
environment, not the full locked repository environment; transitive packages
were resolved by uv. No project synchronization or dependency-file change
was performed. Tests ran serially, with numerical-library thread counts set
to one. Scratch outputs and the shared uv cache are outside the checkout.

The required Gmsh 4.15.1 environment failed resolution before any test ran:

```text
error: No solution found when resolving `--with` dependencies
  cause: Because gmsh==4.15.1 has no wheels with a matching platform tag (e.g., `manylinux_2_39_aarch64`) and you require gmsh==4.15.1, we can conclude that your requirements are unsatisfiable.

hint: Wheels are available for `gmsh` (v4.15.1) on the following platforms: `manylinux_2_24_x86_64`, `macosx_10_15_x86_64`, `macosx_12_0_arm64`, `win_amd64`
```

The pin was retained. No source build, alternate Gmsh version or substitute
module was used. Native FreeCAD and Part are also absent; their existing
optional tests report skips. Full workflow execution, including the pinned
assetutilities checkout and a current-base merge, was not performed.

## Commands and results

The commands preserve the executed arguments. `$UV` denotes the existing uv
executable; `$SCRATCH` denotes the external scratch directory. Filesystem
prefixes are represented by these variables for publication. Before each
command, `TMPDIR` referred to the scratch temporary directory,
`PYTHONDONTWRITEBYTECODE=1`, `UV_CACHE_DIR` referred to the shared cache,
`PYTHONPATH=src`, and `OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS` and
`MKL_NUM_THREADS` were each set to 1 for pytest.

The pinned-environment availability command exited 1; tests with Gmsh 4.15.1
could not run:

```bash
"$UV" run --no-project --python 3.11 \
  --with pytest==8.4.2 --with pytest-asyncio==0.21.1 --with pytest-timeout==2.4.0 \
  --with numpy==1.26.4 --with gmsh==4.15.1 --with PyYAML==6.0.1 \
  --with scipy==1.15.3 --with click==8.3.1 python -c \
  'import sys,platform,gmsh,numpy,yaml,pytest,scipy,click; print(sys.version); print(platform.machine()); print("gmsh",gmsh.__version__); print("numpy",numpy.__version__); print("PyYAML",yaml.__version__); print("pytest",pytest.__version__); print("scipy",scipy.__version__)'
```

The initial selection exited 1: **433 passed, 22 failed, 29 skipped, 6 errors**.
Three failures additionally required pandas through the broader meshing
suite's visualization imports. All other failures and errors required Gmsh.

```bash
"$UV" run --no-project --python 3.11 \
  --with pytest==8.4.2 --with pytest-asyncio==0.21.1 --with pytest-timeout==2.4.0 \
  --with numpy==1.26.4 --with PyYAML==6.0.1 --with scipy==1.15.3 \
  --with click==8.3.1 --with loguru==0.7.3 python -m pytest \
  tests/hydrodynamics/test_oc4_public_mesh.py \
  tests/hydrodynamics/test_oc4_extended_band.py \
  tests/hydrodynamics/test_oc4_complex_export.py \
  tests/hydrodynamics/test_oc4_staged_band.py \
  tests/hydrodynamics/test_oc4_tail_bound.py \
  tests/hydrodynamics/test_oc4_damage_tail.py \
  tests/hydrodynamics/test_oc4_retained_measurements.py \
  tests/solvers/smoke/test_ci_registration.py \
  tests/solvers/gmsh_meshing \
  tests/solvers/openfoam/test_poly_mesh_contract.py \
  tests/solvers/openfoam/test_gmsh_bridge.py \
  tests/solvers/openfoam/test_runner.py \
  tests/solvers/openfoam/test_runner_prebuilt.py \
  tests/solvers/openfoam/test_smoke.py \
  tests/solvers/openfoam/validation/test_sloshing_3d.py \
  tests/scripts/test_cfd_python_install.py \
  tests/scripts/test_dispatch_cfd_run.py \
  tests/scripts/test_synthetic_tank_3d_smoke.py \
  -q -rA -p no:cacheprovider --tb=long --basetemp="$SCRATCH/bt-selected"
```

The metrology retry exited 1: **2 passed, 1 failed, 0 skipped, 0 errors**.
The remaining import failure required Shapely; no source change was made.

```bash
"$UV" run --no-project --python 3.11 \
  --with pytest==8.4.2 --with pytest-asyncio==0.21.1 --with pytest-timeout==2.4.0 \
  --with numpy==1.26.4 --with PyYAML==6.0.1 --with scipy==1.15.3 \
  --with click==8.3.1 --with loguru==0.7.3 \
  --with pandas==2.3.3 --with plotly==5.17.0 --with pydantic==2.12.5 python -m pytest \
  tests/solvers/gmsh_meshing/test_spool_pilot.py::test_metrology_checks_angular_hole_displacement \
  tests/solvers/gmsh_meshing/test_spool_pilot.py::test_bolt_cutters_extend_past_both_flange_faces \
  -q -rA -p no:cacheprovider --tb=long --basetemp="$SCRATCH/bt-metrology"
```

After providing the locked Shapely version, the final selection exited 1:
**436 passed, 19 failed, 29 skipped, 6 errors**, from **490 collected tests**.
These are final unique-test counts; attempts are not added together.

```bash
"$UV" run --no-project --python 3.11 \
  --with pytest==8.4.2 --with pytest-asyncio==0.21.1 --with pytest-timeout==2.4.0 \
  --with numpy==1.26.4 --with PyYAML==6.0.1 --with scipy==1.15.3 \
  --with click==8.3.1 --with loguru==0.7.3 \
  --with pandas==2.3.3 --with plotly==5.17.0 --with pydantic==2.12.5 --with shapely==2.1.2 python -m pytest \
  tests/hydrodynamics/test_oc4_public_mesh.py \
  tests/hydrodynamics/test_oc4_extended_band.py \
  tests/hydrodynamics/test_oc4_complex_export.py \
  tests/hydrodynamics/test_oc4_staged_band.py \
  tests/hydrodynamics/test_oc4_tail_bound.py \
  tests/hydrodynamics/test_oc4_damage_tail.py \
  tests/hydrodynamics/test_oc4_retained_measurements.py \
  tests/solvers/smoke/test_ci_registration.py \
  tests/solvers/gmsh_meshing \
  tests/solvers/openfoam/test_poly_mesh_contract.py \
  tests/solvers/openfoam/test_gmsh_bridge.py \
  tests/solvers/openfoam/test_runner.py \
  tests/solvers/openfoam/test_runner_prebuilt.py \
  tests/solvers/openfoam/test_smoke.py \
  tests/solvers/openfoam/validation/test_sloshing_3d.py \
  tests/scripts/test_cfd_python_install.py \
  tests/scripts/test_dispatch_cfd_run.py \
  tests/scripts/test_synthetic_tank_3d_smoke.py \
  -q -rA -p no:cacheprovider --tb=long --basetemp="$SCRATCH/bt-final"
```

| Test file | Passed | Failed | Skipped | Errors |
| --- | ---: | ---: | ---: | ---: |
| `tests/hydrodynamics/test_oc4_complex_export.py` | 17 | 0 | 0 | 0 |
| `tests/hydrodynamics/test_oc4_damage_tail.py` | 26 | 0 | 0 | 0 |
| `tests/hydrodynamics/test_oc4_extended_band.py` | 31 | 1 | 0 | 1 |
| `tests/hydrodynamics/test_oc4_public_mesh.py` | 19 | 0 | 0 | 2 |
| `tests/hydrodynamics/test_oc4_retained_measurements.py` | 4 | 0 | 0 | 0 |
| `tests/hydrodynamics/test_oc4_staged_band.py` | 12 | 16 | 0 | 0 |
| `tests/hydrodynamics/test_oc4_tail_bound.py` | 17 | 0 | 0 | 0 |
| `tests/scripts/test_cfd_python_install.py` | 12 | 0 | 0 | 0 |
| `tests/scripts/test_dispatch_cfd_run.py` | 21 | 0 | 0 | 0 |
| `tests/scripts/test_synthetic_tank_3d_smoke.py` | 19 | 0 | 0 | 0 |
| `tests/solvers/gmsh_meshing/test_gmsh_cli.py` | 0 | 0 | 5 | 0 |
| `tests/solvers/gmsh_meshing/test_gmsh_step_inp.py` | 2 | 0 | 12 | 0 |
| `tests/solvers/gmsh_meshing/test_gmsh_unit.py` | 6 | 0 | 4 | 0 |
| `tests/solvers/gmsh_meshing/test_spool_pilot.py` | 85 | 0 | 8 | 0 |
| `tests/solvers/gmsh_meshing/test_tank_fixture.py` | 7 | 2 | 0 | 3 |
| `tests/solvers/openfoam/test_gmsh_bridge.py` | 11 | 0 | 0 | 0 |
| `tests/solvers/openfoam/test_poly_mesh_contract.py` | 17 | 0 | 0 | 0 |
| `tests/solvers/openfoam/test_runner.py` | 22 | 0 | 0 | 0 |
| `tests/solvers/openfoam/test_runner_prebuilt.py` | 27 | 0 | 0 | 0 |
| `tests/solvers/openfoam/test_smoke.py` | 60 | 0 | 0 | 0 |
| `tests/solvers/openfoam/validation/test_sloshing_3d.py` | 14 | 0 | 0 | 0 |
| `tests/solvers/smoke/test_ci_registration.py` | 7 | 0 | 0 | 0 |
| **Total** | **436** | **19** | **29** | **6** |

The seven touched hydrodynamics modules account for 126 passed, 17 failed,
0 skipped and 3 errors, from 146 tests. The CI-registration module passed all
7 tests. The workflow's exact pytest selection accounts for 343 passed,
19 failed, 0 skipped and 6 errors, from 368 tests; the additional gmsh meshing
files account for 93 passed and 29 skipped. All 19 final failures and 6 errors
terminate at missing Gmsh. The existing 29 skips comprise 25 Gmsh-dependent
cases and 4 FreeCAD/Part-dependent cases.

Both `test_public_mesh_hydrostatics_and_limits` and
`test_checker_cli_reads_export_and_reports_unknown_mass` error in their
shared `mesh_file` setup at `generate_mesh.py:111` with
`ModuleNotFoundError: No module named 'gmsh'`. Neither assertion body ran.
The Windows fixture-permission problem is not reproduced, but these results
cannot establish that either test passes on Linux with Gmsh installed.

## Full diagnostics and retention

[Complete failure and result diagnostics](linux-test-diagnostics.txt) include
every failure and setup error from all three attempts, with full tracebacks,
and the complete pytest result lists. Captured output from passing tests is
omitted. A scratch script replaced filesystem context with `[path removed]`
and replaced privacy-test parameter strings in passing identifiers. Trailing
whitespace was removed; assertion text and numerical result values
were retained. SHA-256 of the file as committed:
`9aebf5460c1fe27372fb15e87595840fa3d5031093c95436de9904f1dbdccfce`.
The original unsanitized logs remain in external scratch storage.

## Baseline

None. No source defect or tolerance failure was identified. The final failures
and setup errors are unavailable-tool outcomes, rather than demonstrated
baseline failures in main or accepted test passes.

## Hooks that could not run

The pre-commit framework is unavailable and was not invoked. File-level checks
are performed directly; their results are recorded in the pull request comment.

## Follow-up

Repeat the complete pinned workflow selection where Gmsh 4.15.1 is available,
including both named tests and the generator/damaged-hull regressions. Native
lid readback and equivalence, native complex export/reload, capacity, system
mass basis, resonance resolution, strength convergence and whole-band fatigue
acceptance remain OPEN. Existing `release_eligible=false` remains unchanged.

Lesson: collection and partial execution do not establish a generator pass;
test counts must identify unavailable native dependencies separately.
