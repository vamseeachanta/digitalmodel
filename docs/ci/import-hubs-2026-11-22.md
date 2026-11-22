# Import hubs in digitalmodel on 22 November 2026

**Measured.** At main `a79eebfa05eb9a7ccd9bf2413607a7dea3a82410`, replaying the last 40 first-parent commits through the fast-gate selector gives a median of **149.5** test files per commit (mean 324.6, total 12,985). Import hubs do not set that median. Removing *every* load-time import edge in `src/digitalmodel` leaves the median at 149.5 and lowers the total by 2.8% (to 12,620). Detector fallbacks set it: 24 of the 40 commits contain a path that falls back to whole domains, and leaving those paths out gives a median of **3.0** (mean 51.0, total 2,042).

| Replay of 40 commits (test files selected) | Median | Mean | Total |
| --- | ---: | ---: | ---: |
| Selector as designed | 149.5 | 324.6 | 12,985 |
| No load-time import edges at all (lower bound for any hub cut) | 149.5 | 315.5 | 12,620 |
| Fallback paths left out | 3.0 | 51.0 | 2,042 |
| Fallback paths left out, no load-time import edges | 3.0 | 39.0 | 1,561 |
| Selector after the cut in this PR (fatigue re-exports made lazy) | 149.5 | 321.9 | 12,874 |

Hub cuts can therefore lower the mean and the tail, not the median. The median moves only when fallbacks are narrowed: an `agent-pack.md` or other package document routes to every shard, and a new module with no static importer has no row and falls back to the full matrix.

## Top 20 hubs by transitive test reach

*Reach* is the number of gate-selected test files whose load-time import closure contains the module. *Fan-in* counts modules that import it at load. Modules in one source unit with the same reach are listed once (the gate selects by unit), and packages with neither importers nor load-time imports are left out: Python runs a parent `__init__` before any submodule, so their reach is only the sum of their children's. *Per-unit mean* is the replay-independent average number of test files selected by a change to one source unit (65.2 today; 27.0 with no load-time edges). The replay column is the change in the 40-commit total; the median is 149.5 for every row.

| # | Module | Reach | Fan-in | Why it is a hub | Cut | Replay total | Per-unit mean |
| ---: | --- | ---: | ---: | --- | --- | ---: | ---: |
| 1 | `digitalmodel` | 2,276 | 16 | Python runs it before every submodule. Already lazy (PEP 562, #1142); its one edge is `_compat`. | None needed | 0 | 65.2 |
| 2 | `digitalmodel._compat` | 2,276 | 1 | Root `__init__` installs the flat-to-group redirect finder; stdlib-only leaf. | None: a change here does affect every import | 0 | 65.2 |
| 3 | `digitalmodel.citations` (+ `schema`, `resolver`) | 624 | 30 | 30 modules (cathodic protection, asset integrity, `reporting.spec`) import the citation dataclasses at load. | Lazy `__init__` moves nothing (same unit). Even lazy imports in all 30 importers save 0 on the replay. Keep `schema` as the light interface | 0 | 62.7 (all importers lazy) |
| 4 | `digitalmodel._output_paths` | 490 | 3 | Leaf reached through `reporting.engine`. | Function-level import in `reporting.engine` | 0 | 63.1 |
| 5 | `digitalmodel.common.assumption_ledger` | 483 | 6 | Leaf reached through `reporting.provenance`, which `reporting/__init__` re-exports. | Function-level import in `reporting.provenance` | 0 | 64.1 |
| 6 | `digitalmodel.reporting` (12 submodules) | 481 | 14 | `__init__` re-exports 12 submodules; `spec`, `engine` and `provenance` pull in rows 3 to 5. | PEP 562 lazy re-exports in `reporting/__init__` | 0 | 61.7 |
| 7 | `digitalmodel.hydrodynamics` | 448 | 1 | `__init__` re-exports seven modules and, optionally, `capytaine`. | Lazy `capytaine` re-export | 0 | 63.3 |
| 8 | `digitalmodel.hydrodynamics.capytaine` | 448 | 1 | Reached only through row 7. | As row 7 | 0 | 63.3 |
| 9 | `digitalmodel.solvers` | 432 | 0 | One line, `from . import openfoam`, loads OpenFOAM and through it `gmsh_meshing` for every `digitalmodel.solvers.*` test. | Drop or lazy-load the `openfoam` attribute | 0 | 62.4 |
| 10 | `digitalmodel.solvers.gmsh_meshing` | 432 | 1 | `openfoam.gmsh_bridge` imports `_msh2_contract`, whose package `__init__` loads the mesh generator. | Row 9 cut; or split `_msh2_contract` out of the package | 0 | 62.4 (via row 9) |
| 11 | `digitalmodel.solvers.openfoam` | 432 | 2 | Reached through row 9; its `__init__` re-exports 13 modules. | Row 9 cut (lazy `openfoam/__init__` alone: 63.4) | 0 | 62.4 |
| 12 | `digitalmodel.solvers.orcaflex` | 288 | 5 | Genuine shared package (`yaml_utils`, `run_state`). | None recommended | 0 | 63.6 (all importers lazy) |
| 13 | `digitalmodel.materials` | 278 | 10 | Reach comes from its ten direct importers, not its `__init__`. | None recommended | 0 | 65.2 |
| 14 | `digitalmodel.hydrodynamics.diffraction` | 237 | 5 | `__init__` re-exports 23 modules, including solver adapters. | PEP 562 lazy re-exports | 0 | 62.0 |
| 15 | `digitalmodel.structural` | 224 | 1 | Parent of every `structural.*` module; no load-time imports. | None needed | 0 | 65.2 |
| 16 | `digitalmodel.citations.registry` | 171 | 10 | Naval-architecture modules read the registry at load. | None recommended | 0 | 65.2 |
| 17 | `digitalmodel.infrastructure.utils.private_paths` | 161 | 10 | Leaf path helper used at load by ten modules. | None recommended | 0 | 64.7 (all importers lazy) |
| 18 | `digitalmodel.ansys` | 149 | 4 | `__init__` re-exports 14 modules, but every reaching test imports the unit directly. | None needed | 0 | 65.2 |
| 19 | `digitalmodel.cathodic_protection` | 143 | 25 | `__init__` re-exports 26 modules. | PEP 562 lazy re-exports | 0 | 64.4 |
| 20 | `digitalmodel.fatigue` | 135 | 6 | `__init__` re-exports 16 own modules and 8 names from three `structural.fatigue` modules, so every importer of `digitalmodel.fatigue` (riser fatigue, signal-processing curves, VIV) reaches `structural.fatigue`. | **This PR:** PEP 562 lazy re-export of the 8 `structural.fatigue` names | **−111** | 64.5 |

## Why the cut in this PR is row 20

The selection target is the replay, so the cut chosen is the one that shrinks the replay most with a behaviour-neutral change. Among the 20 rows only row 20 changes the replay. Rows 1, 2, 15 and 18 have no cut to make. Rows 3, 12, 13, 16 and 17 are reached through their direct importers, so the only cut is lazy imports in every importer, and even that saves nothing on the replay. Rows 4 to 11, 14 and 19 have small, neutral cuts (rows 6, 9 and 14 lower the per-unit mean most) but no commit in the window changed a unit they route, so their shrink is 0 on this replay. They are the next candidates when the window is longer. Outside the top 20, lazy `__init__` files in `subsea.viv_analysis`, `hydrodynamics.hull_library` and `hydrodynamics.bemrosetta` save 64, 48 and 7 files on the replay.

The cut keeps the eight names in `__all__`, resolves them on first access through a module `__getattr__` (the pattern the root `__init__` already uses), caches them, and lists them in `__dir__`. Attribute access, `from digitalmodel.fatigue import X` and `import *` return the same objects. The only difference is that `import digitalmodel.fatigue` no longer loads `structural.fatigue`, which also removes the import cycle that `structural/fatigue/sn_curves.py` works around. `test_fatigue_migration.py` now checks the re-exports are the same objects as in `structural.fatigue` and imports them from there, so the static graph still routes `structural/fatigue/` changes to it.

## Method

1. **Graph.** Every `.py` file under `src/digitalmodel` is a module. Edges are the `import` and `from` statements that run when the module loads: the module body, class bodies and `if`/`try` blocks, not function bodies or lambdas. Relative imports are resolved against the package; a name resolves to its longest importable prefix. 2,867 modules. The graph code is copied from `scripts/ci/generate_owners.py` on the local branch `claude/m4-fast-gate-shadow` (not on main), with no dependency on that branch.
2. **Tests.** The test universe is the 2,276 files the domain gates select, resolved by `scripts/ci/orphan_tests.py`. A test reaches the modules it imports anywhere in the file, the modules named in its strings, the test helpers it imports, and the conftest files above it. Its closure adds load-time edges and parent `__init__` files.
3. **Fan-in and reach.** Fan-in counts direct load-time importers. Reach counts test files whose closure contains the module.
4. **Replay.** Source units and selection follow the m4 fast gate: a changed test selects itself, a source path selects the tests that reach its unit, and other paths fall back to the detector's domains. Changed paths are `git diff <commit>^ <commit>` for the 40 first-parent commits ending at the head above. The earlier figure of 161 used a different window.
5. **Cuts.** Each cut is simulated by removing the named edges, recomputing the closures and replaying. The per-unit mean averages, over every source unit with at least one reaching test, the number of test files that reach it.

Limits: imports by string (`importlib`, configuration-driven dispatch, the root redirect finder) are invisible to the graph, as they are to the gate; the shadow run measures those misses. Forty commits is a small window: a hub matters only when a commit touches a unit it routes.
