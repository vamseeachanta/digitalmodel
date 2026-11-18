# Regression audit and dependency contracts

The legal regression tests follow the documented manual-audit policy. The
floating-wind dependency tests compare installation pins with retained execution
metadata. VIV screening loads its fatigue backend only when the public fatigue
calculator is requested. These changes address the failures recorded in #3191.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-21 | Align audit and dependency tests with their current contracts. |

## Audit policy

The retirement notice in [legal-scanning.md](../legal-scanning.md) and merged
change #2283 supersede the automatic full-tree identifier gate. A historical
finding-count budget and a test requiring that retired CI step contradicted
that policy. Tests now exercise a manual scan of synthetic clean and flagged
content and check that the workflow retains the retirement contract.

The scanner implementation, deny rules, binary digest manifest, redaction tests,
and secret controls are unchanged. Synthetic scanner coverage does not establish
that the production tree is free of identifiers.

## Solver dependencies

The `fowt` extra in `pyproject.toml` pins the retained calculation environment.
The dependency test checks those pins against the committed execution metadata;
it does not require the current interpreter to contain optional solvers. The
CI-enrolment test still requires that the numerical suite install that extra.

The base Shapely requirement now starts at 2.1.0, the release that introduced
`constrained_delaunay_triangles`. That function is imported by the geometry
module when the workflow is imported, including when no solver is executed.
Source: [Shapely 2.1.0 API reference](https://shapely.readthedocs.io/en/2.1.0/reference/shapely.constrained_delaunay_triangles.html).
The exact solver-extra pins remain unchanged.

## Screening import boundary

Importing `digitalmodel.subsea.viv_analysis.screening` first executes its package
initializer. The initializer previously imported the fatigue calculator, which
reached `pylife` through the shared fatigue package. The VIV explorer therefore
needed the fatigue backend despite using only screening.

The package now resolves `VIVFatigueCalculator` on request, caches the successful
export, and retains that name in public discovery. Direct and package-level
fatigue imports remain supported. `pylife` is already declared as a runtime
dependency and remains required for the fatigue calculation path and its tests.
No engineering equation, coefficient, acceptance limit, or generated result is
changed.

## Worked import example

Inputs: the repository source tree, NumPy, and an interpreter in which `pylife`
is deliberately unavailable. Method: install a blocking import finder, then
import the public screening class in a fresh interpreter. This exercises the
same package-initialization path as the VIV explorer without running a solver.

```bash
PYTHONPATH=src python - <<'PY'
import importlib.abc
import sys

class UnavailableBackend(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] == 'pylife':
            raise ModuleNotFoundError(fullname, name=fullname)

sys.meta_path.insert(0, UnavailableBackend())
from digitalmodel.subsea.viv_analysis import VIVScreening
print(VIVScreening.__name__)
print('digitalmodel.subsea.viv_analysis.fatigue' in sys.modules)
print(any(name.split('.')[0] == 'pylife' for name in sys.modules))
PY
```

Expected answer:

```text
VIVScreening
False
False
```

`tests/subsea/viv_analysis/test_import_boundaries.py` also imports the explorer
with the backend blocked and tests the public lazy export with an isolated test
double. Numerical fatigue tests require the real backend. Full HTML freshness
checks require the other dependencies of every registered generator; this
example establishes only the screening import boundary.
