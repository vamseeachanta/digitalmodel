"""Tree-wide NumPy integration compatibility and independent hand integrals."""

import ast
import json
from pathlib import Path
from types import SimpleNamespace

import numpy as np
import pytest

ROOT = Path(__file__).resolve().parents[2]
FOLDERS = ("src/digitalmodel", "tests", "examples", "scripts", "docs/api")
FALLBACK_BINDING = '_trapezoid = getattr(np, "trapezoid", None) or np.trapz'
_trapezoid = getattr(np, "trapezoid", None) or np.trapz


def _sources():
    for folder in FOLDERS:
        for path in sorted((ROOT / folder).rglob("*")):
            if path.suffix == ".py":
                yield path.relative_to(ROOT).as_posix(), path.read_text(encoding="utf-8")
            elif path.suffix == ".ipynb":
                notebook = json.loads(path.read_text(encoding="utf-8"))
                source = "\n".join(
                    "".join(cell["source"])
                    for cell in notebook["cells"] if cell["cell_type"] == "code"
                )
                yield path.relative_to(ROOT).as_posix(), source


def _legacy_accesses(source):
    """Inspect syntax, leaving strings, comments and non-NumPy routines alone."""
    if "trapz" not in source:
        return []
    tree = ast.parse(source)
    aliases = {"np", "numpy"}
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            aliases.update(alias.asname or alias.name for alias in node.names
                           if alias.name == "numpy")
    lines = source.splitlines()
    bindings = [node for node in tree.body if isinstance(node, ast.Assign)
                and ast.get_source_segment(source, node) == FALLBACK_BINDING]
    allowed = {node.lineno for node in bindings} if len(bindings) == 1 else set()
    offenders = []
    for node in ast.walk(tree):
        if (isinstance(node, ast.Attribute) and node.attr == "trapz"
                and isinstance(node.value, ast.Name) and node.value.id in aliases):
            if node.lineno not in allowed or lines[node.lineno - 1] != FALLBACK_BINDING:
                offenders.append(node.lineno)
        elif (isinstance(node, ast.ImportFrom) and node.module == "numpy"
              and any(alias.name == "trapz" for alias in node.names)):
            offenders.append(node.lineno)
    return offenders


def test_cfd_sources_use_only_single_trapz_fallback():
    """All scoped Python and notebook code uses one module-level fallback."""
    offenders = [f"{name}:{line}" for name, source in _sources()
                 for line in _legacy_accesses(source)]
    assert not offenders, "\n".join(offenders)


@pytest.mark.parametrize("source", [
    "import numpy as numerical\nnumerical.trapz(y, x)",
    "from numpy import trapz as integrate\nintegrate(y, x)",
    "import numpy\nnumpy.trapz(y, x)",
    "def integrate():\n    " + FALLBACK_BINDING,
    FALLBACK_BINDING + "\n" + FALLBACK_BINDING,
])
def test_scan_rejects_legacy_access(source):
    assert _legacy_accesses(source)


def test_scan_allows_prose_other_libraries_and_one_binding():
    source = (FALLBACK_BINDING + '\n# np.trapz is historical prose\n'
              'description = "np.trapz(y, x)"\n'
              'from scipy import integrate\nintegrate.trapz(y, x)\n')
    assert not _legacy_accesses(source)


@pytest.mark.parametrize("available", ["legacy", "modern", "both"])
def test_binding_selects_available_api_without_eager_legacy_lookup(available):
    legacy, modern = object(), object()
    attrs = {}
    if available in ("legacy", "both"):
        attrs["trapz"] = legacy
    if available in ("modern", "both"):
        attrs["trapezoid"] = modern
    namespace = {"np": SimpleNamespace(**attrs)}
    exec(FALLBACK_BINDING, namespace)
    assert namespace["_trapezoid"] is (legacy if available == "legacy" else modern)


@pytest.mark.parametrize(
    "start, stop, slope, intercept, expected",
    [(0.0, 1.0, 1.0, 0.0, 0.5), (0.0, 3.0, 0.0, 2.0, 6.0)],
    ids=["linear-half", "constant-six"],
)
def test_trapezoid_hand_values_regression_pin(start, stop, slope, intercept, expected):
    """Hand integrals: [x²/2]₀¹ = 0.5; [2x]₀³ = 6.0 (synthetic inputs)."""
    x = np.linspace(start, stop, 11)
    y = slope * x + intercept
    assert _trapezoid(y, x) == pytest.approx(expected)


def test_trapezoid_axis_spacing_and_complex_hand_values():
    """Hand rule: sum Δx*(y_left+y_right)/2, NumPy reference, Notes/Examples.

    Synthetic x=[0,1,3], y=2x+1 gives 2+10=12; reverse order gives -12.
    With unit spacing the same y gives 2+5=7; dx=2 gives 14.
    Multiplication by (1+1j) gives 12+12j by linearity.
    Rule: NIST DLMF (2010), Eq. 3.5.1, applied to each linear segment.
    https://dlmf.nist.gov/3.5#E1
    API: https://numpy.org/doc/2.4/reference/generated/numpy.trapezoid.html
    """
    x = np.array([0.0, 1.0, 3.0])
    y = 2 * x + 1
    assert _trapezoid(y, x) == 12
    assert _trapezoid(y[::-1], x[::-1]) == -12
    assert _trapezoid(y, dx=2) == 14
    assert _trapezoid(y * (1 + 1j), x) == 12 + 12j
    np.testing.assert_array_equal(_trapezoid(np.stack([y, y]), x, axis=1), [12, 12])
