"""Tests for polar_force_overlay module (digitalmodel#616).

Test plan rows 1-8, 12, 16, 17 (see docs/plans/2026-05-20-issue-616-*.md §TDD).
"""
from __future__ import annotations

import ast
import pathlib
import subprocess

import pandas as pd
import plotly.graph_objects as go
import pytest

from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile, HullStation, HullType
from digitalmodel.marine_ops.marine_engineering.visualization import (
    FrameConvention,
    ForceArrowKind,
    RadialAxisMode,
    VesselSilhouetteSpec,
    polar_force_overlay,
)
from digitalmodel.marine_ops.marine_engineering.visualization._convention import (
    OCIMF_CONVENTION_AUTHORITY,
)
from digitalmodel.marine_ops.marine_engineering.visualization.polar_force_overlay import (
    _resolve_arrow_direction_in_body_frame,
)


# ---------- Fixtures ----------

def _make_hull_profile(length_bp_m: float = 300.0, beam_m: float = 50.0) -> HullProfile:
    """Construct a minimal valid HullProfile for testing.

    NOTE (plan r1 M2 § Risks Open HullProfile-field-confirmation gate fired here):
    HullProfile uses `length_bp` (Length Between Perpendiculars), NOT LOA. For visual
    silhouette rendering at polar-chart scale the LOA-vs-LBP distinction is small
    (typically LBP ≈ 0.95–0.98 × LOA for tankers). The module uses length_bp as
    the silhouette scale parameter; downstream consumers requiring LOA can multiply
    by their vessel's LOA/LBP ratio.

    Cross-validators in HullProfile enforce station y ≤ beam/2 + 5% tolerance
    and z ≤ depth + 5%. Fixture below builds valid minimal stations.
    """
    half_beam = beam_m / 2.0
    return HullProfile(
        name="test-tanker",
        hull_type=HullType.TANKER,
        length_bp=length_bp_m,
        beam=beam_m,
        draft=20.0,
        depth=25.0,
        source="test fixture for digitalmodel#616",
        stations=[
            HullStation(x_position=0.0,
                        waterline_offsets=[(0.0, 0.0), (20.0, half_beam * 0.5)]),
            HullStation(x_position=length_bp_m / 2.0,
                        waterline_offsets=[(0.0, 0.0), (20.0, half_beam)]),
            HullStation(x_position=length_bp_m,
                        waterline_offsets=[(0.0, 0.0), (20.0, half_beam * 0.5)]),
        ],
    )


def _make_silhouette_spec(length_bp_m: float = 300.0, beam_m: float = 50.0,
                          silhouette_kind: str = "tanker") -> VesselSilhouetteSpec:
    return VesselSilhouetteSpec(
        hull_profile=_make_hull_profile(length_bp_m, beam_m),
        silhouette_kind=silhouette_kind,
    )


def _df_long(headings=(0, 45, 90, 135, 180), value: float = 1.0, component: str = "Y") -> pd.DataFrame:
    """Build a long-format DataFrame: (theta_deg, value, component)."""
    return pd.DataFrame({
        "theta_deg": list(headings),
        "value": [value] * len(headings),
        "component": [component] * len(headings),
    })


def _df_wide(headings=(0, 45, 90, 135, 180)) -> pd.DataFrame:
    """Build a wide-format DataFrame: (theta_deg, fx, fy, fz, mx, my, mz)."""
    return pd.DataFrame({
        "theta_deg": list(headings),
        "fx": [0.5] * len(headings),
        "fy": [1.0] * len(headings),
        "fz": [0.0] * len(headings),
        "mx": [0.0] * len(headings),
        "my": [0.0] * len(headings),
        "mz": [0.1] * len(headings),
    })


# ---------- TDD #1: long-format schema accepted ----------

def test_input_schema_long_format_accepted():
    fig = polar_force_overlay(_df_long(), _make_silhouette_spec())
    assert isinstance(fig, go.Figure)


# ---------- TDD #2: wide-format schema accepted ----------

def test_input_schema_wide_format_accepted():
    fig = polar_force_overlay(_df_wide(), _make_silhouette_spec())
    assert isinstance(fig, go.Figure)


# ---------- TDD #3: invalid schema raises ----------

def test_input_schema_missing_theta_raises():
    bad = pd.DataFrame({"value": [1.0, 2.0], "component": ["Y", "Y"]})
    with pytest.raises(ValueError, match=r"theta_deg"):
        polar_force_overlay(bad, _make_silhouette_spec())


# ---------- TDD #4: theta range validation ----------

def test_input_theta_out_of_range_raises():
    bad = pd.DataFrame({
        "theta_deg": [0.0, 370.0, 90.0],  # 370 out of [0, 360)
        "value": [1.0, 1.0, 1.0],
        "component": ["Y", "Y", "Y"],
    })
    with pytest.raises(ValueError, match=r"theta_deg.*\[0.*360"):
        polar_force_overlay(bad, _make_silhouette_spec())


# ---------- TDD #5: positive Cy arrow direction property (citation-bound) ----------

def test_arrow_direction_ocimf_positive_cy_property():
    """Per plan r1 M1: expected value is derived from OCIMF_CONVENTION_AUTHORITY,
    NOT pinned in the test source. Authority itself is bound by OCIMF MEG3/MEG4
    Annex A citation in _convention.py.
    """
    expected = OCIMF_CONVENTION_AUTHORITY.positive_cy_arrow_at_starboard_incidence_deg()
    assert expected in (90.0, 270.0), (
        f"authority returned {expected}; OCIMF body-fixed lateral must be one of "
        f"{{90.0 (+Y starboard), 270.0 (-Y port)}}"
    )
    actual = _resolve_arrow_direction_in_body_frame(
        theta_incidence_deg=90.0,
        component_sign=+1,
        frame_convention=FrameConvention.INCIDENCE_HEADING_BODY_FIXED,
    )
    assert actual == expected, (
        f"positive Cy at θ=90° produced direction {actual}; expected {expected} "
        f"per OCIMF MEG3/MEG4 convention citation: {OCIMF_CONVENTION_AUTHORITY.citation_text()}"
    )


# ---------- TDD #6: negative Cy arrow direction property (citation-bound) ----------

def test_arrow_direction_ocimf_negative_cy_property():
    expected = OCIMF_CONVENTION_AUTHORITY.negative_cy_arrow_at_starboard_incidence_deg()
    actual = _resolve_arrow_direction_in_body_frame(
        theta_incidence_deg=90.0,
        component_sign=-1,
        frame_convention=FrameConvention.INCIDENCE_HEADING_BODY_FIXED,
    )
    assert actual == expected


# ---------- TDD #7: INERTIAL frame passes theta through unchanged ----------

def test_frame_inertial_passthrough():
    result = _resolve_arrow_direction_in_body_frame(
        theta_incidence_deg=135.0,
        component_sign=+1,
        frame_convention=FrameConvention.FORCE_DIRECTION_INERTIAL,
    )
    assert result == 135.0


# ---------- TDD #8: figure contains silhouette trace with transparent fill ----------

def test_figure_contains_silhouette_trace_transparent():
    fig = polar_force_overlay(_df_long(), _make_silhouette_spec())
    # Find any trace with fill='toself' (silhouette polygon)
    silhouette_traces = [t for t in fig.data if getattr(t, "fill", None) == "toself"]
    assert len(silhouette_traces) >= 1, "no silhouette polygon trace found (fill='toself')"
    sil = silhouette_traces[0]
    opacity = getattr(sil, "opacity", None)
    assert opacity is not None and 0.0 < opacity <= 0.30, (
        f"silhouette opacity must be in (0, 0.30]; got {opacity}"
    )


# ---------- TDD #12: arrow density respects sample step ----------

def test_arrow_density_respects_sample_step():
    """With arrow_sample_step_deg=30, at most 12 arrows over 0-360."""
    dense = pd.DataFrame({
        "theta_deg": list(range(0, 360, 5)),  # every 5° → dense data
        "value": [1.0] * 72,
        "component": ["Y"] * 72,
    })
    fig = polar_force_overlay(
        dense, _make_silhouette_spec(),
        force_arrow_kind=ForceArrowKind.LATERAL_ONLY,
        arrow_sample_step_deg=30.0,
    )
    # Count traces that represent arrows (shafts are line traces, heads are triangle markers).
    # Per the spike, each arrow contributes 2 traces. Bound the count to ≤ 12 arrows × 2 = 24 arrow-traces.
    arrow_traces = [t for t in fig.data if getattr(t.marker, "symbol", None) == "triangle-up"]
    assert len(arrow_traces) <= 12, f"got {len(arrow_traces)} arrow-head traces; expected ≤ 12"


# ---------- TDD #16: arrow direction 180° invariant under sign flip ----------

@pytest.mark.parametrize("theta", [0.0, 45.0, 90.0, 135.0, 180.0])
def test_arrow_direction_180_degree_invariant_under_sign_flip(theta: float):
    """Property: flipping the sign of Cy flips the arrow direction by 180° (mod 360)."""
    dir_plus = _resolve_arrow_direction_in_body_frame(
        theta_incidence_deg=theta,
        component_sign=+1,
        frame_convention=FrameConvention.INCIDENCE_HEADING_BODY_FIXED,
    )
    dir_minus = _resolve_arrow_direction_in_body_frame(
        theta_incidence_deg=theta,
        component_sign=-1,
        frame_convention=FrameConvention.INCIDENCE_HEADING_BODY_FIXED,
    )
    diff = abs((dir_plus - dir_minus) % 360.0)
    # 180 or 180 (mod 360); accept 180.0 exactly
    assert diff == 180.0, f"at θ={theta}: dir_plus={dir_plus}, dir_minus={dir_minus}, diff={diff}; expected 180"


# ---------- TDD #17: only ONE @dataclass declared in visualization module ----------

def test_visualization_module_declares_exactly_one_dataclass():
    """Per plan r1 M2 + r2 n3: the visualization module exists to NOT duplicate
    HullProfile-overlapping geometry. Enforce by AST analysis: across all .py
    files in the visualization package, exactly 1 class is decorated with @dataclass
    (or @dataclass(...)), and that class is VesselSilhouetteSpec.
    """
    pkg_dir = pathlib.Path(__file__).resolve().parents[3] \
        / "src" / "digitalmodel" / "marine_ops" / "marine_engineering" / "visualization"
    # The test file lives in tests/marine_ops/marine_engineering/visualization/, so we walk up
    # to repo root differently. Use git rev-parse to find repo root reliably.
    repo_root = pathlib.Path(subprocess.check_output(
        ["git", "rev-parse", "--show-toplevel"], text=True
    ).strip())
    pkg_dir = repo_root / "src" / "digitalmodel" / "marine_ops" / "marine_engineering" / "visualization"
    assert pkg_dir.is_dir(), f"visualization package not found at {pkg_dir}"

    dataclass_decls: list[tuple[str, str]] = []
    for py in sorted(pkg_dir.glob("*.py")):
        tree = ast.parse(py.read_text())
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                for dec in node.decorator_list:
                    name = None
                    if isinstance(dec, ast.Name):
                        name = dec.id
                    elif isinstance(dec, ast.Call) and isinstance(dec.func, ast.Name):
                        name = dec.func.id
                    elif isinstance(dec, ast.Attribute):
                        name = dec.attr
                    if name == "dataclass":
                        dataclass_decls.append((py.name, node.name))

    assert len(dataclass_decls) == 1, (
        f"expected exactly 1 @dataclass across visualization package; got {len(dataclass_decls)}: "
        f"{dataclass_decls}"
    )
    file, classname = dataclass_decls[0]
    assert classname == "VesselSilhouetteSpec", (
        f"the sole @dataclass must be VesselSilhouetteSpec; got {classname} in {file}"
    )
    assert file == "types.py", (
        f"VesselSilhouetteSpec should live in types.py; found in {file}"
    )
