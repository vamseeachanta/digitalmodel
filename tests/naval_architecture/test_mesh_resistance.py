# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent box calculation; no towing-tank accuracy claim.

Synthetic inputs: L=10, B=2, depth=2, draft=1 m; U=1 m/s,
rho=1000 kg/m^3, nu=1e-5 m^2/s, g=10 m/s^2 (declared test values).
S=LB+2LT+2BT=44 m^2, V=LBT=20 m^3. Re=UL/nu=1e6.
ITTC 7.5-03-02-04 (2024 rev 02), section 4: CF=.075/(6-2)^2
=.0046875. RF=CF*rho*U^2*S/2=103.125 N; PF=RF*U=103.125 W.
With declared k=.2, k*RF=20.625 N. Numerical comparison tolerance
1e-12 is a synthetic arithmetic criterion, not a physical acceptance limit.
"""

from dataclasses import replace
import json
from pathlib import Path
import runpy

import pytest

from digitalmodel.citations.schema import CitationResolutionError
from digitalmodel.naval_architecture.hull_fixtures import box_mesh
from digitalmodel.naval_architecture.mesh_hydrostatics import TriMesh
from digitalmodel.naval_architecture.mesh_resistance import (
    FlowConditions,
    mesh_friction,
)


@pytest.fixture
def conditions():
    return FlowConditions(1, 1000, 1e-5, 10, 1, 0, None, "calm", "smooth", "absent")


def mesh(units="m"):
    vertices, faces = box_mesh(10, 2, 2)
    if units == "mm":
        vertices *= 1000  # SI prefix milli, exact unit conversion.
    return TriMesh(vertices, faces, units=units, axes=("forward", "port", "up"))


def run(conditions, **kwargs):
    return mesh_friction(
        mesh(),
        conditions,
        form_factor_k=0.2,
        implementation_revision="synthetic-revision",
        cite=False,
        **kwargs,
    )


def test_hand_calculation_and_missing_total(conditions):
    result = run(conditions)
    assert result["geometry"]["S"]["value"] == pytest.approx(44, abs=1e-12)
    assert result["geometry"]["V"]["value"] == pytest.approx(20, abs=1e-12)
    assert result["reynolds"] == pytest.approx(1e6)
    assert result["froude"] == pytest.approx(0.1)
    assert result["components"]["friction"]["force_N"] == pytest.approx(103.125)
    assert result["components"]["viscous_form"]["force_N"] == pytest.approx(20.625)
    assert result["friction_power_W"] == pytest.approx(103.125)
    for name in ("wave", "total"):
        assert result["components"][name]["status"] == "NOT_EVALUATED"
        assert result["components"][name]["force_N"] is None
    assert result["effective_power_W"] is None
    assert result["shaft_power_W"] is None
    assert result["validation_status"] == "UNVALIDATED"
    assert result["citation_status"] == "UNRESOLVED"
    assert result["unresolved_citations"]
    json.dumps(result, allow_nan=False)


def test_units_and_deterministic_identity(conditions):
    first = run(conditions)
    same = run(conditions)
    metric = mesh_friction(
        mesh("mm"),
        conditions,
        form_factor_k=0.2,
        implementation_revision="synthetic-revision",
        cite=False,
    )
    assert first == same
    assert first["components"] == metric["components"]
    assert first["input_hash"] != metric["input_hash"]  # Declared unit provenance.
    assert first["input_hash"] != run(replace(conditions, speed_m_s=2))["input_hash"]
    changed = mesh_friction(
        mesh(),
        conditions,
        form_factor_k=None,
        implementation_revision="synthetic-revision",
        cite=False,
    )
    assert changed["input_hash"] != first["input_hash"]
    assert changed["components"]["viscous_form"]["force_N"] is None


@pytest.mark.parametrize(
    "field,value",
    [
        ("speed_m_s", 0),
        ("density_kg_m3", -1),
        ("viscosity_m2_s", 0),
        ("gravity_m_s2", float("nan")),
        ("draft_m", -1),
        ("trim_m", float("inf")),
        ("water_depth_m", 0.5),
        ("free_surface", "waves"),
        ("roughness", "unknown"),
        ("appendages", "present"),
    ],
)
def test_conditions_fail_closed(conditions, field, value):
    with pytest.raises(ValueError):
        replace(conditions, **{field: value})


@pytest.mark.parametrize("k", [-0.1, float("nan"), float("inf")])
def test_invalid_form_factor(conditions, k):
    with pytest.raises(ValueError):
        mesh_friction(
            mesh(),
            conditions,
            form_factor_k=k,
            implementation_revision="synthetic-revision",
            cite=False,
        )


def test_configured_missing_citation_is_not_silenced(conditions, tmp_path):
    with pytest.raises(CitationResolutionError):
        mesh_friction(
            mesh(),
            conditions,
            form_factor_k=None,
            implementation_revision="synthetic-revision",
            repo_root=tmp_path,
        )


def test_provenance_required(conditions):
    with pytest.raises(ValueError, match="revision"):
        mesh_friction(
            mesh(),
            conditions,
            form_factor_k=None,
            implementation_revision="",
            cite=False,
        )


def test_resolved_sidecar_round_trips(conditions, tmp_path):
    from digitalmodel.citations.registry import _EN400_CITATION_TEMPLATE

    template = _EN400_CITATION_TEMPLATE
    target = tmp_path / template["wiki_path"]
    target.parent.mkdir(parents=True)
    target.write_text(
        "---\n"
        + "\n".join(
            f"{key}: {template[key]}" for key in ("code_id", "publisher", "revision")
        )
        + "\n---\nSynthetic citation-resolution fixture.\n"
    )
    result = mesh_friction(
        mesh(),
        conditions,
        form_factor_k=None,
        implementation_revision="synthetic-revision",
        repo_root=tmp_path,
    )
    assert result["citation_status"] == "RESOLVED"
    assert result["citations"][0]["code_id"] == template["code_id"]
    assert result["unresolved_citations"] == []
    json.dumps(result, allow_nan=False)


def test_example_complete_flow():
    example = (
        Path(__file__).resolve().parents[2]
        / "examples/naval_architecture/mesh_resistance_example.py"
    )
    result = runpy.run_path(str(example))["worked_example"]()
    assert result["arithmetic_comparison"]["status"] == "PASS"
    assert result["resistance"]["friction_power_W"] == pytest.approx(103.125)
    assert result["analytical_evidence"]["status"] == "NOT_EVALUATED"


def test_overflowing_form_force_refused(conditions):
    with pytest.raises(ValueError, match="finite"):
        mesh_friction(
            mesh(),
            conditions,
            form_factor_k=1e308,
            implementation_revision="synthetic-revision",
            cite=False,
        )


def test_underflowing_derived_regime_refused(conditions):
    with pytest.raises(ValueError, match="positive"):
        run(replace(conditions, gravity_m_s2=1e308))
