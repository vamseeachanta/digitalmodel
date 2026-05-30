"""Tests for diffraction inverse resolver."""

from __future__ import annotations

from dataclasses import fields
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
    AQWARunResult,
    AQWARunStatus,
)
from digitalmodel.hydrodynamics.diffraction.assumption_ledger import (
    AssumptionLedger,
    AssumptionSource,
    Confidence,
)
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
    RunResult,
    RunStatus,
)
from digitalmodel.hydrodynamics.diffraction.parametric_spec_generator import (
    estimate_cog,
    estimate_cog_from_dimensions,
    estimate_mass,
    estimate_mass_from_dimensions,
    estimate_radii_of_gyration,
    estimate_radii_of_gyration_from_dimensions,
)
from digitalmodel.hydrodynamics.diffraction.report_data_models import (
    DiffractionReportData,
)
from digitalmodel.hydrodynamics.diffraction.report_generator import (
    generate_diffraction_report,
)
from digitalmodel.hydrodynamics.diffraction.resolver import (
    ASSUMPTION_CONTROLLED_FIELDS,
    OUTCOME_REQUIREMENTS,
    Outcome,
    PrincipalDimensions,
    ResolverConfig,
    ResolverInputs,
    resolve,
)
from digitalmodel.hydrodynamics.hull_library.lookup import HullMatch
from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)


def _write_box_mesh(path: Path) -> Path:
    path.write_text(
        "\n".join(
            [
                "# box mesh",
                "0 0 -2",
                "10 0 -2",
                "10 4 -2",
                "0 4 -2",
                "0 0 0",
                "10 0 0",
                "10 4 0",
                "0 4 0",
            ]
        ),
        encoding="utf-8",
    )
    return path


def _detailed_partial(mesh: Path) -> dict:
    return {
        "vessel": {
            "name": "detailed",
            "geometry": {
                "mesh_file": str(mesh),
                "mesh_format": "gdf",
                "symmetry": "none",
                "reference_point": [0.0, 0.0, 0.0],
                "waterline_z": 0.0,
                "length_units": "m",
            },
            "inertia": {
                "mode": "free_floating",
                "mass": 10_000.0,
                "centre_of_gravity": [5.0, 0.0, -1.0],
                "radii_of_gyration": [1.0, 2.5, 2.6],
                "cog_z": -1.0,
            },
        },
        "environment": {
            "water_depth": 100.0,
            "water_density": 1025.0,
            "gravity": 9.80665,
        },
        "frequencies": {
            "input_type": "frequency",
            "range": {"start": 0.2, "end": 2.0, "count": 30},
        },
        "wave_headings": {
            "range": {"start": 0.0, "end": 180.0, "increment": 15.0},
            "symmetry": True,
        },
    }


class FakeLookup:
    def __init__(self, score: float) -> None:
        self.score = score

    def get_hull_form(self, target):  # noqa: ANN001
        return HullMatch(
            hull_id="FAKE",
            similarity_score=self.score,
            scaling_factors={},
            matched_entry={"displacement_t": 12.0},
            source="fake",
        )


def test_barebones_mesh_water_depth_resolves_and_ledgers(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")

    spec, ledger = resolve(
        Outcome.SHIP_RAOS,
        ResolverInputs(mesh_file=str(mesh), water_depth=50.0),
        hull_lookup=FakeLookup(0.1),
    )

    assert isinstance(spec, DiffractionSpec)
    assert spec.vessel.geometry.mesh_file == str(mesh)
    assert spec.vessel.inertia.mass > 0
    assert any(r.field == "vessel.principal_dimensions" for r in ledger)


def test_partial_dimensions_avoid_mesh_dimension_derivation(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")

    spec, ledger = resolve(
        "ship_raos",
        ResolverInputs(
            dimensions=PrincipalDimensions(loa=20.0, beam=5.0, draft=2.0),
            mesh_file=str(mesh),
            water_depth=80.0,
            partial_spec={"vessel": {"name": "partial"}},
        ),
        hull_lookup=FakeLookup(0.1),
    )

    assert spec.vessel.name == "partial"
    assert not any(r.field == "vessel.principal_dimensions" for r in ledger)


def test_detailed_run_has_empty_ledger_and_report_empty_state(
    tmp_path: Path,
) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")

    spec, ledger = resolve(
        Outcome.SHIP_RAOS,
        ResolverInputs(partial_spec=_detailed_partial(mesh)),
    )

    assert spec.vessel.name == "detailed"
    assert len(ledger) == 0
    out = tmp_path / "report.html"
    generate_diffraction_report(
        DiffractionReportData(vessel_name="detailed", mode="compact"),
        out,
        assumption_ledger=ledger,
    )
    text = out.read_text(encoding="utf-8")
    assert 'id="assumptions"' in text
    assert "No assumed values" in text


def test_lookup_threshold_falls_through_below_near_threshold(
    tmp_path: Path,
) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")
    dims = PrincipalDimensions(loa=10.0, beam=4.0, draft=2.0)

    _, high_ledger = resolve(
        Outcome.SHIP_RAOS,
        ResolverInputs(dimensions=dims, mesh_file=str(mesh), water_depth=50.0),
        hull_lookup=FakeLookup(0.99),
    )
    _, low_ledger = resolve(
        Outcome.SHIP_RAOS,
        ResolverInputs(dimensions=dims, mesh_file=str(mesh), water_depth=50.0),
        hull_lookup=FakeLookup(0.1),
    )

    high_mass = [
        r for r in high_ledger if r.field == "vessel.inertia.mass"
    ][0]
    low_mass = [r for r in low_ledger if r.field == "vessel.inertia.mass"][0]
    assert high_mass.source == AssumptionSource.DATABASE_LOOKUP
    assert high_mass.confidence == Confidence.HIGH
    assert low_mass.source == AssumptionSource.ESTIMATED_FROM_DATA


def test_dual_backend_ledger_field_and_report_render(tmp_path: Path) -> None:
    ledger = AssumptionLedger()
    ledger.record(
        "frequencies.range",
        {"start": 0.2},
        AssumptionSource.ASSUMED_DEFAULT,
        "default",
        Confidence.LOW,
    )

    assert "assumption_ledger" in {f.name for f in fields(RunResult)}
    assert "assumption_ledger" in {f.name for f in fields(AQWARunResult)}
    assert (
        RunResult(status=RunStatus.PENDING, assumption_ledger=ledger)
        .assumption_ledger
        is ledger
    )
    assert (
        AQWARunResult(
            status=AQWARunStatus.PENDING, assumption_ledger=ledger
        ).assumption_ledger
        is ledger
    )

    out = tmp_path / "report.html"
    generate_diffraction_report(
        DiffractionReportData(vessel_name="vessel", mode="compact"),
        out,
        assumption_ledger=ledger,
    )
    assert 'id="assumptions"' in out.read_text(encoding="utf-8")


def test_inertia_mode_free_floating_vs_explicit(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")
    dims = PrincipalDimensions(loa=10.0, beam=4.0, draft=2.0)

    free_spec, free_ledger = resolve(
        Outcome.SHIP_RAOS,
        ResolverInputs(
            dimensions=dims,
            mesh_file=str(mesh),
            water_depth=50.0,
            inertia_mode="free_floating",
        ),
        hull_lookup=FakeLookup(0.1),
    )
    explicit_spec, explicit_ledger = resolve(
        Outcome.SHIP_RAOS,
        ResolverInputs(
            dimensions=dims,
            mesh_file=str(mesh),
            water_depth=50.0,
            inertia_mode="explicit",
        ),
        hull_lookup=FakeLookup(0.1),
    )

    assert free_spec.vessel.inertia.radii_of_gyration is not None
    assert explicit_spec.vessel.inertia.radii_of_gyration is None
    assert explicit_spec.vessel.inertia.inertia_tensor is not None
    assert any(r.field == "vessel.inertia.radii_of_gyration" for r in free_ledger)
    assert not any(
        r.field == "vessel.inertia.radii_of_gyration" for r in explicit_ledger
    )


def test_no_circular_import_when_hull_library_imported_first() -> None:
    """Guard against the resolver re-introducing a circular import.

    The diffraction package eagerly imports ``resolver``; if ``resolver``
    imported ``parametric_spec_generator`` / ``hull_library`` at module load,
    the chain hull_library.mesh_generator -> bemrosetta -> diffraction.__init__
    -> resolver -> parametric_spec_generator -> hull_library.mesh_generator
    (half-initialised) raises ImportError. Those edges are lazy; this checks it
    in a fresh interpreter (same-process import caching would mask the cycle).
    """
    import subprocess
    import sys

    code = (
        "import digitalmodel.hydrodynamics.hull_library.mesh_generator\n"
        "import digitalmodel.hydrodynamics.diffraction\n"
        "from digitalmodel.hydrodynamics.diffraction import resolve, Outcome\n"
    )
    result = subprocess.run(
        [sys.executable, "-c", code], capture_output=True, text=True
    )
    assert result.returncode == 0, result.stderr


def test_explicit_inertia_tensor_origin_is_centre_of_mass(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")
    spec, _ = resolve(
        Outcome.SHIP_RAOS,
        ResolverInputs(
            dimensions=PrincipalDimensions(loa=100.0, beam=20.0, draft=10.0),
            mesh_file=str(mesh),
            water_depth=100.0,
            inertia_mode="explicit",
        ),
        hull_lookup=FakeLookup(0.1),
    )

    # Empirical radii of gyration are CG-relative, so the estimated diagonal
    # tensor must be labelled centre_of_mass, not the schema default
    # body_origin (which would mis-state the reference for an offset CoG).
    assert spec.vessel.inertia.inertia_tensor is not None
    assert spec.vessel.inertia.inertia_tensor_origin == "centre_of_mass"


def test_ledger_travels_through_run_entrypoints(tmp_path: Path) -> None:
    from digitalmodel.hydrodynamics.diffraction.aqwa_runner import run_aqwa
    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
        run_orcawave,
    )

    mesh = _write_box_mesh(tmp_path / "box.gdf")
    spec, ledger = resolve(
        Outcome.SHIP_RAOS,
        ResolverInputs(mesh_file=str(mesh), water_depth=50.0),
        hull_lookup=FakeLookup(0.1),
    )
    assert len(ledger) > 0

    # The provenance ledger must ride the result from a real run entry point
    # (dry-run, license-free) so it reaches report generation — not only when
    # a caller hand-constructs the result object.
    orca_result = run_orcawave(
        spec,
        output_dir=tmp_path / "orca",
        dry_run=True,
        assumption_ledger=ledger,
    )
    assert orca_result.assumption_ledger is ledger

    aqwa_result = run_aqwa(
        spec,
        output_dir=tmp_path / "aqwa",
        dry_run=True,
        assumption_ledger=ledger,
    )
    assert aqwa_result.assumption_ledger is ledger


def test_mesh_units_symmetry_and_draft_use_waterline(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "half_box.gdf")

    spec, ledger = resolve(
        Outcome.SHIP_RAOS,
        ResolverInputs(
            mesh_file=str(mesh),
            water_depth=50.0,
            partial_spec={
                "vessel": {
                    "geometry": {
                        "mesh_file": str(mesh),
                        "symmetry": "xz",
                        "length_units": "ft",
                        "waterline_z": 0.0,
                    }
                }
            },
        ),
        hull_lookup=FakeLookup(0.1),
    )

    assert spec.vessel.inertia.centre_of_gravity[0] == pytest.approx(1.524)
    assert spec.vessel.inertia.centre_of_gravity[2] == pytest.approx(-0.3048)
    dims_record = [
        r for r in ledger if r.field == "vessel.principal_dimensions"
    ][0]
    assert dims_record.value["beam"] == pytest.approx(2.4384)
    assert dims_record.value["draft"] == pytest.approx(0.6096)


def test_mesh_dimension_derivation_fail_closed(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")

    with pytest.raises(ValueError, match="unknown length_units"):
        resolve(
            Outcome.SHIP_RAOS,
            ResolverInputs(
                mesh_file=str(mesh),
                water_depth=50.0,
                partial_spec={
                    "vessel": {
                        "geometry": {
                            "mesh_file": str(mesh),
                            "length_units": "furlong",
                        }
                    }
                },
            ),
        )


def test_estimator_dimension_functions_match_profile_wrappers() -> None:
    profile = HullProfile(
        name="profile",
        hull_type=HullType.BARGE,
        stations=[
            HullStation(x_position=0.0, waterline_offsets=[(0.0, 1.0)]),
            HullStation(x_position=10.0, waterline_offsets=[(0.0, 1.0)]),
        ],
        length_bp=10.0,
        beam=4.0,
        draft=2.0,
        depth=3.0,
        source="test",
        block_coefficient=0.8,
        displacement=None,
    )
    dims = PrincipalDimensions(
        length_bp=10.0,
        beam=4.0,
        draft=2.0,
        block_coefficient=0.8,
    )

    assert estimate_mass(profile) == pytest.approx(
        estimate_mass_from_dimensions(dims)
    )
    assert estimate_cog(profile) == estimate_cog_from_dimensions(dims)
    assert estimate_radii_of_gyration(
        profile
    ) == estimate_radii_of_gyration_from_dimensions(dims)


def test_assumption_controlled_manifest_is_ledgered(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")
    _, ledger = resolve(
        Outcome.QTF,
        ResolverInputs(mesh_file=str(mesh), water_depth=50.0),
        hull_lookup=FakeLookup(0.1),
    )

    recorded = {record.field for record in ledger}
    expected = {
        "vessel.geometry.mesh_format",
        "vessel.geometry.symmetry",
        "vessel.geometry.reference_point",
        "vessel.geometry.waterline_z",
        "vessel.geometry.length_units",
        "vessel.inertia.mass",
        "vessel.inertia.centre_of_gravity",
        "vessel.inertia.radii_of_gyration",
        "vessel.inertia.cog_z",
        "environment.water_density",
        "environment.gravity",
        "frequencies.range",
        "wave_headings.range",
        "solver_options.qtf_calculation",
    }
    assert expected <= recorded
    assert expected <= set(ASSUMPTION_CONTROLLED_FIELDS)


def test_outcome_requirements_are_data_driven(tmp_path: Path) -> None:
    mesh = _write_box_mesh(tmp_path / "box.gdf")
    spec, _ = resolve(
        Outcome.QTF,
        ResolverInputs(mesh_file=str(mesh), water_depth=50.0),
        hull_lookup=FakeLookup(0.1),
    )

    assert "solver_options.qtf_calculation" in OUTCOME_REQUIREMENTS[Outcome.QTF]
    assert spec.solver_options.qtf_calculation is True
