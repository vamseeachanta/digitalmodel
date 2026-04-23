from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path
from typing import Any

from digitalmodel.solvers.orcaflex.reporting import OrcaFlexAnalysisReport, generate_orcaflex_report
from digitalmodel.solvers.orcaflex.reporting.models import (
    BCData,
    BCEndData,
    EnvironmentData,
    GeometryData,
    HydroCoeffData,
    KeyPointData,
    LineProfileData,
    LineTypeData,
    LoadCaseData,
    MaterialData,
    MeshData,
    MeshQualityData,
    SeabedModelData,
    SegmentData,
)

FIXTURES_ROOT = Path(__file__).resolve().parents[3] / "fixtures"
REPORTING_FIXTURES_ROOT = FIXTURES_ROOT / "reporting"
FIXTURE_REPORT_DATE = datetime(2025, 8, 14)


def load_json(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as f:
        return json.load(f)


def fixture_metadata_path(fixture_name: str) -> Path:
    return REPORTING_FIXTURES_ROOT / f"{fixture_name}.metadata.json"


def fixture_snapshot_path(fixture_name: str) -> Path:
    return REPORTING_FIXTURES_ROOT / f"{fixture_name}.report.snapshot.html"


def minimal_fixture_metadata_path() -> Path:
    return fixture_metadata_path("minimal_test")


def minimal_fixture_snapshot_path() -> Path:
    return fixture_snapshot_path("minimal_test")


def fpso_fixture_metadata_path() -> Path:
    return fixture_metadata_path("fpso_turret")


def fpso_fixture_snapshot_path() -> Path:
    return fixture_snapshot_path("fpso_turret")


def load_fixture_metadata(fixture_name: str) -> dict[str, Any]:
    return load_json(fixture_metadata_path(fixture_name))


def load_minimal_fixture_metadata() -> dict[str, Any]:
    return load_fixture_metadata("minimal_test")


def load_fpso_fixture_metadata() -> dict[str, Any]:
    return load_fixture_metadata("fpso_turret")


def _build_geometry(metadata: dict[str, Any]) -> GeometryData:
    water_depth = metadata["environment"].get("water_depth_m")
    key_points = [
        KeyPointData(label="Hang-off", arc_length_m=0.0, x=0.0, y=0.0, z=0.0),
        KeyPointData(label="Anchor", arc_length_m=250.0, x=0.0, y=0.0, z=-100.0),
    ]
    if metadata["fixture"]["name"] == "fpso_turret":
        key_points = [
            KeyPointData(label="Turret buoy", arc_length_m=0.0, x=0.0, y=0.0, z=-15.0),
            KeyPointData(label="Vessel", arc_length_m=50.0, x=0.0, y=0.0, z=0.0),
            KeyPointData(label="Leg1 anchor", arc_length_m=250.0, x=120.0, y=0.0, z=-80.0),
        ]

    return GeometryData(
        water_depth_m=water_depth,
        line_profile=LineProfileData(
            arc_length=[0.0, 125.0, 250.0],
            x=[0.0, 0.0, 0.0],
            y=[0.0, 0.0, 0.0],
            z=[0.0, -50.0, -100.0],
        ),
        key_points=key_points,
    )


def _build_materials(metadata: dict[str, Any]) -> MaterialData:
    line_types_meta = metadata["objects"].get("line_types") or [{"name": "Chain"}]
    line_types = []
    weight_profile = [1.0] * len(line_types_meta)
    for idx, lt_meta in enumerate(line_types_meta[:4]):
        line_types.append(
            LineTypeData(
                name=lt_meta.get("name", f"LineType{idx+1}"),
                od=float(lt_meta.get("od", 0.10)),
                id=float(lt_meta.get("id", 0.0)),
                wt=float(lt_meta.get("wt", max(float(lt_meta.get("od", 0.10)) / 2.0, 0.01))),
                grade=lt_meta.get("grade", "N/A"),
                youngs_modulus_mpa=float(lt_meta.get("youngs_modulus_mpa", 210000.0)),
                density_kg_m3=float(lt_meta.get("density_kg_m3", 7850.0)),
                ea_kn=float(lt_meta.get("ea_kn", 1000.0 + idx * 100.0)),
                ei_knm2=float(lt_meta.get("ei_knm2", 100.0 + idx * 10.0)),
                gj_knm2=float(lt_meta.get("gj_knm2", 50.0 + idx * 5.0)),
                mass_per_m_kg_m=float(lt_meta.get("mass_per_m_kg_m", 150.0 + idx * 10.0)),
            )
        )
    return MaterialData(
        line_types=line_types,
        submerged_weight_profile={
            "arc_length": [float(i) * 50.0 for i in range(len(weight_profile))],
            "w_s": weight_profile,
        },
    )


def _build_boundary_conditions(metadata: dict[str, Any]) -> BCData:
    line = metadata["objects"]["lines"][0]
    vessel = metadata["objects"]["vessels"][0]["name"]
    water_depth = metadata["environment"].get("water_depth_m", 100.0)
    end_a_type = line.get("end_a_type", "Fixed")
    end_b_type = line.get("end_b_type", "Vessel")
    return BCData(
        end_a=BCEndData(
            name="End A",
            type=end_a_type,
            x=0.0,
            y=0.0,
            z=-float(water_depth),
            connected_to=line.get("end_a_connection", "Anchored"),
            dof_fixity="All Fixed" if end_a_type == "Fixed" else None,
        ),
        end_b=BCEndData(
            name="End B",
            type=end_b_type,
            x=0.0,
            y=0.0,
            z=0.0,
            connected_to=line.get("end_b_connection", vessel),
            dof_fixity="Vessel Coupled" if end_b_type == "Vessel" else None,
        ),
        seabed=SeabedModelData(
            type="Linear",
            stiffness_kn_m2=100.0,
            friction_axial=0.3,
            friction_lateral=0.3,
            slope_deg=0.0,
        ),
    )


def _build_loads(metadata: dict[str, Any]) -> EnvironmentData:
    env = metadata["environment"]
    water_depth = env.get("water_depth_m", 100.0)
    return EnvironmentData(
        load_cases=[
            LoadCaseData(
                case_id=metadata["report_summary"].get("load_case_id", "Fixture Baseline"),
                hs_m=env.get("wave_height_m"),
                tp_s=env.get("wave_period_s"),
                current_velocity_m_s=env.get("current_speed_mps"),
                current_direction_deg=env.get("current_direction_deg"),
                description=f"Fixture reporting baseline in {water_depth:.1f} m water depth",
            )
        ],
        hydrodynamic_coefficients=[
            HydroCoeffData(
                line_type_name=(metadata["objects"].get("line_types") or [{"name": "Chain"}])[0].get("name", "Chain"),
                cd_normal=1.0,
                ca_normal=1.0,
            )
        ],
        current_profile={
            "depth": [0.0, -float(water_depth)],
            "velocity": [0.0, float(env.get("current_speed_mps", 0.0) or 0.0)],
        },
    )


def _build_mesh() -> MeshData:
    return MeshData(
        total_segment_count=2,
        segments=[
            SegmentData(arc_length_m=0.0, length_m=125.0),
            SegmentData(arc_length_m=125.0, length_m=125.0),
        ],
        quality=MeshQualityData(
            max_adjacent_ratio=1.0,
            worst_ratio_arc_length_m=125.0,
            verdict="PASS",
            adjacent_ratios=[1.0],
        ),
    )


def build_report_from_metadata(metadata: dict[str, Any]) -> OrcaFlexAnalysisReport:
    report_summary = metadata["report_summary"]
    report = OrcaFlexAnalysisReport(
        project_name=report_summary.get("project_name", metadata["fixture"]["name"]),
        structure_id=report_summary.get("structure_id", metadata["fixture"]["name"]),
        structure_type=report_summary["structure_type"],
        analysis_ref=metadata["fixture"]["name"],
        analyst=metadata["provenance"].get("generated_on_machine", "unknown"),
        date=FIXTURE_REPORT_DATE,
        orcaflex_version=metadata["provenance"].get("orcaflex_version"),
        design_codes=["Fixture Baseline"],
        summary_notes=metadata["fixture"].get("description"),
        geometry=_build_geometry(metadata) if report_summary.get("has_geometry") else None,
        materials=_build_materials(metadata) if report_summary.get("has_materials") else None,
        boundary_conditions=_build_boundary_conditions(metadata) if report_summary.get("has_boundary_conditions") else None,
        loads=_build_loads(metadata) if report_summary.get("has_loads") else None,
        mesh=_build_mesh() if report_summary.get("has_mesh") else None,
    )
    return report


def generate_minimal_fixture_report(tmp_path: Path, include_plotlyjs: bool | str = False) -> Path:
    metadata = load_minimal_fixture_metadata()
    report = build_report_from_metadata(metadata)
    output_path = tmp_path / "minimal_test.report.html"
    return generate_orcaflex_report(report, output_path=output_path, include_plotlyjs=include_plotlyjs)


def load_snapshot_text(name: str = "minimal_test.report.snapshot.html") -> str:
    path = REPORTING_FIXTURES_ROOT / name
    return path.read_text(encoding="utf-8")
