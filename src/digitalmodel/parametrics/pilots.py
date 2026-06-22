"""One offline parametric pilot per solver (parametrics P0, #968).

Each pilot builds a ``ParametricStudy`` over a *ready* use-case and a per-case
runner, then returns a ``ParametricResultsSummary``. All run offline / no licence.
ANSYS + OrcaWave produce per-case *inputs* (status ``prepared`` — the solve is the
licensed P1 step); the analytical OrcaFlex FOWT check produces real results
(status ``completed``).
"""

from __future__ import annotations

import shutil
from pathlib import Path

import yaml

from digitalmodel.orcaflex.batch_parametric import (
    CaseResult,
    ParameterSweep,
    ParametricStudy,
)
from digitalmodel.parametrics.sweep import run_sweep

# Ready templates this P0 sweeps over (registry ids in parentheses).
_ORCAWAVE_BASE_SPEC = Path(
    "examples/workflows/orcawave-diffraction-solve/spec.yml"
)  # orcawave-unit-box


# --- ANSYS: padeye geometry sweep -> per-case APDL .inp ----------------------


def ansys_padeye_sweep(
    out_dir: str | Path,
    *,
    thicknesses_mm=(30.0, 40.0, 50.0),
    hole_diameters_mm=(70.0, 90.0),
):
    """Sweep padeye plate thickness x hole diameter -> one .inp per case."""
    from digitalmodel.ansys.padeye import PadeyeGeometry, write_padeye_inp

    out_dir = Path(out_dir)
    study = ParametricStudy(
        name="ansys-padeye",
        parameters=[
            ParameterSweep(name="thickness_mm", values=list(thicknesses_mm), unit="mm"),
            ParameterSweep(
                name="hole_diameter_mm", values=list(hole_diameters_mm), unit="mm"
            ),
        ],
        output_dir=str(out_dir),
    )

    def runner(cfg: dict) -> CaseResult:
        geom = PadeyeGeometry(
            thickness_mm=cfg["thickness_mm"],
            hole_diameter_mm=cfg["hole_diameter_mm"],
        )
        inp = write_padeye_inp(geom, out_dir / cfg["case_id"] / "padeye.inp")
        return CaseResult(
            case_id=cfg["case_id"],
            parameters={
                "thickness_mm": cfg["thickness_mm"],
                "hole_diameter_mm": cfg["hole_diameter_mm"],
            },
            status="prepared",
            notes=str(inp),
        )

    return run_sweep(study, runner)


# --- OrcaWave: water-depth sweep -> per-case validated diffraction spec -------


def orcawave_depth_sweep(
    out_dir: str | Path,
    *,
    water_depths_m=(100.0, 250.0, 500.0),
    repo_root: str | Path | None = None,
):
    """Sweep OrcaWave water depth -> one validated diffraction spec per case."""
    from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

    root = Path(repo_root) if repo_root else Path.cwd()
    base_spec = root / _ORCAWAVE_BASE_SPEC
    base_dir = base_spec.parent
    spec_dict = yaml.safe_load(base_spec.read_text())
    mesh_file = spec_dict["vessel"]["geometry"]["mesh_file"]
    out_dir = Path(out_dir)

    study = ParametricStudy(
        name="orcawave-water-depth",
        parameters=[
            ParameterSweep(name="water_depth", values=list(water_depths_m), unit="m")
        ],
        output_dir=str(out_dir),
    )

    def runner(cfg: dict) -> CaseResult:
        case_dir = out_dir / cfg["case_id"]
        case_dir.mkdir(parents=True, exist_ok=True)
        # Copy the mesh next to the per-case spec so relative refs resolve.
        shutil.copy(base_dir / mesh_file, case_dir / mesh_file)
        case_spec = dict(spec_dict)
        case_spec["environment"] = {
            **spec_dict["environment"],
            "water_depth": cfg["water_depth"],
        }
        spec_path = case_dir / "spec.yml"
        spec_path.write_text(yaml.safe_dump(case_spec, sort_keys=False))

        issues = SpecConverter(spec_path).validate()
        status = "prepared" if not issues else "failed"
        return CaseResult(
            case_id=cfg["case_id"],
            parameters={"water_depth": cfg["water_depth"]},
            status=status,
            notes=str(spec_path) if not issues else "; ".join(issues),
        )

    return run_sweep(study, runner)


# --- OrcaFlex: FOWT watch-circle sweep -> real analytical results ------------


def orcaflex_fowt_watch_circle_sweep(
    *,
    watch_circle_radii_m=(15.0, 25.0, 35.0),
    cable=None,
):
    """Sweep FOWT watch-circle radius -> closed-form MBR check per case (offline)."""
    from digitalmodel.orcaflex.fowt_mooring_workflow import FOWTMooringWorkflow

    base_cable = cable or {
        "suspended_length": 320.0,
        "hang_off_elevation": 90.0,
        "nominal_horizontal_span": 260.0,
        "mbr_limit_m": 4.5,
        "mooring_type": "hybrid",
        "floater_type": "semi",
    }
    study = ParametricStudy(
        name="orcaflex-fowt-watch-circle",
        parameters=[
            ParameterSweep(
                name="watch_circle_radius", values=list(watch_circle_radii_m), unit="m"
            )
        ],
    )

    def runner(cfg: dict) -> CaseResult:
        wf_cfg = {
            "fowt_mooring": {
                "watch_circle_radius": cfg["watch_circle_radius"],
                "cable": dict(base_cable),
            }
        }
        out = FOWTMooringWorkflow().router(wf_cfg)
        r = out["fowt_mooring"]["result"]
        governing = float(r["governing_bend_radius_m"])
        limit = float(r["mbr_limit_m"])
        return CaseResult(
            case_id=cfg["case_id"],
            parameters={"watch_circle_radius": cfg["watch_circle_radius"]},
            status="completed",
            min_clearance_m=round(float(r["margin_m"]), 3),
            max_utilisation=round(limit / governing, 4) if governing else None,
            notes=f"passes={r['passes']} governing_bend_radius_m={governing}",
        )

    return run_sweep(study, runner)
