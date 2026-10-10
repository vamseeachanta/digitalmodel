"""Fixture provenance and report-mode contracts for issue 2183."""

import hashlib
from pathlib import Path

import pytest
import yaml

from digitalmodel.asset_integrity.riser_joint_ffs import RiserJointFFSWorkflow

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = ROOT / "examples/workflows/riser-joint-ffs/input.yml"


def run_report(tmp_path):
    cfg = yaml.safe_load(EXAMPLE.read_text())
    cfg["Analysis"] = {
        "result_folder": str(tmp_path),
        "analysis_root_folder": str(EXAMPLE.parent),
    }
    return RiserJointFFSWorkflow().router(cfg)["riser_joint_ffs"]


def test_report_reads_four_grids_and_records_exact_bytes(tmp_path):
    result = run_report(tmp_path)
    assert len(result["scans"]) == 4
    assert len(result["placements"]) == 4  # one placement assessment per scan
    report = Path(result["report_html"]).read_text()
    assert report == (EXAMPLE.parent / "report.html").read_text()
    assert "Provenance" in report and "anonymized" in report
    assert "API 579-1/ASME FFS-1 2021 Edition criteria are applied" not in report
    assert len(result["provenance"]["sources"]) == 6  # grids, register, README
    for source in result["provenance"]["sources"]:
        digest = hashlib.sha256((ROOT / source["path"]).read_bytes()).hexdigest()
        assert digest == source["sha256"]
        assert digest in report
    assert "NaN" not in Path(result["result_json"]).read_text()
    assert "String placement" in report and "Fleet roll-up" in report
    assert "zone_life_margin_multipliers" in report
    assert "weld_default_cvn_joules" in report


@pytest.mark.parametrize("values", ["\n", "0,1\n", "inf,1\n", "-1,1\n"])
def test_invalid_grid_fails_without_report(tmp_path, values):
    cfg = yaml.safe_load(EXAMPLE.read_text())
    grid = tmp_path / "invalid.csv"
    grid.write_text(values)
    cfg["riser_joint_ffs"]["report"]["scans"][0]["grid_csv"] = str(grid)
    cfg["Analysis"] = {
        "result_folder": str(tmp_path),
        "analysis_root_folder": str(EXAMPLE.parent),
    }
    with pytest.raises(ValueError, match="grid"):
        RiserJointFFSWorkflow().router(cfg)
    assert not (tmp_path / "riser-joint-ffs.html").exists()


@pytest.mark.parametrize(
    "method,expected_length",
    [
        ("b31g", 17.751),
        ("modified_b31g", 8.176),
        ("dnv_f101", 7.664),
    ],
)
def test_envelope_point_against_direct_pressure_engine(method, expected_length):
    from digitalmodel.asset_integrity.ffs_acceptance_curves import _pipe_safe_pressure
    from digitalmodel.asset_integrity.riser_joint_ffs import level1_flaw_envelope

    env = level1_flaw_envelope(
        21.25, 0.875, "X80", 3000, method=method, depth_fracs=[0.75]
    )
    length = env["max_acceptable_length_in"][0]
    assert length == pytest.approx(expected_length, abs=0.0005)
    pressure = _pipe_safe_pressure(method, 21.25, 0.875, 0.75 * 0.875, length, "X80")
    assert pressure == pytest.approx(3000, abs=0.2)  # rounded envelope length
    assert (
        _pipe_safe_pressure(method, 21.25, 0.875, 0.75 * 0.875, length * 0.999, "X80")
        > 3000
    )
    assert (
        _pipe_safe_pressure(method, 21.25, 0.875, 0.75 * 0.875, length * 1.001, "X80")
        < 3000
    )


def test_catalog_does_not_deny_existing_composition_record():
    from digitalmodel.asset_integrity import offering_catalog as catalog

    text = catalog.render_markdown(catalog.load())
    assert "`workflow` rows are not qualified as `live`" in text


@pytest.mark.parametrize(
    "field,value",
    [
        ("campaign_years", -1),
        ("campaign_water_depth_ft", 0),
        ("design_pressure_psi", float("nan")),
        ("n_campaigns", 0),
    ],
)
def test_invalid_campaign_basis_is_rejected(tmp_path, field, value):
    cfg = yaml.safe_load(EXAMPLE.read_text())
    cfg["riser_joint_ffs"]["report"][field] = value
    cfg["Analysis"] = {
        "result_folder": str(tmp_path),
        "analysis_root_folder": str(EXAMPLE.parent),
    }
    with pytest.raises(ValueError, match="basis"):
        RiserJointFFSWorkflow().router(cfg)
    assert not (tmp_path / "riser-joint-ffs.html").exists()


def test_unmatched_scan_is_rejected(tmp_path):
    cfg = yaml.safe_load(EXAMPLE.read_text())
    cfg["riser_joint_ffs"]["report"]["scans"][0]["joint_id"] = "RJ-missing"
    cfg["Analysis"] = {
        "result_folder": str(tmp_path),
        "analysis_root_folder": str(EXAMPLE.parent),
    }
    with pytest.raises(ValueError, match="exactly one matching"):
        RiserJointFFSWorkflow().router(cfg)


@pytest.mark.parametrize(
    "field,value",
    [
        ("min_life_years", float("nan")),
        ("min_life_years", float("inf")),
        ("min_life_years", -1),
        ("joint_id", None),
    ],
)
def test_unselected_register_rows_cannot_inflate_fleet(tmp_path, field, value):
    import pandas as pd

    cfg = yaml.safe_load(EXAMPLE.read_text())
    spec = cfg["riser_joint_ffs"]["report"]
    register = pd.read_csv(EXAMPLE.parent / spec["register_csv"])
    assert register.iloc[0]["joint_id"] == "RJ-108"  # not one of the four grids
    register.loc[0, field] = value
    path = tmp_path / "register.csv"
    register.to_csv(path, index=False)
    spec["register_csv"] = str(path)
    cfg["Analysis"] = {
        "result_folder": str(tmp_path),
        "analysis_root_folder": str(EXAMPLE.parent),
    }
    with pytest.raises(ValueError, match="Main register"):
        RiserJointFFSWorkflow().router(cfg)
    assert not (tmp_path / "riser-joint-ffs.html").exists()


def test_placement_uses_worst_joint_life_and_governing_measured_scan(tmp_path):
    import pandas as pd

    cfg = yaml.safe_load(EXAMPLE.read_text())
    spec = cfg["riser_joint_ffs"]["report"]
    register = pd.read_csv(EXAMPLE.parent / spec["register_csv"])
    row = (
        register[(register.component == "Main") & (register.joint_id == "RJ-101")]
        .iloc[0]
        .copy()
    )
    row["scan_location"] = "Other End"
    row["min_life_years"] = 0.1
    register = pd.concat([register, row.to_frame().T], ignore_index=True)
    path = tmp_path / "register.csv"
    register.to_csv(path, index=False)
    spec["register_csv"] = str(path)
    cfg["Analysis"] = {
        "result_folder": str(tmp_path),
        "analysis_root_folder": str(EXAMPLE.parent),
    }
    result = RiserJointFFSWorkflow().router(cfg)["riser_joint_ffs"]
    scan_placements = [p for p in result["placements"] if p["joint_id"] == "RJ-101"]
    assert all(p["min_life_years"] == 0.1 for p in scan_placements)
    assert all(p["verdict"] == "REPAIR" for p in scan_placements)
    governing = next(p for p in result["joint_governing"] if p["joint_id"] == "RJ-101")
    assert governing["acceptable_depth_ft"] == min(
        p["acceptable_depth_ft"] for p in scan_placements
    )
    assert len(result["joint_governing"]) == 3


@pytest.mark.parametrize("method", ["b31g", "modified_b31g", "dnv_f101"])
def test_high_pressure_campaign_envelope_has_strict_reduction(method):
    from digitalmodel.asset_integrity.riser_joint_ffs import level1_flaw_envelope

    basis = dict(
        od_in=21.25,
        wt_in=0.875,
        grade="X80",
        design_pressure_psi=3000,
        method=method,
        depth_fracs=[0.75],
    )
    start = level1_flaw_envelope(**basis)
    end = level1_flaw_envelope(
        **basis, corrosion_rate_in_per_yr=0.25 / 25.4, campaign_years=3
    )
    assert end["max_acceptable_length_in"][0] < start["max_acceptable_length_in"][0]


@pytest.mark.parametrize(
    "method,limit",
    [
        ("b31g", 0.80),
        ("modified_b31g", 0.80),
        ("dnv_f101", 0.85),
        ("bs7910_option1_fad", 0.60),
    ],
)
@pytest.mark.parametrize("growth_rate", [0.0, 0.01])
def test_report_flags_depth_applicability_at_each_campaign(method, limit, growth_rate):
    from digitalmodel.asset_integrity.riser_joint_ffs import level1_flaw_envelope
    from digitalmodel.asset_integrity.riser_joint_report import _envelope_sections

    args = {"region": "weld"} if method == "bs7910_option1_fad" else {"method": method}
    basis = dict(
        od_in=21.25,
        wt_in=0.875,
        grade="X80",
        design_pressure_psi=500,
        depth_fracs=[limit - 0.05, limit, limit + 0.05],
        **args,
    )
    start = level1_flaw_envelope(**basis)
    end = level1_flaw_envelope(
        **basis, corrosion_rate_in_per_yr=growth_rate, campaign_years=3
    )
    rendered = "".join(
        _envelope_sections(
            {
                "scans": [
                    {
                        "scan": "synthetic",
                        "measured_min_wt_in": 0.6,
                        "unmeasured_cells": 0,
                        "envelopes": {method: {"start": start, "campaign_end": end}},
                    }
                ]
            }
        )
    )
    cells = [row.split("</tr>")[0] for row in rendered.split("<tr>")[2:]]
    assert "OUT OF APPLICABILITY" not in cells[0]
    assert cells[1].count("Not evaluated: OUT OF APPLICABILITY") == int(growth_rate > 0)
    assert cells[2].count("Not evaluated: OUT OF APPLICABILITY") == 2


def test_hash_bound_report_sources_preserve_bytes_under_autocrlf(tmp_path):
    import os
    import subprocess

    # A disposable Git repository prevents conversion probes modifying caller state.
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    env.update(GIT_CONFIG_GLOBAL=os.devnull, GIT_CONFIG_SYSTEM=os.devnull)

    def git(*args):
        return subprocess.run(
            ["git", *args], cwd=tmp_path, env=env, check=True, capture_output=True
        )

    git("init", "-q")
    git("config", "core.autocrlf", "true")
    git("config", "core.safecrlf", "false")
    paths = [Path(".gitattributes"), EXAMPLE.parent.relative_to(ROOT) / "report.html"]
    paths += [
        p.relative_to(ROOT)
        for p in (ROOT / "tests/asset_integrity/test_data/real_inspection").iterdir()
        if p.is_file()
    ]
    expected = {str(p): (ROOT / p).read_bytes() for p in paths}
    for name, data in expected.items():
        target = tmp_path / name
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(data)
    git("add", "--", *expected)
    git("checkout-index", "--all", "--prefix=converted/")
    for name, data in expected.items():
        assert (tmp_path / "converted" / name).read_bytes() == data, name
