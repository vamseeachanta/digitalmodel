"""Tests for the deterministic, config-driven installation-pamphlet workflow.

Golden numbers are taken from the validated upstream prototype (compute_pamphlet):
  * rao_basis="barge"     -> transit Hs limit at Tp=6 s ~= 0.52 m
  * rao_basis="drillship" -> transit Hs limit at Tp=6 s ~= 1.84 m
  * small 50 te mudmat is defensible with crane utilisation < 5 %
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.installation.installation_pamphlet import calculations as calc
from digitalmodel.installation.installation_pamphlet.render import render_pamphlet_html
from digitalmodel.installation.installation_pamphlet.workflow import (
    router,
    scan_completed_runs,
)

REPO_ROOT = Path(__file__).resolve().parents[2]
DATA_DIR = REPO_ROOT / "examples" / "demos" / "gtm" / "data"
BASE_CONFIG = (
    REPO_ROOT
    / "src/digitalmodel/base_configs/modules/installation_pamphlet/installation_pamphlet.yml"
)

DRILLSHIP_RUN = {
    "run_id": "lr_acma_8de9c018adbd",
    "workflow": "orcawave-diffraction",
    "input": "acma/orcawave-diffraction-fpso/input.yml",
}


# ---------------------------------------------------------------- fixtures
@pytest.fixture(scope="module")
def vessel_info():
    from digitalmodel.marine_ops.vessel_db.loader import installation_vessels

    vlib = installation_vessels()

    def disp(k, i):
        return i.get("_display") or k

    key, info = next((k, i) for k, i in vlib.items() if "bokalift" in disp(k, i).lower())
    cc = info["crane_curve"]
    return {
        "name": disp(key, info),
        "type": info.get("vessel_type"),
        "owner": info.get("owner_operator"),
        "max_hook_te": float(cc.max_hook_load_te),
        "deck_area_m2": info.get("deck_area_m2"),
        "deck_strength_t_per_m2": info.get("deck_strength_t_per_m2"),
        "dp_class": info.get("dp_class"),
        "crane_radii_m": [float(r) for r in cc.radii_m],
        "crane_caps_te": [float(c) for c in cc.capacities_te],
    }


@pytest.fixture(scope="module")
def lifts():
    mud = calc.load_mudmats(DATA_DIR / "mudmat_structures.json")
    jmp = calc.load_jumpers(DATA_DIR / "rigid_jumpers.json")
    return [
        {"item": "Mudmat (small)", "kind": "mudmat", "mass_air_te": mud["MUD-S"]["mass_properties"]["mass_air_te"]},
        {"item": "Manifold/PLET proxy", "kind": "structure", "mass_air_te": 250.0},
        {"item": "Rigid jumper (long 100 m)", "kind": "jumper", "mass_air_te": jmp["JMP-100"]["total_mass_air_te"]},
    ]


def _assemble(vessel_info, lifts, basis, runs=None):
    return calc.assemble_result(
        vessel_info=vessel_info,
        lifts=lifts,
        radius_m=40.0,
        daf=1.30,
        rao_basis=basis,
        operations=calc.DEFAULT_OPERATIONS,
        tp_grid_s=calc.DEFAULT_TP_GRID_S,
        hs_scatter_limits=[0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0],
        completed_runs=runs if runs is not None else [],
    )


# ---------------------------------------------------------------- select_rao_basis
def test_select_rao_basis_empty_is_barge():
    assert calc.select_rao_basis([]) == "barge"


def test_select_rao_basis_fpso_diffraction_is_drillship():
    runs = [{"run_id": "r1", "workflow": "diffraction", "input": "x/orcawave-diffraction-fpso/in.yml"}]
    assert calc.select_rao_basis(runs) == "drillship"


def test_select_rao_basis_bokalift_is_vessel():
    runs = [{"run_id": "r1", "workflow": "anything", "input": "x/bokalift2-mesh/in.yml"}]
    assert calc.select_rao_basis(runs) == "vessel"


def test_select_rao_basis_generic_diffraction_is_drillship():
    runs = [{"run_id": "r1", "workflow": "orcawave", "input": "x/in.yml"}]
    assert calc.select_rao_basis(runs) == "drillship"


# ---------------------------------------------------------------- provenance
def test_build_provenance_drillship_manifest():
    prov = calc.build_provenance([DRILLSHIP_RUN], "drillship")
    assert prov["rao_basis"] == "drillship"
    assert prov["rao_provenance"] == "ship_proxy"
    assert prov["basis_run_id"] == "lr_acma_8de9c018adbd"
    assert prov["n_completed_runs"] == 1
    ids = [t["id"] for t in prov["tasks"]]
    assert ids == ["T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"]
    by_id = {t["id"]: t for t in prov["tasks"]}
    assert by_id["T1"]["status"] == "actual"
    assert by_id["T3"]["status"] == "ship_proxy"
    assert by_id["T3"]["run_id"] == "lr_acma_8de9c018adbd"
    assert by_id["T5"]["status"] == "pending"
    assert by_id["T8"]["status"] == "proxy"


def test_build_provenance_barge_default_has_no_basis_run():
    prov = calc.build_provenance([], "barge")
    assert prov["rao_provenance"] == "proxy"
    assert prov["basis_run_id"] is None


def test_build_provenance_t8_flips_to_catalog_when_used():
    by_id_proxy = {
        t["id"]: t for t in calc.build_provenance([], "barge")["tasks"]
    }
    assert by_id_proxy["T8"]["status"] == "proxy"  # default unchanged

    prov = calc.build_provenance([], "barge", structure_catalog_used=True)
    t8 = {t["id"]: t for t in prov["tasks"]}["T8"]
    assert t8["status"] == "actual"
    assert t8["status"] != "proxy"
    assert "subsea_structures.json" in t8["source"]


# ---------------------------------------------------------------- structure catalog
def test_load_structures_manifold_mass():
    structures = calc.load_structures(DATA_DIR / "subsea_structures.json")
    assert set(["PLET-100", "MAN-250", "TMPL-180"]).issubset(structures)
    manifold = structures["MAN-250"]
    assert manifold["mass_properties"]["mass_air_te"] == pytest.approx(250.0)
    # buoyancy + submerged weight reconcile with mass_air * g (seawater 1025, g 9.80665)
    mp = manifold["mass_properties"]
    total = mp["buoyancy_force_kn"] + mp["submerged_weight_kn"]
    assert total == pytest.approx(250000 * 9.80665 / 1000, abs=0.05)


def test_golden_catalog_structure_lift_is_defensible(vessel_info):
    structures = calc.load_structures(DATA_DIR / "subsea_structures.json")
    manifold_te = structures["MAN-250"]["mass_properties"]["mass_air_te"]
    lifts = [
        {
            "item": "Production manifold (250 te)",
            "kind": "structure",
            "source": "structure",
            "mass_air_te": manifold_te,
        }
    ]
    res = _assemble(vessel_info, lifts, "drillship", runs=[DRILLSHIP_RUN])
    row = res["lifts"][0]
    assert row["mass_air_te"] == pytest.approx(250.0)
    assert row["hook_load_te"] == pytest.approx(250.0 * 1.30, abs=0.05)  # mass x DAF
    assert row["defensible"] is True
    assert 0.0 < row["utilization_pct"] <= 100.0
    # the catalog-sourced structure flips T8 provenance to the real catalog
    t8 = {t["id"]: t for t in res["provenance"]["tasks"]}["T8"]
    assert t8["status"] == "actual"


# ---------------------------------------------------------------- splash-zone (T5)
def test_compute_splashzone_envelope_golden():
    """Closed-form per-structure splash-zone Hs limit vs Tp (DNV-RP-H103)."""
    mud = calc.load_mudmats(DATA_DIR / "mudmat_structures.json")
    env = calc.compute_splashzone_envelope(mud["MUD-S"], calc.DEFAULT_TP_GRID_S)
    assert env["id"] == "MUD-S"
    assert env["daf"] == pytest.approx(1.30)
    hs = env["hs_by_tp"]
    # positive, golden values (locked with tolerance)
    assert hs["6.0"] == pytest.approx(2.11, abs=0.02)
    assert hs["16.0"] == pytest.approx(5.70, abs=0.02)
    assert all(v > 0 for v in hs.values())
    # monotone increasing in Tp (longer period -> milder kinematics -> higher Hs)
    vals = [hs[f"{float(t):.1f}"] for t in calc.DEFAULT_TP_GRID_S]
    assert vals == sorted(vals)
    # flat-bottom mudmats are slamming-governed
    assert set(env["gov_by_tp"].values()) == {"slam"}


def test_splashzone_denser_structure_tolerates_higher_hs():
    """A dense, small-footprint structure tolerates higher Hs than a light,
    large-area one (the large flat bottom is slamming-limited)."""
    sub = calc.load_structures(DATA_DIR / "subsea_structures.json")
    plet = calc.compute_splashzone_envelope(sub["PLET-100"], calc.DEFAULT_TP_GRID_S)
    tmpl = calc.compute_splashzone_envelope(sub["TMPL-180"], calc.DEFAULT_TP_GRID_S)
    # PLET-100: W_sub 852 kN over A_b 48 m2 ; SPS template: 1535 kN over 192 m2
    assert plet["hs_by_tp"]["6.0"] == pytest.approx(2.66, abs=0.02)
    assert tmpl["hs_by_tp"]["6.0"] == pytest.approx(1.50, abs=0.02)
    assert plet["hs_by_tp"]["6.0"] > tmpl["hs_by_tp"]["6.0"]
    assert tmpl["gov_by_tp"]["6.0"] == "slam"


def _lifts_with_records():
    mud = calc.load_mudmats(DATA_DIR / "mudmat_structures.json")
    sub = calc.load_structures(DATA_DIR / "subsea_structures.json")
    return [
        {
            "item": "Mudmat (small)", "kind": "mudmat",
            "mass_air_te": mud["MUD-S"]["mass_properties"]["mass_air_te"],
            "catalog_record": mud["MUD-S"],
        },
        {
            "item": "Production manifold (250 te)", "kind": "structure", "source": "structure",
            "mass_air_te": sub["MAN-250"]["mass_properties"]["mass_air_te"],
            "catalog_record": sub["MAN-250"],
        },
    ]


def test_splashzone_set_in_payload_and_provenance(vessel_info):
    res = _assemble(vessel_info, _lifts_with_records(), "drillship", runs=[DRILLSHIP_RUN])
    sz = res["splashzone"]
    assert [s["id"] for s in sz] == ["MUD-S", "MAN-250"]
    # combined limit = min(vessel heavy-lift envelope, structure splash-zone) per Tp
    hl = res["envelopes"]["heavy_lift"]["hs_by_tp"]
    for s in sz:
        for k, comb in s["combined_hs_by_tp"].items():
            assert comb == pytest.approx(min(s["hs_by_tp"][k], hl[k]), abs=0.01)
    # T5 provenance flips off "pending"
    t5 = {t["id"]: t for t in res["provenance"]["tasks"]}["T5"]
    assert t5["status"] == "actual"
    assert t5["status"] != "pending"
    assert "splash-zone" in t5["basis"].lower()


def test_splashzone_renders_section(vessel_info):
    res = _assemble(vessel_info, _lifts_with_records(), "drillship", runs=[DRILLSHIP_RUN])
    html = render_pamphlet_html(res, "LBL")
    assert "Per-structure splash-zone limits (DNV-RP-H103)" in html
    assert "MUD-S" in html
    assert "Governing lowering Hs" in html  # combined limit table


def test_splashzone_absent_when_no_catalog_records(vessel_info, lifts):
    """Without catalog records (hand-built lifts), T5 stays pending and the
    splash-zone section is not rendered — keeps existing goldens green."""
    res = _assemble(vessel_info, lifts, "barge")
    assert res["splashzone"] == []
    t5 = {t["id"]: t for t in res["provenance"]["tasks"]}["T5"]
    assert t5["status"] == "pending"
    assert "Per-structure splash-zone limits" not in render_pamphlet_html(res, "LBL")


# ---------------------------------------------------------------- golden numbers
def test_golden_barge_transit_hs_and_lift(vessel_info, lifts):
    res = _assemble(vessel_info, lifts, "barge")
    hs6 = res["envelopes"]["transit"]["hs_by_tp"]["6.0"]
    assert hs6 == pytest.approx(0.52, abs=0.02)
    small = res["lifts"][0]
    assert small["item"] == "Mudmat (small)"
    assert small["mass_air_te"] == pytest.approx(50.0)
    assert small["defensible"] is True
    assert small["utilization_pct"] < 5.0


def test_golden_drillship_transit_hs(vessel_info, lifts):
    res = _assemble(vessel_info, lifts, "drillship", runs=[DRILLSHIP_RUN])
    hs6 = res["envelopes"]["transit"]["hs_by_tp"]["6.0"]
    assert hs6 == pytest.approx(1.84, abs=0.05)
    assert res["rao_provenance"] == "ship_proxy"
    assert res["provenance"]["basis_run_id"] == "lr_acma_8de9c018adbd"


# ---------------------------------------------------------------- determinism
def test_pure_pipeline_is_deterministic(vessel_info, lifts):
    r1 = _assemble(vessel_info, lifts, "drillship", runs=[DRILLSHIP_RUN])
    r2 = _assemble(vessel_info, lifts, "drillship", runs=[DRILLSHIP_RUN])
    assert r1 == r2
    h1 = render_pamphlet_html(r1, "LBL")
    h2 = render_pamphlet_html(r2, "LBL")
    assert h1 == h2  # byte-identical


def test_generated_label_is_injected(vessel_info, lifts):
    res = _assemble(vessel_info, lifts, "barge")
    html = render_pamphlet_html(res, "SENTINEL-LABEL-123")
    assert "SENTINEL-LABEL-123" in html
    # no wall-clock leakage: default label appears only when none supplied
    assert "(generated by installation_pamphlet workflow)" not in html


def test_render_contains_core_sections(vessel_info, lifts):
    res = _assemble(vessel_info, lifts, "drillship", runs=[DRILLSHIP_RUN])
    html = render_pamphlet_html(res, "LBL")
    assert "Installation Vessel Pamphlet" in html
    assert "Operational Envelopes" in html
    assert "Live Weather-Window Monitor" in html
    assert "Statistical Failure-Risk Analysis" in html
    # live monitor widget is present but is the optional client-side script
    assert "marine-api.open-meteo.com" in html
    assert "Analysis progress" in html


# ---------------------------------------------------------------- scan helper
def test_scan_completed_runs(tmp_path):
    import json

    rd = tmp_path / "results"
    rd.mkdir()
    (rd / "a.json").write_text(
        json.dumps(
            {
                "run_id": "lr_a",
                "state": "finished",
                "returncode": 0,
                "audit": {"workflow": "orcawave-diffraction", "input_relpath": "fpso/in.yml"},
            }
        )
    )
    (rd / "b.json").write_text(
        json.dumps({"run_id": "lr_b", "state": "finished", "returncode": 1})
    )
    (rd / "c.json").write_text(json.dumps({"run_id": "lr_c", "state": "running"}))
    runs = scan_completed_runs(rd)
    assert [r["run_id"] for r in runs] == ["lr_a"]
    assert runs[0]["workflow"] == "orcawave-diffraction"
    assert calc.select_rao_basis(runs) == "drillship"


def test_scan_missing_dir_returns_empty(tmp_path):
    assert scan_completed_runs(tmp_path / "nope") == []


# ---------------------------------------------------------------- router (integration)
def test_router_on_base_config_writes_artifacts(tmp_path):
    cfg = yaml.safe_load(BASE_CONFIG.read_text())
    cfg["installation_pamphlet"]["output_dir"] = str(tmp_path)
    cfg["_config_file_path"] = str(tmp_path / "case.yml")  # stem -> "case"

    out = router(cfg)

    html_path = tmp_path / "case_pamphlet.html"
    data_path = tmp_path / "case_pamphlet_data.json"
    assert html_path.exists()
    assert data_path.exists()
    assert out["outputs"]["pamphlet_html"]
    assert out["outputs"]["pamphlet_data_json"]

    summary = out["installation_pamphlet"]["summary"]
    assert summary["n_lifts"] == 6
    assert summary["rao_basis"] == "drillship"  # auto-selected from completed_runs
    assert summary["all_lifts_defensible"] is True

    # the base config now sources the 250 te manifold from subsea_structures.json,
    # which flips the T8 structure-catalog provenance off the retired proxy
    result = out["installation_pamphlet"]["result"]
    t8 = {t["id"]: t for t in result["provenance"]["tasks"]}["T8"]
    assert t8["status"] == "actual"
    assert t8["status"] != "proxy"
    man = next(r for r in result["lifts"] if "manifold" in r["item"].lower())
    assert man["mass_air_te"] == pytest.approx(250.0)

    # the workflow attaches catalog records to mudmat/structure lifts, so the
    # per-structure splash-zone envelope (T5) is computed and flips off "pending"
    assert len(result["splashzone"]) >= 4  # 3 mudmats + 1 manifold
    t5 = {t["id"]: t for t in result["provenance"]["tasks"]}["T5"]
    assert t5["status"] == "actual"

    html = html_path.read_text()
    assert "Installation Vessel Pamphlet" in html
    assert "Per-structure splash-zone limits (DNV-RP-H103)" in html


def test_router_is_deterministic_on_base_config(tmp_path):
    def run(dirname):
        d = tmp_path / dirname
        d.mkdir()
        cfg = yaml.safe_load(BASE_CONFIG.read_text())
        cfg["installation_pamphlet"]["output_dir"] = str(d)
        cfg["_config_file_path"] = str(d / "case.yml")
        router(cfg)
        return (d / "case_pamphlet.html").read_bytes(), (d / "case_pamphlet_data.json").read_bytes()

    html_a, data_a = run("run_a")
    html_b, data_b = run("run_b")
    assert html_a == html_b  # byte-identical HTML
    assert data_a == data_b  # byte-identical JSON
