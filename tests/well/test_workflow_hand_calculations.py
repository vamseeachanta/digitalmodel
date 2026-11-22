"""Direct workflow readback against hand values and exact limiting cases.

Only public well adapters are called. Files are written beneath pytest's
externally configured temporary directory. No licensed solver is used.
Numerical tolerances of 1e-12 relative cover binary arithmetic on the supplied
inputs, not an engineering allowance. Bore-design lookups, nonzero ROP and
standard casing/envelope acceptance remain NOT_EVALUATED.
"""

import csv
import json
from pathlib import Path

import pytest

from digitalmodel.well.workflow import (
    run_casing_design,
    run_rop_analysis,
    run_swab_surge,
    run_tubular_design,
    run_well_bore_design,
    run_well_hydraulics,
)


def _saved_summary(cfg, directory, calculation):
    path = Path(cfg["outputs"]["summary_json"])
    assert path.parent == directory
    saved = json.loads(path.read_text(encoding="utf-8"))
    assert saved["calculation"] == calculation
    return saved


def test_bore_design_adapter_preserves_input_and_cost_additivity(tmp_path):
    """Unqualified lookup model: total cost must sum its reported components.

    No published basis establishes the canned production/casing/cost numbers;
    their absolute values remain NOT_EVALUATED. Specified water depth is 500 m.
    """
    cfg = {"well_bore_design": {
        "hole_type": "standard_hole", "water_depth_m": 500.0,
        "outputs": {"directory": str(tmp_path)},
    }}
    out = run_well_bore_design(cfg)
    saved = _saved_summary(out, tmp_path, "well_bore_design")
    assert saved["water_depth_m"] == 500.0
    assert saved["hole_type"] == "standard_hole"
    cost = out["well_bore_design"]["cost"]
    assert saved["total_cost_usd"] == pytest.approx(
        cost["drilling_usd"] + cost["casing_usd"] + cost["completion_usd"], rel=1e-12,
    )
    assert saved["casing_count"] == len(out["well_bore_design"]["casing_program"])


def test_hydraulics_adapter_zero_flow_zero_rheology_hand_limit(tmp_path):
    """No flow gives velocity 0; PV=YP=0 gives pressure 0 and ECD=12 ppg.

    The zero-pressure limit needs no empirical friction coefficient or unit
    conversion. All supplied dimensions and rheology are synthetic inputs.
    """
    cfg = {"well_hydraulics": {
        "section": {
            "pipe_od": 6.0, "pipe_id": 4.0, "hole_diameter": 10.0,
            "mud_weight": 12.0, "flow_rate": 0.0, "tvd": 2_000.0,
            "plastic_viscosity": 0.0, "yield_point": 0.0,
        },
        "outputs": {"directory": str(tmp_path)},
    }}
    out = run_well_hydraulics(cfg)
    saved = _saved_summary(out, tmp_path, "well_hydraulics")
    expected = {
        "annular_velocity_ft_min": 0.0, "pressure_drop_annulus_psi": 0.0,
        "ecd_ppg": 12.0, "cuttings_transport_ratio": 0.0,
    }
    assert saved["result"] == expected
    assert out["well_hydraulics"]["result"] == expected


def test_swab_adapter_open_pipe_hand_velocity_zero_rheology(tmp_path):
    """Area balance: steel=5*pi, return area=20*pi, displacement=1/4.

    With supplied clinging=1/4, effective velocity=160*(1/4+1/4)=80 ft/min.
    PV=YP=0 gives pressure 0 and both EMWs 12 ppg (slice 3 hand limit).
    This does not qualify the nonzero friction normalization.
    """
    cfg = {"swab_surge": {
        "section": {
            "pipe_od": 6.0, "pipe_id": 4.0, "hole_diameter": 10.0,
            "mud_weight": 12.0, "tvd": 2_000.0,
            "plastic_viscosity": 0.0, "yield_point": 0.0,
            "clinging_constant": 0.25, "pipe_config": "open",
        },
        "trip": {"trip_speed_ft_min": 160.0},
        "outputs": {"directory": str(tmp_path)},
    }}
    out = run_swab_surge(cfg)
    saved = _saved_summary(out, tmp_path, "swab_surge")
    expected = {
        "trip_speed_ft_min": 160.0, "effective_velocity_ft_min": 80.0,
        "pressure_psi": 0.0, "surge_emw_ppg": 12.0, "swab_emw_ppg": 12.0,
    }
    assert saved["result"]["at_trip_speed"] == expected
    assert out["swab_surge"]["result"]["at_trip_speed"] == expected


def test_rop_adapter_zero_weight_and_rotation_hand_limit(tmp_path):
    """No applied bit weight or rotation gives ROP=0 for either model.

    Both sensitivities are zero with the other input still zero. This
    limiting case needs no empirical coefficient or published fitted dataset.
    Nonzero ROP remains NOT_EVALUATED.
    """
    cfg = {"rop_analysis": {
        "bourgoyne_young": {
            "parameters": {f"a{i}": 0.0 for i in range(1, 9)},
            "inputs": {
                "depth_ft": 2_000.0, "wob_klb": 0.0, "rpm": 0.0,
                "bit_dia_in": 6.0, "overbalance_kpsi": 0.0, "bit_wear": 0.0,
            },
        },
        "warren": {
            "parameters": {"K": 1.0, "a": 1.0, "b": 1.0},
            "inputs": {"wob_klb": 0.0, "rpm": 0.0, "bit_dia_in": 6.0},
        },
        "outputs": {"directory": str(tmp_path)},
    }}
    out = run_rop_analysis(cfg)
    saved = _saved_summary(out, tmp_path, "rop_analysis")["result"]
    assert saved["bourgoyne_young"]["rop_ft_hr"] == 0.0
    assert saved["warren"]["rop_ft_hr"] == 0.0
    assert saved["bourgoyne_young"]["sensitivity_wob_ft_hr_per_klb"] == 0.0
    assert saved["bourgoyne_young"]["sensitivity_rpm_ft_hr_per_rpm"] == 0.0
    assert out["rop_analysis"]["result"]["warren"]["rop_ft_hr"] == 0.0


def test_tubular_adapter_annulus_and_axial_force_hand_values(tmp_path):
    """Slice 1: ID=8.681, wall=.472, area=pi*9.153*.472 in2.

    Area=13.572358847521082 in2; at specified 80000 psi the force is
    1085788.707801687 lbf. Stress=force/area is OpenStax, University Physics
    Volume 1 (2016), section 12.3, equation 12.34. Supplied burst/collapse
    ratings below are illustrative inputs, not qualified standard capacities.
    """
    cfg = {"tubular_design": {
        "geometry": {
            "od_in": 9.625, "id_in": 8.681, "smys_psi": 80_000.0,
            "burst_rating_psi": 5_000.0, "collapse_rating_psi": 2_000.0,
        },
        "n_points": 12, "design_factor": 1.0,
        "outputs": {"directory": str(tmp_path)},
    }}
    out = run_tubular_design(cfg)
    saved = _saved_summary(out, tmp_path, "tubular_design")
    geometry = saved["geometry"]
    assert geometry["wall_thickness_in"] == pytest.approx(0.472, rel=1e-12)
    assert geometry["cross_section_area_in2"] == pytest.approx(13.572358847521082, rel=1e-12)
    assert geometry["yield_force_lbf"] == pytest.approx(1_085_788.707801687, rel=1e-12)
    with Path(out["outputs"]["envelope_csv"]).open(newline="") as stream:
        rows = list(csv.DictReader(stream))
    assert len(rows) == 24  # Two requested curves, twelve samples each.
    assert {r["envelope"] for r in rows} == {"api_ellipse", "vme_isotropic"}


def test_casing_adapter_geometry_and_rounded_axial_hand_value(tmp_path):
    """Slice 1 geometry gives 1085788.707801687 lbf at specified 80000 psi.

    The adapter's nearest-1000-lbf output contract gives 1086000 lbf.
    Stress=force/area: OpenStax (2016), section 12.3, equation 12.34.
    Current catalog N80 maps to that strength and 47 ppf maps to .472 in;
    catalog provenance and API rounding qualification remain NOT_EVALUATED.
    This test does not assert standard burst/collapse/design-factor acceptance.
    """
    cfg = {"casing_design": {
        "well": {
            "td_ft": 2_000.0, "mud_ppg": 12.0, "toc_ft": 500.0,
            "outer_shoe_ft": 1_000.0, "reservoir_pressure_psi": 1_000.0,
            "packer_fluid_ppg": 10.0, "frac_fluid_ppg": 10.0,
            "mix_water_ppg": 10.0, "gas_gradient_psi_ft": 0.0,
        },
        "products": [{"grade": "N80", "od_in": 9.625, "weight_ppf": 47.0}],
        "outputs": {"directory": str(tmp_path)},
    }}
    out = run_casing_design(cfg)
    saved = _saved_summary(out, tmp_path, "casing_design")
    row = saved["products"][0]
    assert saved["product_count"] == 1
    assert row["od_in"] == 9.625
    assert row["wall_in"] == 0.472
    assert row["body_yield_lbf"] == 1_086_000.0
    with Path(out["outputs"]["products_csv"]).open(newline="") as stream:
        csv_rows = list(csv.DictReader(stream))
    assert len(csv_rows) == 1
    assert float(csv_rows[0]["body_yield_lbf"]) == 1_086_000.0


@pytest.mark.parametrize(
    "adapter, section, missing",
    [(run_well_bore_design, "well_bore_design", "hole_type"),
     (run_well_hydraulics, "well_hydraulics", "section"),
     (run_swab_surge, "swab_surge", "section"),
     (run_rop_analysis, "rop_analysis", "bourgoyne_young"),
     (run_tubular_design, "tubular_design", "geometry"),
     (run_casing_design, "casing_design", "well")],
)
def test_adapter_missing_required_input_refuses_before_output(
    adapter, section, missing, tmp_path,
):
    cfg = {section: {"outputs": {"directory": str(tmp_path)}}}
    with pytest.raises(KeyError, match=missing):
        adapter(cfg)
    assert list(tmp_path.iterdir()) == []
