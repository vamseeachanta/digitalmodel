"""Report semantics and spatial preview checks for assumed example geometry."""
import re

from digitalmodel.asset_integrity.assessment import example_vessel_data as ev
from digitalmodel.asset_integrity.assessment import example_vessel_report as report


def fixture():
    basis = ev._basis(ev.AREAS)
    basis.update(future_horizon_years=5, future_corrosion_rate_mm_per_year=0.1)
    grids = {a.area_id: ev.sample(a, 6.25) for a in ev.AREAS}
    studies = {a.area_id: ev.sampling_study(a) for a in ev.AREAS}
    for study in studies.values():
        for row in study["resolutions"]:
            for old, new in (("loss_volume_mm3", "developed_loss_mm3"),
                             ("volume_error_fraction", "developed_loss_error_fraction")):
                if old in row:
                    row[new] = row.pop(old)
    return basis, grids, studies


def test_report_uses_basis_and_reports_provisional():
    basis, grids, studies = fixture()
    basis.update(nominal_mm=19, uncertainty_mm=0.3, future_loss_mm=0.6,
                 inside_radius_mm=1200, shell_tangent_length_mm=7000,
                 target_pressure_mpa_g=1.7, reduced_pressure_trial_mpa_g=0.9,
                 assessment_temperature_c=110, future_horizon_years=6,
                 future_corrosion_rate_mm_per_year=0.12)
    studies["A"]["status"] = "PROVISIONAL"
    html = report.render_report(basis, grids, studies)
    for expected in ("2400", "7000", "19.000", "0.300", "0.600", "1.70", "0.90",
                     "110", "6 years", "0.120", "PROVISIONAL", "NOT EVALUATED"):
        assert expected in html
    assert "physical removed volume" in html
    assert "closed-form" in html
    assert "independent analytic" not in html
    assert "finest retained" in html


def test_preview_retains_minimum_and_equal_spatial_scale():
    grid = ev.sample(ev.AREAS[3], 6.25)
    preview = report._preview(grid)
    assert len(preview["cells"]) <= 40 * 40
    assert min(c[4] for c in preview["cells"]) == min(r["assessed_mm"] for r in grid["rows"])
    x0, x1, s0, s1 = preview["bounds"]
    cell = preview["cells"][0]
    assert cell[1] > cell[0] and cell[3] > cell[2]
    svg = report._heatmap("D", grid, 16)
    assert "Equal axial/arc scale" in svg
    assert "minimum-bin preview" in svg
    assert "colorbar-D" in svg
    assert "16.000 mm" in svg and "0.000 mm" in svg
    match = re.search(r'data-map-width="([\d.]+)" data-map-height="([\d.]+)"', svg)
    assert match
    assert abs(float(match[1]) / float(match[2]) - (x1 - x0) / (s1 - s0)) < 0.001
