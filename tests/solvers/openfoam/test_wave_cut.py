"""Synthetic regression tests for the OpenFOAM wave-cut reducer."""

import csv
import json
import math

import pytest

from digitalmodel.solvers.openfoam.wave_cut import reduce_files


def _write_plane(path, wavelength=10.0, amplitude=0.4, waterline=3.0):
    rows = []
    for i in range(401):
        x = -20.0 + i * 0.1
        surface = waterline + amplitude * math.sin(2.0 * math.pi * x / wavelength)
        for dz in (-0.6, -0.2, 0.2, 0.6):
            z = surface + dz
            alpha = max(0.0, min(1.0, 0.5 - dz / 0.4))
            rows.append((x, 5.0, z, alpha))
    path.write_text(
        f"# alpha.water  POINT_DATA {len(rows)}\n# x y z  alpha.water\n"
        + "".join("%.6f %.6f %.6f %.6f\n" % row for row in rows)
    )


def _write_iso(path, wavelength=10.0, amplitude=0.4, waterline=3.0):
    rows = []
    for i in range(401):
        x = -20.0 + i * 0.1
        for y in (-3.0, -1.0, 1.0, 3.0):
            eta = amplitude * math.sin(2.0 * math.pi * x / wavelength)
            rows.append((x, y, waterline + eta, 0.5))
    path.write_text(
        f"# alpha.water  POINT_DATA {len(rows)}\n# x y z  alpha.water\n"
        + "".join("%.6f %.6f %.6f %.6f\n" % row for row in rows)
    )


def test_plane_recovers_wave_and_writes_outputs(tmp_path):
    raw = tmp_path / "cut.raw"
    _write_plane(raw)
    summary = reduce_files(
        {"cut": raw}, tmp_path / "out", waterline=3.0, stern=20.1,
        wavelength=10.0, bin_width=0.1,
    )

    cut = summary["cuts"]["cut"]
    assert cut["dominant_wavelength_zero_crossing_m"] == pytest.approx(10, rel=0.02)
    assert cut["dominant_wavelength_fft_m"] == pytest.approx(10, rel=0.02)
    assert cut["crest"]["eta_m"] == pytest.approx(0.4, rel=0.02)
    assert cut["trough"]["eta_m"] == pytest.approx(-0.4, rel=0.02)
    assert (tmp_path / "out" / "cut.csv").is_file()
    assert (tmp_path / "out" / "wave_cut.svg").read_text().startswith("<?xml")
    assert json.loads((tmp_path / "out" / "summary.json").read_text()) == summary
    with (tmp_path / "out" / "cut.csv").open() as stream:
        assert next(csv.reader(stream)) == ["x", "eta"]


def test_iso_surface_produces_half_wavelength_wedge_rms_bins(tmp_path):
    raw = tmp_path / "iso.raw"
    _write_iso(raw)
    summary = reduce_files(
        {"iso": raw}, tmp_path / "out", waterline=3.0, stern=0.1,
        wavelength=10.0, bin_width=0.5,
    )

    iso = summary["iso_surfaces"]["iso"]
    assert iso["kind"] == "iso_surface"
    assert iso["radial_bin_width_m"] == 5.0
    assert len(iso["wedge_rms"]) >= 3
    assert all(item["rms_eta_m"] == pytest.approx(0.4 / math.sqrt(2), rel=0.08)
               for item in iso["wedge_rms"] if item["point_count"] >= 100)


def test_rejects_malformed_raw_file(tmp_path):
    raw = tmp_path / "bad.raw"
    raw.write_text("# x y z alpha.water\n1 2 nope 0.5\n")
    with pytest.raises(ValueError, match="no numeric rows"):
        reduce_files({"bad": raw}, tmp_path / "out", 0.0, 0.0, 10.0, 0.5)
