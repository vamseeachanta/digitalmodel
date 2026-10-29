"""Sourced range checks for shallow-water absorption; no solver is needed."""

import json
import math
import re

import pytest

from digitalmodel.solvers.openfoam.validation.wave_tank import (
    GRAVITY,
    WaveTankConfig,
    build_wave_tank_case,
    dispersion_wavenumber,
    extract_wave_quality,
    shallow_water_absorption_status,
)


def period_for_kh(kh, depth):
    """Independent hand calculation from Higuera (2018), section 3, eq. (2).

    Choose dimensionless q=kh and depth h. Then k=q/h, omega squared
    is g*q*tanh(q)/h, and T=2*pi*sqrt(h/(g*q*tanh(q))). The existing
    gravity convention is reused, so no new gravity value is introduced.
    """
    return 2 * math.pi * math.sqrt(depth / (GRAVITY * kh * math.tanh(kh)))


@pytest.mark.parametrize("kh,valid", [
    (math.pi / 20, True), (math.pi / 5, False), (2 * math.pi, False),
])
def test_absorption_range_from_independent_dispersion(kh, valid):
    """Table 1 of Higuera (2018): kh < pi/10; kh > pi is deep water.

    Test half/double the shallow limit and twice the deep-water boundary.
    Synthetic depth h=1 m; period_for_kh documents the inverse hand
    calculation. pytest.approx's default tolerance only covers roundoff.
    """
    status = shallow_water_absorption_status(period_for_kh(kh, 1), 1)
    assert status["kh"] == pytest.approx(kh)
    assert status["kh_limit"] == math.pi / 10
    assert status["valid"] is valid
    assert status["regime"] == ("shallow" if valid else "outside_shallow_water")
    assert "1810.03492" in status["reference"]


@pytest.mark.parametrize("relation,valid", [("below", True), ("equal", False), ("above", False)])
def test_strict_absorption_boundary(monkeypatch, relation, valid):
    """Higuera (2018), Table 1 uses strict '<', with no tolerance band.

    Inject k at depth 1 to isolate the comparator from dispersion rounding;
    adjacent floats on each side are obtained with nextafter, not a margin.
    """
    limit = math.pi / 10
    kh = {"below": math.nextafter(limit, 0), "equal": limit,
          "above": math.nextafter(limit, math.inf)}[relation]
    monkeypatch.setattr(
        "digitalmodel.solvers.openfoam.validation.wave_tank.dispersion_wavenumber",
        lambda period, depth: kh,
    )
    assert shallow_water_absorption_status(1, 1)["valid"] is valid


@pytest.mark.parametrize("bad", [0, -1, math.nan, math.inf, -math.inf])
def test_absorption_rejects_invalid_period_or_depth(bad, tmp_path):
    """Non-finite/nonpositive inputs cannot define the cited dispersion law."""
    for period, depth in [(bad, 1), (1, bad)]:
        with pytest.raises(ValueError, match="finite and positive"):
            dispersion_wavenumber(period, depth)
        with pytest.raises(ValueError, match="finite and positive"):
            build_wave_tank_case(WaveTankConfig(wave_period=period, depth=depth), tmp_path)
    assert list(tmp_path.iterdir()) == []


def test_builder_rejects_historical_default_without_writing(tmp_path):
    """Default T=3 s, h=0.4 m is outside Table 1; no replacement is assumed.

    Independent check: at q=pi/10, g*q*tanh(q)/h < (2*pi/T)^2.
    Since q*tanh(q) increases for positive q, the solution exceeds pi/10.
    """
    cfg = WaveTankConfig()
    q = math.pi / 10
    assert GRAVITY * q * math.tanh(q) / cfg.depth < (2 * math.pi / cfg.wave_period)**2
    with pytest.raises(ValueError, match="shallowWaterAbsorption.*kh.*codedSource"):
        build_wave_tank_case(cfg, tmp_path / "uncreated")
    assert not (tmp_path / "uncreated").exists()


def test_builder_preserves_existing_case_on_rejection(tmp_path):
    """Rejection must precede writing to a previously created case."""
    cfg = WaveTankConfig()
    case = tmp_path / cfg.name
    case.mkdir()
    marker = case / "provenance.json"
    marker.write_text("previous case\n")
    with pytest.raises(ValueError, match="shallowWaterAbsorption"):
        build_wave_tank_case(cfg, tmp_path)
    assert marker.read_text() == "previous case\n"
    assert list(case.iterdir()) == [marker]


@pytest.mark.parametrize("kh", [math.pi / 5, 2 * math.pi])
def test_builder_refuses_intermediate_and_deep_water(tmp_path, kh):
    """Prescribe intermediate/deep kh from Higuera (2018), Table 1.

    The inverse hand calculation supplies periods at h=1 m. Neither
    outside regime permits writing a shallow-water absorber.
    """
    cfg = WaveTankConfig(wave_period=period_for_kh(kh, 1), depth=1)
    with pytest.raises(ValueError, match="shallowWaterAbsorption"):
        build_wave_tank_case(cfg, tmp_path)
    assert list(tmp_path.iterdir()) == []


def test_valid_build_records_absorption_status(tmp_path):
    """Use the manufactured shallow q=pi/20 case from the inverse calculation."""
    # The historical depth is retained so the waterline remains inside the tank.
    depth = WaveTankConfig().depth
    cfg = WaveTankConfig(wave_period=period_for_kh(math.pi / 20, depth), depth=depth)
    case = build_wave_tank_case(cfg, tmp_path)
    status = json.loads((case / "provenance.json").read_text())["absorption"]
    assert status["kh"] == pytest.approx(math.pi / 20)
    assert status["valid"] is True
    assert "shallowWaterAbsorption" in (case / "constant/waveProperties").read_text()


@pytest.mark.parametrize("kh,valid", [(math.pi / 20, True), (math.pi / 5, False)])
def test_extraction_labels_historical_results(tmp_path, kh, valid):
    """Synthetic gauges allow both regimes to be inspected without a solve.

    Select q, h=1 m, k=q/h, and T by the inverse hand calculation above.
    Elevations are h+a*cos(omega*t-k*x), with arbitrary a=1/100 m.
    Only regime metadata is asserted; no simulated reflection is claimed.
    """
    import numpy as np

    cfg = WaveTankConfig(wave_period=period_for_kh(kh, 1), depth=1)
    # Four periods with 100 samples per period resolve zero crossings;
    # these are synthetic sampling choices, not physical acceptance limits.
    t = np.linspace(0, 4 * cfg.wave_period, 401)
    eta = [cfg.depth + 0.01 * np.cos(2 * math.pi * t / cfg.wave_period - kh * x)
           for x in cfg.gauges_x]
    gauge_dir = tmp_path / "postProcessing/waveGauges/0"
    gauge_dir.mkdir(parents=True)
    np.savetxt(gauge_dir / "height.dat", np.column_stack([t, *eta]))
    result = extract_wave_quality(tmp_path, cfg, window=(t[0], t[-1]))
    assert result["absorption"]["kh"] == pytest.approx(kh)
    assert result["absorption"]["valid"] is valid


@pytest.mark.parametrize("depth,period", [(0.3, 3.554184), (0.3000026, 3.5542)])
@pytest.mark.parametrize("existing", [False, True])
def test_builder_rejects_serialized_boundary_crossing(tmp_path, depth, period, existing):
    """Six significant digits can move either input outside Table 1's range.

    The inverse dispersion calculation places each requested pair inside
    the limit and its serialized pair outside, without replacing the solver.
    """
    limit_period = period_for_kh(math.pi / 10, depth)
    assert period > limit_period
    emitted_depth, emitted_period = float(f"{depth:.6g}"), float(f"{period:.6g}")
    assert emitted_period < period_for_kh(math.pi / 10, emitted_depth)
    cfg = WaveTankConfig(depth=depth, wave_period=period)
    parent = tmp_path / "cases"
    case = parent / cfg.name
    if existing:
        case.mkdir(parents=True)
        (case / "provenance.json").write_text("previous case\n")
    with pytest.raises(ValueError, match="shallowWaterAbsorption"):
        build_wave_tank_case(cfg, parent)
    if existing:
        assert (case / "provenance.json").read_text() == "previous case\n"
        assert [p.name for p in case.iterdir()] == ["provenance.json"]
    else:
        assert not parent.exists()


def test_serialized_inputs_and_provenance_agree(tmp_path):
    """Read the emitted dictionaries and independently check their regime.

    Both inputs lose precision, while remaining inside the strict limit.
    Provenance must describe those emitted values and their wave theory.
    """
    cfg = WaveTankConfig(depth=0.3000004, wave_period=3.554194)
    case = build_wave_tank_case(cfg, tmp_path)
    waveprops = (case / "constant/waveProperties").read_text()
    setfields = (case / "system/setFieldsDict").read_text()
    period = float(re.search(r"wavePeriod\s+([^;]+);", waveprops).group(1))
    depth = float(re.search(r"box\s+\([^)]*\)\s+\([^)]*\s+(\S+)\);", setfields).group(1))
    assert period != cfg.wave_period
    assert depth != cfg.depth
    q = math.pi / 10
    assert (2 * math.pi / period)**2 < GRAVITY * q * math.tanh(q) / depth
    status = shallow_water_absorption_status(period, depth)
    assert status["valid"] is True
    prov = json.loads((case / "provenance.json").read_text())
    assert prov["absorption"] == status
    assert prov["wave"]["T_s"] == period
    assert prov["wave"]["depth_m"] == depth
    k = dispersion_wavenumber(period, depth)
    assert prov["wave"]["wavenumber_rad_m"] == k
    assert prov["wave"]["wavelength_m"] == 2 * math.pi / k
    assert prov["wave"]["celerity_m_s"] == (2 * math.pi / period) / k
