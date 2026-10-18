"""Independent regressions for audit sections 1-9; references in citation sidecar."""

import numpy as np
import pytest
from pydantic import ValidationError
from digitalmodel.orcawave.panel_mesh import (
    GDFMesh,
    PanelQuad,
    compute_mesh_summary,
    read_gdf,
    write_gdf,
)
from digitalmodel.orcawave.motion_statistics import (
    ResponseSpectrum,
    ScatterCell,
    short_term_statistics,
    long_term_extreme,
    motion_sickness_incidence,
    wf_weighting,
)
from digitalmodel.orcawave.wave_spectrum import (
    SpectrumParameters,
    generate_spectrum,
    pierson_moskowitz,
)
from digitalmodel.orcawave.hydro_coefficients import (
    from_native_added_mass,
    to_wamit_added_mass,
    to_wamit_damping,
    from_wamit_added_mass,
)


def box(lid=True, inward=False, half=False):
    """Outward 2 x 2 x 2 box, or y>=0 half with symmetry."""
    x, y = 1.0, (1.0 if half else 2.0)
    faces = [
        [[-x, 0, -2], [-x, y, -2], [x, y, -2], [x, 0, -2]],
        [[-x, 0, -2], [-x, 0, 0], [-x, y, 0], [-x, y, -2]],
        [[x, 0, -2], [x, y, -2], [x, y, 0], [x, 0, 0]],
        [[-x, 0, -2], [x, 0, -2], [x, 0, 0], [-x, 0, 0]],
        [[-x, y, -2], [-x, y, 0], [x, y, 0], [x, y, -2]],
    ]
    if lid:
        faces.append([[-x, 0, 0], [x, 0, 0], [x, y, 0], [-x, y, 0]])
    return GDFMesh(
        isy=int(half),
        panels=[PanelQuad(vertices=f[::-1] if inward else f) for f in faces],
    )


@pytest.mark.parametrize("lid,half", [(True, False), (False, False), (False, True)])
def test_01_volume_waterline(lid, half):
    summary = compute_mesh_summary(box(lid=lid, half=half))
    assert summary.displaced_volume == pytest.approx(8, rel=0.005)
    assert summary.waterplane_area == pytest.approx(4, rel=0.005)


def test_01_inward_flagged():
    summary = compute_mesh_summary(box(inward=True))
    assert summary.displaced_volume == -8
    assert any("Nonpositive signed volume" in x for x in summary.refinement_suggestions)


def test_04_rayleigh_maxima():
    # sigma=1, Tz=10 s, N=1000; Gumbel expansion Emax=b+Euler/b.
    response = ResponseSpectrum(
        frequencies=[], spectral_density=[], m0=1, m2=(2 * np.pi / 10) ** 2, m4=1
    )
    stats = short_term_statistics(response, duration_s=10000)
    assert stats.most_probable_maximum_3hr == pytest.approx(3.71692218885, rel=1e-5)
    assert stats.expected_maximum_3hr == pytest.approx(3.8722137659, rel=1e-5)


def test_05_significant_amplitudes():
    from digitalmodel.orcawave.motion_statistics import compute_response_spectrum

    omega = np.linspace(0.1, 2, 200)
    r = compute_response_spectrum(
        omega, np.full(200, 0.8), pierson_moskowitz(omega, 3, 10)
    )
    stats = short_term_statistics(r)
    assert stats.significant_single_amplitude == pytest.approx(
        2 * np.sqrt(r.m0), rel=1e-9
    )
    assert stats.significant_double_amplitude == pytest.approx(
        4 * np.sqrt(r.m0), rel=1e-9
    )
    with pytest.warns(DeprecationWarning):
        assert stats.significant == stats.significant_single_amplitude


def test_02_single_cell_cycles():
    # Two bins with energy only at omega=2pi/10: m0=1, Tz=10 exactly.
    omega = np.array([2 * np.pi / 10, 2 * np.pi / 10 + 1])
    result = long_term_extreme(
        [ScatterCell(hs=1, tp=10, probability=1)],
        np.ones(2),
        omega,
        lambda *args: np.array([2.0, 0.0]),
    )
    assert result.extreme_response == pytest.approx(6.256365056, rel=0.001)


def test_02_two_cell_cycle_mixture():
    # Rates p/Tz=.04,.03; sigmas 1,2; 100-year return.
    # Use separate spectra on an increasing grid with exact moments at endpoints.
    omega = np.array([2 * np.pi / 20, 2 * np.pi / 10])
    width = omega[1] - omega[0]

    def spectrum(w, hs, tp):
        return np.array([0, 2 / width]) if hs == 1 else np.array([8 / width, 0])

    result = long_term_extreme(
        [
            ScatterCell(hs=1, tp=10, probability=0.4),
            ScatterCell(hs=2, tp=20, probability=0.6),
        ],
        np.ones(2),
        omega,
        spectrum,
    )
    # rates are .04 and .03; hand root sqrt(8*ln(.03*3155760000)).
    assert result.extreme_response == pytest.approx(12.1213653667, rel=0.001)


def test_02_probability_sum():
    with pytest.raises(ValueError, match="sum to 1"):
        long_term_extreme(
            [ScatterCell(hs=1, tp=10, probability=0.8)],
            np.ones(2),
            np.array([0.1, 0.2]),
            lambda *args: np.ones(2),
        )


def test_03_annex_d_weighted_dose():
    result = motion_sickness_incidence(0.5, 2, 0.15, already_weighted=True)
    assert result.msdv_z == pytest.approx(42.426406871, rel=1e-9)
    assert result.msi_percent == pytest.approx(14.142135624, rel=0.001)
    assert "Eq. (D.2)" in result.citations[0]
    assert "verification pending" in result.qualification


@pytest.mark.parametrize(
    "frequency,expected", [(0.1, 0.6951), (10 ** (-0.8), 1.004), (1, 0.02352)]
)
def test_03_wf_table_b5(frequency, expected):
    assert wf_weighting(frequency) == pytest.approx(expected, rel=0.0005)


def test_03_explicit_inputs():
    with pytest.raises(TypeError):
        motion_sickness_incidence(0.5)


def test_06_symmetry_defaults_and_domain():
    mesh = box()
    assert GDFMesh(panels=mesh.panels).isx == GDFMesh().isy == 0
    text = write_gdf(mesh).splitlines()
    text[2] = "0"
    assert read_gdf("\n".join(text)).isy == 0
    with pytest.raises(ValidationError):
        GDFMesh(isy=2)


def test_07_explicit_two_peak():
    with pytest.raises(ValidationError):
        SpectrumParameters(spectrum_type="OchiHubble", hs=4, tp=10)
    result = generate_spectrum(
        SpectrumParameters(
            spectrum_type="OchiHubble", hs=4, tp=10, hs2=2, tp2=16, lambda1=2, lambda2=1
        ),
        0.01,
        10,
        20000,
    )
    assert result.hs_computed == pytest.approx(np.sqrt(20), rel=0.01)


def test_08_native_fixture_and_wamit_refusal():
    # Independently written SI fixture, explicitly omega rather than WAMIT PER.
    m = from_native_added_mass("0.6283185307179586 3 3 8200000\n")[0]
    assert m.frequency_rad_s == pytest.approx(2 * np.pi / 10)
    assert m.to_numpy()[2, 2] == 8200000
    for function in (to_wamit_added_mass, to_wamit_damping, from_wamit_added_mass):
        with pytest.raises(ValueError, match="WAMIT"):
            function("10 3 3 1 0")


def test_09_vertex_stream():
    text = write_gdf(box())
    lines = text.splitlines()
    tokens = " ".join(lines[4:]).split()
    vertices = [" ".join(tokens[i : i + 3]) for i in range(0, len(tokens), 3)]
    parsed = read_gdf("\n".join(lines[:4] + vertices))
    assert len(parsed.panels) == 6
    assert compute_mesh_summary(parsed).displaced_volume == 8


@pytest.mark.parametrize("body", ["0 0 -2 " * 20, "0 0 -2 " * 28, "nan " * 72])
def test_09_bad_count_or_nonfinite(body):
    with pytest.raises(ValueError):
        read_gdf("fixture\n1 9.81\n0 0\n6\n" + body)


def test_09_negative_npan():
    with pytest.raises(ValueError, match="NPAN"):
        read_gdf("fixture\n1 9.81\n0 0\n-1\n")


def test_01_zero_volume_flagged():
    summary = compute_mesh_summary(GDFMesh())
    assert summary.displaced_volume == 0
    assert any(
        "Nonpositive signed volume" in item for item in summary.refinement_suggestions
    )


def test_11_complex_response_energy():
    from digitalmodel.orcawave.motion_statistics import compute_response_spectrum

    # |2i|**2 * unit density integrated over a unit-wide grid = 4.
    result = compute_response_spectrum(
        np.array([1.0, 2.0]), np.array([2j, 2j]), np.ones(2)
    )
    assert result.m0 == pytest.approx(4, rel=1e-12)
