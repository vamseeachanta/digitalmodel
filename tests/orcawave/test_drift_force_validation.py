# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Input-domain tests and hand calculations for slow drift.

Reference: Orcina WebHelp (accessed 2026-10-23), 'Vessel theory: Wave drift and
sum frequency loads', 'Second order load theory'. All inputs are constructed,
not measured. Numerical test tolerances are specified per calculation below.
"""

import numpy as np
import pytest

from digitalmodel.orcawave.drift_forces import (
    MeanDriftCoefficients,
    QTFMatrix,
    full_qtf_slowly_varying,
    newman_approximation,
)


def _qtf(omega, real=None, imag=None):
    n = len(omega)
    return QTFMatrix(
        dof="surge", heading_deg=0, frequencies_rad_s=list(omega),
        real_part=np.ones((n, n)).tolist() if real is None else real,
        imag_part=np.zeros((n, n)).tolist() if imag is None else imag,
    )


def _coefficients(omega, values=None):
    values = np.ones(len(omega)).tolist() if values is None else values
    return MeanDriftCoefficients(
        frame="x_forward_y_port_z_up", heading_convention="from_clockwise_bow",
        heading_deg=0, frequencies_rad_s=list(omega),
        surge_kn_m2=values, sway_kn_m2=values, yaw_knm_m2=values,
    )


def _evaluate(method, omega, spectrum=None, **kwargs):
    spectrum = np.ones(len(omega)) if spectrum is None else spectrum
    if method == "full":
        return full_qtf_slowly_varying(_qtf(omega), spectrum, omega, **kwargs)
    return newman_approximation(_coefficients(omega), spectrum, omega, **kwargs)


@pytest.mark.parametrize("method", ["full", "newman"])
@pytest.mark.parametrize("omega, message", [
    ([], "nonempty one-dimensional"),
    ([1, 1, 2], "strictly increasing"),
    ([3, 2, 1], "strictly increasing"),
    ([1, 2, 4], "uniform"),
    ([0, 1, 2], "positive"),
    ([-1, 1, 3], "positive"),
    ([1, np.nan, 3], "finite"),
    ([1, 2, np.inf], "finite"),
])
def test_invalid_grids_are_refused(method, omega, message):
    with pytest.raises(ValueError, match=message):
        _evaluate(method, omega)


@pytest.mark.parametrize("method", ["full", "newman"])
@pytest.mark.parametrize("spectrum, message", [
    ([1, 2], "shape"),
    ([[1, 2, 3]], "shape"),
    ([1, -1, 1], "nonnegative"),
    ([1, np.nan, 1], "finite"),
    ([1, np.inf, 1], "finite"),
])
def test_invalid_spectra_are_refused(method, spectrum, message):
    with pytest.raises(ValueError, match=message):
        _evaluate(method, [1, 2, 3], spectrum)


@pytest.mark.parametrize("method", ["full", "newman"])
def test_grid_must_be_one_dimensional(method):
    # Build a valid model first so this exercises function validation, not Pydantic.
    model = _qtf([1, 2]) if method == "full" else _coefficients([1, 2])
    function = full_qtf_slowly_varying if method == "full" else newman_approximation
    with pytest.raises(ValueError, match="one-dimensional"):
        function(model, [1, 1], [[1, 2]])


@pytest.mark.parametrize("method", ["full", "newman"])
@pytest.mark.parametrize("rtol", [-1, np.nan, np.inf, 1])
def test_invalid_grid_tolerances_are_refused(method, rtol):
    # rtol < 1 is necessary to distinguish zero from a missing nonzero partner.
    with pytest.raises(ValueError, match="grid_rtol"):
        _evaluate(method, [1, 2, 3], grid_rtol=rtol)


@pytest.mark.parametrize("rtol", [-1, np.nan, np.inf, 1])
def test_invalid_qtf_tolerances_are_refused(rtol):
    with pytest.raises(ValueError, match="qtf_rtol"):
        _evaluate("full", [1, 2, 3], qtf_rtol=rtol)


@pytest.mark.parametrize("method", ["full", "newman"])
def test_grid_tolerance_is_explicit_and_relative(method):
    # Constructed perturbation: spacings 1 and 1.001. A 1 % user limit accepts;
    # exact equality refuses. These are test inputs, not engineering criteria.
    omega = [1, 2, 3.001]
    _evaluate(method, omega, grid_rtol=0.01)
    with pytest.raises(ValueError, match="uniform"):
        _evaluate(method, omega, grid_rtol=0)


@pytest.mark.parametrize("method", ["full", "newman"])
def test_default_grid_tolerance_accepts_linspace_roundoff(method):
    _evaluate(method, np.linspace(0.2, 1.224, 257))


@pytest.mark.parametrize("method", ["full", "newman"])
def test_single_component_has_no_beat_pairs(method):
    # No i<j pairs exist; the exact variance and Newman zero-lag PSD are zero.
    np.testing.assert_array_equal(_evaluate(method, [1]), 0)


@pytest.mark.parametrize("real, imag, message", [
    ([[1, 2], [2]], [[0, 0], [0, 0]], "complete square"),
    ([[1, 2]], [[0, 0], [0, 0]], "complete square"),
    ([[1, 2], [2, 1]], [[0]], "complete square"),
    ([[1, np.nan], [np.nan, 1]], [[0, 0], [0, 0]], "finite"),
    ([[1, 2], [2, 1]], [[0, np.inf], [-np.inf, 0]], "finite"),
    ([[1, 2], [0, 1]], [[0, 0], [0, 0]], "Hermitian"),
    ([[1, 0], [2, 1]], [[0, 0], [0, 0]], "Hermitian"),
    ([[1, 2], [3, 1]], [[0, 0], [0, 0]], "Hermitian"),
    ([[1, 2], [2, 1]], [[0, 1], [1, 0]], "Hermitian"),
    ([[1, 2], [2, 1]], [[1, 0], [0, 0]], "real diagonal"),
])
def test_incomplete_or_nonhermitian_qtfs_are_refused(real, imag, message):
    with pytest.raises(ValueError, match=message):
        full_qtf_slowly_varying(_qtf([1, 2], real, imag), [1, 1], [1, 2])


@pytest.mark.parametrize("scale", [1e-20, 1, 1e20])
def test_missing_triangle_is_refused_at_any_coefficient_scale(scale):
    with pytest.raises(ValueError, match="Hermitian"):
        full_qtf_slowly_varying(
            _qtf([1, 2], [[0, 0], [scale, 0]]), [1, 1], [1, 2],
        )


def test_qtf_relative_tolerance_does_not_fill_or_mutate_data():
    qtf = _qtf([1, 2], [[1, 2], [2.001, 1]])
    before = qtf.model_dump()
    full_qtf_slowly_varying(qtf, [1, 1], [1, 2], qtf_rtol=0.01)
    assert qtf.model_dump() == before
    with pytest.raises(ValueError, match="Hermitian"):
        full_qtf_slowly_varying(qtf, [1, 1], [1, 2], qtf_rtol=0)


def test_qtf_integration_grid_is_validated_separately():
    with pytest.raises(ValueError, match="QTF frequencies.*uniform"):
        full_qtf_slowly_varying(_qtf([1, 2, 4]), [1, 1, 1], [1, 2.5, 4])


@pytest.mark.parametrize("frequencies, message", [
    ([], "nonempty"),
    ([1, 1], "strictly increasing"),
    ([2, 1], "strictly increasing"),
    ([0, 1], "positive"),
    ([1, np.nan], "finite"),
])
def test_invalid_qtf_coordinates_with_valid_spectrum(frequencies, message):
    with pytest.raises(ValueError, match=f"QTF frequencies.*{message}"):
        full_qtf_slowly_varying(_qtf(frequencies), [1, 1], [1, 2])


@pytest.mark.parametrize("imag", [[[0, 1], [0, 0]], [[0, 0], [-1, 0]]])
def test_large_real_part_cannot_mask_missing_imaginary_triangle(imag):
    # Constructed scale separation, not a physical coefficient assumption.
    with pytest.raises(ValueError, match="Hermitian"):
        full_qtf_slowly_varying(
            _qtf([1, 2], [[1e20, 1e20], [1e20, 1e20]], imag), [1, 1], [1, 2],
        )


def test_zero_qtf_is_a_complete_valid_matrix():
    # All pair amplitudes are exactly zero, hence their variance sum is zero.
    assert full_qtf_slowly_varying(
        _qtf([1, 2], [[0, 0], [0, 0]]), [1, 1], [1, 2],
    ) == 0


def test_singleton_still_requires_valid_qtf():
    with pytest.raises(ValueError, match="real diagonal"):
        full_qtf_slowly_varying(_qtf([1], [[1]], [[1]]), [1], [1])


@pytest.mark.parametrize("frequencies", [[1, 2], [3, 4]])
def test_full_qtf_rejects_spectrum_extrapolation(frequencies):
    with pytest.raises(ValueError, match="cover"):
        full_qtf_slowly_varying(_qtf(frequencies), [1, 1], [2, 3])


@pytest.mark.parametrize("dof", ["surge", "sway", "yaw"])
def test_worked_newman_example(dof):
    """Hand calculation using the cited second-order convention.

    Construct omega=[1,2,3], S=[1,2,1], D=[1,2,3], dw=1. Arithmetic Newman
    Q12=1.5, Q23=2.5, Q13=2. For mu=1, PSD=8*(1.5^2*1*2+2.5^2*2*1)=136;
    for mu=2, PSD=8*2^2*1*1=32. Thus variance=168 and SD=sqrt(168).
    The 1e-12 comparison is a numerical test budget for these exact small sums.
    """
    omega, spectrum = [1, 2, 3], [1, 2, 1]
    result = newman_approximation(_coefficients(omega, [1, 2, 3]), spectrum, omega, dof)
    np.testing.assert_array_equal(result, [0, 136, 32])
    qtf = _qtf(omega, [[1, 1.5, 2], [1.5, 2, 2.5], [2, 2.5, 3]])
    assert full_qtf_slowly_varying(qtf, spectrum, omega) == pytest.approx(
        np.sqrt(168), rel=1e-12,
    )


def test_complex_hermitian_pair_and_interpolated_spectrum():
    """Hand result: Q12=3+4i => |Q12|^2=25, dw=1, S1=1 and S2=2.

    Spectrum [1,1.5,2] interpolates at QTF frequencies [1,2]. Variance is
    8*25*1*2=400, so SD=20 exactly, to a 1e-12 numerical comparison budget.
    """
    qtf = _qtf([1, 2], [[1, 3], [3, 2]], [[0, 4], [-4, 0]])
    assert full_qtf_slowly_varying(qtf, [1, 1.5, 2], [1, 1.5, 2]) == pytest.approx(
        20, rel=1e-12,
    )


@pytest.mark.parametrize("frequencies, values, message", [
    ([], [], "nonempty"),
    ([2, 1], [1, 1], "strictly increasing"),
    ([1, 1], [1, 1], "strictly increasing"),
    ([1, np.nan], [1, 1], "finite"),
    ([1, 2], [1], "shape"),
    ([1, 2], [1, np.nan], "finite"),
])
def test_newman_coefficients_are_validated(frequencies, values, message):
    with pytest.raises(ValueError, match=message):
        newman_approximation(_coefficients(frequencies, values), [1, 1], [1, 2])


def test_newman_rejects_unknown_dof():
    with pytest.raises(ValueError, match="dof"):
        _evaluate("newman", [1, 2], dof="heave")


def test_newman_allows_nonuniform_coefficient_interpolation_grid():
    # Only the integration grid must be uniform. Constant D=1 and S=1 on three
    # unit-spaced bins give PSD[1]=8*(1+1)=16, PSD[2]=8*1=8 exactly.
    result = newman_approximation(_coefficients([1, 1.25, 3]), [1, 1, 1], [1, 2, 3])
    np.testing.assert_array_equal(result, [0, 16, 8])
