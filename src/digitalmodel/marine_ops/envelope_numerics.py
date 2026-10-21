"""Envelope input and quadrature contracts; see docs/envelope-basis.md B1–B5."""
import numpy as np

_trapezoid = getattr(np, "trapezoid", None) or np.trapz

DOF_UNITS = dict(surge='m', sway='m', heave='m', roll='deg', pitch='deg', yaw='deg')
MAX_EXTRAPOLATED_ENERGY = .01  # Response-moment screening tolerance, basis B4.


def positive(value, name):
    if not np.isscalar(value) or not np.isfinite(value) or value <= 0:
        raise ValueError(f'{name} must be finite and positive')


def finite_grid(values, name, *, positive_values=False):
    array = np.asarray(values, dtype=float)
    if array.ndim != 1 or not array.size or not np.all(np.isfinite(array)):
        raise ValueError(f'{name} must be a non-empty finite 1D grid')
    if positive_values and np.any(array <= 0):
        raise ValueError(f'{name} must be positive')
    return array


def validate_operation(op):
    positive(op.alpha, 'alpha')
    if op.alpha > 1:
        raise ValueError('alpha must be <= 1')
    if not op.limits:
        raise ValueError('operation must contain motion limits')
    for limit in op.limits:
        if limit.dof not in DOF_UNITS:
            raise ValueError(f'unknown DOF: {limit.dof}')
        if limit.unit != DOF_UNITS[limit.dof]:
            raise ValueError(f'invalid motion unit for {limit.dof}: {limit.unit}')
        positive(limit.max_significant, 'motion limit')


def validate_component(component, dof):
    if component is None or component.dof.name.lower() != dof:
        raise ValueError(f'missing or mismatched RAO component: {dof}')
    if component.unit != DOF_UNITS[dof] + '/m':
        raise ValueError(f'unsupported RAO unit for {dof}: {component.unit}')
    if component.frequencies.unit != 'rad/s' or component.headings.unit != 'deg':
        raise ValueError('frequency/heading units must be rad/s and deg')
    freqs = finite_grid(component.frequencies.values, 'frequencies', positive_values=True)
    heads = finite_grid(component.headings.values, 'headings')
    if len(freqs) < 2 or len(np.unique(freqs)) != len(freqs):
        raise ValueError('frequencies must contain at least two distinct values')
    if len(np.unique(heads % 360)) != len(heads):
        raise ValueError('headings must be distinct modulo 360')
    magnitude = np.asarray(component.magnitude, dtype=float)
    if magnitude.shape != (len(freqs), len(heads)):
        raise ValueError('RAO magnitude shape must be (n_frequency, n_heading)')
    if len(freqs) == len(heads):
        raise ValueError('ambiguous square RAO grid: provide a non-square grid')
    if not np.all(np.isfinite(magnitude)) or np.any(magnitude < 0):
        raise ValueError('RAO magnitude must be finite and non-negative')


def integrate(x, y):
    return float(_trapezoid(y, x))


def spectrum(spectra, hs, tp, gamma):
    """Peak-scaled JONSWAP; captured raw variance before Hs normalization (B3)."""
    wp = 2 * np.pi / tp
    omega, density = spectra.jonswap(
        hs, tp, gamma=gamma, freq_min=.01 * wp, freq_max=8 * wp, n_points=4097,
    )
    if (not np.all(np.isfinite(density)) or np.any(density < 0)
            or not np.isfinite(integrate(omega, density)) or integrate(omega, density) <= 0):
        raise ValueError('spectrum must contain finite positive variance')
    # Dimensionless raw shape: integral PM = 1. Enhancement is unity at 8wp.
    x = omega / wp
    sigma = np.where(x <= 1, .07, .09)
    raw = 5 * x**-5 * np.exp(-1.25 / x**4) * gamma**np.exp(
        -(x - 1)**2 / (2 * sigma**2))
    captured = integrate(x, raw)
    tail = -np.expm1(-1.25 / 8**4)  # B3 exact PM tail; JONSWAP enhancement underflows.
    fraction = captured / (captured + tail)
    if not np.isfinite(fraction) or fraction < .99:
        raise ValueError('spectrum captured variance must be at least 99%')
    return omega, density, float(fraction)


def extrapolated_fraction(component, omega, density, rao_on_omega):
    """Out-of-band response-moment share under held-edge continuation (B4).

    This bound assumes no unseen out-of-band resonance. The caller gates each
    required heading, including headings that do not govern the forecast limit.
    """
    freqs = np.asarray(component.frequencies.values)
    low = max(float(freqs.min()), float(omega[0]))
    high = min(float(freqs.max()), float(omega[-1]))
    grid = np.unique(np.r_[omega, low, high]) if low < high else omega
    magnitude = np.interp(grid, omega, rao_on_omega)
    if low < high:
        # At an inserted boundary use the held edge, not an interpolation across
        # the last in-band and first out-of-band calculation samples.
        magnitude[grid <= low] = rao_on_omega[0]
        magnitude[grid >= high] = rao_on_omega[-1]
    with np.errstate(over='ignore', invalid='ignore'):
        response = magnitude**2 * np.interp(grid, omega, density)
    total = integrate(grid, response)
    if not np.isfinite(total) or total < 0:
        raise ValueError('response moment must be finite and non-negative')
    if total == 0:
        return 0.0
    if low >= high:
        return 1.0
    # Integrate both tails explicitly to avoid cancellation for decaying edges.
    below, above = grid <= low, grid >= high
    excluded = integrate(grid[below], response[below]) + integrate(grid[above], response[above])
    return float(np.clip(excluded / total, 0, 1))


def heading_column(available, target):
    """Circular distance, bounded by half the smallest observed step (B5)."""
    angles = np.asarray(available) % 360
    distance = np.abs((angles - target + 180) % 360 - 180)
    column = int(np.argmin(distance))
    sorted_angles = np.sort(angles)
    steps = np.diff(np.r_[sorted_angles, sorted_angles[0] + 360])
    tolerance = float(steps.min() / 2) if len(angles) > 1 else 0.
    if distance[column] > tolerance + 1e-9:
        raise ValueError(f'heading {target} exceeds supported heading mismatch')
    return column
