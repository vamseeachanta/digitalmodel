# ABOUTME: Wind-assist integration: user-supplied thrust matrix (thrust vs true
# ABOUTME: wind speed/angle) applied over a route wind rose -> power/fuel saving.
"""Wind-assist propulsion integration (concept-design screening).

A wind-assist device (rotor, rigid wingsail, DynaRig, ...) is characterised
by a *thrust matrix*: net thrust along the ship track [kN] tabulated
against true wind speed [m/s] and true wind angle off the bow [deg,
0..180]. The matrix is a user input (vendor polar or model-test data); this
module only applies it.

Per wind-rose bin the equivalent shaft-power saving is::

    dP_shaft [kW] = T [kN] * V_ship [m/s] / eta_D

where ``T * V_ship`` is the effective (towrope) power delivered by the
device and ``eta_D`` is the quasi-propulsive coefficient of the
conventional propulsion chain the thrust displaces (a kW of towrope power
replaces ``1/eta_D`` kW of shaft power). The saving in each bin is capped
at the required shaft power (the device cannot drive the demand negative
at fixed speed). The expected saving is the probability-weighted mean over
the rose; probability mass not covered by the rose is treated as calm (no
thrust).

Interpolation: bilinear inside the matrix. True wind angle is folded into
[0, 180] (port/starboard symmetry) and clamped to the tabulated range.
True wind speed above the tabulated maximum is clamped to the edge row
(devices are sheeted out / depowered); below the tabulated minimum the
edge-row thrust is scaled by ``(tws / tws_min)**2`` (aerodynamic force
scales with dynamic pressure), reaching zero in a calm.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from digitalmodel.alt_fuel_ship_sizing.constants import KNOT_MPS


@dataclass(frozen=True)
class ThrustMatrix:
    """Thrust [kN] vs true wind speed [m/s] x true wind angle [deg 0..180]."""

    tws_mps: tuple[float, ...]
    twa_deg: tuple[float, ...]
    thrust_kn: tuple[tuple[float, ...], ...]  # [i_tws][j_twa]

    def __post_init__(self) -> None:
        if len(self.tws_mps) < 2 or len(self.twa_deg) < 2:
            raise ValueError("ThrustMatrix needs at least 2 tws and 2 twa points")
        if any(b <= a for a, b in zip(self.tws_mps, self.tws_mps[1:])):
            raise ValueError("ThrustMatrix.tws_mps must be strictly increasing")
        if any(b <= a for a, b in zip(self.twa_deg, self.twa_deg[1:])):
            raise ValueError("ThrustMatrix.twa_deg must be strictly increasing")
        if self.tws_mps[0] < 0.0:
            raise ValueError("ThrustMatrix.tws_mps must be non-negative")
        if self.twa_deg[0] < 0.0 or self.twa_deg[-1] > 180.0:
            raise ValueError("ThrustMatrix.twa_deg must lie in [0, 180]")
        if len(self.thrust_kn) != len(self.tws_mps) or any(
            len(row) != len(self.twa_deg) for row in self.thrust_kn
        ):
            raise ValueError(
                "ThrustMatrix.thrust_kn shape must be (len(tws_mps), len(twa_deg))"
            )

    @staticmethod
    def from_lists(
        tws_mps: list[float], twa_deg: list[float], thrust_kn: list[list[float]]
    ) -> "ThrustMatrix":
        return ThrustMatrix(
            tws_mps=tuple(float(v) for v in tws_mps),
            twa_deg=tuple(float(v) for v in twa_deg),
            thrust_kn=tuple(tuple(float(v) for v in row) for row in thrust_kn),
        )


@dataclass(frozen=True)
class WindRoseBin:
    """One route wind-rose bin: true wind seen by the ship, with probability."""

    tws_mps: float
    twa_deg: float
    probability: float

    def __post_init__(self) -> None:
        if self.tws_mps < 0.0:
            raise ValueError("WindRoseBin.tws_mps must be non-negative")
        if not 0.0 <= self.probability <= 1.0:
            raise ValueError("WindRoseBin.probability must be in [0, 1]")


@dataclass(frozen=True)
class WindAssistBinResult:
    tws_mps: float
    twa_deg: float
    probability: float
    thrust_kn: float
    power_saving_kw: float


@dataclass(frozen=True)
class WindAssistResult:
    expected_power_saving_kw: float
    saving_fraction: float
    effective_shaft_power_kw: float
    required_shaft_power_kw: float
    bins: list[WindAssistBinResult] = field(default_factory=list)


def fold_twa_deg(twa_deg: float) -> float:
    """Fold any wind angle to [0, 180] using port/starboard symmetry."""
    angle = abs(twa_deg) % 360.0
    return 360.0 - angle if angle > 180.0 else angle


def interpolate_thrust(matrix: ThrustMatrix, tws_mps: float, twa_deg: float) -> float:
    """Bilinear thrust lookup with the edge handling documented above."""
    if tws_mps < 0.0:
        raise ValueError("tws_mps must be non-negative")
    twa = min(max(fold_twa_deg(twa_deg), matrix.twa_deg[0]), matrix.twa_deg[-1])

    scale = 1.0
    tws = tws_mps
    if tws <= matrix.tws_mps[0]:
        low = matrix.tws_mps[0]
        scale = (tws / low) ** 2 if low > 0.0 else 1.0
        tws = low
    elif tws >= matrix.tws_mps[-1]:
        tws = matrix.tws_mps[-1]

    i = _bracket(matrix.tws_mps, tws)
    j = _bracket(matrix.twa_deg, twa)
    t0, t1 = matrix.tws_mps[i], matrix.tws_mps[i + 1]
    a0, a1 = matrix.twa_deg[j], matrix.twa_deg[j + 1]
    ft = (tws - t0) / (t1 - t0)
    fa = (twa - a0) / (a1 - a0)
    z00 = matrix.thrust_kn[i][j]
    z01 = matrix.thrust_kn[i][j + 1]
    z10 = matrix.thrust_kn[i + 1][j]
    z11 = matrix.thrust_kn[i + 1][j + 1]
    value = (
        z00 * (1.0 - ft) * (1.0 - fa)
        + z10 * ft * (1.0 - fa)
        + z01 * (1.0 - ft) * fa
        + z11 * ft * fa
    )
    return scale * value


def _bracket(grid: tuple[float, ...], value: float) -> int:
    """Index i with grid[i] <= value <= grid[i+1] (value already clamped)."""
    for i in range(len(grid) - 2, -1, -1):
        if value >= grid[i]:
            return i
    return 0


def wind_assist_saving(
    matrix: ThrustMatrix,
    wind_rose: list[WindRoseBin],
    ship_speed_kn: float,
    required_shaft_power_kw: float,
    propulsive_efficiency: float,
    probability_tolerance: float = 1e-6,
) -> WindAssistResult:
    """Expected shaft-power saving of a wind-assist device over a wind rose."""
    if ship_speed_kn <= 0.0:
        raise ValueError("ship_speed_kn must be positive")
    if required_shaft_power_kw < 0.0:
        raise ValueError("required_shaft_power_kw must be non-negative")
    if not 0.0 < propulsive_efficiency <= 1.0:
        raise ValueError("propulsive_efficiency must be in (0, 1]")
    if not wind_rose:
        raise ValueError("wind_rose must be a non-empty list")
    total_probability = sum(b.probability for b in wind_rose)
    if total_probability > 1.0 + probability_tolerance:
        raise ValueError(
            f"wind_rose probabilities sum to {total_probability:.6f} > 1"
        )

    speed_mps = ship_speed_kn * KNOT_MPS
    bins: list[WindAssistBinResult] = []
    expected = 0.0
    for item in wind_rose:
        thrust = interpolate_thrust(matrix, item.tws_mps, item.twa_deg)
        thrust = max(thrust, 0.0)  # drag-dominated bins give no propulsion credit
        saving = thrust * speed_mps / propulsive_efficiency
        saving = min(saving, required_shaft_power_kw)
        expected += item.probability * saving
        bins.append(
            WindAssistBinResult(
                tws_mps=item.tws_mps,
                twa_deg=item.twa_deg,
                probability=item.probability,
                thrust_kn=thrust,
                power_saving_kw=saving,
            )
        )

    fraction = (
        expected / required_shaft_power_kw if required_shaft_power_kw > 0.0 else 0.0
    )
    return WindAssistResult(
        expected_power_saving_kw=expected,
        saving_fraction=fraction,
        effective_shaft_power_kw=required_shaft_power_kw - expected,
        required_shaft_power_kw=required_shaft_power_kw,
        bins=bins,
    )
