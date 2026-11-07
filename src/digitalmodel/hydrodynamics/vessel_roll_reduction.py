"""Dimensional roll reduction from supplied, linearized tank impedance.

No empirical tank coefficients or vessel defaults are supplied. This supplements
``utube_tank`` without treating its normalized authority as vessel-specific data.
SOURCES identifies public equations, accessed 2026-10-19; online editions have
no stated revision. This module neither imports nor invokes a licensed solver.
"""

from dataclasses import asdict, dataclass
from html import escape
import math
from typing import Iterable


SOURCES = (
    (
        "Marine Craft Model, Kinetics, surface-craft restoring matrix (online edition)",
        "https://www.fossen.biz/html/marineCraftModel.html",
    ),
    (
        "Modal analysis: Theory, Outline theory (online edition)",
        "https://www.orcina.com/webhelp/OrcaFlex/Content/html/Modalanalysis,Theory.htm",
    ),
    (
        "Results: Roll damping, Percentage of critical (online edition)",
        "https://www.orcina.com/webhelp/OrcaWave/Content/html/Results,Rolldamping.htm",
    ),
)


def _positive(name: str, value: float) -> None:
    if (
        not isinstance(value, (int, float))
        or isinstance(value, bool)
        or not math.isfinite(value)
        or value <= 0
    ):
        raise ValueError(f"{name} must be finite and positive")


def _source(value: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(
            "source must identify the input basis and applicable condition"
        )


@dataclass(frozen=True)
class VesselRollBasis:
    """One loading condition, SI units; natural period includes added inertia.

    Mass and GM shall represent the same baseline condition as the period and
    damping. Baseline includes frozen tank contents if the incremental tank
    impedance is relative to frozen contents; do not count those terms twice.
    Source shall identify loading condition, references and measurement date.
    """

    displacement_mass_kg: float
    gm_m: float
    natural_period_s: float
    hull_damping_ratio: float
    gravity_m_s2: float
    source: str

    def __post_init__(self) -> None:
        for name in (
            "displacement_mass_kg",
            "gm_m",
            "natural_period_s",
            "gravity_m_s2",
        ):
            _positive(name, getattr(self, name))
        if not math.isfinite(self.hull_damping_ratio) or self.hull_damping_ratio < 0:
            raise ValueError("hull_damping_ratio must be finite and non-negative")
        _source(self.source)

    @property
    def restoring_stiffness(self) -> float:
        """C44 [N m/rad], small-angle surface-craft restoring matrix, SOURCES[0]."""
        return self.displacement_mass_kg * self.gravity_m_s2 * self.gm_m

    @property
    def effective_inertia(self) -> float:
        """I44 + A44 [kg m²], inversion of T=2*pi*sqrt(I/C), SOURCES[1]."""
        omega_n = 2 * math.pi / self.natural_period_s
        return self.restoring_stiffness / omega_n**2

    @property
    def linear_damping(self) -> float:
        """B44 [N m s/rad], B=2*zeta*sqrt(I*C), SOURCES[2]."""
        return (
            2
            * self.hull_damping_ratio
            * math.sqrt(self.effective_inertia * self.restoring_stiffness)
        )


@dataclass(frozen=True)
class TankImpedance:
    """Tank-on-vessel moment/roll [N m/rad], exp(+i*omega*t) convention.

    A dissipative tank has NEGATIVE imaginary moment/roll. Values shall be
    linearized at the stated roll amplitude. Source shall identify geometry,
    tank mass, reservoir separation/area/fill, conduit type/effective length,
    damping calibration, tuning condition and baseline subtraction. No tuning
    or amplitude iteration is inferred by this interface.
    """

    conduit_area_m2: float
    forcing_period_s: float
    moment_per_roll: complex
    roll_amplitude_rad: float
    source: str

    def __post_init__(self) -> None:
        for name in ("conduit_area_m2", "forcing_period_s", "roll_amplitude_rad"):
            _positive(name, getattr(self, name))
        if not (
            isinstance(self.moment_per_roll, (int, float, complex))
            and not isinstance(self.moment_per_roll, bool)
            and math.isfinite(self.moment_per_roll.real)
            and math.isfinite(self.moment_per_roll.imag)
        ):
            raise ValueError("moment_per_roll must have finite components")
        _source(self.source)


@dataclass(frozen=True)
class ReductionRow:
    """Responses [rad/(N m)] to identical harmonic excitation moments."""

    tank: TankImpedance
    bare_response: complex
    coupled_response: complex
    reduction_percent: float


@dataclass(frozen=True)
class ReductionSweep:
    """Calculation status is distinct from engineering acceptance."""

    status: str
    vessel: VesselRollBasis | None
    rows: tuple[ReductionRow, ...]
    missing: tuple[str, ...]


def reduction_sweep(
    vessel: VesselRollBasis | None, points: Iterable[TankImpedance]
) -> ReductionSweep:
    """Evaluate an area sweep at one frequency and linearization amplitude.

    Linear harmonic reduction of SOURCES[0]'s equation of motion gives
    Z0=C-I*omega²+i*omega*B. With tank moment H*phi on the right,
    Z=Z0-H. Reduction=100*(1-|Z0/Z|), a percentage definition.
    Negative reduction means amplification. This is not a band-peak metric.
    """
    tanks = tuple(points)
    missing = []
    if vessel is None:
        missing.append(
            "vessel basis: mass, GM, natural period, damping, gravity and source"
        )
    if not tanks:
        missing.append(
            "tank impedance: area, frequency, amplitude and source per point"
        )
    if missing:
        return ReductionSweep("NOT_EVALUATED", vessel, (), tuple(missing))
    assert vessel is not None
    if len({t.conduit_area_m2 for t in tanks}) != len(tanks):
        raise ValueError("conduit areas must be unique")
    if len({(t.forcing_period_s, t.roll_amplitude_rad) for t in tanks}) != 1:
        raise ValueError(
            "area sweep requires the same forcing period and roll amplitude"
        )
    try:
        stiffness = vessel.restoring_stiffness
        inertia = vessel.effective_inertia
        damping = vessel.linear_damping
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("derived vessel basis must be finite") from exc
    _positive("derived restoring stiffness", stiffness)
    _positive("derived effective inertia", inertia)
    if not math.isfinite(damping):
        raise ValueError("derived damping must be finite")
    rows = []
    for tank in sorted(tanks, key=lambda t: t.conduit_area_m2):
        omega = 2 * math.pi / tank.forcing_period_s  # Period definition, SOURCES[1].
        try:
            bare = complex(stiffness - inertia * omega**2, omega * damping)
        except OverflowError as exc:
            raise ValueError("derived impedance must be finite") from exc
        coupled = bare - tank.moment_per_roll
        if not all(math.isfinite(v) for z in (bare, coupled) for v in (z.real, z.imag)):
            raise ValueError("derived impedance must be finite")
        if bare == 0 or coupled == 0:
            raise ValueError(
                "singular roll impedance; no finite response is established"
            )
        bare_response, coupled_response = 1 / bare, 1 / coupled
        reduction = 100 * (1 - abs(bare / coupled))
        values = (
            bare_response.real,
            bare_response.imag,
            coupled_response.real,
            coupled_response.imag,
            reduction,
        )
        if not all(math.isfinite(v) for v in values):
            raise ValueError("derived response must be finite")
        rows.append(ReductionRow(tank, bare_response, coupled_response, reduction))
    return ReductionSweep("EVALUATED", vessel, tuple(rows), ())


def _curve(rows: tuple[ReductionRow, ...]) -> str:
    """Return a static vector plot; coordinates are display conventions only."""
    if not rows:
        return ""
    areas = [r.tank.conduit_area_m2 for r in rows]
    reductions = [r.reduction_percent for r in rows]
    xmin, xmax = min(areas), max(areas)
    ymin, ymax = min(0, min(reductions)), max(0, max(reductions))
    coordinates = " ".join(
        f"{40 + 400 * (x - xmin) / (xmax - xmin or 1):.1f},"
        f"{180 - 140 * (y - ymin) / (ymax - ymin or 1):.1f}"
        for x, y in zip(areas, reductions)
    )
    markers = "".join(
        f'<circle cx="{xy.split(",")[0]}" cy="{xy.split(",")[1]}" r="3" fill="blue"/>'
        for xy in coordinates.split()
    )
    return (
        '<svg viewBox="0 0 500 230" role="img" '
        'aria-label="Roll reduction versus conduit area; values in table">'
        '<path d="M40 40 V180 H440" fill="none" stroke="black"/>'
        f'<polyline points="{coordinates}" fill="none" stroke="blue"/>{markers}'
        '<text x="40" y="25">Reduction (%)</text>'
        '<text x="40" y="215">Conduit area (m²); point values in table</text></svg>'
    )


def render_roll_reduction_html(
    result: ReductionSweep,
    *,
    revision: str,
    date: str,
    change: str,
    previous_revisions: tuple[tuple[str, str, str], ...] = (),
) -> str:
    """Render an analysis section with the current change reason and prior rows."""
    history = previous_revisions + ((revision, date, change),)
    revision_rows = "".join(
        "<tr>" + "".join(f"<td>{escape(cell)}</td>" for cell in row) + "</tr>"
        for row in history
    )
    inputs = ""
    if result.vessel is not None:
        inputs = "".join(
            f"<dt>{escape(k)}</dt><dd>{escape(str(v))}</dd>"
            for k, v in asdict(result.vessel).items()
        )
    rows = "".join(
        f"<tr><td>{r.tank.conduit_area_m2}</td>"
        f"<td>{r.reduction_percent:.1f}</td>"
        f"<td>{escape(str(r.tank.moment_per_roll))}</td>"
        f"<td>{r.tank.forcing_period_s}</td>"
        f"<td>{escape(str(r.tank.roll_amplitude_rad))}</td>"
        f"<td>{escape(r.tank.source)}</td></tr>"
        for r in result.rows
    )
    sources = "".join(
        f'<li><a href="{escape(url, quote=True)}">{escape(title)}</a></li>'
        for title, url in SOURCES
    )
    missing = "".join(f"<li>{escape(item)}</li>" for item in result.missing)
    return (
        '<!doctype html><html lang="en"><meta charset="utf-8">'
        "<title>Anti-roll roll-reduction analysis</title><body>"
        "<h1>Anti-roll roll-reduction analysis</h1><h2>Summary</h2>"
        f"<p>{escape(result.status)}: single-frequency, prescribed-amplitude calculation. "
        "Engineering acceptance and a real-vessel result are not established.</p>"
        "<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th>"
        f"<th>Change and reason</th></tr>{revision_rows}</table>"
        "<h2>Method and sources</h2><p>C=mgGM; I=C/(2π/T)²; B=2ζ√(IC). "
        "Z0=C−Iω²+iωB; Z=Z0−H; reduction=100(1−|Z0/Z|). "
        "The exp(+iωt) convention is used; H is tank-on-vessel moment/roll. "
        "Negative reduction denotes amplification.</p>"
        f"<ul>{sources}</ul><h2>Inputs (SI)</h2><dl>{inputs}</dl>"
        f"<h2>Missing evidence</h2><ul>{missing}</ul><h2>Area sweep</h2>{_curve(result.rows)}"
        "<table><tr><th>Area (m²)</th><th>Reduction (%)</th><th>H (N m/rad)</th>"
        "<th>Period (s)</th><th>Roll amplitude (rad)</th><th>Input source</th></tr>"
        f"{rows}</table><h2>Limits and way forward</h2><p>"
        "Supply the confirmed loading condition and calibrated tank impedance for "
        "each area, with geometry and amplitude basis. No automatic tuning, "
        "quadratic-loss iteration, sea-state response or frequency-band peak "
        "reduction is calculated. Baseline inertia, GM and incremental tank moment "
        "must use consistent frozen-content and free-surface conventions.</p></body></html>"
    )
