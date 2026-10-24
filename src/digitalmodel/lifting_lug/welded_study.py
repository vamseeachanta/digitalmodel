"""Offline elastic demand study; no capacity, solver, or source qualification.

Only an idealized rectangular plate with two equal, co-projected straight base
welds is supported. Geometry is caller-supplied, never inferred from a drawing.
Equations are independently derived in docs/domains/lifting/welded-padeye.html.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
from decimal import Decimal
from hashlib import sha256
from itertools import product
from pathlib import Path
import json
import math

METHOD = "rectangular-double-line-elastic-v1"
# Study ranges, not code limits: repository issue-2117 plan, Table 1 (2026-09-13).
AXES = (
    (0.75, 1, 1.25, 1.5, 1.75),
    (0.8, 0.9, 1, 1.1, 1.2),
    (1, 1.25, 1.5),
    (0, 30, 45),
)
UNITS = dict(
    zip(
        (
            "fx_kip",
            "fy_kip",
            "mz_kip_in",
            "resultant_kip",
            "qmax_kip_per_in",
            "throat_ksi",
            "sigma_left_ksi",
            "sigma_right_ksi",
            "tau_average_ksi",
        ),
        ("kip", "kip", "kip-in", "kip", "kip/in", "ksi", "ksi", "ksi", "ksi"),
    )
)
SOURCES = [
    {
        "document": "University Physics Volume 1",
        "edition": "2016",
        "section": "12.1 Conditions for Static Equilibrium",
        "url": "https://openstax.org/books/university-physics-volume-1/pages/12-1-conditions-for-static-equilibrium",
        "use": "force and moment equilibrium; derived line-group equations",
    },
    {
        "document": "Mechanics of Materials",
        "section": "4.2 Stresses in Beams",
        "edition": "public online edition accessed 2026-10-20",
        "url": "https://eng.libretexts.org/Bookshelves/Mechanical_Engineering/Mechanics_of_Materials_(Roylance)/04:_Bending/4.02:_Stresses_in_Beams",
        "use": "elastic section stresses; rectangular inertia derived by integration",
    },
    {
        "document": "Design and Construction of Lifting Beams",
        "edition": "1991",
        "section": "example pp. 156-158, Fig. 16A",
        "url": "https://ej.aisc.org/index.php/engj/article/download/578/577/577",
        "use": "historical target only; drawing reproduction NOT_EVALUATED",
    },
    {
        "document": "Design and Construction of Lifting Beams erratum",
        "edition": "2004",
        "section": "erratum",
        "url": "https://ej.aisc.org/index.php/engj/article/download/834/833/833",
        "use": "historical qualification prerequisite; no capacity values used",
    },
]
LIMITATIONS = {
    "historical_reproduction": "NOT_EVALUATED: qualified source profile and weld geometry absent",
    "modern_code_capacity": "NOT_EVALUATED: criteria and material inputs absent",
    "fit_and_clearance": "NOT_EVALUATED: supporting member and shackle geometry absent",
    "local_stress_and_buckling": "NOT_EVALUATED: nominal section method only",
    "finite_element_verification": "NOT_EVALUATED: no solver execution",
}


def _positive(*values):
    if any(
        isinstance(v, bool)
        or not isinstance(v, (float, int))
        or not math.isfinite(v)
        or v <= 0
        for v in values
    ):
        raise ValueError("inputs must be finite positive numbers")


def _source(value):
    if not isinstance(value, str) or not value.strip():
        raise ValueError("source description is required")


@dataclass(frozen=True)
class Load:
    gross_kip: float
    factor: float
    lug_count: int
    source: str

    def __post_init__(self):
        _positive(self.gross_kip, self.factor)
        if type(self.lug_count) is not int or self.lug_count <= 0:
            raise ValueError("lug_count must be a positive integer")
        _source(self.source)
        # Canonical continuous inputs make revisions independent of numeric syntax.
        for name in ("gross_kip", "factor"):
            object.__setattr__(self, name, float(getattr(self, name)))

    @property
    def fy(self):
        # OpenStax (2016), §12.1: equal vertical reactions sum to factored weight.
        # gross_kip is UNFACTORED; the explicit factor is applied exactly once.
        result = self.gross_kip * self.factor / self.lug_count
        if not math.isfinite(result) or result <= 0:
            raise ValueError("factored load is not representable")
        return result


@dataclass(frozen=True)
class Geometry:
    width: float
    height: float
    hole_x: float
    hole_y: float
    hole_radius: float
    weld_leg: float
    source: str

    def __post_init__(self):
        _source(self.source)
        values = asdict(self)
        values.pop("source")
        if any(
            isinstance(v, bool)
            or not isinstance(v, (int, float))
            or not math.isfinite(v)
            for v in values.values()
        ):
            raise ValueError("geometry must be finite numeric values")
        for name, value in values.items():
            object.__setattr__(self, name, float(value))

    def scaled(self, scale):
        # Scaling about the hole, then translating the base centroid to (0,0),
        # scales B,H,rx,ry while leaving the actual hole radius unchanged.
        return (
            self.width * scale,
            self.height * scale,
            self.hole_x * scale,
            self.hole_y * scale,
        )

    def invalid_reason(self, scale):
        b, h, x, y = self.scaled(scale)
        if min(b, h, self.hole_radius, self.weld_leg) <= 0:
            return "nonpositive geometry"
        if (
            abs(x) + self.hole_radius >= b / 2
            or not self.hole_radius < y < h - self.hole_radius
        ):
            return "hole lacks positive edge ligament in rectangular profile"
        return None


def demands(geometry, load, thickness, scale, weld_multiplier, angle, direction):
    """Nine signed nominal demands; inch/kip inputs give ksi section stresses."""
    _positive(thickness, scale, weld_multiplier)
    if isinstance(angle, bool) or not math.isfinite(angle) or not 0 <= angle < 90:
        raise ValueError("angle must be finite in [0,90) degrees from upward vertical")
    if type(direction) is not int or direction not in (-1, 1):
        raise ValueError("direction must be -1 or +1")
    reason = geometry.invalid_reason(scale)
    if reason:
        raise ValueError(reason)
    b, _, rx, ry = geometry.scaled(scale)
    fy = load.fy
    fx = direction * fy * math.tan(math.radians(angle))
    moment = rx * fy - ry * fx  # OpenStax (2016), §12.1: r cross F.
    # Two equal straight lines: L=2B; J=2*integral(-B/2,B/2,x² dx)=B³/6.
    # Derived from equilibrium, not an empirical weld coefficient (method note).
    length = 2 * b
    polar = b**3 / 6
    qmax = max(
        math.hypot(fx / length, fy / length + moment * x / polar)
        for x in (-b / 2, b / 2)
    )
    # Equal-leg right triangle: throat=a/sqrt(2), by Pythagoras (method note).
    throat = geometry.weld_leg * weld_multiplier / math.sqrt(2)
    axial = fy / (b * thickness)
    # Mechanics of Materials §4.2: Mc/I; I=t*B³/12, c=B/2 => 6M/(tB²).
    bending = 6 * moment / (thickness * b**2)
    result = dict(
        zip(
            UNITS,
            (
                fx,
                fy,
                moment,
                math.hypot(fx, fy),
                qmax,
                qmax / throat,
                axial - bending,
                axial + bending,
                fx / (b * thickness),
            ),
        )
    )
    if not all(math.isfinite(v) for v in result.values()):
        raise ValueError("demands are not representable")
    return result


def _key(parameters):
    if len(parameters) != len(AXES):
        raise ValueError(
            "four parameters required: thickness, scale, weld multiplier, angle"
        )
    result = []
    for value in parameters:
        if isinstance(value, bool):
            raise ValueError("boolean parameter")
        decimal = Decimal(str(value))
        if not decimal.is_finite():
            raise ValueError("nonfinite parameter")
        if decimal.is_zero():
            decimal = Decimal(0)
        result.append(format(decimal.normalize(), "f"))
    return result


def _row(geometry, load, parameters):
    t, scale, weld, angle = parameters
    reason = geometry.invalid_reason(scale)
    directions = {}
    for name, sign in (("positive", 1), ("negative", -1)):
        status = (
            "NOT_APPLICABLE"
            if angle == 0 and sign == -1
            else "INVALID_GEOMETRY"
            if reason
            else "CALCULATED_SCREENING"
        )
        values = (
            demands(geometry, load, t, scale, weld, angle, sign)
            if status == "CALCULATED_SCREENING"
            else None
        )
        directions[name] = {
            "status": status,
            "values": values,
            "reason": "duplicate zero-angle direction"
            if status == "NOT_APPLICABLE"
            else reason,
        }
    envelopes = None
    if not reason:
        envelopes = {}
        applicable = [
            (k, d["values"]) for k, d in directions.items() if d["values"] is not None
        ]
        for response, unit in UNITS.items():
            # Stable insertion order makes exact ties select positive direction.
            name, values = max(applicable, key=lambda pair: abs(pair[1][response]))
            envelopes[response] = {
                "value": values[response],
                "direction": name,
                "unit": unit,
                "status": "CALCULATED_SCREENING",
                "verification": "NOT_EVALUATED",
            }
    return {
        "key": _key(parameters),
        "status": "INVALID_GEOMETRY" if reason else "CALCULATED_SCREENING",
        "reason": reason,
        "benchmark_status": "UNVERIFIED_SOURCE",
        "directions": directions,
        "envelopes": envelopes,
    }


def _digest(payload):
    return sha256(
        json.dumps(
            payload,
            sort_keys=True,
            separators=(",", ":"),
            ensure_ascii=False,
            allow_nan=False,
        ).encode()
    ).hexdigest()


def build_study(geometry, load):
    """Complete fixed grid, including invalid rows; revision binds all content."""
    import copy

    load.fy  # Fail before generation, even if geometry would withhold every row.
    data = {
        "method": METHOD,
        "code_sha256": sha256(
            Path(__file__).read_bytes().replace(b"\r\n", b"\n")
        ).hexdigest(),
        "criteria_revision": "2026-10-20",
        "geometry": asdict(geometry),
        "load": asdict(load),
        "load_already_factored": False,
        "units": UNITS.copy(),
        "axes": [list(a) for a in AXES],
        "citations": copy.deepcopy(SOURCES),
        "limitations": LIMITATIONS.copy(),
        "intended_use": "screening",
        "rows": [_row(geometry, load, p) for p in product(*AXES)],
    }
    # Content addressing detects accidental mutation; it is not an approval signature.
    return {**data, "revision": _digest(data)}


def validate(data):
    payload = {k: v for k, v in data.items() if k != "revision"}
    if _digest(payload) != data["revision"]:
        raise ValueError("dataset digest mismatch")
    if data["method"] != METHOD:
        raise ValueError("unsupported method")
    return data


def lookup(data, revision, parameters, intended_use):
    """Exact-node diagnostic lookup; never interpolation or design approval."""
    import copy

    validate(data)
    if revision != data["revision"]:
        raise ValueError("dataset revision mismatch")
    if intended_use != "screening":
        raise ValueError("intended use must be screening")
    key = _key(parameters)
    for row in data["rows"]:
        if row["key"] == key:
            return copy.deepcopy(row)
    raise KeyError(f"off-grid parameters: {key}")


def write_outputs(data, directory):
    """Write the versioned table and human-readable report; see output module."""
    from digitalmodel.lifting_lug.welded_study_output import write_outputs as write

    write(data, directory)
