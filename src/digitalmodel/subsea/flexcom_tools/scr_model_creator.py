"""Prepare a uniform SCR mesh and render explicit legacy file templates.

Basis: Catenary Curve (2019), equations (1), (3), (4), (9), (10):
https://www.mygeodesy.id.au/documents/Catenary%20Curve.pdf

The cable is inextensible, perfectly flexible, uniformly weighted, and tangent
to a horizontal seabed at the origin. Coordinates are metres, Z upward, with
the mean hang-off in the positive X/Z plane. No seabed tail is generated.
Rendering does not establish native syntax, analysis completeness, or agreement
with the unavailable workbook. Always consult qualification_status().

Submerged weight is pipe plus contents weight less displaced-water weight. Forces
are effective tensions under the closed-end pressure-force convention, not wall
tensions. Basis: OrcaFlex, Line theory: Line pressure effects (accessed 2026-10-19):
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Linetheory,Linepressureeffects.htm
No internal/external pressures or pressure areas are supplied; wall tension is
NOT_EVALUATED.
"""

from collections.abc import Mapping
from dataclasses import dataclass
import math

from jinja2 import StrictUndefined
from jinja2.sandbox import ImmutableSandboxedEnvironment


# Allocation bound for the preparation mesh, not an engineering convergence limit.
MAX_ELEMENT_COUNT = 10_000


@dataclass(frozen=True)
class Offset:
    """Horizontal hang-off translation relative to the analytical mean position."""

    name: str
    dx_m: float
    dy_m: float


@dataclass(frozen=True)
class SCRModelConfig:
    """Explicit SI inputs and caller-supplied templates, with no design defaults.

    horizontal_tension_n is the horizontal effective tension. submerged_weight_n_m
    is the uniform weight of pipe plus contents less displaced-water weight.
    element_count is limited to 1..MAX_ELEMENT_COUNT to bound list allocation;
    this limit does not establish mesh convergence.
    template_sets maps each supported analysis type to mdl/inf/off template text.
    parameters carries additional explicit template inputs, such as stiffness or
    hydrodynamic coefficients; no engineering value is inferred from the mode.
    template_source identifies the supplied template revision and provenance.
    """

    analysis_type: str
    height_m: float
    horizontal_tension_n: float
    submerged_weight_n_m: float
    element_count: int
    offsets: tuple[Offset, ...]
    template_sets: Mapping[str, Mapping[str, str]]
    template_source: str
    parameters: Mapping[str, float | str]


def qualification_status() -> dict[str, str]:
    """Expose missing evidence independently of analytical or rendering success."""
    return dict.fromkeys(
        (
            "workbook_extraction",
            "archive_equivalence",
            "native_model_validity",
            "storm_response",
            "fatigue_damage",
            "viv_response",
            "wall_tension",
        ),
        "NOT_EVALUATED",
    )


def _finite_number(value: object, name: str, *, positive: bool = False) -> None:
    try:
        valid = (
            not isinstance(value, bool)
            and isinstance(value, (int, float))
            and math.isfinite(value)
            and (not positive or value > 0)
        )
    except OverflowError:
        valid = False
    if not valid:
        raise ValueError(
            f"{name} must be a finite {'positive ' if positive else ''}number"
        )


def _validate(config: SCRModelConfig) -> None:
    if config.analysis_type not in ("storm", "fatigue", "viv"):
        raise ValueError("analysis_type must be storm, fatigue or viv")
    for name in ("height_m", "horizontal_tension_n", "submerged_weight_n_m"):
        _finite_number(getattr(config, name), name, positive=True)
    if (
        type(config.element_count) is not int
        or not 1 <= config.element_count <= MAX_ELEMENT_COUNT
    ):
        raise ValueError(f"element_count must be an integer in 1..{MAX_ELEMENT_COUNT}")
    if not config.offsets:
        raise ValueError("offsets must contain at least one explicit boundary position")
    names = set()
    for offset in config.offsets:
        if (
            not isinstance(offset, Offset)
            or not isinstance(offset.name, str)
            or not offset.name.strip()
            or any(character in offset.name for character in "\r\n\x00")
            or offset.name in names
        ):
            raise ValueError("offset names must be nonempty, single-line and unique")
        names.add(offset.name)
        _finite_number(offset.dx_m, "offset dx_m")
        _finite_number(offset.dy_m, "offset dy_m")
    if not isinstance(config.parameters, Mapping):
        raise ValueError("parameters must be a mapping of named scalar inputs")
    for key, value in config.parameters.items():
        if not isinstance(key, str) or not key.strip():
            raise ValueError("parameters keys must be nonempty strings")
        if isinstance(value, str):
            if any(character in value for character in "\r\n\x00"):
                raise ValueError("parameters text must be single-line without NUL")
        else:
            _finite_number(value, f"parameters {key}")


def _geometry(config: SCRModelConfig) -> dict:
    # With uniform submerged weight, H, V and T below are effective tensions;
    # the module basis documents the pressure-force convention.
    # Catenary Curve (2019), eq. (4): a=H/w; eqs. (9), (10):
    # y=a*cosh(x/a), s=a*sinh(x/a), with y=a+z, hence s²=h(h+2a).
    a = config.horizontal_tension_n / config.submerged_weight_n_m
    h = config.height_m
    length = math.sqrt(h * (h + 2 * a))
    span = a * math.asinh(length / a) if a > 0 else math.nan
    vertical = config.submerged_weight_n_m * length
    # Eq. (3): T²=H²+(w*s)². hypot avoids unnecessary intermediate overflow.
    tension = math.hypot(config.horizontal_tension_n, vertical)
    if not all(
        math.isfinite(v) and v > 0 for v in (a, length, span, vertical, tension)
    ):
        raise ValueError(
            "derived catenary geometry is outside the finite numeric range"
        )
    return {
        "catenary_parameter_m": a,
        "suspended_length_m": length,
        "horizontal_span_m": span,
        "top_vertical_tension_n": vertical,
        "top_tension_n": tension,
    }


def build_model_context(config: SCRModelConfig) -> dict:
    """Calculate equal-arc mean geometry and translated hang-off boundaries.

    top_tension_n is mean top effective tension; top_vertical_tension_n and
    horizontal_tension_n are its effective-force components. Wall tension is
    NOT_EVALUATED because pressures and pressure areas are not supplied.
    Element lengths are arc lengths, not chord lengths. Offsets do not solve
    equilibrium again and do not change the mean mesh or its tension result.
    The calling application is responsible for mesh convergence and native
    boundary, material, environment and load-case specification.
    """
    _validate(config)
    geometry = _geometry(config)
    a = geometry["catenary_parameter_m"]
    length = geometry["suspended_length_m"]
    nodes, elements = _mesh(config.element_count, a, length)
    offsets = _offset_coordinates(config, geometry["horizontal_span_m"])
    return {
        **geometry,
        "analysis_type": config.analysis_type,
        "height_m": config.height_m,
        "horizontal_tension_n": config.horizontal_tension_n,
        "submerged_weight_n_m": config.submerged_weight_n_m,
        "nodes": nodes,
        "elements": elements,
        "offsets": offsets,
        "parameters": dict(config.parameters),
        "template_source": config.template_source,
        "qualification": qualification_status(),
    }


def _mesh(element_count: int, a: float, length: float) -> tuple[list[dict], list[dict]]:
    nodes = []
    for index in range(element_count + 1):
        s = length * (index / element_count)
        # Eqs. (9), (10): x=a*asinh(s/a); z=sqrt(a²+s²)-a.
        # Rationalization gives z=s*(s/(hypot(a,s)+a)), avoiding cancellation.
        z = s * (s / (math.hypot(a, s) + a))
        nodes.append(
            {
                "id": index + 1,
                "s_m": s,
                "x_m": a * math.asinh(s / a),
                "y_m": 0.0,
                "z_m": z,
            }
        )
    elements = [
        {
            "id": index + 1,
            "start": index + 1,
            "end": index + 2,
            "arc_length_m": length / element_count,
        }
        for index in range(element_count)
    ]
    return nodes, elements


def _offset_coordinates(config: SCRModelConfig, span: float) -> list[dict]:
    positions = []
    for offset in config.offsets:
        # Cartesian translation and Euclidean distance; no empirical coefficient.
        x = span + offset.dx_m
        magnitude = math.hypot(offset.dx_m, offset.dy_m)
        if not math.isfinite(x) or not math.isfinite(magnitude):
            raise ValueError(
                "derived offset coordinates are outside the finite numeric range"
            )
        if x <= 0:
            raise ValueError("offset hang-off must remain ahead of touchdown (x > 0)")
        positions.append(
            {
                "name": offset.name,
                "dx_m": offset.dx_m,
                "dy_m": offset.dy_m,
                "x_m": x,
                "y_m": offset.dy_m,
                "z_m": config.height_m,
                "magnitude_m": magnitude,
            }
        )
    return positions


def generate_flexcom_model(config: SCRModelConfig) -> dict[str, str]:
    """Render mdl/inf/off strings from the selected explicit template set.

    Templates are trusted application inputs, not a native syntax definition.
    There is no template fallback between modes, no filesystem loader, no output
    write and no solver call. Missing variables and empty outputs are errors.
    Successful rendering leaves all qualification statuses NOT_EVALUATED.
    """
    context = build_model_context(config)
    if (
        not isinstance(config.template_source, str)
        or not config.template_source.strip()
    ):
        raise ValueError("template_source must identify the supplied template basis")
    templates = (
        config.template_sets.get(config.analysis_type)
        if isinstance(config.template_sets, Mapping)
        else None
    )
    if not isinstance(templates, Mapping) or set(templates) != {"mdl", "inf", "off"}:
        raise ValueError("selected template set must contain exactly mdl, inf, off")
    environment = ImmutableSandboxedEnvironment(
        undefined=StrictUndefined,
        autoescape=False,
        keep_trailing_newline=True,
    )
    rendered = {}
    for extension in ("mdl", "inf", "off"):
        text = templates[extension]
        if not isinstance(text, str):
            raise ValueError(f"{extension} template must be text")
        content = environment.from_string(text).render(context)
        if not content.strip():
            raise ValueError(f"{extension} template produced empty output")
        rendered[extension] = content
    return rendered
