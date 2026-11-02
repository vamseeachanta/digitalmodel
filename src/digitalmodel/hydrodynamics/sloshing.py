# ABOUTME: Analytical tank-sloshing natural periods and class-simplified convective
# ABOUTME: response, with a resonance check against vessel-motion / mooring periods.

"""
Tank sloshing — analytical natural periods and simplified convective response.

This is the fast, class-reviewable first tier of a sloshing assessment: it tells
you the tank's natural sloshing periods and whether any of them coincide with the
vessel's motion or mooring periods (the coupling that external diffraction —
AQWA / OrcaWave — does not capture).  When resonance is flagged, or for high fill
and violent/impact sloshing, the follow-on tier is a VOF free-surface CFD run;
this module deliberately covers only the analytical / simplified tier.

Methods
-------
* Rectangular (prismatic) tank, linear potential-flow theory:

      omega_n^2 = g * (n*pi/L) * tanh(n*pi*h/L),   n = 1, 2, ...

  the odd modes (n = 1, 3, ...) are the antisymmetric sloshing modes excited by
  lateral (sway) motion.

* Upright circular-cylindrical tank, linear potential-flow theory:

      omega_mn^2 = g * (eps_mn/R) * tanh(eps_mn*h/R)

  where ``eps_mn`` is the n-th root of J_m'(x) = 0.  The m = 1 family is the
  lateral (antisymmetric) sloshing family; ``eps_11 = 1.8412`` gives the
  fundamental sloshing mode.

* API 650 Annex E convective (sloshing) period for a flat-bottom cylindrical
  tank — the accepted simplified storage-tank form:

      Tc = 1.8 * Ks * sqrt(D),   Ks = 0.578 / sqrt(tanh(3.68 * H / D))

  (D, H in metres; Tc in seconds).

References
----------
* Faltinsen, O.M. & Timokha, A.N. — Sloshing (Cambridge, 2009), linear modal
  natural frequencies for rectangular and upright cylindrical tanks.
* API STD 650, Annex E (Seismic Design of Storage Tanks) — convective period Tc.
* DNV / ABS / BV / ClassNK simplified sloshing assessment procedures (screening
  tier); GTT methodology for membrane LNG containment.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import List

# Standard gravity (m/s^2)
G_STD = 9.80665

# Roots of J_1'(x) = 0 — the lateral (antisymmetric) sloshing family for an
# upright circular-cylindrical tank.  eps_11 = 1.8412 is the fundamental mode.
_BESSEL_J1_PRIME_ROOTS: List[float] = [1.84118378, 5.33144277, 8.53631637, 11.70600490]


# ---------------------------------------------------------------------------
# Result container
# ---------------------------------------------------------------------------


@dataclass
class SloshingResult:
    """
    Result of an analytical sloshing screen for a single tank.

    Attributes
    ----------
    geometry : str
        "rectangular" or "cylindrical".
    natural_periods : list of float
        Sloshing natural periods (s), fundamental first.
    fundamental_period : float
        First (longest) sloshing natural period (s).
    convective_period : float or None
        API 650 Annex E convective period (s); None when not applicable.
    resonance : bool
        True when a natural period falls within tolerance of an excitation period.
    resonant_periods : list of float
        The natural periods (s) that triggered the resonance flag.
    """

    geometry: str
    natural_periods: List[float]
    fundamental_period: float
    convective_period: float | None = None
    resonance: bool = False
    resonant_periods: List[float] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Natural periods
# ---------------------------------------------------------------------------


def rectangular_tank_periods(
    length: float, fill_depth: float, n_modes: int = 3, g: float = G_STD
) -> List[float]:
    """
    Sloshing natural periods (s) of a rectangular tank, antisymmetric modes.

    Returns the periods of the first ``n_modes`` antisymmetric modes
    (n = 1, 3, 5, ...) excited by lateral motion, longest first.
    """
    if length <= 0 or fill_depth <= 0:
        raise ValueError("length and fill_depth must be positive")
    periods: List[float] = []
    n = 1
    while len(periods) < n_modes:
        k = n * math.pi / length
        omega_sq = g * k * math.tanh(k * fill_depth)
        periods.append(2.0 * math.pi / math.sqrt(omega_sq))
        n += 2  # antisymmetric modes only
    return periods


def cylindrical_tank_periods(
    diameter: float, fill_depth: float, n_modes: int = 3, g: float = G_STD
) -> List[float]:
    """
    Sloshing natural periods (s) of an upright cylindrical tank, lateral family.

    Uses the m = 1 (antisymmetric) family; returns the first ``n_modes`` periods
    longest first.
    """
    if diameter <= 0 or fill_depth <= 0:
        raise ValueError("diameter and fill_depth must be positive")
    if n_modes > len(_BESSEL_J1_PRIME_ROOTS):
        raise ValueError(f"n_modes must be <= {len(_BESSEL_J1_PRIME_ROOTS)}")
    radius = diameter / 2.0
    periods: List[float] = []
    for eps in _BESSEL_J1_PRIME_ROOTS[:n_modes]:
        omega_sq = g * (eps / radius) * math.tanh(eps * fill_depth / radius)
        periods.append(2.0 * math.pi / math.sqrt(omega_sq))
    return periods


def api650_convective_period(diameter: float, fill_height: float) -> float:
    """
    API 650 Annex E convective (sloshing) period Tc (s); D, H in metres.

        Tc = 1.8 * Ks * sqrt(D),   Ks = 0.578 / sqrt(tanh(3.68 * H / D))
    """
    if diameter <= 0 or fill_height <= 0:
        raise ValueError("diameter and fill_height must be positive")
    ks = 0.578 / math.sqrt(math.tanh(3.68 * fill_height / diameter))
    return 1.8 * ks * math.sqrt(diameter)


def _ellipse_equivalent_rectangle(width: float, height: float, fill_fraction: float):
    """
    Equivalent-rectangle reduction of an elliptical / obround / horizontal-cylinder
    cross-section at a partial fill.

    Returns ``(a, b_s, h_eq)`` — semi-major (half-width) ``a``, the free-surface
    width ``b_s`` and the equivalent still-liquid depth ``h_eq`` — such that the
    transverse sloshing frequency follows Lamb's rectangular relation with
    L -> b_s and h -> h_eq.  ``fill_fraction`` = liquid depth / tank height (0..1).
    """
    a = width / 2.0
    b = height / 2.0
    f = min(max(fill_fraction, 0.02), 0.98)  # clamp: b_s -> 0 at the extremes
    s = 2.0 * f - 1.0
    disc = math.sqrt(max(1.0 - s * s, 1e-9))
    b_s = 2.0 * a * disc  # free-surface width
    area_frac = 0.5 + (math.asin(s) + s * disc) / math.pi  # A_L / (pi a b)
    h_eq = b * area_frac * (math.pi / 2.0) / disc
    return a, b_s, h_eq


def oval_tank_periods(
    width: float, height: float, fill_fraction: float, n_modes: int = 3, g: float = G_STD
) -> List[float]:
    """
    Transverse sloshing natural periods (s) of an elliptical / obround / horizontal
    cylindrical tank via the equivalent-rectangle method (Lamb, with the free-surface
    width and equivalent depth from the cross-section geometry at ``fill_fraction``).

    ``width`` and ``height`` are the full cross-section dimensions; a horizontal
    circular cylinder is the special case ``width == height == diameter``.

    Validity: the equivalent-rectangle approximation is good over the mid-fill
    range (fill_fraction ~0.15-0.75, within ~10-15% of the exact potential-flow
    value for a circle at half fill).  Near a full tank the free-surface width
    ``b_s`` collapses and the approximation over-predicts the frequency — those
    high-fill / violent cases are the ones a VOF free-surface CFD run resolves.
    """
    if width <= 0 or height <= 0:
        raise ValueError("width and height must be positive")
    if not 0.0 < fill_fraction < 1.0:
        raise ValueError("fill_fraction must be in (0, 1)")
    _, b_s, h_eq = _ellipse_equivalent_rectangle(width, height, fill_fraction)
    periods: List[float] = []
    n = 1
    while len(periods) < n_modes:
        k = n * math.pi / b_s
        omega_sq = g * k * math.tanh(k * h_eq)
        periods.append(2.0 * math.pi / math.sqrt(omega_sq))
        n += 2  # antisymmetric transverse modes
    return periods


def horizontal_cylinder_periods(
    diameter: float, fill_fraction: float, n_modes: int = 3, g: float = G_STD
) -> List[float]:
    """
    Transverse sloshing natural periods (s) of a HORIZONTAL circular cylinder — the
    road-tanker / rail-tank-car / IMO Type-C shape — at ``fill_fraction`` (0..1).

    Equivalent-rectangle special case of :func:`oval_tank_periods` with
    width = height = diameter.
    """
    return oval_tank_periods(diameter, diameter, fill_fraction, n_modes, g)


def resonance_check(
    natural_periods: List[float], excitation_periods: List[float], tolerance: float = 0.15
) -> List[float]:
    """
    Return the natural periods within ``tolerance`` (fractional) of any excitation
    period — e.g. vessel roll / sway / mooring periods.  Empty list => no resonance.
    """
    hits: List[float] = []
    for tn in natural_periods:
        for te in excitation_periods:
            if te > 0 and abs(tn - te) / te <= tolerance:
                hits.append(tn)
                break
    return hits


# ---------------------------------------------------------------------------
# Registry workflow
# ---------------------------------------------------------------------------


def screen_tank(
    geometry: str,
    fill_depth: float,
    length: float | None = None,
    diameter: float | None = None,
    n_modes: int = 3,
    excitation_periods: List[float] | None = None,
    resonance_tolerance: float = 0.15,
    g: float = G_STD,
) -> SloshingResult:
    """Analytical sloshing screen for one tank; see module docstring for methods."""
    geometry = geometry.lower()
    if geometry == "rectangular":
        if length is None:
            raise ValueError("rectangular geometry requires length")
        periods = rectangular_tank_periods(length, fill_depth, n_modes, g)
        convective = None
    elif geometry == "cylindrical":
        if diameter is None:
            raise ValueError("cylindrical geometry requires diameter")
        periods = cylindrical_tank_periods(diameter, fill_depth, n_modes, g)
        convective = api650_convective_period(diameter, fill_depth)
    else:
        raise ValueError("geometry must be 'rectangular' or 'cylindrical'")

    resonant = resonance_check(periods, excitation_periods or [], resonance_tolerance)
    return SloshingResult(
        geometry=geometry,
        natural_periods=periods,
        fundamental_period=periods[0],
        convective_period=convective,
        resonance=bool(resonant),
        resonant_periods=resonant,
    )


def router(cfg: dict) -> dict:
    """
    Registry entry point.  Expects a ``sloshing`` config block, e.g.::

        {"sloshing": {"geometry": "cylindrical", "diameter": 40.0,
                       "fill_depth": 20.0, "excitation_periods": [12.0]}}

    Returns a plain-dict result for serialisation.
    """
    params = cfg.get("sloshing", cfg)
    result = screen_tank(
        geometry=params["geometry"],
        fill_depth=params["fill_depth"],
        length=params.get("length"),
        diameter=params.get("diameter"),
        n_modes=params.get("n_modes", 3),
        excitation_periods=params.get("excitation_periods"),
        resonance_tolerance=params.get("resonance_tolerance", 0.15),
        g=params.get("g", G_STD),
    )
    return {
        "geometry": result.geometry,
        "natural_periods": result.natural_periods,
        "fundamental_period": result.fundamental_period,
        "convective_period": result.convective_period,
        "resonance": result.resonance,
        "resonant_periods": result.resonant_periods,
    }

# ---------------------------------------------------------------------------
# API 650 Annex E public-source seismic screen
# ---------------------------------------------------------------------------

_ANNEX_E_CITATIONS = (
    {
        "type": "Citation", "standard": "API STD 650", "revision": "11th edition, 2007",
        "section": "E.4.5.2; E.4.6.1; E.6.1.1-E.6.1.5; E.6.2.1.1.1; Table E-6",
        "source": "https://law.resource.org/pub/us/cfr/ibr/002/api.650.2007.pdf",
    },
    {
        "type": "Citation", "standard": "API STD 650", "revision": "12th edition, Addendum 2, January 2016",
        "section": "E.7.2, equations 1-5 and Table E.7",
        "source": "https://www.api.org/~/media/files/publications/addenda-and-errata/650_e12%20addendum%202.pdf",
    },
    {
        "type": "Citation", "standard": "NCREE, Seismic Assessment of Steel Liquid Storage Tanks",
        "revision": "2014 research highlights (103E4)", "section": "pp. 85-87, Table 1",
        "source": "https://www.ncree.niar.org.tw/assets/file/103E4.pdf",
    },
)


@dataclass(frozen=True)
class AnnexEResult:
    """SI seismic demands, conditional dispositions and public-source sidecar.

    Weights/shears are N, heights m, moments N m and stresses Pa. Acceleration
    coefficients are fractions of g. There is no overall compliance verdict.
    ``freeboard_ok=None`` means no numeric minimum applies to the selected case;
    the advisory wave clearance remains reported. J is a self-anchorage screen,
    not an anchor design or a completed self-anchorage qualification.
    """

    product_weight_n: float
    impulsive_weight_n: float
    convective_weight_n: float
    xi_m: float
    xc_m: float
    xis_m: float
    xcs_m: float
    convective_period_s: float
    ai_g: float
    ac_g: float
    af_g: float
    impulsive_shear_n: float
    convective_shear_n: float
    base_shear_n: float
    ringwall_moment_nm: float
    slab_moment_nm: float
    wave_height_m: float
    required_freeboard_m: float
    advisory_freeboard_m: float
    freeboard_required: bool
    freeboard_ok: bool | None
    anchorage_ratio_j: float
    anchorage_status: str
    evaluation_depth_m: float
    impulsive_hoop_force_n_per_m: float
    convective_hoop_force_n_per_m: float
    hydrostatic_hoop_stress_pa: float
    hydrodynamic_hoop_stress_pa: float
    combined_hoop_stress_pa: float
    citations: tuple[dict[str, str], ...]
    limitations: tuple[str, ...]


def _annex_e_number(name: str, value: float, *, positive: bool = False) -> float:
    """Validate a scalar SI input; no implicit unit or string conversions."""
    if isinstance(value, (bool, str)):
        raise ValueError(f"{name} must be a finite numeric scalar")
    try:
        value = float(value)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError(f"{name} must be a finite numeric scalar") from exc
    if not math.isfinite(value) or value < 0 or (positive and value == 0):
        qualifier = "positive" if positive else "nonnegative"
        raise ValueError(f"{name} must be finite and {qualifier}")
    return value


def api650_annex_e(
    *,
    diameter_m: float,
    fill_height_m: float,
    liquid_density_kg_m3: float,
    shell_weight_n: float,
    roof_weight_n: float,
    shell_cg_m: float,
    roof_cg_m: float,
    roof_reaction_n: float,
    shell_thickness_m: float,
    sds_g: float,
    sd1_g: float,
    tl_s: float,
    importance_factor: float,
    rwi: float,
    rwc: float,
    k: float,
    minimum_ai_g: float,
    vertical_acceleration_g: float,
    annular_resistance_n_per_m: float,
    seismic_use_group: str,
    available_freeboard_m: float,
    purchaser_freeboard: bool,
    evaluation_depth_m: float | None = None,
) -> AnnexEResult:
    """Calculate an Annex E public-source screen for a ground-supported tank.

    Scope is welded, flat-bottom, circular tanks at zero internal gas pressure,
    without isolation, baffles or soil-structure interaction. This is an explicitly
    versioned screening basis, not certification to a complete current edition.
    All arguments are SI, except dimensionless factors and accelerations in g.
    Thickness is NET wall in m (not mm); density is kg/m3 (not specific gravity).
    Shell/roof CGs are heights above the tank base; roof_reaction_n is only the
    portion of roof weight supported by the shell, not automatically all roof
    weight. Annular resistance is a separately qualified line load, including
    applicable vertical effects, geometry and plate-strength limits.

    Public method references (URLs are emitted in ``citations``):

    * API STD 650, 11th ed. (2007), E.4.5.2(a,c): Tc=1.8*Ks*sqrt(D),
      Ks=.578/sqrt(tanh(3.68H/D)); existing period helper is reused.
    * E.4.6.1(1,2,4,5): Ai=max(SDS*I/Rwi, minimum_ai_g, .007);
      Ac=min(Ai, K*SD1*I/Rwc * min(1/Tc, TL/Tc**2)). SDS and SD1 are
      DESIGN spectrum parameters including site effects, not mapped Ss/S1.
      The caller SHALL supply the governing minimum_ai_g (at least .007),
      including any additional site/high-hazard minimum in the project edition.
      This routine does not select that minimum from site-class or hazard tables.
    * E.6.1.1(1-3): Wi/Wp=tanh(.866D/H)/(.866D/H) for D/H>=1.333,
      otherwise 1-.218D/H; Wc/Wp=.230D/H*tanh(3.67H/D). Wp=rho*g*pi*D²H/4
      is weight by volume; the two approximate effective weights need not sum Wp.
    * E.6.1.2.1(1-3): Xi=.375H for D/H>=1.3333, otherwise (.5-.094D/H)H;
      Xc=[1-(cosh(z)-1)/(z*sinh(z))]H, z=3.67H/D.
      E.6.1.2.2(1-3): Xis=.375[1+1.333(u/tanh(u)-1)]H for D/H>=1.333,
      otherwise (.5+.060D/H)H, u=.866D/H; Xcs=Xc+.937H/(z*sinh(z)).
      Slab equivalent heights include bottom pressure and may exceed fill height.
    * E.6.1(1-3), E.6.1.5(1,2): Vi=Ai*(Ws+Wr+Wi), Vc=Ac*Wc;
      V=hypot(Vi,Vc); moments=hypot(Ai*(Wi*Xi+Ws*Xs+Wr*Xr),Ac*Wc*Xc),
      with Xis/Xcs substituted for slab moment. No appurtenance weights are added.
    * E.6.1.4(1a-6): at Y below the surface, Ni [N/mm] is
      8.48*Ai*G*D*H*(Y/H-.5*(Y/H)**2)*tanh(u) for D/H>=1.333;
      otherwise 5.22*Ai*G*D²*(q-.5q²), q=Y/(.75D), for Y<.75D,
      and 2.6*Ai*G*D² below that. Nc=1.85*Ac*G*D²*cosh(3.68(H-Y)/D)
      /cosh(3.68H/D), G=rho/1000. N/mm is multiplied by 1000 to N/m.
      Hydrostatic stress=rho*g*Y*D/(2t); dynamic=hypot(Ni,Nc)/t;
      combined=hydrostatic+hypot(dynamic,Av*hydrostatic), using the 2007 SRSS
      vertical combination. Material acceptance is not evaluated.
    * E.6.2.1.1.1(1,2), Table E-6: J=Mrw/[D²*(wt*(1-.4Av)+wa)] at
      zero gas pressure; wt=(Ws+roof_reaction)/(pi*D). Thresholds .785/1.54
      distinguish no calculated uplift, conditional uplift and redesign/anchors.
      Shell compression, annular plate, piping and anchor checks remain required.
    * API STD 650, 12th ed., Addendum 2 (January 2016), E.7.2(1-5):
      delta_s=.42D*Af. For SUG I/II, Af=K*SD1*I*min(1/Tc,4/Tc²);
      for III, Af=K*SD1*min(1/Tc,TL/Tc²) (unit importance for wave only).
      Table E.7 sets full-wave freeboard for III and .7-wave for II when
      SDS>=.33; other cases have advisory .7-wave clearance. The purchaser
      boolean adopts that .7-wave minimum in otherwise advisory cases. No credit
      is taken for containment or a roof designed for sloshing impact.
    * NCREE 2014 research highlights, "Seismic Assessment of Steel Liquid Storage
      Tanks", pp. 85-87, Table 1 independently presents the two-mode load method.

    All project factors are required: no material or factor table is embedded.
    For a floating roof, include its inertial weight at its actual CG, but retain
    a separate assessment of roof lift, rim/seal damage and jamming. This linear
    screen does not calculate shell-roof impact pressure or floating-roof motion.
    """
    if len(_ANNEX_E_CITATIONS) != 3 or any(
        not citation.get(key)
        for citation in _ANNEX_E_CITATIONS
        for key in ("type", "standard", "revision", "section", "source")
    ):
        raise ValueError("Annex E citation basis is missing or incomplete")
    # Validate every numeric argument before evaluating engineering expressions.
    values = locals().copy()
    positive_names = {
        "diameter_m", "fill_height_m", "liquid_density_kg_m3", "shell_thickness_m",
        "tl_s", "importance_factor", "rwi", "rwc", "k",
    }
    numeric_names = positive_names | {
        "shell_weight_n", "roof_weight_n", "shell_cg_m", "roof_cg_m", "roof_reaction_n",
        "sds_g", "sd1_g", "minimum_ai_g", "vertical_acceleration_g",
        "annular_resistance_n_per_m", "available_freeboard_m",
    }
    v = {name: _annex_e_number(name, values[name], positive=name in positive_names)
         for name in numeric_names}
    if seismic_use_group not in ("I", "II", "III"):
        raise ValueError("seismic_use_group must be I, II or III")
    if not isinstance(purchaser_freeboard, bool):
        raise ValueError("purchaser_freeboard must be a boolean")
    if v["minimum_ai_g"] < .007:
        raise ValueError("minimum_ai_g must include the governing floor, at least .007")
    if v["vertical_acceleration_g"] >= 1:
        raise ValueError("vertical_acceleration_g must be less than 1 g")
    if v["roof_reaction_n"] > v["roof_weight_n"]:
        raise ValueError("roof_reaction_n cannot exceed roof_weight_n")
    d, h, rho, t = (v[name] for name in (
        "diameter_m", "fill_height_m", "liquid_density_kg_m3", "shell_thickness_m"))
    y = h if evaluation_depth_m is None else _annex_e_number("evaluation_depth_m", evaluation_depth_m)
    if y > h:
        raise ValueError("evaluation_depth_m cannot exceed fill_height_m")
    try:
        ratio = d / h
        u, z = .866 * ratio, 3.67 / ratio
        wp = rho * G_STD * math.pi * d**2 * h / 4
        wi = wp * (math.tanh(u) / u if ratio >= 1.333 else 1 - .218 * ratio)
        wc = wp * .230 * ratio * math.tanh(z)
        xi = h * (.375 if ratio >= 1.3333 else .5 - .094 * ratio)
        xc = h * (1 - math.tanh(z / 2) / z)
        xis = h * (.375 * (1 + 1.333 * (u / math.tanh(u) - 1))
                   if ratio >= 1.333 else .5 + .060 * ratio)
        # Reciprocal sinh in exponential form avoids overflow for deep tanks.
        inv_sinh = 2 * math.exp(-z) / (-math.expm1(-2*z))
        xcs = xc + .937 * h * inv_sinh / z
        tc = api650_convective_period(d, h)
        ai = max(v["sds_g"] * v["importance_factor"] / v["rwi"], v["minimum_ai_g"])
        force_shape = min(1 / tc, v["tl_s"] / tc**2)
        ac = min(ai, v["k"] * v["sd1_g"] * v["importance_factor"] / v["rwc"] * force_shape)
        wave_tl = v["tl_s"] if seismic_use_group == "III" else 4.0
        wave_i = 1.0 if seismic_use_group == "III" else v["importance_factor"]
        af = v["k"] * v["sd1_g"] * wave_i * min(1 / tc, wave_tl / tc**2)
        vi = ai * (v["shell_weight_n"] + v["roof_weight_n"] + wi)
        vc = ac * wc
        dry_moment = v["shell_weight_n"] * v["shell_cg_m"] + v["roof_weight_n"] * v["roof_cg_m"]
        mrw = math.hypot(ai * (wi * xi + dry_moment), ac * wc * xc)
        ms = math.hypot(ai * (wi * xis + dry_moment), ac * wc * xcs)
        wave = .42 * d * af
        advisory = wave * (1.0 if seismic_use_group == "III" else .7)
        freeboard_required = (seismic_use_group == "III" or purchaser_freeboard or
                              seismic_use_group == "II" and v["sds_g"] >= .33)
        required = advisory if freeboard_required else 0.0
        wt = (v["shell_weight_n"] + v["roof_reaction_n"]) / (math.pi * d)
        resistance = wt * (1 - .4 * v["vertical_acceleration_g"]) + v["annular_resistance_n_per_m"]
        if resistance <= 0:
            raise ValueError("positive self-anchorage resistance is required")
        j = mrw / (d**2 * resistance)
        sg = rho / 1000
        if ratio >= 1.333:
            q = y / h
            ni = 8480 * ai * sg * d * h * (q - .5*q*q) * math.tanh(u)
        elif y < .75*d:
            q = y / (.75*d)
            ni = 5220 * ai * sg * d**2 * (q - .5*q*q)
        else:
            ni = 2600 * ai * sg * d**2
        b = 3.68 * h / d
        depth = 3.68 * y / d
        cosh_ratio = (math.exp(-depth) + math.exp(-2*b+depth)) / (1 + math.exp(-2*b))
        nc = 1850 * ac * sg * d**2 * cosh_ratio
        hydro = rho * G_STD * y * d / (2*t)
        dynamic = math.hypot(ni, nc) / t
        combined = hydro + math.hypot(dynamic, v["vertical_acceleration_g"] * hydro)
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("Annex E inputs exceed the numerical calculation range") from exc
    results = (wp, wi, wc, xi, xc, xis, xcs, tc, ai, ac, af, vi, vc,
               math.hypot(vi, vc), mrw, ms, wave, required, advisory, j, y,
               ni, nc, hydro, dynamic, combined)
    if not all(math.isfinite(value) for value in results):
        raise ValueError("Annex E results exceed the finite numerical range")
    status = ("no_calculated_uplift" if j <= .785 else
              "uplift_requires_further_checks" if j <= 1.54 else
              "redesign_or_mechanical_anchorage_required")
    return AnnexEResult(
        product_weight_n=wp, impulsive_weight_n=wi, convective_weight_n=wc,
        xi_m=xi, xc_m=xc, xis_m=xis, xcs_m=xcs, convective_period_s=tc,
        ai_g=ai, ac_g=ac, af_g=af, impulsive_shear_n=vi, convective_shear_n=vc,
        base_shear_n=math.hypot(vi, vc), ringwall_moment_nm=mrw, slab_moment_nm=ms,
        wave_height_m=wave, required_freeboard_m=required, advisory_freeboard_m=advisory,
        freeboard_required=freeboard_required,
        freeboard_ok=(v["available_freeboard_m"] >= required) if freeboard_required else None,
        anchorage_ratio_j=j, anchorage_status=status, evaluation_depth_m=y,
        impulsive_hoop_force_n_per_m=ni, convective_hoop_force_n_per_m=nc,
        hydrostatic_hoop_stress_pa=hydro, hydrodynamic_hoop_stress_pa=dynamic,
        combined_hoop_stress_pa=combined,
        citations=tuple(dict(citation) for citation in _ANNEX_E_CITATIONS),
        limitations=(
            "Public-source screening basis: 2007 load equations and 2016 freeboard; current-edition compliance is not established.",
            "Site minima, factors and annular resistance require project qualification.",
            "Shell compression, annular plate, piping, sliding, anchor and foundation capacities are not evaluated.",
            "No hoop allowable is supplied; material acceptance is not evaluated.",
            "Floating-roof lift, jamming, seal damage and shell-roof impact pressure require separate analysis.",
        ),
    )
