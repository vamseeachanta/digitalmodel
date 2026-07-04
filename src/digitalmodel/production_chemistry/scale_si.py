# ABOUTME: Mineral-scale saturation indices (SI = log10 IAP/Ksp) in the Oddo–Tomson
# ABOUTME: conditional-solubility framework: calcite (CO2-partitioned), sulfates, halite.

"""Produced-water mineral-scale saturation indices (Oddo–Tomson framework).

Basis
-----
Oddo, J.E. and Tomson, M.B., "Why Scale Forms in the Oil Field and Methods
To Predict It", *SPE Production & Facilities* 9(1), 1994 (SPE-21710-PA).
That paper is the practitioner-standard scale-prediction framework:

* every scale family is scored by a saturation index
  ``SI = log10(IAP / Ksp_cond)``, where IAP is the ion-activity (here
  ion-concentration) product and ``Ksp_cond`` is a *conditional* solubility
  product correlated empirically as a polynomial in temperature, pressure,
  and ionic strength (the activity coefficients are absorbed into the
  conditional constant);
* for calcite the carbonate system is handled through CO2 partitioning —
  the in-situ pH is controlled by the CO2 content of the gas phase at the
  local T/P, so the calcite SI is written either in a pH form
  (``SI = log10([Ca][HCO3]) + pH - K(T, P, I)``) or, preferably, in a
  gas-CO2 form that needs no measured pH
  (``SI = log10([Ca][HCO3]^2 / fCO2) + K(T, P, I)``);
* the sulfate scales (gypsum / hemihydrate / anhydrite, barite, celestite)
  and halite use the plain conditional-solubility form
  ``SI = log10([M][X]) + pK_cond(T, P, I)``.

Coefficient honesty statement (IMPORTANT)
-----------------------------------------
The polynomial *form* above is the published Oddo–Tomson framework.  The
numeric coefficient sets shipped in :data:`DEFAULT_COEFFICIENTS` are **not**
transcribed from the 1994 paper — they are configurable defaults constructed
to be physically consistent:

* anchored at 25 degC / 1 atm / zero ionic strength to standard-handbook
  thermodynamic solubility products and carbonate equilibrium constants
  (order: pKsp calcite 8.48, barite 9.98, celestite 6.63, gypsum 4.58,
  anhydrite 4.36; pK1/pK2 carbonic acid 6.35/10.33; CO2 Henry constant
  pKH ~ 1.47 in molal/bar);
* with temperature, pressure, and ionic-strength trends that reproduce the
  well-known qualitative behaviors (calcite SI rises with temperature and
  with pressure drop; barite solubility increases with temperature below
  ~150 degC; gypsum→anhydrite stability crossover near ~40 degC;
  salting-in at moderate ionic strength).

Calibrate/override the coefficients (``coefficients=`` argument) against a
trusted dataset before any quantitative use.  Every result carries a
``basis`` string restating this.

Other engineering assumptions (documented, deliberate)
------------------------------------------------------
* Concentrations are entered in mg/L and converted to mol/L; mol/L is used
  interchangeably with molality (dilute-solution assumption — at very high
  TDS this adds error that the conditional constants partially absorb).
* Brine mixing is ideal-volumetric: species concentrations mix linearly
  with volume fraction; hydrogen-ion concentration (not pH) mixes linearly.
  No precipitation/re-equilibration during mixing is modeled — the sweep
  answers "what is the *tendency* of the freshly mixed water".
* CO2 fugacity is approximated by its partial pressure (fugacity
  coefficient 1.0) — a configurable hook absorbs non-ideality into the
  conditional constant.
* Water of hydration (gypsum) and the water activity are absorbed into the
  conditional constants.

Units: temperature degF, pressure psia, concentrations mg/L, ionic
strength mol/L.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Iterable, Mapping, Optional, Sequence, Tuple

import pandas as pd

__all__ = [
    "BrineComposition",
    "CorrelationCoefficients",
    "DEFAULT_COEFFICIENTS",
    "SCALE_FAMILIES",
    "ScaleIndexResult",
    "default_coefficients",
    "ionic_strength",
    "mix_brines",
    "mixing_sweep",
    "saturation_indices",
    "si_profile",
]

# ---------------------------------------------------------------------------
# Ion bookkeeping (derivation-anchored: molecular weights and charges are
# standard-handbook values; ionic strength is the textbook definition).
# ---------------------------------------------------------------------------

#: (molecular weight g/mol, ionic charge) per BrineComposition field.
ION_PROPERTIES: Mapping[str, Tuple[float, int]] = {
    "na": (22.990, +1),
    "k": (39.098, +1),
    "mg": (24.305, +2),
    "ca": (40.078, +2),
    "sr": (87.620, +2),
    "ba": (137.327, +2),
    "fe": (55.845, +2),
    "cl": (35.453, -1),
    "so4": (96.060, -2),
    "hco3": (61.016, -1),
}

SCALE_FAMILIES: Tuple[str, ...] = (
    "calcite",
    "gypsum",
    "hemihydrate",
    "anhydrite",
    "barite",
    "celestite",
    "halite",
)


@dataclass
class BrineComposition:
    """Produced-water ion composition (all concentrations in mg/L).

    ``hco3`` is the bicarbonate alkalinity expressed as mg/L HCO3-.
    ``ph`` is the measured pH (optional); ``co2_mole_fraction_gas`` is the
    CO2 mole fraction in the associated gas (optional).  When the CO2
    fraction is given the calcite SI uses the Oddo–Tomson gas-CO2 route
    (in-situ pH recomputed at each T/P); otherwise the measured-pH route.
    """

    na: float = 0.0
    k: float = 0.0
    mg: float = 0.0
    ca: float = 0.0
    sr: float = 0.0
    ba: float = 0.0
    fe: float = 0.0
    cl: float = 0.0
    so4: float = 0.0
    hco3: float = 0.0
    tds: Optional[float] = None
    ph: Optional[float] = None
    co2_mole_fraction_gas: Optional[float] = None

    def __post_init__(self) -> None:
        for name in ION_PROPERTIES:
            if getattr(self, name) < 0:
                raise ValueError(f"{name} concentration must be >= 0 mg/L")
        if self.tds is not None and self.tds < 0:
            raise ValueError("tds must be >= 0 mg/L")
        if self.co2_mole_fraction_gas is not None and not (
            0.0 <= self.co2_mole_fraction_gas <= 1.0
        ):
            raise ValueError("co2_mole_fraction_gas must be in [0, 1]")

    def molarity(self, ion: str) -> float:
        """Concentration of ``ion`` in mol/L (from mg/L)."""
        mw, _z = ION_PROPERTIES[ion]
        return getattr(self, ion) / mw / 1000.0

    @property
    def tds_mg_l(self) -> float:
        """TDS in mg/L — the given value, else the sum of listed ions."""
        if self.tds is not None:
            return self.tds
        return sum(getattr(self, name) for name in ION_PROPERTIES)

    def ionic_strength(self) -> float:
        """Ionic strength I = 1/2 * sum(c_i * z_i^2) in mol/L."""
        return 0.5 * sum(
            self.molarity(name) * z * z for name, (_mw, z) in ION_PROPERTIES.items()
        )


def ionic_strength(brine: BrineComposition) -> float:
    """Module-level convenience for :meth:`BrineComposition.ionic_strength`."""
    return brine.ionic_strength()


# ---------------------------------------------------------------------------
# Conditional-constant correlation (the Oddo–Tomson polynomial form).
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class CorrelationCoefficients:
    """One conditional-constant polynomial in the Oddo–Tomson form.

    ``value(T, P, I) = a0 + a_t*T + a_t2*T^2 + a_t3*T^3 + a_p*P
                       + a_sqrt_i*sqrt(I) + a_i*I``

    with T in degF, P in psia, I in mol/L.  Depending on the family the
    value is used as a conditional pKsp (sulfates, halite) or as the
    lumped carbonate-system constant (calcite pH / gas-CO2 routes).
    """

    a0: float = 0.0
    a_t: float = 0.0
    a_t2: float = 0.0
    a_t3: float = 0.0
    a_p: float = 0.0
    a_sqrt_i: float = 0.0
    a_i: float = 0.0

    def value(self, t_f: float, p_psia: float, i_molar: float) -> float:
        if i_molar < 0:
            raise ValueError("ionic strength must be >= 0")
        return (
            self.a0
            + self.a_t * t_f
            + self.a_t2 * t_f**2
            + self.a_t3 * t_f**3
            + self.a_p * p_psia
            + self.a_sqrt_i * math.sqrt(i_molar)
            + self.a_i * i_molar
        )


#: Configurable default coefficient sets — see the module docstring
#: "Coefficient honesty statement".  These are physically-consistent
#: defaults in the published Oddo–Tomson functional form, anchored to
#: standard 25 degC thermodynamic constants; they are NOT the coefficient
#: values printed in the 1994 paper.  Override via ``coefficients=``.
DEFAULT_COEFFICIENTS: Mapping[str, CorrelationCoefficients] = {
    # SI = log10(mCa * mHCO3) + pH - K(T,P,I); K ~ (pK2 - pKsp,calcite)
    # plus activity absorption.  Anchor K(77F, 14.7 psia, I=0) ~ 1.85.
    "calcite_ph": CorrelationCoefficients(
        a0=2.274, a_t=-5.5e-3, a_p=3.5e-5, a_sqrt_i=2.55, a_i=-1.0
    ),
    # SI = log10(mCa * mHCO3^2 / fCO2[psia]) + K(T,P,I);
    # K ~ (pKH + pK1 + pKsp,calcite - pK2) + psia/bar offset.
    # Anchor K(77F, I=0) ~ 5.97 (bar basis) + 1.161 = 6.538 (psia basis).
    "calcite_co2": CorrelationCoefficients(
        a0=6.538, a_t=7.7e-3, a_p=-3.5e-5, a_sqrt_i=-3.05, a_i=1.2
    ),
    # In-situ pH estimate from the gas CO2 fraction:
    # pH = log10(mHCO3 / fCO2[psia]) + K(T,I); anchor ~ (pKH + pK1) at 77F
    # = 7.82 (bar basis) + 1.161 = 8.981 (psia basis).
    "ph_from_co2": CorrelationCoefficients(
        a0=8.796, a_t=2.4e-3, a_sqrt_i=-0.51, a_i=0.3
    ),
    # Sulfates & halite: SI = log10(m_cation * m_anion) + pKsp_cond(T,P,I).
    # Gypsum CaSO4.2H2O — pKsp(77F) ~ 4.58, solubility max near ~104F.
    "gypsum": CorrelationCoefficients(
        a0=4.701, a_t=-2.5e-3, a_t2=1.2e-5, a_p=-4.0e-5, a_sqrt_i=-4.0, a_i=1.7
    ),
    # Hemihydrate CaSO4.0.5H2O — metastable/more soluble than gypsum at low
    # T, strongly retrograde; crosses gypsum near ~180F.
    "hemihydrate": CorrelationCoefficients(
        a0=2.783, a_t=1.1e-2, a_p=-4.0e-5, a_sqrt_i=-4.0, a_i=1.7
    ),
    # Anhydrite CaSO4 — pKsp(77F) ~ 4.36, retrograde; gypsum/anhydrite
    # stability crossover near ~104F (~40 degC).
    "anhydrite": CorrelationCoefficients(
        a0=3.744, a_t=8.0e-3, a_p=-4.0e-5, a_sqrt_i=-4.0, a_i=1.7
    ),
    # Barite BaSO4 — pKsp(77F) ~ 9.98; solubility increases with T below
    # ~300F (SI falls with T at low T), increases with P.
    "barite": CorrelationCoefficients(
        a0=10.785, a_t=-1.2e-2, a_t2=2.0e-5, a_p=-5.0e-5, a_sqrt_i=-4.0, a_i=1.7
    ),
    # Celestite SrSO4 — pKsp(77F) ~ 6.63, mildly retrograde above ambient.
    "celestite": CorrelationCoefficients(
        a0=6.399, a_t=3.0e-3, a_p=-4.0e-5, a_sqrt_i=-4.0, a_i=1.7
    ),
    # Halite NaCl — conditional constant anchored so a saturated NaCl brine
    # (~6.1 mol/L Na and Cl at 77F) sits at SI ~ 0; solubility nearly flat,
    # slightly increasing with T.  (Framework form; halite is scored on the
    # same conditional-solubility basis, not from an Oddo–Tomson table.)
    "halite": CorrelationCoefficients(a0=-1.561, a_t=-2.0e-4),
}


def default_coefficients() -> dict[str, CorrelationCoefficients]:
    """A mutable copy of :data:`DEFAULT_COEFFICIENTS` for user calibration."""
    return dict(DEFAULT_COEFFICIENTS)


# ---------------------------------------------------------------------------
# Saturation indices
# ---------------------------------------------------------------------------

_BASIS_PREFIX = (
    "Oddo-Tomson conditional-solubility SI framework (SPE Prod & Facilities "
    "9(1), 1994)"
)
_BASIS_HONESTY = (
    "coefficients are configurable physically-consistent defaults anchored "
    "to standard 25 degC constants, NOT verified paper values — calibrate "
    "before quantitative use"
)


@dataclass(frozen=True)
class ScaleIndexResult:
    """Saturation index for one scale family at one (T, P) state."""

    family: str
    si: float
    log10_iap: float
    pk_cond: float
    basis: str
    details: Mapping[str, float] = field(default_factory=dict)


def _log10_or_nan(x: float) -> float:
    return math.log10(x) if x > 0.0 else float("nan")


def _sulfate_type_result(
    family: str,
    cation: str,
    brine: BrineComposition,
    t_f: float,
    p_psia: float,
    i_molar: float,
    coeff: CorrelationCoefficients,
    anion: str = "so4",
    label: str = "sulfate",
) -> ScaleIndexResult:
    m_cat = brine.molarity(cation)
    m_an = brine.molarity(anion)
    log_iap = _log10_or_nan(m_cat * m_an)
    pk = coeff.value(t_f, p_psia, i_molar)
    si = log_iap + pk
    basis = (
        f"{_BASIS_PREFIX}: {label} conditional-solubility form "
        f"SI = log10([{cation.upper()}][{anion.upper()}]) + pKsp_cond(T,P,I); "
        f"{_BASIS_HONESTY}"
    )
    return ScaleIndexResult(
        family=family,
        si=si,
        log10_iap=log_iap,
        pk_cond=pk,
        basis=basis,
        details={"ionic_strength_mol_l": i_molar},
    )


def _calcite_result(
    brine: BrineComposition,
    t_f: float,
    p_psia: float,
    i_molar: float,
    coefficients: Mapping[str, CorrelationCoefficients],
) -> ScaleIndexResult:
    m_ca = brine.molarity("ca")
    m_hco3 = brine.molarity("hco3")
    details: dict[str, float] = {"ionic_strength_mol_l": i_molar}

    if brine.co2_mole_fraction_gas is not None and brine.co2_mole_fraction_gas > 0:
        # Gas-CO2 route (preferred: in-situ pH follows CO2 partitioning at
        # the local T/P instead of trusting a surface-measured pH).
        f_co2 = brine.co2_mole_fraction_gas * p_psia  # fugacity coeff = 1.0
        coeff = coefficients["calcite_co2"]
        log_iap = _log10_or_nan(m_ca * m_hco3**2 / f_co2)
        k = coeff.value(t_f, p_psia, i_molar)
        si = log_iap + k
        ph_est = _log10_or_nan(m_hco3 / f_co2) + coefficients["ph_from_co2"].value(
            t_f, p_psia, i_molar
        )
        details["f_co2_psia"] = f_co2
        details["estimated_ph"] = ph_est
        basis = (
            f"{_BASIS_PREFIX}: calcite gas-CO2 route "
            "SI = log10([Ca][HCO3]^2 / fCO2) + K(T,P,I) with in-situ pH from "
            f"CO2 partitioning (fugacity coeff = 1.0); {_BASIS_HONESTY}"
        )
        return ScaleIndexResult("calcite", si, log_iap, k, basis, details)

    if brine.ph is not None:
        coeff = coefficients["calcite_ph"]
        log_iap = _log10_or_nan(m_ca * m_hco3)
        k = coeff.value(t_f, p_psia, i_molar)
        si = log_iap + brine.ph - k
        basis = (
            f"{_BASIS_PREFIX}: calcite measured-pH route "
            "SI = log10([Ca][HCO3]) + pH - K(T,P,I) (pH assumed valid at "
            f"the stated T/P); {_BASIS_HONESTY}"
        )
        details["ph_used"] = brine.ph
        return ScaleIndexResult("calcite", si, log_iap, k, basis, details)

    raise ValueError(
        "calcite SI needs either co2_mole_fraction_gas (preferred, gas-CO2 "
        "route) or ph on the BrineComposition"
    )


def saturation_indices(
    brine: BrineComposition,
    t_f: float,
    p_psia: float,
    coefficients: Optional[Mapping[str, CorrelationCoefficients]] = None,
) -> dict[str, ScaleIndexResult]:
    """Saturation index per scale family at one (T degF, P psia) state.

    Returns ``{family: ScaleIndexResult}`` for the families in
    :data:`SCALE_FAMILIES`.  ``SI > 0`` means supersaturated (scaling
    tendency); families whose ions are absent return ``si = nan``.
    """
    if p_psia <= 0:
        raise ValueError("p_psia must be > 0")
    coeffs = dict(DEFAULT_COEFFICIENTS)
    if coefficients:
        coeffs.update(coefficients)
    i_molar = brine.ionic_strength()

    results: dict[str, ScaleIndexResult] = {
        "calcite": _calcite_result(brine, t_f, p_psia, i_molar, coeffs)
    }
    for family, cation in (
        ("gypsum", "ca"),
        ("hemihydrate", "ca"),
        ("anhydrite", "ca"),
        ("barite", "ba"),
        ("celestite", "sr"),
    ):
        results[family] = _sulfate_type_result(
            family, cation, brine, t_f, p_psia, i_molar, coeffs[family]
        )
    results["halite"] = _sulfate_type_result(
        "halite",
        "na",
        brine,
        t_f,
        p_psia,
        i_molar,
        coeffs["halite"],
        anion="cl",
        label="halite",
    )
    return results


# ---------------------------------------------------------------------------
# SI trending along a T/P path (bottomhole -> wellhead)
# ---------------------------------------------------------------------------


def si_profile(
    brine: BrineComposition,
    tp_profile: Sequence[Tuple[float, float]],
    coefficients: Optional[Mapping[str, CorrelationCoefficients]] = None,
) -> pd.DataFrame:
    """SI trending along a production-string T/P path.

    ``tp_profile`` is an ordered list of ``(t_f, p_psia)`` steps, typically
    bottomhole first and wellhead last.  Returns one row per step with
    columns ``step, t_f, p_psia, si_<family>..., dsi_<family>...`` where
    ``dsi`` is the SI change relative to the first (bottomhole) step.

    The composition is held fixed along the path (no precipitation en
    route); only the thermodynamic driving force is trended.
    """
    steps = list(tp_profile)
    if not steps:
        raise ValueError("tp_profile must contain at least one (t_f, p_psia) step")
    rows = []
    for idx, (t_f, p_psia) in enumerate(steps):
        res = saturation_indices(brine, t_f, p_psia, coefficients)
        row: dict[str, float] = {"step": idx, "t_f": t_f, "p_psia": p_psia}
        for family in SCALE_FAMILIES:
            row[f"si_{family}"] = res[family].si
        rows.append(row)
    df = pd.DataFrame(rows)
    for family in SCALE_FAMILIES:
        df[f"dsi_{family}"] = df[f"si_{family}"] - df[f"si_{family}"].iloc[0]
    return df


# ---------------------------------------------------------------------------
# Brine mixing (waterflood / seawater-injection compatibility)
# ---------------------------------------------------------------------------


def mix_brines(
    brine_a: BrineComposition, brine_b: BrineComposition, fraction_b: float
) -> BrineComposition:
    """Ideal-volumetric mix: ``fraction_b`` of B with ``1 - fraction_b`` of A.

    Documented assumptions: species concentrations (mg/L) and TDS mix
    linearly with volume fraction; hydrogen-ion concentration (10**-pH)
    mixes linearly (an approximation — no carbonate re-speciation); the gas
    CO2 mole fraction mixes linearly when both brines define it, else it is
    dropped.  No precipitation during mixing is modeled.
    """
    if not 0.0 <= fraction_b <= 1.0:
        raise ValueError("fraction_b must be in [0, 1]")
    fa, fb = 1.0 - fraction_b, fraction_b
    kwargs: dict[str, float] = {
        name: fa * getattr(brine_a, name) + fb * getattr(brine_b, name)
        for name in ION_PROPERTIES
    }
    kwargs["tds"] = fa * brine_a.tds_mg_l + fb * brine_b.tds_mg_l

    if brine_a.ph is not None and brine_b.ph is not None:
        h_mix = fa * 10.0 ** (-brine_a.ph) + fb * 10.0 ** (-brine_b.ph)
        kwargs["ph"] = -math.log10(h_mix)
    elif fraction_b == 0.0:
        kwargs["ph"] = brine_a.ph
    elif fraction_b == 1.0:
        kwargs["ph"] = brine_b.ph

    ya, yb = brine_a.co2_mole_fraction_gas, brine_b.co2_mole_fraction_gas
    if ya is not None and yb is not None:
        kwargs["co2_mole_fraction_gas"] = fa * ya + fb * yb
    elif fraction_b == 0.0:
        kwargs["co2_mole_fraction_gas"] = ya
    elif fraction_b == 1.0:
        kwargs["co2_mole_fraction_gas"] = yb

    return BrineComposition(**kwargs)


def mixing_sweep(
    brine_a: BrineComposition,
    brine_b: BrineComposition,
    fractions: Iterable[float],
    t_f: float = 77.0,
    p_psia: float = 14.7,
    coefficients: Optional[Mapping[str, CorrelationCoefficients]] = None,
) -> pd.DataFrame:
    """Mixed-water SI versus mixing fraction of ``brine_b`` at fixed T/P.

    The classic waterflood-compatibility sweep (e.g. barium-rich formation
    water A mixed with sulfate-rich seawater B).  Concentrations mix
    linearly with volume fraction — see :func:`mix_brines` for the full
    assumption list.  Returns columns ``fraction_b, si_<family>...``.
    """
    rows = []
    for f in fractions:
        mixed = mix_brines(brine_a, brine_b, float(f))
        res = saturation_indices(mixed, t_f, p_psia, coefficients)
        row: dict[str, float] = {"fraction_b": float(f)}
        for family in SCALE_FAMILIES:
            row[f"si_{family}"] = res[family].si
        rows.append(row)
    if not rows:
        raise ValueError("fractions must contain at least one value")
    return pd.DataFrame(rows)
