"""ASME VIII-1 (2019) Appendix 2 analytical rating, US customary units.

Integral non-cast flanges only, including straight hubs. Loose/lap-joint,
external-pressure, weld/neck qualification and material selection are outside
this screening calculation. Geometry is supplied net of corrosion allowance.
Temperature-dependent allowables are supplied by the caller, never inferred.
"""

from dataclasses import asdict
from math import isfinite, pi, sin, sqrt
from copy import deepcopy

from .factors import flange_factors, hub_factors

from digitalmodel.citations import Citation, validate_citation

STRESS_CLAUSES = {
    "S_H": "App 2-7(a) eq. (8); App 2-8(a)(1)",
    "S_R": "App 2-7(a) eq. (9); App 2-8(a)(2)",
    "S_T": "App 2-7(a) eq. (10); App 2-8(a)(3)",
    "average_HR": "App 2-8(a)(4)",
    "average_HT": "App 2-8(a)(4)",
}

WIKI_PATH = "wikis/engineering-standards/wiki/standards/asme-sec-viii-div1-app2.md"


def citations():
    """Validate each clause before returning standards-derived results."""
    sections = ["App 2-5(c) eq. (1)", "App 2-5(c) eq. (2)",
                "App 2-5(d)", "App 2-5(e)", "Table 2-5.1", "Table 2-5.2",
                "App 2-6; Table 2-6", "App 2-6 eq. (7)", "Fig 2-7.1", "Table 2-7.1",
                "App 2-7(a) eqs. (8)-(10)", "App 2-8(a)", "App 2-14"]
    sections.extend(dict.fromkeys(STRESS_CLAUSES.values()))
    result = []
    for section in sections:
        citation = Citation(
            code_id="asme-viii-1-app2", publisher="ASME", revision="2019",
            section=section, wiki_path=WIKI_PATH, source_sibling="generic",
        )
        validate_citation(citation)
        result.append(asdict(citation))
    return result


def analyze(settings):
    """Calculate minimum bolt loads and areas (in, lbf, psi)."""
    settings = _validated(settings)
    references = citations()
    gasket, bolts = settings["gasket"], settings["bolts"]
    b0 = gasket["b0"]
    b = b0 if b0 <= 0.25 else 0.5 * sqrt(b0)
    G = ((gasket["outer_diameter"] + gasket["inner_diameter"]) / 2
         if b0 <= 0.25 else gasket["outer_diameter"] - 2 * b)
    P = settings["design"]["P"]
    H = 0.785 * G**2 * P
    Hp = 2 * b * 3.14 * G * gasket["m"] * P
    Wm1 = H + Hp
    Wm2 = 3.14 * b * G * gasket["y"]
    Am = max(Wm1 / bolts["Sb"], Wm2 / bolts["Sa"])
    Ab = bolts["count"] * bolts["root_area"]
    result = {
        "gasket": {"b": b, "G": G},
        "bolt_loads": {"H": H, "Hp": Hp, "Wm1": Wm1, "Wm2": Wm2,
                       "Am": Am, "Ab": Ab,
                       "W_operating": Wm1, "W_seating": (Am + Ab) * bolts["Sa"] / 2},
        "citations": references,
    }
    return _rating(settings, result)


def _check(name, demand, allowable, clause, units):
    return {"name": name, "demand": demand, "allowable": allowable,
            "utilization": demand / allowable, "passes": demand <= allowable,
            "clause": clause, "units": units}


def _rating(settings, result):
    geo = settings["geometry"]
    B, t, g0, g1 = (geo[k] for k in ("B", "t", "g0", "g1"))
    factors = flange_factors(geo["A"] / B)
    h0 = sqrt(B * g0)
    factors.update(hub_factors(g0=g0, g1=g1, h=geo["h"], h0=h0))
    e = factors["F"] / h0
    d = (factors["U"] / factors["V"]) * h0 * g0**2
    L = (t*e + 1) / factors["T"] + t**3 / d
    bolts = settings["bolts"]
    spacing = geo["C"] * sin(pi / bolts["count"])
    Bsc = max(1.0, sqrt(spacing / (2*bolts["diameter"] + t)))
    factors.update({"h0": h0, "e": e, "d": d, "L": L, "Bsc": Bsc})
    R = (geo["C"] - B)/2 - g1
    hD = R + g1/2
    hG = (geo["C"] - result["gasket"]["G"])/2
    hT = (R + g1 + hG)/2
    loads = result["bolt_loads"]
    HD = 0.785 * B**2 * settings["design"]["P"]
    HT = loads["H"] - HD
    HG = loads["W_operating"] - loads["H"]
    moments = {"operating": HD*hD + HT*hT + HG*hG,
               "seating": loads["W_seating"]*hG}
    result.update({"factors": factors, "moments": moments,
                   "moment_arms": {"hD": hD, "hT": hT, "hG": hG},
                   "component_loads": {"HD": HD, "HT": HT, "HG": HG}})
    checks = [_check("bolt_area", loads["Am"], loads["Ab"], "App 2-5(d)", "in^2")]
    conditions = {}
    for condition, moment in moments.items():
        stresses, condition_checks = _stress_checks(condition, moment, settings, factors)
        conditions[condition] = stresses
        checks.extend(condition_checks)
    governing = max(checks, key=lambda row: row["utilization"])
    result.update({"conditions": conditions, "checks": checks,
                   "governing_check": governing["name"],
                   "governing_utilization": governing["utilization"],
                   "screening_status": "pass" if all(c["passes"] for c in checks) else "fail"})
    return result


def _stress_checks(condition, moment, settings, factors):
    geo, design = settings["geometry"], settings["design"]
    B, t, g1 = (geo[k] for k in ("B", "t", "g1"))
    L, e = factors["L"], factors["e"]
    stress_moment = abs(moment)
    if design.get("apply_bolt_spacing_correction", False):
        stress_moment *= factors["Bsc"]
    SH = factors["f"] * stress_moment / (L * g1**2 * B)
    SR = (1.33*t*e + 1) * stress_moment / (L * t**2 * B)
    ST = factors["Y"] * stress_moment / (t**2 * B) - factors["Z"]*SR
    stresses = {"S_H": SH, "S_R": SR, "S_T": ST,
                "average_HR": (abs(SH)+abs(SR))/2,
                "average_HT": (abs(SH)+abs(ST))/2}
    Sf = design[f"Sf_{condition}"]
    checks = [_check(f"{condition}.{name}", abs(value),
                     1.5*Sf if name == "S_H" else Sf,
                     STRESS_CLAUSES[name], "psi") for name, value in stresses.items()]
    J = (52.14 * abs(moment) * factors["V"] /
         (L * design[f"E_{condition}"] * geo["g0"]**2 * factors["h0"] * 0.3))
    stresses["J"] = J
    checks.append(_check(f"{condition}.J", J, 1.0, "App 2-14", "dimensionless"))
    return stresses, checks


def _number(value, name, *, zero_allowed=False):
    if isinstance(value, bool):
        raise ValueError(f"{name} must be a finite number")
    try:
        number = float(value)
    except (ValueError, TypeError) as exc:
        raise ValueError(f"{name} must be a finite number") from exc
    if not isfinite(number) or (number < 0 if zero_allowed else number <= 0):
        raise ValueError(f"{name} must be finite and {'nonnegative' if zero_allowed else 'positive'}")
    return number


def _validated(settings):
    data = deepcopy(settings)
    if data.get("flange_type") != "integral":
        raise ValueError("Only integral non-cast flanges are supported")
    if data.get("units") != "US_customary":
        raise ValueError("units must be US_customary (in, lbf, psi)")
    groups = {"geometry": ("A", "B", "C", "t", "g0", "g1", "h"),
              "bolts": ("diameter", "root_area", "Sa", "Sb"),
              "gasket": ("b0", "m", "y", "outer_diameter", "inner_diameter"),
              "design": ("P", "Sf_operating", "Sf_seating", "E_operating", "E_seating")}
    try:
        for group, keys in groups.items():
            for key in keys:
                data[group][key] = _number(data[group][key], f"{group}.{key}",
                                           zero_allowed=key in ("P", "m", "y"))
        count = data["bolts"]["count"]
        if type(count) is not int or count < 3:
            raise ValueError("bolts.count must be an integer >= 3")
        temperature = data["design"].get("temperature_F")
        if temperature is not None and not isfinite(float(temperature)):
            raise ValueError("temperature_F must be finite or null if unreported")
    except (KeyError, TypeError) as exc:
        raise ValueError("Required flange inputs are missing or malformed") from exc
    correction = data["design"].get("apply_bolt_spacing_correction", False)
    if type(correction) is not bool:
        raise ValueError("apply_bolt_spacing_correction must be boolean")
    _validate_geometry(data)
    return data


def _validate_geometry(data):
    geo, gasket, bolts = (data[k] for k in ("geometry", "gasket", "bolts"))
    if not geo["B"] < geo["C"] < geo["A"] or geo["g1"] < geo["g0"]:
        raise ValueError("Geometry requires B < C < A and g1 >= g0")
    if bolts["root_area"] > pi * bolts["diameter"]**2 / 4:
        raise ValueError("bolts.root_area cannot exceed the nominal bolt cross-section")
    if geo["C"] - bolts["diameter"] <= geo["B"] + 2*geo["g1"]:
        raise ValueError("Bolt holes must clear the hub")
    if geo["C"] + bolts["diameter"] >= geo["A"]:
        raise ValueError("Bolt holes must lie inside the flange edge")
    if not geo["B"] <= gasket["inner_diameter"] < gasket["outer_diameter"] < geo["C"] - bolts["diameter"]:
        raise ValueError("Gasket must clear the bore and lie wholly inside the bolt holes")
    if gasket["b0"] > (gasket["outer_diameter"] - gasket["inner_diameter"]) / 2:
        raise ValueError("Basic gasket width cannot exceed radial contact width")
