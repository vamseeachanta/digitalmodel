"""Fixed, offline acceptance criteria for the approved issue 2094 examples."""
import hashlib
import json
import math
from pathlib import Path, PureWindowsPath


def number(value):
    assert isinstance(value, (float, int)) and not isinstance(value, bool)
    assert math.isfinite(value), "nonfinite evidence"
    return value


def close(actual, expected, tolerance):
    assert abs(number(actual) - number(expected)) <= tolerance + 1e-12, (
        f"recorded {actual} differs from {expected} beyond rounding {tolerance}"
    )


def _finite_tree(value):
    if isinstance(value, dict):
        for item in value.values():
            _finite_tree(item)
    elif isinstance(value, list):
        for item in value:
            _finite_tree(item)
    elif isinstance(value, (float, int)) and not isinstance(value, bool):
        number(value)


def _unique_object(pairs):
    result = {}
    for key, value in pairs:
        assert key not in result, f"duplicate JSON key {key}"
        result[key] = value
    return result


def parse_digest(text):
    tokens = text.strip().replace("\n", ",").split(",")
    tokens = [token.strip() for token in tokens if token.strip()]
    assert tokens and len(tokens) % 2 == 0, "incomplete digest"
    result = {}
    for label, raw in zip(tokens[::2], tokens[1::2]):
        label = label.strip().lower()
        assert label not in result, f"duplicate digest key {label}"
        result[label] = number(float(raw))
    return result


def load_golden(case_dir):
    golden = case_dir / "golden"
    files = list(golden.glob("*_result.csv"))
    assert len(files) == 1, "exactly one golden result CSV is required"
    digest = parse_digest(files[0].read_text(encoding="utf-8"))
    provenance = json.loads((golden / "PROVENANCE.json").read_text(encoding="utf-8"),
                            object_pairs_hook=_unique_object)
    _finite_tree(provenance)
    name = provenance["input"]["deck"]
    assert isinstance(name, str) and name and name not in {".", ".."}
    assert not any(character in name for character in '/\\:')
    assert not PureWindowsPath(name).drive and Path(name).name == name
    deck = case_dir / name
    assert not deck.is_symlink() and deck.resolve().parent == case_dir.resolve()
    assert hashlib.sha256(deck.read_bytes()).hexdigest() == provenance["input"]["sha256"], (
        "deck SHA-256 differs from the recorded golden input"
    )
    return digest, provenance


def validate_acceptance(digest, provenance, module):
    """D2 requires compliant, sub-yield examples regardless of artifact flags."""
    acceptance = provenance["acceptance"]
    conditions = getattr(module, "CONDITIONS", None)
    yield_mpa = number(conditions.yield_strength_mpa if conditions else module.GEOM.yield_strength_mpa)
    allowable = number(conditions.allowable_stress_mpa if conditions else
                       module.GEOM.yield_strength_mpa / module.GEOM.design_factor)
    peak, uc = number(digest["max_seqv_mpa"]), number(digest["uc"])
    assert allowable > 0 and yield_mpa > 0 and 0 <= peak < yield_mpa
    assert 0 <= uc < 1 and peak / allowable < 1, "D2 requires unity below one"
    assert acceptance["expected_status"] == "within_allowable"
    assert acceptance["linear_elastic_limit_exceeded"] is False
    # CSV F12.4 stress/allowable and F10.5 unity: half-last-place bounds.
    close(digest["allowable_mpa"], allowable, 0.00005)
    close(acceptance["allowable_mpa"], digest["allowable_mpa"], 0.0001)
    close(acceptance["unity_check"], uc, 0.00001)
    rounding = 0.000005 + 0.00005 / allowable + peak * 0.00005 / allowable**2
    close(uc, peak / allowable, rounding)
    close(acceptance["peak_vs_yield_pct"], peak / yield_mpa * 100, 0.05)
    close(provenance["peak_location"]["node"], digest["peak_node"], 0)


def validate_pv_comparator(digest, provenance, expected_vm, hoop_mpa):
    comparator = provenance["comparator"]
    assert 0 < number(comparator["tolerance_pct"]) <= 1, "fixed PV ceiling is 1%"
    deviation = abs(number(digest["max_seqv_mpa"]) - expected_vm) / expected_vm * 100
    assert deviation <= comparator["tolerance_pct"]
    close(comparator["observed_von_mises_mpa"], digest["max_seqv_mpa"], 0.0001)
    close(comparator["expected_von_mises_mpa"], expected_vm, 0.005)
    close(comparator["sigma_theta_mpa"], hoop_mpa, 0.005)
    close(comparator["deviation_pct"], deviation, 0.005)


def _scientific_rounding(value):
    # E20.12 emits 12 significant mantissa digits (0.ddd...E+NN).
    return 0.5 * 10 ** (math.floor(math.log10(abs(value))) - 11) if value else 0


def _supported_balance(digest):
    keys = ("applied_fx_n", "applied_fy_n", "reaction_fx_n", "reaction_fy_n", "force_residual_n")
    assert all(key in digest for key in keys), "supported load requires all force components"
    ax, ay, rx, ry, recorded = [number(digest[key]) for key in keys]
    load = number(math.hypot(ax, ay))
    assert load > 0, "supported load must be nonzero"
    residual = number(math.hypot(ax + rx, ay + ry))
    assert recorded >= 0, "force residual is a nonnegative norm"
    assert residual <= 0.001 * load, "force imbalance exceeds 0.1% of applied load"
    rounding = math.hypot(_scientific_rounding(ax) + _scientific_rounding(rx),
                          _scientific_rounding(ay) + _scientific_rounding(ry))
    close(recorded, residual, rounding + _scientific_rounding(recorded))


def _check_applied_load(case, digest, provenance, module):
    if case == "pressure-vessel":
        close(provenance["equilibrium"]["applied_axial_load_n"], 0, 0)
    elif case == "mudmat":
        close(provenance["comparator"]["applied_vertical_load_n"],
              module.GEOM.vertical_load_kn * 1000, 0)
        _check_mudmat_fields(digest, provenance, module.GEOM)
    else:
        load = module.GEOM.sling_load_kn * 1000
        angle = math.radians(module.GEOM.sling_angle_deg)
        for key, expected in (("applied_fx_n", load * math.sin(angle)),
                              ("applied_fy_n", load * math.cos(angle))):
            close(digest[key], expected, _scientific_rounding(expected))


def _check_mudmat_fields(digest, provenance, geom):
    load = geom.vertical_load_kn * 1000
    eccentricity = abs(geom.moment_kNm * 1e6 / load)
    length = geom.mat_length_mm - 2 * eccentricity
    area = length * geom.mat_width_mm
    assert length > 0 and area > 0
    pressure = load / area
    derived = provenance['derived_quantities']
    close(digest['eccentricity_mm'], eccentricity, 0.00005)
    close(digest['q_soil_mpa'], pressure, 0.0000005)
    close(derived['eccentricity_mm'], eccentricity, 0.00005)
    close(derived['soil_pressure_mpa'], pressure, 0.0000005)
    close(derived['effective_length_mm'], length, 0.00005)
    close(derived['effective_area_mm2'], area, 0.00005)
    close(derived['patch_start_x_mm'], 2 * eccentricity, 0.00005)


def validate_equilibrium(case, digest, provenance, module=None):
    if module is not None:
        _check_applied_load(case, digest, provenance, module)
    if case == "padeye":
        _supported_balance(digest)
        return
    assert case in {"pressure-vessel", "mudmat"}
    tolerance = number(provenance.get("comparator", {}).get("tolerance_n", 1))
    assert 0 < tolerance <= 1, "fixed self-balanced reaction ceiling is 1 N"
    keys = ("reaction_fx_n", "reaction_fy_n") if case == "pressure-vessel" else ("reaction_fz_n",)
    assert all(key in digest for key in keys), "missing reaction component"
    assert math.hypot(*(number(digest[key]) for key in keys)) <= tolerance, (
        'self-balanced reaction resultant exceeds fixed tolerance'
    )
    for key in keys:
        assert abs(number(digest[key])) <= tolerance
        recorded = provenance["equilibrium" if case == "pressure-vessel" else "comparator"]
        close(recorded["observed_" + key], digest[key], 0.0001)
    if case == "mudmat":
        comparator = provenance["comparator"]
        applied = number(comparator["applied_vertical_load_n"])
        assert applied > 0
        close(comparator["relative_residual"], abs(digest["reaction_fz_n"]) / applied, 0.00005 / applied)
