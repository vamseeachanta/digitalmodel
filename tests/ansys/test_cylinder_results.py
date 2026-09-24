"""Synthetic protocol fixtures; no native format qualification is claimed."""
from decimal import Decimal

import pytest

from digitalmodel.ansys.cylinder_results import (
    EvidenceError, parse_station_values, parse_e24, validate_recovery,
)


def station_metadata():
    return [dict(id=f"{r}_y{y}", node_id=n + 1, x_mm=x, y_mm=str(y),
                 radial_id=r, node_type="corner",
                 adjacent_element_ids=list(range(1, count + 1)))
            for n, (y, r, x, count) in enumerate(
                (y, r, x, count) for y in (60, 120, 180)
                for r, x, count in (("inner", "750", 2),
                                    ("middle", "780", 4), ("outer", "810", 2)))]


def e24(value):
    mantissa, exponent = f"{Decimal(value):.16E}".split('E')
    exponent = int(exponent) if Decimal(value) else 0
    return f'{mantissa}E{exponent:+03d}'.rjust(24)


def export(stations=None):
    stations = stations or station_metadata()
    return b"".join(
        (f"{'P10N16':8}{s['node_id']:8.0f}{e24(s['x_mm'])}"
         f"{e24(s['y_mm'])}{c:8}{e24(v)}\n").encode()
        for s in stations for c, v in
        (("SX", "-10"), ("SZ", "130"), ("SY", "0"),
         ("SXY", "0"), ("UX", ".5"), ("UY", "-.02")))


def test_keyed_reordering_and_vm_after_components():
    rows = export().splitlines(keepends=True)
    values = parse_station_values(b"".join(reversed(rows)), "P10N16", station_metadata())
    assert len(values) == 63
    assert values['inner_y120', 'sigma_r'] == -10
    assert abs(values['inner_y120', 'sigma_vm'] ** 2 - Decimal(18300)) < Decimal('1e-20')


@pytest.mark.parametrize("mutation", ["missing", "duplicate", "case", "component", "coordinate", "truncated", "nan"])
def test_invalid_keyed_evidence_refuses(mutation):
    raw = export()
    if mutation == "missing": raw = b"\n".join(raw.splitlines()[1:]) + b"\n"
    if mutation == "duplicate": raw += raw.splitlines(keepends=True)[0]
    if mutation == "case": raw = raw.replace(b'P10N16', b'P10N8 ')
    if mutation == "component": raw = raw.replace(b'SXY     ', b'SXZ     ')
    if mutation == "coordinate": raw = raw.replace(e24('750').encode(), e24('751').encode())
    if mutation == "truncated": raw = raw[:-2]
    if mutation == "nan": raw = raw.replace(e24('-10').encode(), b' '*21 + b'NaN')
    with pytest.raises(EvidenceError):
        parse_station_values(raw, "P10N16", station_metadata())


@pytest.mark.parametrize("bad", ["  1.00000000E+02".rjust(24), "0".rjust(24), "*"*24])
def test_low_precision_nonprotocol_numeric_field_refuses(bad):
    with pytest.raises(EvidenceError): parse_e24(bad.encode(), 'MPa')


def test_readback_checks_full_adjacency_and_precision():
    stations = station_metadata()
    values = parse_station_values(export(), 'P10N16', stations)
    listing = {key: e24(val).encode() for key, val in values.items() if key[1] != 'sigma_vm'}
    contributions = {s['id']: {e: {q: listing[s['id'], q] for q in
                     ('sigma_r', 'sigma_theta', 'sigma_z', 'tau_rz')}
                    for e in s['adjacent_element_ids']} for s in stations}
    validate_recovery(values, stations, listing, contributions)
    contributions['inner_y120'].pop(1)
    with pytest.raises(EvidenceError):
        validate_recovery(values, stations, listing, contributions)


@pytest.mark.parametrize('sign', [-1, 1])
def test_substituted_listing_refuses(sign):
    stations = station_metadata()
    values = parse_station_values(export(), 'P10N16', stations)
    listing = {key: e24(val).encode() for key, val in values.items() if key[1] != 'sigma_vm'}
    listing['inner_y120', 'u_r'] = e24(Decimal('.5') + sign * Decimal('.0001')).encode()
    with pytest.raises(EvidenceError): validate_recovery(values, stations, listing, {})


def test_extraction_ignores_ambient_decimal_precision_and_traps():
    from decimal import Context, Inexact, ROUND_DOWN, localcontext
    raw,stations=export(),station_metadata()
    expected=parse_station_values(raw,'P10N16',stations)
    with localcontext(Context(prec=6,rounding=ROUND_DOWN)) as ctx:
        ctx.traps[Inexact]=True
        assert parse_station_values(raw,'P10N16',stations)==expected


def test_mutated_decimal_default_context_cannot_change_results():
    from decimal import DefaultContext, Inexact, ROUND_DOWN
    raw,stations=export(),station_metadata()
    expected=parse_station_values(raw,'P10N16',stations)
    rounding,traps=DefaultContext.rounding,dict(DefaultContext.traps)
    try:
        DefaultContext.rounding=ROUND_DOWN
        DefaultContext.traps[Inexact]=True
        assert parse_station_values(raw,'P10N16',stations)==expected
    finally:
        DefaultContext.rounding=rounding
        DefaultContext.traps.update(traps)
