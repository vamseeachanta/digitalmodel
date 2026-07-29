"""Rod and connection transformations for reference-catalog extraction."""

from collections import defaultdict
from decimal import Decimal
import re

from ._reference_catalog_io import decimal_text, is_numeric, read_rows, text
from ._reference_catalog_manifest import counts


def rod_detail_rows(path):
    records, dimensions = read_rows(path, "Sheet1")
    groups = defaultdict(list)
    quarantine = []
    for source_row, record in records:
        key = _parse_rod_label(record["Rod Grade"])
        values = tuple(
            decimal_text(record[name])
            for name in (
                "Rod Area", "Unit Weight", "Modulus of Elast",
                "Spd of Sound", "Tensil Strength",
            )
        )
        if key is None:
            quarantine.append(_quarantine_row(source_row, record, "non-keyable label"))
        else:
            groups[key].append((source_row, record["Rod Grade"], values))
    emitted, duplicates = [], 0
    for (grade, diameter), values in sorted(groups.items()):
        if len({value[2] for value in values}) != 1:
            quarantine.extend(_conflict_rows(values))
            continue
        emitted.append(_canonical_rod(grade, diameter, values))
        duplicates += len(values) - 1
    counts = {
        "source_rows": len(records), "emitted_rows": len(emitted),
        "duplicate_rows": duplicates, "quarantined_rows": len(quarantine),
        "worksheet_rows": dimensions[0], "worksheet_columns": dimensions[1],
    }
    return emitted, quarantine, counts


def rods_catalog_rows(path):
    records, dimensions = read_rows(path, "Rods Catalog", first_row=3)
    usable = [record for record in records if is_numeric(record[1]["."])]
    rows = [_rods_catalog_row(row_number, row) for row_number, row in usable]
    source_counts = {
        **counts(len(rows), dimensions), "source_rows": len(records),
        "rejected_rows": len(records) - len(rows),
    }
    return rows, dimensions, source_counts


def _rods_catalog_row(row_number, row):
    return {
        "source_row": row_number, "catalog_id": decimal_text(row["."]),
        "description": text(row["DESC"]),
        "tensile_strength_psi": decimal_text(row["TENSILE"]),
        "area_in2": decimal_text(row["AREA"]),
        "modulus_mpsi": decimal_text(row["MOE"]),
        "velocity_kft_s": decimal_text(row["VELOCITY"]),
        "unit_weight_lbf_ft": decimal_text(row["DENSITY"]),
        "catalog_velocity_relative_residual": _velocity_residual(
            row["AREA"], row["DENSITY"],
            Decimal(str(row["MOE"])) * Decimal("1000000"),
            Decimal(str(row["VELOCITY"])) * Decimal("1000"),
        ),
        "elastorq_raw": decimal_text(row["ELASTORQ"]),
    }


def _parse_rod_label(value):
    label = text(value).strip('"')
    match = re.fullmatch(r"(.*?)\s*-\s*(\d+(?:\.\d+)?)", label)
    if not match:
        return None
    grade = re.sub(r"\s+", " ", match.group(1)).strip().upper()
    diameter = decimal_text(match.group(2))
    if not grade or Decimal(diameter).as_tuple().exponent < -3:
        return None
    return grade, diameter


def _conflict_rows(values):
    rows = []
    for source_row, label, properties in values:
        record = {
            "Rod Grade": label, "Rod Area": properties[0],
            "Unit Weight": properties[1], "Modulus of Elast": properties[2],
            "Spd of Sound": properties[3], "Tensil Strength": properties[4],
        }
        rows.append(_quarantine_row(
            source_row, record, "conflicting normalized key"
        ))
    return rows


def _canonical_rod(grade, diameter, values):
    area, weight, modulus_mpsi, velocity_kft_s, tensile = values[0][2]
    modulus_psi = Decimal(modulus_mpsi) * Decimal("1000000")
    density = Decimal(weight) / (Decimal(area) * Decimal("12"))
    velocity = (modulus_psi * Decimal("386.0886") / density).sqrt() / 12
    return {
        "grade": grade, "diameter_in": diameter, "area_in2": area,
        "unit_weight_lbf_ft": weight,
        "modulus_psi": decimal_text(modulus_psi),
        "catalog_sonic_velocity_ft_s": decimal_text(
            Decimal(velocity_kft_s) * 1000
        ),
        "weight_derived_velocity_ft_s": format(velocity, ".12f"),
        "tensile_strength_psi": tensile,
        "raw_sonic_velocity_kft_s": velocity_kft_s,
        "catalog_velocity_relative_residual": _velocity_residual(
            area, weight, modulus_psi, Decimal(velocity_kft_s) * 1000
        ),
        "source_rows": ";".join(str(value[0]) for value in values),
        "raw_labels": ";".join(text(value[1]) for value in values),
    }


def _velocity_residual(area, weight, modulus_psi, catalog_velocity):
    density = Decimal(str(weight)) / (Decimal(str(area)) * Decimal("12"))
    computed = (Decimal(modulus_psi) * Decimal("386.0886") / density).sqrt() / 12
    return format(abs(computed - catalog_velocity) / catalog_velocity, ".12f")


def _quarantine_row(source_row, record, reason):
    return {
        "source_row": source_row, "raw_label": text(record.get("Rod Grade")),
        "raw_area": decimal_text(record.get("Rod Area")),
        "raw_unit_weight": decimal_text(record.get("Unit Weight")),
        "raw_modulus": decimal_text(record.get("Modulus of Elast")),
        "raw_sonic_velocity": decimal_text(record.get("Spd of Sound")),
        "raw_tensile_strength": decimal_text(record.get("Tensil Strength")),
        "reason": reason,
    }


def connection_rows(path):
    records, dimensions = read_rows(path, "Rod ODs")
    rows = [{
        "source_row": source_row, "raw_rod_od": text(record["Rod OD"]),
        "rod_od_in": decimal_text(record["Rod OD"]),
        "raw_connection_size": text(record["Rod Connection Size"]),
        "connection_size_in": "",
        "disposition": "quarantined" if record["Rod Connection Size"] else "unmapped",
    } for source_row, record in records]
    return rows, dimensions


def connection_lookup_rows(path, couplings):
    records, dimensions = read_rows(
        path, "Look-up", first_row=3, header_row=2, stop_at_blank=True
    )
    rows = []
    for source_row, record in records:
        raw_rod, raw_coupling = record["Rod OD"], record["Coupling OD"]
        try:
            rod_od = _fraction_decimal(raw_rod)
            coupling_od = _fraction_decimal(raw_coupling)
            matched = any(
                row["rod_diameter_in"] == rod_od
                and row["coupling_diameter_in"] == coupling_od
                for row in couplings
            )
            disposition = "verified" if matched else "quarantined"
            reason = "" if matched else "no exact coupling catalog match"
        except (ArithmeticError, ValueError):
            rod_od, coupling_od = "", ""
            disposition, reason = "quarantined", "malformed mapping"
        rows.append({
            "source_row": source_row, "raw_rod_od": text(raw_rod),
            "raw_coupling_od": text(raw_coupling), "rod_od_in": rod_od,
            "coupling_od_in": coupling_od, "disposition": disposition,
            "reason": reason,
        })
    return rows, dimensions


def _fraction_decimal(value):
    raw = text(value).replace(" ", "")
    if not raw:
        raise ValueError("blank fraction")
    whole, fraction = ("0", raw)
    if "+" in raw:
        whole, fraction = raw.split("+", 1)
    if "/" in fraction:
        numerator, denominator = fraction.split("/", 1)
        number = Decimal(whole) + Decimal(numerator) / Decimal(denominator)
    else:
        number = Decimal(raw)
    if not number.is_finite() or number <= 0:
        raise ValueError(f"invalid positive fraction: {value!r}")
    return decimal_text(number)
