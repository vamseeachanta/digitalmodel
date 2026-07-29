"""Frozen source paths and allowlisted schemas for catalog extraction."""

from pathlib import Path
import re


CATALOG_BOOK = Path("data/2018/Sucker Rod Pump Database 02_23_18.xlsx")
ROD_DETAIL_BOOK = Path("REF/Rod Detail Table.xlsx")
COUPLING_BOOK = Path("REF/Rod Coupling/nexus_catalog_couplings.xlsx")
RODPUMP_BOOK = Path("data/Rodpump Pumping Unit (1).xlsx")
CONNECTION_BOOK = Path("data/2018/UniqueRodODData.xlsx")
TUBING_BOOK = Path("REF/Tubing Stretch Table.xls")
PROHIBITED_PATTERNS = (
    re.compile(
        r"(?<!\d)(?:\d{14}|\d{3}-\d{2}-\d{6}-\d{2}(?:-\d{2})?)(?!\d)"
    ),
    re.compile(r"\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}\b", re.IGNORECASE),
    re.compile(r"\b\d+(?:\.\d+)?\s*(?:bopd|bwpd|mcfd|mcf/d|bbl/d)\b", re.I),
    re.compile(r"\b(?:[a-z0-9-]+\.)+(?:internal|local|corp)\b", re.I),
    re.compile(r"(?:/mnt/|[A-Z]:\\)", re.I),  # abs-path-allowed
    re.compile(r"\b(?:well|lease|cost[\s_-]*cent(?:er|re))[\s_-]*(?:name|id)\b", re.I),
)
SURFACE_FIELDS = [
    "source_catalog", "source_identifier", "source_row", "manufacturer_key",
    "model_key", "geometry_code", "gearbox_rating_raw", "beam_rating_raw",
    "max_stroke_length_raw", "dimensional_a_raw", "dimensional_c_raw",
    "dimensional_i_raw", "dimensional_k_raw", "dimensional_p_raw",
    *[f"stroke_length_pin_{number}_raw" for number in range(1, 9)],
    *[f"radius_pin_{number}_raw" for number in range(1, 9)],
    "structural_imbalance_raw", "phase_angle_raw",
    "counterbalance_effect_raw", "air_balance_raw",
    "air_balance_dimensional_d_raw", "air_balance_dimensional_f_raw",
    "air_balance_dimensional_h_raw",
]
SURFACE_MAP = {
    "manufacturer_key": "P.Unit Manf.",
    "model_key": "Pumping Unit Description / Information",
    "geometry_code": "P.Unit Geom.",
    "gearbox_rating_raw": "Gear Box Rating",
    "beam_rating_raw": "Beam Rating",
    "max_stroke_length_raw": "Max. S.Len.",
    "dimensional_a_raw": 'Dimen. "A"',
    "dimensional_c_raw": 'Dimen. "C"',
    "dimensional_i_raw": 'Dimen. "I"',
    "dimensional_k_raw": 'Dimen. "K"',
    "dimensional_p_raw": 'Dimen. "P"',
    **{f"stroke_length_pin_{n}_raw": f"S.Len. Pin {n}" for n in range(1, 9)},
    **{f"radius_pin_{n}_raw": f"Radius Pin {n}" for n in range(1, 9)},
    "structural_imbalance_raw": "Struct. Unbal.",
    "phase_angle_raw": "Phase Angle",
    "counterbalance_effect_raw": "CBE Tbl.?",
    "air_balance_raw": "AirBal. CylDia.",
    "air_balance_dimensional_d_raw": 'AB Dim. "D"',
    "air_balance_dimensional_f_raw": 'AB Dim. "F"',
    "air_balance_dimensional_h_raw": 'AB Dim. "H"',
}
RODPUMP_MAP = {
    "source_identifier": "Id",
    "manufacturer_key": "PumpingUnitManufacturer",
    "model_key": "Description",
    "geometry_code": "PumpingUnitGeometry",
    "gearbox_rating_raw": "GearBoxRating",
    "beam_rating_raw": "BeamRating",
    "max_stroke_length_raw": "MaxStrokeLength",
    "dimensional_a_raw": "DimensionalA",
    "dimensional_c_raw": "DimensionalC",
    "dimensional_i_raw": "DimensionalI",
    "dimensional_k_raw": "DimensionalK",
    "dimensional_p_raw": "DimensionalP",
    **{f"stroke_length_pin_{n}_raw": f"SLPin{n}" for n in range(1, 9)},
    **{f"radius_pin_{n}_raw": f"RPin{n}" for n in range(1, 9)},
    "structural_imbalance_raw": "StructuralImbalance",
    "phase_angle_raw": "PhaseAngle",
    "counterbalance_effect_raw": "CounterBalanceEffect",
    "air_balance_raw": "AirBalance",
    "air_balance_dimensional_d_raw": "ABDimensionalD",
    "air_balance_dimensional_f_raw": "ABDimensionalF",
    "air_balance_dimensional_h_raw": "ABDimensionalH",
}
OUTPUT_DEFINITIONS = {
    "rod_details.csv": [
        "grade", "diameter_in", "area_in2", "unit_weight_lbf_ft",
        "modulus_psi", "catalog_sonic_velocity_ft_s",
        "weight_derived_velocity_ft_s", "tensile_strength_psi",
        "raw_sonic_velocity_kft_s", "source_rows", "raw_labels",
    ],
    "rod_details_quarantine.csv": [
        "source_row", "raw_label", "raw_area", "raw_unit_weight",
        "raw_modulus", "raw_sonic_velocity", "raw_tensile_strength", "reason",
    ],
    "rods_catalog.csv": [
        "source_row", "catalog_id", "description", "tensile_strength_psi",
        "area_in2", "modulus_mpsi", "velocity_kft_s",
        "unit_weight_lbf_ft", "elastorq_raw",
    ],
    "rod_guides.csv": ["source_row", "manufacturer", "model_type", "material"],
    "couplings.csv": [
        "source_row", "rod_diameter_in", "manufacturer", "size",
        "coupling_type", "coupling_diameter_in", "coupling_length_in",
        "tensile_strength_psi", "friction_coefficient",
    ],
    "rod_connections.csv": [
        "source_row", "raw_rod_od", "rod_od_in", "raw_connection_size",
        "connection_size_in", "disposition",
    ],
    "rod_connection_lookup.csv": [
        "source_row", "raw_rod_od", "raw_coupling_od", "rod_od_in",
        "coupling_od_in", "disposition", "reason",
    ],
    "surface_unit_catalog.csv": SURFACE_FIELDS,
    "rodpump_units.csv": SURFACE_FIELDS,
}


def unit_for_field(filename, field):
    overrides = {
        "source_row": "worksheet_row",
        "modulus_psi": "psi",
        "raw_modulus": "million_psi",
        "raw_sonic_velocity": "thousand_ft_per_s",
        "raw_tensile_strength": "psi",
        "modulus_mpsi": "million_psi",
        "velocity_kft_s": "thousand_ft_per_s",
        "catalog_sonic_velocity_ft_s": "ft/s",
        "weight_derived_velocity_ft_s": "ft/s",
        "unit_weight_lbf_ft": "lbf/ft",
        "raw_unit_weight": "lbf/ft",
        "friction_coefficient": "dimensionless",
    }
    if field in overrides:
        return overrides[field]
    if filename in {"surface_unit_catalog.csv", "rodpump_units.csv"}:
        return "text" if field in {
            "source_catalog", "manufacturer_key", "model_key", "geometry_code"
        } else "unverified_source_unit"
    if field.endswith("_in2") or field == "raw_area":
        return "in^2"
    if field.endswith("_in"):
        return "in"
    if field.endswith("_psi"):
        return "psi"
    return "text_or_identifier"
