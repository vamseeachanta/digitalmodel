"""
ABOUTME: Vessel database — load/normalise curated vessel data (data/vessels/) for
diffraction (mass properties, RAO references) and installation-envelope analysis
(crane curves, particulars).

See data/vessels/README.md for the "flag, don't fake" provenance contract.
"""

from __future__ import annotations

from digitalmodel.marine_ops.vessel_db.gyradii import (
    GyradiiEstimate,
    estimate_gyradii,
)
from digitalmodel.marine_ops.vessel_db.loader import (
    ProvenanceViolation,
    Record,
    datasets,
    installation_vessels,
    iter_records,
    load_crane_curves,
    load_dataset,
    normalize_vessel_name,
    parse_value,
    validate_provenance,
    vessels_dir,
)
from digitalmodel.marine_ops.vessel_db.wed_adapter import (
    construction_crane_vessels,
    construction_vessels,
    drilling_rigs,
    fleet_summary,
    resolve_wed_curated_dir,
)

__all__ = [
    "GyradiiEstimate",
    "estimate_gyradii",
    "Record",
    "ProvenanceViolation",
    "datasets",
    "installation_vessels",
    "iter_records",
    "load_crane_curves",
    "load_dataset",
    "normalize_vessel_name",
    "parse_value",
    "validate_provenance",
    "vessels_dir",
    # worldenergydata adapter (source of truth for collection)
    "construction_crane_vessels",
    "construction_vessels",
    "drilling_rigs",
    "fleet_summary",
    "resolve_wed_curated_dir",
]
