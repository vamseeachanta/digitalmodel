"""Self-contained schema inputs and optional companion-library discovery."""

import os
from pathlib import Path

import pytest


@pytest.fixture
def source_fields():
    return {
        "vendor": "Helix Energy Solutions Group",
        "vendor_slug": "helix-esg",
        "document_name": "15k Intervention Riser System LTR brochure",
        "revision_date": "2023-11-17",
        "origin_url": "urn:example:provider-brochure",
        "private_mount_path": "private-brochure-reference",
        "inventory_ref": "provider-inventory-reference",
        "retrieved_date": "2026-05-14",
    }


@pytest.fixture
def parameters_payload(source_fields):
    """Synthetic schema input; no brochure or private storage is accessed."""
    return {
        "source": source_fields,
        "system": {"name": "15k Intervention Riser System", "slug": "irs-15k"},
        "specifications": {
            "mwd_m": {"value": 3048.0, "unit": "m"},
            "mwp_psi": {"value": 15000.0, "unit": "psi"},
            "bore_production_id_m": {"value": 0.16193, "unit": "m"},
            "design_temp_min_degF": {"value": 35.0, "unit": "degF"},
            "design_temp_max_degF": {"value": 300.0, "unit": "degF"},
            "subsea_package_weight_kg": {"value": 109770.0, "unit": "kg"},
            "control_system": {
                "value": "MUX with integrated redundancy via HIPPS",
                "unit": "categorical",
            },
            "bore_variants": [
                {"value": 6.375, "unit": "in", "note": "Production bore"},
                {"value": 2.0625, "unit": "in", "note": "Annulus bore"},
            ],
        },
        "orcaflex_mapping": {"mwd_m": "Environment.WaterDepth"},
    }


@pytest.fixture
def workspace_hub_root() -> Path:
    configured_root = os.environ.get("WORKSPACE_HUB_ROOT")
    if configured_root:
        return Path(configured_root)
    return Path(__file__).resolve().parents[3]


@pytest.fixture
def irs_15k_yaml_path(workspace_hub_root: Path) -> Path:
    return workspace_hub_root / "docs/provider-data/helix-esg/irs-15k/parameters.yaml"


@pytest.fixture
def irs_15k_record(irs_15k_yaml_path):
    """Require the pilot when the second stage explicitly configures its library."""
    from digitalmodel.provider_data import load_provider_yaml

    if not irs_15k_yaml_path.exists():
        if os.environ.get("WORKSPACE_HUB_ROOT"):
            pytest.fail("WORKSPACE_HUB_ROOT is set but companion IRS YAML is missing")
        pytest.skip("Companion provider library has not supplied the IRS YAML yet")
    return load_provider_yaml(irs_15k_yaml_path)
