"""Inventory screening integration and public-scope regression tests."""

import importlib.util
import shutil
import tomllib
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline
from digitalmodel.hydrodynamics.hull_library.curvature_screen import hullprod_available
from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    HullMeshGenerator,
    MeshGeneratorConfig,
)
from digitalmodel.hydrodynamics.hull_library.panel_catalog import PanelCatalog

ROOT = Path(__file__).resolve().parents[3]
COLUMNS = [
    "hull_id",
    "hull_type",
    "panels",
    "lref",
    "lref_mode",
    "I_D",
    "I_D_plus",
    "I_D_minus",
    "a_flat",
    "a_single",
    "a_elliptic",
    "a_saddle",
    "reliability",
    "crease_dominated",
]
needs_hullprod = pytest.mark.skipif(
    not hullprod_available(), reason="optional hullprod extra not installed"
)


def test_curvature_is_in_test_extra():
    data = tomllib.loads((ROOT / "pyproject.toml").read_text(encoding="utf-8"))
    extras = data["project"]["optional-dependencies"]
    assert "hullprod>=1.0.1,<2" in extras["test"]
    assert "hullprod>=1.0.1,<2" in extras["curvature"]


@pytest.fixture
def inventory():
    spec = importlib.util.spec_from_file_location(
        "screen_inventory", ROOT / "scripts/hull_library/screen_inventory.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def entry(hull_id, path, **metadata):
    return dict(
        hull_id=hull_id,
        hull_type="ship",
        name=hull_id,
        source="test",
        panel_format="gdf",
        file_path=path,
        **metadata,
    )


@pytest.fixture
def synthetic_catalog(tmp_path, ship_profile):
    shutil.copyfile(
        ROOT / "tests/hydrodynamics/bemrosetta/fixtures/sample_box.gdf",
        tmp_path / "box.gdf",
    )
    mesh = HullMeshGenerator().generate(
        ship_profile, MeshGeneratorConfig(target_panels=200)
    )
    MeshPipeline().convert_by_name(mesh, "gdf", tmp_path / "ship.gdf")
    catalog = tmp_path / "catalog.yaml"
    data = {
        "version": "1.0",
        "provenance": "preserve me",
        "entries": [
            entry("box", "box.gdf"),
            entry("ship", "ship.gdf", length_m=ship_profile.length_bp, Lpp=90),
        ],
    }
    catalog.write_text(yaml.safe_dump(data), encoding="utf-8")
    return catalog, tmp_path / "table.md"


@needs_hullprod
def test_catalog_gains_signatures_and_table_columns(inventory, synthetic_catalog):
    catalog, table = synthetic_catalog
    failures = inventory.screen_inventory(
        catalog.parent, catalog, table, controls=False
    )
    assert failures == {}
    loaded = PanelCatalog.from_yaml(catalog)
    box, ship = loaded.entries
    assert all(e.panel_count > 0 and e.vertex_count > 0 for e in loaded.entries)
    assert box.curvature_signature.lref_mode != "explicit_user"
    assert ship.curvature_signature.lref == 100
    assert ship.curvature_signature.lref_mode == "explicit_user"
    assert all(e.curvature_signature.hull_type == e.hull_type for e in loaded.entries)
    assert yaml.safe_load(catalog.read_text())["provenance"] == "preserve me"
    assert yaml.safe_load(catalog.read_text())["entries"][1]["Lpp"] == 90
    text = table.read_text(encoding="utf-8")
    assert "| " + " | ".join(COLUMNS) + " |" in text
    assert "Serani & Maki (2026)" in text and "2609.27544" in text
    assert "| box |" in text and "| ship |" in text
    before = catalog.read_bytes(), table.read_bytes()
    inventory.screen_inventory(catalog.parent, catalog, table, controls=False)
    assert before == (catalog.read_bytes(), table.read_bytes())


@needs_hullprod
def test_failure_rows_clear_stale_signature(inventory, synthetic_catalog):
    catalog, table = synthetic_catalog
    inventory.screen_inventory(catalog.parent, catalog, table, controls=False)
    (catalog.parent / "box.gdf").unlink()
    (catalog.parent / "ship.gdf").write_text("invalid mesh", encoding="utf-8")
    failures = inventory.screen_inventory(
        catalog.parent, catalog, table, controls=False
    )
    assert set(failures) == {"box", "ship"}
    assert "missing" in failures["box"].lower()
    assert "load" in failures["ship"].lower()
    assert all(
        e.curvature_signature is None for e in PanelCatalog.from_yaml(catalog).entries
    )
    assert "| box |" in table.read_text() and "| ship |" in table.read_text()


@needs_hullprod
def test_aliases_and_lpp_are_preserved(inventory, synthetic_catalog):
    catalog, table = synthetic_catalog
    data = yaml.safe_load(catalog.read_text())
    data["entries"].append(entry("alias", "ship.gdf", Lpp=75))
    catalog.write_text(yaml.safe_dump(data), encoding="utf-8")
    assert (
        inventory.screen_inventory(catalog.parent, catalog, table, controls=False) == {}
    )
    aliases = PanelCatalog.from_yaml(catalog).entries
    assert len(aliases) == 3
    assert aliases[-1].curvature_signature.lref == 75
    assert "| alias |" in table.read_text()


def test_discovery_retains_missing_and_duplicate_basenames(inventory, tmp_path):
    manifest = tmp_path / "scripts/consolidate_panels.py"
    manifest.parent.mkdir()
    manifest.write_text(
        "GDF_COPIES = [('missing.gdf', 'primitives', 'box.gdf')]\n",
        encoding="utf-8",
    )
    for folder in ("one", "two"):
        path = tmp_path / "docs/domains/orcawave/examples" / folder / "mesh.gdf"
        path.parent.mkdir(parents=True)
        path.touch()
    paths = inventory.discover_meshes(tmp_path)
    assert len(paths) == 3
    assert tmp_path / "missing.gdf" in paths
    assert len({inventory.mesh_id(p, tmp_path) for p in paths}) == 3


@pytest.mark.parametrize("path", ["../outside.gdf", "../../outside.stl"])
def test_external_paths_rejected_without_loading(
    inventory, tmp_path, path, monkeypatch
):
    catalog, table = tmp_path / "catalog.yaml", tmp_path / "table.md"
    catalog.write_text(yaml.safe_dump({"entries": [entry("outside", path)]}))
    monkeypatch.setattr(
        inventory.MeshPipeline, "load", lambda *a: pytest.fail("external read")
    )
    with pytest.raises(ValueError, match="repository"):
        inventory.screen_inventory(tmp_path, catalog, table, controls=False)
    assert not table.exists()


@needs_hullprod
def test_control_rows_match_analytical_baselines(inventory):
    controls = inventory.control_signatures()
    assert set(controls) == {"control_sphere", "control_cylinder", "control_wigley"}
    assert controls["control_sphere"].I_D == pytest.approx(4, rel=1e-3)
    assert controls["control_cylinder"].a_single == pytest.approx(1)
    assert controls["control_wigley"].I_D == pytest.approx(4.16, abs=0.1)


@needs_hullprod
def test_invalid_length_does_not_abort_inventory(inventory, synthetic_catalog):
    catalog, table = synthetic_catalog
    data = yaml.safe_load(catalog.read_text())
    data["entries"][0]["length_m"] = "invalid"
    catalog.write_text(yaml.safe_dump(data), encoding="utf-8")
    failures = inventory.screen_inventory(
        catalog.parent, catalog, table, controls=False
    )
    assert set(failures) == {"box"}
    assert "invalid reference length" in failures["box"]
    saved = yaml.safe_load(catalog.read_text())
    assert "curvature_signature" not in saved["entries"][0]
    assert saved["entries"][1]["curvature_signature"]["lref"] == 100


@needs_hullprod
@pytest.mark.parametrize("fmt", ["dat", "stl"])
def test_catalog_mesh_formats(inventory, synthetic_catalog, fmt):
    catalog, table = synthetic_catalog
    mesh = MeshPipeline().load(catalog.parent / "ship.gdf")
    MeshPipeline().convert_by_name(mesh, fmt, catalog.parent / f"ship.{fmt}")
    data = yaml.safe_load(catalog.read_text())
    data["entries"][1]["file_path"] = f"ship.{fmt}"
    # Existing PanelFormat does not include STL; loading follows the file suffix.
    catalog.write_text(yaml.safe_dump(data), encoding="utf-8")
    assert (
        inventory.screen_inventory(catalog.parent, catalog, table, controls=False) == {}
    )
    assert PanelCatalog.from_yaml(catalog).entries[1].curvature_signature.lref == 100
