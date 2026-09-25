"""Owner decisions C13/C16: the s7 de-identification map is private.

``scripts/sanitize_s7_models.py`` used to carry its real-to-neutral name map
in the public source, which is the de-identification key itself. The map now
lives in a private JSON file read at run time from ``DIGITALMODEL_S7_SANITIZE_MAP``
or ``~/.config/digitalmodel/s7-sanitize-map.json``. Without it the script must
stop, never run with an empty map (that would publish unsanitised models).

Every map in these tests is synthetic.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts" / "sanitize_s7_models.py"
ENV = "DIGITALMODEL_S7_SANITIZE_MAP"

SYNTHETIC = {
    "default_s7_root": "",
    "sanitization_map": {
        "Acmefield": "deepwater_field_x",
        "Acmefield North": "deepwater_field_y",
        "HOSTX99": "",
    },
    "category_map": {
        "acme/jumpers": "jumper/generic",
        "acme/jumpers/special": "jumper/special",
        "general": "reference",
    },
    "exclusions": ["acme/private", "general/cad"],
}


def _load():
    spec = importlib.util.spec_from_file_location("_s7_sanitize_under_test", SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


@pytest.fixture
def no_home_map(monkeypatch, tmp_path):
    """No env var and an empty home, so no private map can be found."""
    monkeypatch.delenv(ENV, raising=False)
    home = tmp_path / "home"
    home.mkdir()
    monkeypatch.setenv("HOME", str(home))
    monkeypatch.setenv("USERPROFILE", str(home))
    return home


@pytest.fixture
def synthetic_map(monkeypatch, tmp_path):
    path = tmp_path / "s7-map.json"
    path.write_text(json.dumps(SYNTHETIC), encoding="utf-8")
    monkeypatch.setenv(ENV, str(path))
    return path


# -- the public script carries no names ---------------------------------------


def test_the_script_defines_no_name_tables():
    mod = _load()
    for attr in ("SANITIZATION_MAP", "CATEGORY_MAP", "EXCLUSIONS", "DEFAULT_S7_ROOT"):
        assert not hasattr(mod, attr), f"{attr} must come from the private map"


def test_the_script_source_carries_no_private_map_entry():
    """Checked against the real private map where it is installed."""
    candidates = [os.environ.get(ENV)] if os.environ.get(ENV) else []
    candidates.append(
        str(Path.home() / ".config" / "digitalmodel" / "s7-sanitize-map.json")
    )
    real = next((Path(p) for p in candidates if p and Path(p).is_file()), None)
    if real is None:
        pytest.skip("private s7 map not installed on this machine")
    data = json.loads(real.read_text(encoding="utf-8"))
    source = SCRIPT.read_text(encoding="utf-8").lower()
    # The names themselves, and the top-level source folder of every category
    # and exclusion (a project folder); deeper folder words are generic.
    tokens = set(data.get("sanitization_map", {}))
    for key in list(data.get("category_map", {})) + list(data.get("exclusions", [])):
        tokens.add(key.split("/")[0])
    if data.get("default_s7_root"):
        tokens.add(data["default_s7_root"])
    leaked = sorted(t for t in tokens if len(t) >= 4 and t.lower() in source)
    assert not leaked, f"{len(leaked)} private map entries appear in the public script"


# -- fail closed ---------------------------------------------------------------


def test_a_missing_configured_map_is_an_error(monkeypatch, tmp_path):
    mod = _load()
    monkeypatch.setenv(ENV, str(tmp_path / "absent.json"))
    with pytest.raises(mod.SanitizeMapError, match=ENV):
        mod.load_sanitize_config()


def test_no_map_anywhere_is_an_error(no_home_map):
    mod = _load()
    with pytest.raises(mod.SanitizeMapError):
        mod.load_sanitize_config()


def test_the_home_config_map_is_used_when_the_variable_is_unset(no_home_map):
    mod = _load()
    cfg_dir = no_home_map / ".config" / "digitalmodel"
    cfg_dir.mkdir(parents=True)
    (cfg_dir / "s7-sanitize-map.json").write_text(
        json.dumps(SYNTHETIC), encoding="utf-8"
    )
    cfg = mod.load_sanitize_config()
    assert cfg.sanitization_map == SYNTHETIC["sanitization_map"]


@pytest.mark.parametrize(
    "broken",
    [
        {},
        {"sanitization_map": {}},
        {"sanitization_map": {"Acmefield": 3}},
        {"sanitization_map": {"Acmefield": "x"}, "category_map": []},
        {"sanitization_map": {"Acmefield": "x"}, "exclusions": "acme"},
    ],
)
def test_a_malformed_map_is_an_error(monkeypatch, tmp_path, broken):
    mod = _load()
    path = tmp_path / "bad.json"
    path.write_text(json.dumps(broken), encoding="utf-8")
    monkeypatch.setenv(ENV, str(path))
    with pytest.raises(mod.SanitizeMapError):
        mod.load_sanitize_config()


def test_run_without_a_map_stops_before_touching_anything(no_home_map, tmp_path):
    mod = _load()
    src = tmp_path / "s7"
    src.mkdir()
    (src / "model.yml").write_text("General:\n  x: 1\n", encoding="utf-8")
    out = tmp_path / "out"
    args = argparse.Namespace(
        s7_root=str(src),
        output_root=str(out),
        dry_run=False,
        skip_dat=True,
        verbose=False,
        map=None,
    )
    assert mod.run(args) != 0
    assert not out.exists()


# -- behaviour with a synthetic map -------------------------------------------


def test_sanitize_text_applies_the_map_longest_first(synthetic_map):
    mod = _load()
    cfg = mod.load_sanitize_config()
    text = "User: someone\nAcmefield North riser near Acmefield on HOSTX99\n"
    out, transforms = mod.sanitize_text(text, cfg)
    assert out == "deepwater_field_y riser near deepwater_field_x on \n"
    assert any("metadata header" in t for t in transforms)


def test_categories_and_exclusions_come_from_the_map(synthetic_map):
    mod = _load()
    cfg = mod.load_sanitize_config()
    assert mod.resolve_category("acme/jumpers/special/a", cfg) == "jumper/special"
    assert mod.resolve_category("acme/jumpers/b", cfg) == "jumper/generic"
    assert mod.resolve_category("elsewhere", cfg) is None
    assert mod.is_excluded("acme/private/x.yml", cfg)
    assert not mod.is_excluded("acme/jumpers/x.yml", cfg)


def test_target_file_names_are_sanitised(synthetic_map, tmp_path):
    mod = _load()
    cfg = mod.load_sanitize_config()
    target = mod.target_yml_path(tmp_path, "jumper/generic", "Acmefield case.dat", cfg)
    assert (
        target
        == tmp_path / "jumper/generic" / "monolithic" / "deepwater_field_x case.yml"
    )


def test_an_explicit_map_argument_wins(monkeypatch, tmp_path, no_home_map):
    mod = _load()
    path = tmp_path / "explicit.json"
    path.write_text(json.dumps(SYNTHETIC), encoding="utf-8")
    cfg = mod.load_sanitize_config(path)
    assert "Acmefield" in cfg.sanitization_map


def test_a_dry_run_with_a_synthetic_map_writes_nothing(synthetic_map, tmp_path):
    mod = _load()
    src = tmp_path / "s7" / "acme" / "jumpers"
    src.mkdir(parents=True)
    (src / "Acmefield.yml").write_text("General:\n  Acmefield: 1\n", encoding="utf-8")
    out = tmp_path / "out"
    args = argparse.Namespace(
        s7_root=str(tmp_path / "s7"),
        output_root=str(out),
        dry_run=True,
        skip_dat=True,
        verbose=False,
        map=None,
    )
    assert mod.run(args) == 0
    assert not out.exists()
