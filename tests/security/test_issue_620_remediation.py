# ABOUTME: Security regression tests for issue #620 remediation
# Covers: yaml.safe_load rejects code-exec tags; SQL queries are parameterized
# (no string interpolation of values into the query text).

import re
from pathlib import Path

import pytest

yaml = pytest.importorskip("yaml")


SRC = Path(__file__).resolve().parents[2] / "src" / "digitalmodel"


def test_safe_load_rejects_python_object_tag():
    """yaml.safe_load must refuse the !!python/object code-execution tag.

    This is the property that yaml.load(Loader=yaml.Loader) FAILED to provide
    and is why every config/data load was switched to safe_load in #620.
    """
    malicious = "!!python/object/apply:os.system ['echo pwned']"
    with pytest.raises(yaml.constructor.ConstructorError):
        yaml.safe_load(malicious)


def test_safe_load_still_parses_plain_config():
    """Behavior-preserving: ordinary config YAML still loads as a dict."""
    data = yaml.safe_load("a: 1\nb:\n  - x\n  - y\n")
    assert data == {"a": 1, "b": ["x", "y"]}


def test_passing_ship_safeloader_supports_custom_tags():
    """The passing_ship parser must keep !var/!eval working under SafeLoader."""
    cfg_mod = pytest.importorskip(
        "digitalmodel.hydrodynamics.passing_ship.configuration"
    )
    parser = cfg_mod.YAMLConfigParser()
    # !!python tags must still be rejected even with custom constructors loaded.
    with pytest.raises(yaml.constructor.ConstructorError):
        yaml.load(
            "!!python/object/apply:os.system ['x']",
            Loader=yaml.SafeLoader,
        )
    # !eval (safe arithmetic) must still evaluate.
    parser._extract_variables("variables:\n  base: 10\n")
    out = yaml.load("value: !eval '2 + 3'\n", Loader=yaml.SafeLoader)
    assert out["value"] == 5


def test_application_manager_sql_is_parameterized():
    """The remediated SQL must use named bind params, not .format() values.

    Guards against re-introducing f-string/.format() interpolation into the
    ApplicationManager queries flagged in #620.
    """
    text = (SRC / "asset_integrity" / "common" / "ApplicationManager.py").read_text()
    # No SQL keyword line should be built with .format() value interpolation.
    for line in text.splitlines():
        stripped = line.strip()
        if stripped.startswith("#"):
            continue
        if re.search(r"(SELECT|UPDATE|INSERT|WHERE).*\{[0-9]?\}.*\.format\(", stripped):
            pytest.fail(f"Unparameterized SQL .format() found: {stripped}")
    # The remediated queries use named bind params.
    assert ":application_name" in text
    assert ":application_id" in text
    assert ":run_name" in text


def test_database_helpers_accept_params():
    """get_df_from_query / executeNoDataQuery expose a params kwarg."""
    text = (SRC / "asset_integrity" / "common" / "database.py").read_text()
    assert "def get_df_from_query(self, query, params=None)" in text
    assert "def executeNoDataQuery(self, query, arg_array=[], params=None)" in text
