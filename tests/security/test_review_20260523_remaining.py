# ABOUTME: Security regression tests for the REMAINING deterministic findings
# from the 2026-05-23 digitalmodel review (those not already closed by #620/#618).
# Covers: vertical_riser dispatch no longer NameErrors; PostgreSQL connection
# URL is built with escaping (URL.create, not raw .format); IEX API key is not
# stamped into os.environ; tempfile.mktemp removed from blender CAD converters.

import re
from pathlib import Path

import pytest

SRC = Path(__file__).resolve().parents[2] / "src" / "digitalmodel"


def test_engine_exposes_vertical_riser_symbol():
    """[High] engine.py dispatched `vertical_riser(cfg_base)` with the import
    commented out -> guaranteed NameError for `basename: vertical_riser`.
    The import is restored; the symbol must be resolvable in the engine module.
    """
    engine = pytest.importorskip("digitalmodel.engine")
    assert callable(getattr(engine, "vertical_riser", None))


def test_engine_vertical_riser_import_not_commented():
    """Guard against re-commenting the vertical_riser import."""
    text = (SRC / "engine.py").read_text()
    assert "from digitalmodel.subsea.vertical_riser.vertical_riser import vertical_riser" in text
    assert "# from digitalmodel.subsea.vertical_riser.vertical_riser import" not in text


def test_engine_no_dead_cfg_is_none_after_attrdict():
    """[Low] dead `raise ValueError("cfg is None")` after cfg was assigned a
    non-None AttributeDict must be gone.
    """
    text = (SRC / "engine.py").read_text()
    assert 'raise ValueError("cfg is None")' not in text


def test_postgres_connection_uses_url_create_not_format():
    """[High] PG credentials were f-string/.format()'d into a URI, corrupting it
    when the password contains reserved chars. Must use SQLAlchemy URL.create.
    """
    text = (SRC / "asset_integrity" / "common" / "database.py").read_text()
    assert "URL.create(" in text
    # The old vulnerable pattern must be gone from non-comment code.
    code_lines = [ln for ln in text.splitlines() if not ln.strip().startswith("#")]
    assert "postgresql+psycopg2://{0}:{1}@" not in "\n".join(code_lines)


def test_postgres_url_create_escapes_special_chars():
    """Behavioral: URL.create must percent-escape a password with reserved chars
    so it cannot break out of / leak into adjacent URI fields.
    """
    URL = pytest.importorskip("sqlalchemy.engine").URL
    url = URL.create(
        "postgresql+psycopg2",
        username="user",
        password="p@ss:w/rd?#",
        host="h",
        port=5432,
        database="db",
    )
    rendered = url.render_as_string(hide_password=False)
    # Raw reserved chars must not appear in the credential segment.
    cred_segment = rendered.split("@", 1)[0]
    for ch in ("@", ":w", "/rd", "?", "#"):
        assert ch not in cred_segment.replace("postgresql+psycopg2://", "")
    # And the password round-trips correctly.
    assert url.password == "p@ss:w/rd?#"


@pytest.mark.parametrize(
    "relpath",
    [
        "infrastructure/utils/finance_components.py",
        "specialized/finance/stock_analysis/finance_components_get_data.py",
    ],
)
def test_iex_key_not_written_to_environ(relpath):
    """[High] secret must not be stamped into the global process environment."""
    text = (SRC / relpath).read_text()
    assert 'os.environ["IEX_API_KEY"] =' not in text
    assert 'os.environ.get("IEX_API_KEY")' in text
    assert "api_key=iex_api_key" in text


@pytest.mark.parametrize(
    "relpath",
    [
        "solvers/blender_automation/converters/cad_importer.py",
        "solvers/blender_automation/utils/batch_processor.py",
    ],
)
def test_blender_temp_files_no_mktemp(relpath):
    """[High] deprecated, race-prone tempfile.mktemp (TOCTOU) must be replaced
    with NamedTemporaryFile and cleaned up in finally.
    """
    text = (SRC / relpath).read_text()
    # Ignore comment lines (they reference mktemp to explain the fix).
    code = "\n".join(
        ln for ln in text.splitlines() if not ln.strip().startswith("#")
    )
    assert "tempfile.mktemp(" not in code
    assert "NamedTemporaryFile(" in code
    assert "finally:" in code


def test_orcaflexanalysis_no_empty_try_pass():
    """[Low] nonsensical `try: pass except Exception:` must be gone."""
    text = (SRC / "solvers" / "orcaflex" / "OrcaFlexAnalysis.py").read_text()
    # Collapse the file and look for the empty try body pattern.
    assert re.search(r"try:\s*\n\s*pass\s*\n\s*except", text) is None
