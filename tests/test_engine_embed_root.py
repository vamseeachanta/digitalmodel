"""Embed-root isolation + backward-compat tests for digitalmodel's engine.

Ports the assetutilities #3297 embed contract onto digitalmodel's OWN engine
(workspace-hub#3307). Verifies:

- ``engine(cfg=..., embed=True, root_folder=<dir>)`` writes ONLY under <dir>
  (results + cfg-dump + the wall-thickness quickcheck html/json/csv), nothing at
  cwd or the input dir.
- ``_config_dir_path`` is rebased to the injected root and NOT clobbered by the
  default-path re-copy block.
- ``log_to_file`` default mirrors #3297 (=True): an embed call without the flag
  writes a .log; a caller passing log_to_file=False writes none.
- Re-entrancy: repeated embed calls with different roots stay isolated.
- Backward compatible: the default/file path is unchanged (outputs under the
  input dir, _config_dir_path == input dir) and the new params are additive.

The wall_thickness basename is used because it is the canonical _config_dir_path
consumer routable through engine() today. The committed quickcheck cache fixture
drives a hermetic, offline run.
"""

from __future__ import annotations

import inspect
import shutil
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "src"))

from digitalmodel.engine import engine  # noqa: E402

CACHE_FIXTURE = (
    REPO_ROOT
    / "examples"
    / "structural"
    / "wall_thickness_quickcheck"
    / "data"
    / "quickcheck_cache.json"
)
EXAMPLE_WORKFLOW = REPO_ROOT / "examples" / "workflows" / "wall-thickness-quickcheck"


def _embed_cfg(cache_path: Path | None = None) -> dict:
    """A minimal in-memory wall_thickness cfg for an embed run.

    The cache is an ABSOLUTE path to the committed fixture (resolves regardless of
    the rebased config dir); the output paths are RELATIVE so they land under the
    injected root once configure_embed rebases _config_dir_path.
    """
    return {
        "basename": "wall_thickness",
        "default": {"log_level": "INFO"},
        "wall_thickness": {
            "quickcheck": {
                "cache": str(cache_path or CACHE_FIXTURE),
                "output_html": "results/wt_quickcheck.html",
                "output_json": "results/wt_quickcheck.json",
                "output_csv": "results/wt_quickcheck.csv",
            }
        },
    }


# --------------------------------------------------------------------------- #
# Guard / signature
# --------------------------------------------------------------------------- #


def test_embed_requires_cfg_and_root():
    """embed=True without root_folder fails fast (ValueError)."""
    with pytest.raises(ValueError):
        engine(cfg=_embed_cfg(), embed=True)


def test_engine_signature_additive():
    """New params are additive; log_to_file default is True (mirrors #3297)."""
    params = inspect.signature(engine).parameters
    assert params["root_folder"].default is None
    assert params["log_to_file"].default is True  # NOT False
    assert params["embed"].default is False


# --------------------------------------------------------------------------- #
# Embed isolation
# --------------------------------------------------------------------------- #


def test_embed_rebases_config_dir_path(tmp_path, monkeypatch):
    """(crux) embed sets cfg_base['_config_dir_path'] == root, not cwd/input dir."""
    root = tmp_path / "root"
    root.mkdir()
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    monkeypatch.chdir(scratch)

    cfg_base = engine(cfg=_embed_cfg(), embed=True, root_folder=str(root))

    assert cfg_base["_config_dir_path"] == str(root)


def test_embed_quickcheck_writes_under_root(tmp_path, monkeypatch):
    """The wall-thickness router writes html/json/csv under root; nothing at cwd."""
    root = tmp_path / "root"
    root.mkdir()
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    monkeypatch.chdir(scratch)
    before = set(scratch.iterdir())

    cfg_base = engine(
        cfg=_embed_cfg(), embed=True, root_folder=str(root), log_to_file=False
    )

    qc = cfg_base["wall_thickness"]["quickcheck"]
    for key in ("report_html", "result_json", "result_csv"):
        out = Path(qc[key])
        assert out.exists(), f"{key} not written: {out}"
        assert str(out).startswith(str(root)), f"{key} escaped root: {out}"

    # The scratch cwd gained nothing.
    assert set(scratch.iterdir()) == before


def test_embed_writes_only_under_root(tmp_path, monkeypatch):
    """results dir + cfg-dump land under root; cwd gains nothing."""
    root = tmp_path / "root"
    root.mkdir()
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    monkeypatch.chdir(scratch)
    before = set(scratch.iterdir())

    engine(cfg=_embed_cfg(), embed=True, root_folder=str(root), log_to_file=False)

    assert (root / "results").is_dir()
    # save_cfg dumps the cfg under <root>/results
    assert any((root / "results").iterdir())
    assert set(scratch.iterdir()) == before


def test_embed_no_logfile_no_logs_dir(tmp_path, monkeypatch):
    """Caller passing log_to_file=False writes no .log and creates no logs/."""
    root = tmp_path / "root"
    root.mkdir()
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    monkeypatch.chdir(scratch)

    engine(cfg=_embed_cfg(), embed=True, root_folder=str(root), log_to_file=False)

    assert not (root / "logs").exists()
    assert not list(tmp_path.rglob("*.log"))


def test_embed_default_log_to_file_true_writes_log(tmp_path, monkeypatch):
    """(mirror-guard) embed WITHOUT explicit log_to_file inherits True -> writes .log."""
    root = tmp_path / "root"
    root.mkdir()
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    monkeypatch.chdir(scratch)

    engine(cfg=_embed_cfg(), embed=True, root_folder=str(root))

    logs = list((root / "logs").glob("*.log"))
    assert logs, "expected a .log under <root>/logs when log_to_file defaults True"


def test_embed_repeated_calls_reentrant(tmp_path, monkeypatch):
    """(re-entrancy) two sequential embed calls with different roots stay isolated."""
    root_a = tmp_path / "root_a"
    root_a.mkdir()
    root_b = tmp_path / "root_b"
    root_b.mkdir()
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    monkeypatch.chdir(scratch)

    cfg_a = engine(
        cfg=_embed_cfg(), embed=True, root_folder=str(root_a), log_to_file=False
    )
    cfg_b = engine(
        cfg=_embed_cfg(), embed=True, root_folder=str(root_b), log_to_file=False
    )

    assert cfg_a["_config_dir_path"] == str(root_a)
    assert cfg_b["_config_dir_path"] == str(root_b)
    # Each root holds only its own quickcheck outputs.
    json_a = Path(cfg_a["wall_thickness"]["quickcheck"]["result_json"])
    json_b = Path(cfg_b["wall_thickness"]["quickcheck"]["result_json"])
    assert json_a.parent.parent == root_a and json_a.exists()
    assert json_b.parent.parent == root_b and json_b.exists()
    # call-1 root never received call-2 results and vice versa.
    assert not list(root_a.rglob("*wt_quickcheck*"))[0].is_relative_to(root_b)
    assert not list(root_b.rglob("*wt_quickcheck*"))[0].is_relative_to(root_a)


def test_embed_skips_config_dir_recopy(tmp_path, monkeypatch):
    """A stale _config_dir_path on the input cfg does NOT survive into cfg_base."""
    root = tmp_path / "root"
    root.mkdir()
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    monkeypatch.chdir(scratch)

    cfg = _embed_cfg()
    cfg["_config_dir_path"] = "/stale/dir"

    cfg_base = engine(
        cfg=cfg, embed=True, root_folder=str(root), log_to_file=False
    )

    assert cfg_base["_config_dir_path"] == str(root)
    assert cfg_base["_config_dir_path"] != "/stale/dir"


# --------------------------------------------------------------------------- #
# Backward compatibility (default/file path unchanged)
# --------------------------------------------------------------------------- #


def test_default_path_unchanged_golden(tmp_path, monkeypatch):
    """Default/file path: outputs under the input-file dir; _config_dir_path == input dir."""
    work = tmp_path / "wt-quickcheck"
    shutil.copytree(EXAMPLE_WORKFLOW, work)
    # Start from a stale results dir cleared so we can assert fresh writes.
    results = work / "results"
    if results.exists():
        shutil.rmtree(results)
    monkeypatch.chdir(tmp_path)

    input_yml = work / "input.yml"
    cfg_base = engine(inputfile=str(input_yml))

    # Today's contract: _config_dir_path is the input-file dir (NOT a root override).
    assert cfg_base["_config_dir_path"] == str(work)
    qc = cfg_base["wall_thickness"]["quickcheck"]
    # Outputs land under the input-file dir, exactly as before the embed port.
    for key in ("report_html", "result_json", "result_csv"):
        out = Path(qc[key])
        assert out.exists()
        assert str(out).startswith(str(work)), f"{key} escaped input dir: {out}"
