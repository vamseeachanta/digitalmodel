# ABOUTME: Registry result: descriptors parse + ResultLocator + fail-closed + embed isolation (#3285).
"""Cross-cutting workflow-API tests: descriptor parsing, fail-closed, embed isolation."""

from __future__ import annotations

import os
from pathlib import Path

from assetutilities.workflow_api import ResultLocator

from digitalmodel.workflow_api import load_registry, run_workflow
from digitalmodel.workflow_api.runner import resolve_registry_row

NEW_ROWS = {"ffs-metal-loss", "buckling-parametric", "mooring-design-mbl"}


def test_result_descriptors_parse_and_match_locator():
    rows = load_registry()["workflows"]
    by_id = {row["id"]: row for row in rows}
    # every row with a result: descriptor parses into a valid ResultLocator
    for row in rows:
        if "result" in row:
            locator = ResultLocator.from_row(row)
            assert locator.kind in {"files", "in_memory"}
            if locator.kind == "in_memory":
                assert locator.key, f"{row['id']} in_memory missing key"
    # the four #3285 descriptors specifically
    assert ResultLocator.from_row(by_id["ffs-metal-loss"]).kind == "in_memory"
    assert ResultLocator.from_row(by_id["ffs-metal-loss"]).key == "ffs"
    assert ResultLocator.from_row(by_id["buckling-parametric"]).kind == "files"
    assert ResultLocator.from_row(by_id["mooring-design-mbl"]).key == "mooring_mbl"
    assert ResultLocator.from_row(by_id["wall-thickness-quickcheck"]).kind == "files"


def test_descriptor_absence_still_valid_superset():
    # a row WITHOUT a result: descriptor still yields a valid (default files) locator
    rows = load_registry()["workflows"]
    no_descriptor = next(r for r in rows if "result" not in r)
    assert ResultLocator.from_row(no_descriptor).kind == "files"


def test_new_rows_are_full_durable_rows():
    # MAJOR-1: the new rows carry the full durable key-set (not descriptor-only),
    # so tests/workflows/test_durable_workflows.py parametrizes over them w/o KeyError.
    for wid in NEW_ROWS:
        row = resolve_registry_row(wid)
        for key in ("basename", "input", "outputs", "test", "runtime"):
            assert key in row, f"{wid} missing durable key {key}"


def test_run_workflow_unknown_id_error_envelope():
    env = run_workflow("does-not-exist")
    assert env.status == "error"
    assert env.result == {}
    assert env.determinism["result_hash"] is None
    assert env.warnings


def test_embed_run_writes_only_under_root(tmp_path, monkeypatch):
    # consume #3307: run_workflow leaves nothing outside its throwaway tempdir root
    # and does not change cwd (depends on #3307's _config_dir_path rebase).
    monkeypatch.chdir(tmp_path)
    before = set(os.listdir(tmp_path))
    env = run_workflow("ffs-metal-loss")
    assert env.status == "ok", env.warnings
    assert set(os.listdir(tmp_path)) == before  # no stray writes into cwd
    assert Path.cwd() == tmp_path
