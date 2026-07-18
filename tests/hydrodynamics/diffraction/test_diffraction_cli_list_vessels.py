"""--list-vessels must resolve vessel configs relative to the installed
package (orchestrator's configs/vessels), not a stale CWD-relative src/ path.
Regression for digitalmodel #326."""

from __future__ import annotations

from types import SimpleNamespace

from digitalmodel.hydrodynamics.diffraction import diffraction_cli


def _args(**overrides):
    base = dict(list_vessels=True, vessel=None, config=None, verbose=False)
    base.update(overrides)
    return SimpleNamespace(**base)


def test_list_vessels_finds_configs_from_any_cwd(tmp_path, monkeypatch, capsys):
    # Run from an unrelated CWD: the old CWD-relative
    # src/digitalmodel/modules/... path can never exist here.
    monkeypatch.chdir(tmp_path)
    diffraction_cli.run_orcawave(_args())
    out = capsys.readouterr().out
    assert "No vessel configurations found" not in out
    assert "sea_cypress" in out  # shipped vessel config must be discoverable
