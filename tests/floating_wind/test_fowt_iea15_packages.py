"""Load cases and licensed-run packages of the IEA 15 MW / VolturnUS-S example.

Split from test_fowt_static_checks.py. Package regeneration must never delete
files it did not write, and packaged hydrodynamic files must be the bytes
pinned for the public revision.
"""
from __future__ import annotations

import copy
import hashlib
import json
import os
import shutil
from pathlib import Path

import pytest

from digitalmodel.floating_wind.iea15_orcaflex_spec import WAVE_CASES, load_sources
from digitalmodel.floating_wind.run_package_folder import (
    MANIFEST_NAME,
    prepare_case_folder,
    record_case_folder,
)

ROOT = Path(__file__).resolve().parents[2]


@pytest.fixture(scope="module")
def sources():
    return load_sources()


# ------------------------------------------------- load cases and packages
EQ = {"surge": 0.387, "heave": -0.382, "pitch": -1.417}


def test_load_case_matrix(sources):
    from digitalmodel.floating_wind.iea15_orcaflex_spec import load_case_specs
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    basis, data = sources
    cases = load_case_specs(basis, data, EQ)
    assert list(cases) == ["LC0_statics", "LC1_surge_decay", "LC2_heave_decay",
                           "LC3_pitch_decay", "LC4a_rated_thrust_statics", "LC4_operating",
                           "LC5_parked_extreme"]
    for spec in cases.values():
        # Validate a copy: the Waves validator moves flat keys into 'trains' in place.
        ProjectInputSpec(**copy.deepcopy(spec))
    surge = cases["LC1_surge_decay"]["fowt"]["hull"]
    assert surge["included_in_statics"] == "None"
    assert surge["initial_position"] == pytest.approx([EQ["surge"] + 10.0, 0.0, EQ["heave"]])
    pitch = cases["LC3_pitch_decay"]["fowt"]["hull"]
    assert pitch["initial_orientation"][1] == pytest.approx(EQ["pitch"] + 5.0)
    heave = cases["LC2_heave_decay"]["fowt"]["hull"]
    assert heave["initial_position"][2] == pytest.approx(EQ["heave"] - 2.0)
    rated = cases["LC4a_rated_thrust_statics"]["fowt"]["rotor_thrust"]["thrust"]
    assert rated * 1000 == pytest.approx(basis["turbine"]["rated_rotor_thrust"]["value"])
    op = cases["LC4_operating"]
    assert op["environment"]["raw_properties"]["WaveTrains"][0]["WaveTp"] == pytest.approx(
        basis["environment"]["operating"]["peak_period"]["value"])
    assert op["environment"]["raw_properties"]["WaveTrains"][0]["WaveGamma"] == pytest.approx(
        basis["environment"]["operating"]["jonswap_gamma"]["value"])
    # The generated vessel type carries no load RAOs, so the generated model
    # keeps first-order wave loading off; run.md switches it on after the
    # licensed WAMIT import supplies the RAOs.
    assert WAVE_CASES == ("LC4_operating", "LC5_parked_extreme")
    for name in WAVE_CASES:
        assert cases[name]["fowt"]["hull"].get("include_wave_load_first_order", False) is False
    parked = cases["LC5_parked_extreme"]
    assert "rotor_thrust" not in parked["fowt"]
    assert parked["environment"]["waves"]["height"] == pytest.approx(10.7)


def test_sea_state_emits_peak_period_not_zero_crossing(sources, tmp_path):
    import yaml as _yaml

    from digitalmodel.floating_wind.iea15_orcaflex_spec import load_case_specs
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    spec = load_case_specs(*sources, EQ)["LC5_parked_extreme"]
    ModularModelGenerator.from_spec(ProjectInputSpec(**spec)).generate(tmp_path)
    env = _yaml.safe_load((tmp_path / "includes" / "03_environment.yml").read_text())
    train = env["Environment"]["WaveTrains"][0]
    assert train["WaveType"] == "JONSWAP"
    assert train["WaveTp"] == pytest.approx(14.2)
    assert "WaveTz" not in train
    assert train["WaveGamma"] == pytest.approx(2.75)
    assert train["WaveHs"] == pytest.approx(10.7)

def test_run_packages_written(tmp_path):
    import importlib.util

    script = ROOT / "scripts" / "fowt" / "write_iea15_run_packages.py"
    spec = importlib.util.spec_from_file_location("packages", script)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    assert module.main(["x", str(tmp_path)]) == 0
    index = json.loads((tmp_path / "index.json").read_text())
    assert len(index["cases"]) == 7
    for case in index["cases"]:
        folder = tmp_path / case["case"]
        assert (folder / "model" / "master.yml").exists()
        text = (folder / "run.md").read_text()
        assert "## Expected" in text or "## Limitations" in text
        assert "validated" in text  # the do-not-overclaim instruction is present


def test_wave_packages_include_wamit_import_output(tmp_path, monkeypatch):
    """The importer needs the first-order text output, with a selectable suffix.

    Placeholder bytes stand in for the public files here, so their digests are
    pinned for this test only; the real pins are tested separately below.
    """
    import importlib.util

    script = ROOT / "scripts" / "fowt" / "write_iea15_run_packages.py"
    spec = importlib.util.spec_from_file_location("packages", script)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    source = tmp_path / "source"
    source.mkdir()
    stem = Path(module.WAMIT[0]).stem
    output_name = f"{stem}_1stOrder.out.wamit"
    import_name = Path(output_name).with_suffix("").name
    files = {f"{stem}{suffix}": f"reference {suffix}\r\n".encode()
             for suffix in (".1", ".3", ".hst")}
    files[output_name] = b"first-order WAMIT output\r\n"
    for name, content in files.items():
        (source / name).write_bytes(content)
    monkeypatch.setattr(module, "WAMIT_SHA256", {
        name: hashlib.sha256(content).hexdigest() for name, content in files.items()})

    out = tmp_path / "packages"
    assert module.main(["x", str(out), str(source)]) == 0
    index = json.loads((out / "index.json").read_text())
    wave_cases = [case for case in index["cases"] if case["needs_wamit_import"]]
    assert len(wave_cases) == 2
    for case in index["cases"]:
        folder = out / case["case"]
        if not case["needs_wamit_import"]:
            assert not (folder / "hydro").exists()
            continue
        expected = {import_name if name == output_name else name: content
                    for name, content in files.items()}
        assert {p.name: p.read_bytes() for p in (folder / "hydro").iterdir()} == expected
        instructions = (folder / "run.md").read_text()
        assert f"`hydro/{import_name}`" in instructions
        assert "requested data" in instructions
        assert "centre of mass" in instructions
        assert "phase origins" in instructions
        assert "Wave load (1st order)" in instructions


def _packages_module():
    import importlib.util

    script = ROOT / "scripts" / "fowt" / "write_iea15_run_packages.py"
    spec = importlib.util.spec_from_file_location("packages", script)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_run_package_rerun_keeps_saved_results(tmp_path):
    """A rerun must never delete files the generator did not write (e.g. .sim results)."""
    module = _packages_module()
    assert module.main(["x", str(tmp_path)]) == 0
    first = (tmp_path / "LC0_statics" / "model" / "master.yml").read_bytes()
    # An untouched package regenerates in place.
    assert module.main(["x", str(tmp_path)]) == 0
    assert (tmp_path / "LC0_statics" / "model" / "master.yml").read_bytes() == first
    saved = tmp_path / "LC0_statics" / "model" / "LC0_statics.sim"
    saved.write_bytes(b"solver results")
    with pytest.raises(FileExistsError, match="LC0_statics.sim"):
        module.main(["x", str(tmp_path)])
    assert saved.read_bytes() == b"solver results"
    assert (tmp_path / "LC0_statics" / "model" / "master.yml").read_bytes() == first


def test_case_folder_refuses_unrecorded_or_edited_content(tmp_path):
    folder = tmp_path / "case"
    prepare_case_folder(folder)  # absent: created
    (folder / "sub").mkdir()
    (folder / "sub" / "a.yml").write_text("a", encoding="utf-8")
    record_case_folder(folder)
    assert (folder / MANIFEST_NAME).is_file()
    (folder / "sub" / "a.yml").write_text("edited", encoding="utf-8")
    with pytest.raises(FileExistsError, match="a.yml"):
        prepare_case_folder(folder)
    assert (folder / "sub" / "a.yml").read_text(encoding="utf-8") == "edited"
    (folder / "sub" / "a.yml").write_text("a", encoding="utf-8")
    prepare_case_folder(folder)  # unchanged generated content: cleared
    assert folder.is_dir() and not any(folder.iterdir())
    legacy = tmp_path / "legacy"
    legacy.mkdir()
    (legacy / "run.md").write_text("old", encoding="utf-8")
    with pytest.raises(FileExistsError, match="no generator manifest"):
        prepare_case_folder(legacy)  # written before manifests existed: never deleted
    assert (legacy / "run.md").read_text(encoding="utf-8") == "old"
    (folder / "x.txt").write_text("x", encoding="utf-8")
    record_case_folder(folder)
    (folder / MANIFEST_NAME).write_text("{not json", encoding="utf-8")
    with pytest.raises(FileExistsError, match="manifest"):
        prepare_case_folder(folder)
    assert (folder / "x.txt").exists()

# ------------------------------------------- packaged hydrodynamic source pins
def test_wamit_package_pins_cover_every_packaged_file():
    """Independent pins for all four packaged files; .1 and .hst equal the
    digests the extractor recorded for the same revision."""
    module = _packages_module()
    assert set(module.WAMIT_SHA256) == set(module.WAMIT)
    _, data = load_sources()
    recorded = {Path(k).name: v for k, v in data["source_sha256"].items()}
    stem = Path(module.WAMIT[0]).stem
    for name in (f"{stem}.1", f"{stem}.hst"):
        assert module.WAMIT_SHA256[name] == recorded[name]
    assert all(len(v) == 64 and int(v, 16) >= 0 for v in module.WAMIT_SHA256.values())


def _placeholder_wamit(module, folder):
    folder.mkdir()
    for name in module.WAMIT:
        (folder / name).write_bytes(b"not the pinned file\n")


def test_wave_packages_refuse_unpinned_wamit_bytes(tmp_path):
    """Stale or altered files are refused before any package is written."""
    module = _packages_module()
    source = tmp_path / "source"
    _placeholder_wamit(module, source)
    out = tmp_path / "packages"
    with pytest.raises(ValueError, match="WAMIT source digest mismatch"):
        module.main(["x", str(out), str(source)])
    assert not out.exists()


def test_unpinned_wamit_bytes_leave_existing_packages_unchanged(tmp_path):
    module = _packages_module()
    out = tmp_path / "packages"
    assert module.main(["x", str(out)]) == 0
    before = {p.relative_to(out): p.read_bytes() for p in out.rglob("*") if p.is_file()}
    source = tmp_path / "source"
    _placeholder_wamit(module, source)
    with pytest.raises(ValueError, match="WAMIT source digest mismatch"):
        module.main(["x", str(out), str(source)])
    after = {p.relative_to(out): p.read_bytes() for p in out.rglob("*") if p.is_file()}
    assert after == before


@pytest.mark.skipif(not os.environ.get("DIGITALMODEL_IEA15_RAW"),
                    reason="raw IEA files not present; set DIGITALMODEL_IEA15_RAW")
def test_wave_packages_accept_pinned_files_and_refuse_one_altered_byte(tmp_path):
    module = _packages_module()
    raw = Path(os.environ["DIGITALMODEL_IEA15_RAW"])
    source = tmp_path / "source"
    source.mkdir()
    for name in module.WAMIT:
        shutil.copyfile(raw / name, source / name)
    module.verify_wamit_sources(source)  # the pinned set is accepted
    radiation = source / f"{Path(module.WAMIT[0]).stem}.3"
    content = bytearray(radiation.read_bytes())
    content[-2] = ord("0") if content[-2] != ord("0") else ord("1")
    radiation.write_bytes(bytes(content))
    with pytest.raises(ValueError, match=f"WAMIT source digest mismatch: {radiation.name}"):
        module.verify_wamit_sources(source)



def test_packaged_hydro_bytes_are_the_verified_bytes(tmp_path, monkeypatch):
    """The bytes written into hydro/ are the bytes that were hashed: a source
    file replaced after verification must not reach a package."""
    module = _packages_module()
    source = tmp_path / "source"
    source.mkdir()
    files = {name: f"verified {name}\n".encode() for name in module.WAMIT}
    for name, content in files.items():
        (source / name).write_bytes(content)
    monkeypatch.setattr(module, "WAMIT_SHA256", {
        name: hashlib.sha256(content).hexdigest() for name, content in files.items()})
    verify = module.verify_wamit_sources

    def verify_then_replace(folder):
        verified = verify(folder)
        for name in module.WAMIT:
            (folder / name).write_bytes(b"replaced after verification\n")
        return verified

    monkeypatch.setattr(module, "verify_wamit_sources", verify_then_replace)
    out = tmp_path / "packages"
    assert module.main(["x", str(out), str(source)]) == 0
    for name in module.WAVE_CASES:
        hydro = {p.name: p.read_bytes() for p in (out / name / "hydro").iterdir()}
        expected = {module.WAMIT_IMPORT if n == module.WAMIT_OUTPUT else n: c
                    for n, c in files.items()}
        assert hydro == expected
