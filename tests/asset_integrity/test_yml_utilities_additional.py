from __future__ import annotations

import builtins
from pathlib import Path
from types import SimpleNamespace

import pytest
import yaml

from digitalmodel.asset_integrity.common import yml_utilities as yu


def test_ymlinput_raises_for_invalid_file() -> None:
    with pytest.raises(FileNotFoundError):
        yu.ymlInput("does-not-exist.yml")


def test_ymlinput_raises_explicit_exception_when_validation_returns_false(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(yu, "is_file_valid_func", lambda _p: False)
    with pytest.raises(Exception, match="Not valid file"):
        yu.ymlInput("invalid.yml")


def test_ymlinput_loads_and_merges_update_file(tmp_path: Path) -> None:
    default_yml = tmp_path / "default.yml"
    update_yml = tmp_path / "update.yml"
    default_yml.write_text(yaml.safe_dump({"a": 1, "nested": {"x": 1, "y": 2}}))
    update_yml.write_text(yaml.safe_dump({"nested": {"x": 9}, "b": 2}))

    cfg = yu.ymlInput(str(default_yml), str(update_yml))
    assert cfg["a"] == 1
    assert cfg["b"] == 2
    assert cfg["nested"]["x"] == 9
    assert cfg["nested"]["y"] == 2


def test_ymlinput_uses_stream_fallback_on_composer_error(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    default_yml = tmp_path / "default.yml"
    default_yml.write_text("a: 1\n")

    def raise_composer_error(_stream):
        raise yaml.composer.ComposerError("ctx", None, "problem", None)

    monkeypatch.setattr(yu.yaml, "safe_load", raise_composer_error)
    monkeypatch.setattr(yu, "yml_read_stream", lambda _p: {"from_stream": True})
    cfg = yu.ymlInput(str(default_yml))
    assert cfg == {"from_stream": True}


def test_update_deep_replaces_non_mapping_base() -> None:
    result = yu.update_deep(42, {"a": 1})
    assert result == {"a": 1}


def test_yml_read_stream_merges_multiple_docs(tmp_path: Path) -> None:
    stream_file = tmp_path / "stream.yml"
    stream_file.write_text(
        "---\na: 1\nnested:\n  x: 1\n---\nb: 2\nnested:\n  y: 3\n"
    )
    cfg = yu.yml_read_stream(str(stream_file))
    assert cfg["a"] == 1
    assert cfg["b"] == 2
    assert cfg["nested"]["x"] == 1
    assert cfg["nested"]["y"] == 3


def test_yml_read_stream_ignores_non_dict_docs(tmp_path: Path) -> None:
    stream_file = tmp_path / "stream.yml"
    stream_file.write_text("---\na: 1\n---\n3\n")
    cfg = yu.yml_read_stream(str(stream_file))
    assert cfg == {"a": 1}


def test_yml_read_stream_handles_non_generator_loader(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    stream_file = tmp_path / "stream.yml"
    stream_file.write_text("a: 1\n")
    monkeypatch.setattr(yu.yaml, "safe_load_all", lambda _f: [{"a": 1}])
    cfg = yu.yml_read_stream(str(stream_file))
    assert cfg == {}


def test_yml_read_stream_raises_stopping_program_on_bad_source(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    def raise_os_error(*_args, **_kwargs):
        raise OSError("boom")

    monkeypatch.setattr(builtins, "open", raise_os_error)
    with pytest.raises(Exception, match="Stopping Program"):
        yu.yml_read_stream("bad.yml")


def test_ymlinput_ignores_bad_update_file_and_keeps_defaults(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    default_yml = tmp_path / "default.yml"
    update_yml = tmp_path / "update.yml"
    default_yml.write_text(yaml.safe_dump({"keep": 1, "nested": {"x": 2}}))
    update_yml.write_text(":\n  - bad-yaml")

    cfg = yu.ymlInput(str(default_yml), str(update_yml))
    assert cfg["keep"] == 1
    assert cfg["nested"]["x"] == 2
    assert "Update Input file could not be loaded successfully" in capsys.readouterr().out


def test_analyze_yaml_keys_prints_root_keys(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    ww = yu.WorkingWithYAML()
    yml = tmp_path / "sample.yml"
    yml.write_text(yaml.safe_dump({"alpha": 1, "beta": 2}))
    ww.analyze_yaml_keys(str(yml))
    out = capsys.readouterr().out
    assert "alpha" in out and "beta" in out


def test_get_library_filename_returns_direct_existing_file(tmp_path: Path) -> None:
    existing = tmp_path / "direct.yml"
    existing.write_text("a: 1\n")
    ww = yu.WorkingWithYAML()
    resolved = ww.get_library_filename({"filename": str(existing)})
    assert resolved == str(existing)


def test_get_library_filename_uses_library_path_when_direct_missing(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    lib_dir = tmp_path / "fakepkg"
    lib_dir.mkdir(parents=True)
    fake_init = lib_dir / "__init__.py"
    fake_init.write_text("# fake package\n")
    rel_name = "library.yml"
    (lib_dir / rel_name).write_text("k: v\n")

    monkeypatch.setattr(
        yu.importlib.util,
        "find_spec",
        lambda _name: SimpleNamespace(origin=str(fake_init)),
    )
    ww = yu.WorkingWithYAML()
    resolved = ww.get_library_filename({"filename": rel_name})
    assert resolved == str(lib_dir / rel_name)


def test_get_library_filename_raises_when_not_found(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    lib_dir = tmp_path / "fakepkg"
    lib_dir.mkdir(parents=True)
    fake_init = lib_dir / "__init__.py"
    fake_init.write_text("# fake package\n")
    monkeypatch.setattr(
        yu.importlib.util,
        "find_spec",
        lambda _name: SimpleNamespace(origin=str(fake_init)),
    )
    ww = yu.WorkingWithYAML()
    with pytest.raises(FileNotFoundError):
        ww.get_library_filename({"filename": "missing.yml"})


def test_compare_yaml_root_keys_same_and_different(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    ww = yu.WorkingWithYAML()
    a = tmp_path / "a.yml"
    b = tmp_path / "b.yml"
    c = tmp_path / "c.yml"
    a.write_text(yaml.safe_dump({"x": 1, "y": 2}))
    b.write_text(yaml.safe_dump({"x": 10, "y": 20}))
    c.write_text(yaml.safe_dump({"x": 1, "z": 2}))

    ww.compare_yaml_root_keys(str(a), str(b))
    out1 = capsys.readouterr().out
    assert "same root keys" in out1

    ww.compare_yaml_root_keys(str(a), str(c))
    out2 = capsys.readouterr().out
    assert "The root keys" in out2


def test_compare_yaml_files_deepdiff_emits_same_message_for_identical(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    ww = yu.WorkingWithYAML()
    a = tmp_path / "a.yml"
    b = tmp_path / "b.yml"
    content = {"k": {"x": 1, "y": 2}}
    a.write_text(yaml.safe_dump(content))
    b.write_text(yaml.safe_dump(content))

    ww.compare_yaml_files_deepdiff({"file_name1": str(a), "file_name2": str(b)})
    out = capsys.readouterr().out
    assert "Yaml files are the same" in out


def test_save_diff_files_writes_expected_outputs_and_invokes_save_data(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    ww = yu.WorkingWithYAML()
    a = tmp_path / "a.yml"
    b = tmp_path / "b.yml"
    a.write_text("a: 1\n")
    b.write_text("a: 2\n")

    calls: list[tuple[str, object]] = []

    def fake_save(data, path):
        calls.append((str(path), data))

    monkeypatch.setattr(yu, "saveDataYaml", fake_save)
    diff = {"values_changed": {"root['a']": {"old_value": 1, "new_value": 2}}}
    ww.save_diff_files(diff, {"file_name1": str(a), "file_name2": str(b)}, deepdiff_save=True)
    out = capsys.readouterr().out
    assert "Yaml files are different" in out
    assert any("deepdiff" in c[0] for c in calls)
    assert any("values_changed" in c[0] for c in calls)
    updated = next(tmp_path.glob("wwyaml_*_updated_values.yml"))
    assert "new_value" not in updated.read_text()
    assert "root['a']" in updated.read_text()


def test_save_diff_files_reports_same_when_no_diff(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    ww = yu.WorkingWithYAML()
    a = tmp_path / "a.yml"
    b = tmp_path / "b.yml"
    a.write_text("a: 1\n")
    b.write_text("a: 1\n")
    ww.save_diff_files({}, {"file_name1": str(a), "file_name2": str(b)})
    assert "Yaml files are the same" in capsys.readouterr().out


def test_save_diff_files_handles_mixed_diff_keys(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    ww = yu.WorkingWithYAML()
    a = tmp_path / "a.yml"
    b = tmp_path / "b.yml"
    a.write_text("a: 1\n")
    b.write_text("a: 2\n")

    monkeypatch.setattr(yu, "saveDataYaml", lambda *_args, **_kwargs: None)
    diff = {
        "dictionary_item_added": "root['b']",
        "values_changed": {"root['a']": {"old_value": 1, "new_value": 2}},
    }
    ww.save_diff_files(diff, {"file_name1": str(a), "file_name2": str(b)})
    updated = next(tmp_path.glob("wwyaml_*_updated_values.yml"))
    text = updated.read_text()
    assert "root['a']" in text


def test_compare_yaml_files_deepdiff_diff_path_hits_current_signature_bug(
    tmp_path: Path,
) -> None:
    ww = yu.WorkingWithYAML()
    a = tmp_path / "a.yml"
    b = tmp_path / "b.yml"
    a.write_text(yaml.safe_dump({"k": 1}))
    b.write_text(yaml.safe_dump({"k": 2}))
    with pytest.raises(TypeError):
        ww.compare_yaml_files_deepdiff({"file_name1": str(a), "file_name2": str(b)})


def test_compare_yaml_file_contents_deepdiff_uses_map_and_saves_diff(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    ww = yu.WorkingWithYAML()
    a = tmp_path / "a.yml"
    b = tmp_path / "b.yml"
    a.write_text(yaml.safe_dump({"root": {"v": 1}}))
    b.write_text(yaml.safe_dump({"root": {"v": 2}}))

    captured: list[dict] = []

    def fake_save_diff(file_diff, cfg, deepdiff_save=False):
        captured.append(file_diff)

    class DummyReadData:
        @staticmethod
        def key_chain(data, *keys):
            cur = data
            for key in keys:
                cur = cur[key]
            return cur

    monkeypatch.setattr(yu, "read_data", DummyReadData())
    monkeypatch.setattr(ww, "save_diff_files", fake_save_diff)
    cfg = {
        "file_name1": str(a),
        "file_name2": str(b),
        "map_list": {"file_name1": ["root"], "file_name2": ["root"]},
    }
    ww.compare_yaml_file_contents_deepdiff(cfg)
    assert captured
    assert "values_changed" in captured[0]


def test_get_library_yaml_file_reads_from_pkgutil_when_file_missing(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    ww = yu.WorkingWithYAML()
    monkeypatch.setattr(yu.os.path, "isfile", lambda _p: False)
    monkeypatch.setattr(
        yu.pkgutil,
        "get_data",
        lambda _lib, _name: b"alpha: 1\nbeta:\n  x: 2\n",
    )
    data = ww.get_library_yaml_file({"filename": "missing-local.yml"})
    assert data["alpha"] == 1
    assert data["beta"]["x"] == 2


def test_get_library_yaml_file_reads_local_file_when_present(tmp_path: Path) -> None:
    ww = yu.WorkingWithYAML()
    yml = tmp_path / "lib.yml"
    yml.write_text("x: 10\n")
    data = ww.get_library_yaml_file({"filename": str(yml)})
    assert data["x"] == 10
