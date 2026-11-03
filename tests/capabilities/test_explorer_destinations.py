"""Independent path expectations for the explorer (issue #2315).

Only the final suffix changes: analysis -> analysis.json, while the parent
archive.html.json is unchanged. A supplied sidecar in details must be linked
from pages by ../details/data.json. Fixture hashes must survive rejection.
The reused display fixture is synthetic; no engineering result is tested.
"""

from hashlib import sha256
import json
from pathlib import Path
import runpy
from urllib.parse import unquote

import pytest


def _renderer():
    fixtures = runpy.run_path(str(Path(__file__).with_name("test_wall_thickness_3d_page.py")))
    return fixtures["_load_module"](), fixtures["_study"]()


@pytest.mark.parametrize("name", ["analysis.json", "analysis.JSON"])
def test_colliding_inferred_outputs_are_rejected(tmp_path, name):
    renderer, study = _renderer()
    source = tmp_path / "input.json"
    source.write_text(json.dumps(study))
    output = tmp_path / name
    output.write_bytes(b"retained data")
    before = {p: sha256(p.read_bytes()).digest() for p in (source, output)}
    with pytest.raises(ValueError, match="distinct"):
        renderer.main(["--source", str(source), "--output", str(output)])
    assert {p: sha256(p.read_bytes()).digest() for p in before} == before


@pytest.mark.parametrize("alias", ["same", "symlink", "hardlink", "input"])
def test_explicit_output_and_input_aliases_preserve_data(tmp_path, alias):
    renderer, study = _renderer()
    source = tmp_path / "input.json"
    source.write_text(json.dumps(study))
    output = tmp_path / "analysis.html"
    sidecar = tmp_path / "details.json"
    retained = tmp_path / "retained.data"
    retained.write_bytes(b"retained data")
    if alias == "same":
        sidecar = output
    elif alias == "input":
        sidecar = source
    elif alias == "symlink":
        output.symlink_to(retained)
        sidecar.symlink_to(retained)
    else:
        output.hardlink_to(retained)
        sidecar.hardlink_to(retained)
    before = {p: sha256(p.read_bytes()).digest() for p in (source, retained)}
    with pytest.raises(ValueError, match="distinct"):
        renderer.main(["--source", str(source), "--output", str(output), "--sidecar", str(sidecar)])
    assert {p: sha256(p.read_bytes()).digest() for p in before} == before
    if alias in {"same", "input"}:
        assert not output.exists()


@pytest.mark.parametrize("name", ["analysis", "analysis.html"])
@pytest.mark.parametrize("explicit", [False, True])
def test_names_and_cross_directory_links(tmp_path, name, explicit):
    renderer, study = _renderer()
    source = tmp_path / "input.json"
    source.write_text(json.dumps(study))
    before = sha256(source.read_bytes()).digest()
    output = tmp_path / "archive.html.json" / name
    sidecar = tmp_path / "details #data" / "data.json" if explicit else output.parent / "analysis.json"
    arguments = ["--source", str(source), "--output", str(output)]
    if explicit:
        arguments += ["--sidecar", str(sidecar)]
    renderer.main(arguments)
    page = output.read_text()
    # Extract the literal argument to fetch rather than reproducing URL generation.
    url = json.loads(page.split("fetch(", 1)[1].split(",", 1)[0])
    assert (output.parent / unquote(url)).resolve() == sidecar
    assert json.loads(sidecar.read_text())["m"]["qualification"]["status"] == "NOT_EVALUATED"
    assert sha256(source.read_bytes()).digest() == before


def test_symlinked_page_links_from_its_actual_directory(tmp_path):
    renderer, study = _renderer()
    source = tmp_path / "input.json"
    source.write_text(json.dumps(study))
    actual = tmp_path / "pages" / "analysis.html"
    actual.parent.mkdir()
    output = tmp_path / "analysis.html"
    output.symlink_to(actual)
    renderer.main(["--source", str(source), "--output", str(output)])
    url = json.loads(actual.read_text().split("fetch(", 1)[1].split(",", 1)[0])
    assert (actual.parent / unquote(url)).resolve() == tmp_path / "analysis.json"
