"""Offline qualification preparation; these tests establish no native FE result."""
import importlib.util
from pathlib import Path

from digitalmodel.ansys.padeye import PadeyeGeometry, generate_padeye_apdl


def load_build():
    path = Path(__file__).resolve().parents[2] / "examples/ansys/padeye/build.py"
    spec = importlib.util.spec_from_file_location("padeye_preparation_build", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_equilibrium_uses_only_support_nodes_in_global_coordinates():
    post = generate_padeye_apdl(PadeyeGeometry()).split("/POST1", 1)[1]
    before_fsum = post.split("\nFSUM\n", 1)[0]
    assert "RSYS,0" in before_fsum
    assert "CSYS,0" in before_fsum
    assert before_fsum.rfind("NSEL,S,LOC,Y,0") > before_fsum.rfind("ALLSEL,ALL")
    for axis in ("x", "y"):
        assert f"*GET,rf{axis},FSUM,0,ITEM,F{axis.upper()}" in post
    assert "balance_n = SQRT((rfx + applied_fx)**2 + (rfy + applied_fy)**2)" in post
    for label in ("reaction_fx_n", "reaction_fy_n", "force_residual_n"):
        assert label in post
    assert "FSUM,0,ITEM,FZ" not in post
    assert "reaction_fz_n" not in post


def test_peak_location_is_recorded_before_support_selection():
    post = generate_padeye_apdl(PadeyeGeometry()).split("/POST1", 1)[1]
    assert post.index("NSORT,S,EQV") < post.index("*GET,peak_node,SORT,0,IMAX")
    for axis in ("x", "y", "z"):
        line = f"*GET,peak_{axis},NODE,peak_node,LOC,{axis.upper()}"
        assert post.index(line) < post.index("NSEL,S,LOC,Y,0")
        assert f"peak_{axis}_mm" in post
    assert "peak_node," in post


def test_candidate_keeps_design_load_and_prepares_two_meshes(tmp_path):
    build = load_build()
    assert build.GEOM.sling_load_kn == 500.0
    assert build.GEOM.thickness_mm == 80.0
    paths = build.prepare_mesh_study(tmp_path)
    assert len(paths) == 2
    texts = [path.read_text(encoding="utf-8") for path in paths]
    assert "ESIZE,10.0" in texts[0]
    assert "ESIZE,5.0" in texts[1]
    def without_mesh(text):
        return [line for line in text.splitlines() if not line.startswith(("ESIZE,", "mesh_size ="))]
    assert without_mesh(texts[0]) == without_mesh(texts[1])
    assert sorted(path.suffix for path in tmp_path.rglob("*.*")) == [".inp", ".inp"]
    assert all("QUALIFICATION PENDING" in text for text in texts)


def test_candidate_states_plane_stress_and_pin_contact_limitations():
    build = load_build()
    deck = generate_padeye_apdl(build.GEOM)
    assert "t/D = 1.0" in deck
    assert "plane-stress idealisation" in deck
    assert "No pin/contact or 3D qualification" in deck
    assert "t/D = 1" in build.__doc__
