"""Guards on the four #1173 case-construction scripts.

`stage1_dtchull.sh`, `setup_fine.sh`, `setup_wallref.sh` and `setup_yplus.sh`
are the scripts that decide MESH QUALITY. Everything downstream — y+, C_v, the
grid-convergence ratio, the Richardson extrapolation — is a consequence of the
dictionaries they write. A defect here is not a crash; it is a plausible-looking
number produced by a mesh nobody intended, discovered days and tens of core-hours
later.

The properties pinned below are the ones whose violation is silent:

  1. no host-specific case root — the originals hard-coded ``$HOME/cfd/dm1173``
     and so ran on exactly one account on exactly one machine (#2023);
  2. a terminal marker on BOTH paths — a lane that marks only success makes
     silence mean "running" and "died" at the same time;
  3. ``mpirun`` never inherits stdin — it reads and closes it, which has
     detached this chain from its driver before;
  4. the OpenFOAM bashrc is never sourced under ``set -e``/``set -u`` — it
     dereferences unset variables and calls ``pop_var_context``;
  5. the RELATIVE/ABSOLUTE layer distinction survives — ``setup_wallref.sh``
     scales ``finalLayerThickness`` as a fraction of the local cell size, while
     ``setup_yplus.sh`` pins an ABSOLUTE ``firstLayerThickness`` derived from a
     target y+. Converting one into the other silently changes the near-wall
     mesh by orders of magnitude while both dictionaries still parse;
  6. a ``mapFieldsPar``-mapped field is never re-initialised — ``setFields``
     over a mapped IC destroys the prolongation and the run looks fine;
  7. physically meaningful values are named parameters or derived in the open,
     not literals buried in a sed expression.

Most assertions run against a COMMENT-STRIPPED copy of each script. These
scripts document the exact defects they guard against, quoting the wrong
values in prose, so a naive substring search finds the documentation and
passes while the code does the opposite of what it says.
"""
from __future__ import annotations

import os
import re
import shutil
import subprocess
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
CFD = REPO / "scripts" / "cfd"
CONFIG = REPO / "config" / "cfd" / "kcs_chain.yml"

NAMES = ("stage1_dtchull", "setup_fine", "setup_wallref", "setup_yplus")
SCRIPTS = {n: CFD / f"{n}.sh" for n in NAMES}

needs_bash = pytest.mark.skipif(shutil.which("bash") is None,
                                reason="bash not available")


def _read(name: str) -> str:
    p = SCRIPTS[name]
    assert p.is_file(), f"{name} not found at {p}"
    return p.read_text()


def _strip(text: str) -> str:
    """Executable lines, with backslash continuations joined.

    Joining matters: a redirect or a guard that lands on the continuation line
    of a multi-line command is part of that command, and a per-line scan would
    report it missing.
    """
    body = "\n".join(ln for ln in text.splitlines()
                     if not ln.lstrip().startswith("#"))
    return body.replace("\\\n", " ")


@pytest.fixture(scope="module")
def source():
    """Whole file, comments included — for assertions about the WHY."""
    return _read


@pytest.fixture(scope="module")
def code():
    """Executable lines only.

    The comments in these scripts quote the defective values they replaced
    (``relativeSizes true``, ``finalLayerThickness 0.7``, the pasted
    ``1.8764e-03``). Asserting on comment-stripped source is the difference
    between testing the guard and testing its documentation.
    """
    return lambda name: _strip(_read(name))


# --------------------------------------------------------------------------- #
# Properties every case-construction script must hold
# --------------------------------------------------------------------------- #

@needs_bash
@pytest.mark.parametrize("name", NAMES)
def test_script_is_syntactically_valid(name: str):
    """A syntax error surfaces only after the mesh phase has been queued."""
    r = subprocess.run(["bash", "-n", str(SCRIPTS[name])],
                       capture_output=True, text=True)
    assert r.returncode == 0, r.stderr


@pytest.mark.parametrize("name", NAMES)
def test_no_host_specific_case_root(name: str, code):
    """The reason none of these could be re-run from a clean checkout."""
    body = code(name)
    assert "$HOME/cfd" not in body, "the original's one-host case root is back"
    offenders = [ln.strip() for ln in body.splitlines()
                 if re.search(r"(/home/|/Users/|/mnt/(ace|local-analysis))", ln)]
    assert not offenders, f"developer-machine path: {offenders}"
    assert "cfd_root" in body or "DM_CFD_ROOT" in body, \
        "the case root must come from DM_CFD_ROOT via the shared contract"


@pytest.mark.parametrize("name", NAMES)
def test_sources_the_shared_contract_relative_to_itself(name: str, code):
    """Sourcing by absolute path would reintroduce the host dependency."""
    body = code(name)
    assert "cfd_chain.sh" in body
    assert re.search(r'dirname "\$\{BASH_SOURCE\[0\]\}"', body), \
        "the library path must be resolved from the script's own location"


@pytest.mark.parametrize("name", NAMES)
def test_writes_a_terminal_marker_on_success_and_on_failure(name: str, code):
    """Silence must never be readable as success."""
    body = code(name)
    assert "cfd_marker_ok" in body, "no success marker"
    assert "cfd_marker_fail" in body, "no failure marker"
    assert re.search(r"^\s*trap\b", body, re.M), \
        "the failure marker must be armed by a trap, not left to the happy path"


@pytest.mark.parametrize("name", NAMES)
def test_every_mpirun_gets_an_explicit_stdin(name: str, code):
    """mpirun reads and closes stdin, which detaches it from its driver."""
    bad = [ln.strip() for ln in code(name).splitlines()
           if "mpirun" in ln and "< /dev/null" not in ln]
    assert not bad, f"mpirun without an explicit stdin: {bad}"


@pytest.mark.parametrize("name", NAMES)
def test_never_matches_the_process_table_on_a_command_line(name: str, code):
    """`pgrep -f` matches the ssh/driver line carrying the pattern.

    That produced a 13.5 h zombie waiter on this fleet, and the matching
    `pkill -f` killed the operator's own session.
    """
    assert "pgrep -f" not in code(name)
    assert "pkill -f" not in code(name)


@pytest.mark.parametrize("name", NAMES)
def test_the_openfoam_bashrc_is_never_sourced_under_a_strict_shell(
        name: str, code):
    """etc/bashrc dereferences unset vars and calls pop_var_context.

    Under `set -u` or `set -e` the shell aborts mid-source, leaving a half-built
    environment that fails much later and much less legibly. Either use
    cfd_load_openfoam (which saves and restores the flags), or drop them
    immediately before the source.
    """
    body = code(name)
    lines = body.splitlines()
    for i, ln in enumerate(lines):
        if re.search(r"(source|^\s*\.)\s+\S*etc/bashrc", ln):
            window = "\n".join(lines[max(0, i - 6):i])
            assert "set +eu" in window, (
                f"{name}: unguarded bashrc source at line {i + 1}: {ln.strip()}")


@needs_bash
@pytest.mark.parametrize("name", NAMES)
def test_fails_closed_when_the_case_root_is_unset(name: str, tmp_path):
    """Building into an unintended directory is worse than not building."""
    env = {k: v for k, v in os.environ.items() if not k.startswith("DM_CFD_")}
    env["CFD_LOG"] = str(tmp_path / "log")
    env["CFD_MARKER"] = str(tmp_path / "marker")
    r = subprocess.run(["bash", str(SCRIPTS[name])], env=env, cwd=str(tmp_path),
                       capture_output=True, text=True, timeout=120)
    assert r.returncode != 0, "ran with no case root configured"
    assert "DM_CFD_ROOT" in r.stderr, r.stderr


# --------------------------------------------------------------------------- #
# stage1_dtchull.sh — the UNMODIFIED tutorial baseline
# --------------------------------------------------------------------------- #

def test_stage1_never_edits_the_copied_tutorial(code):
    """Its only value is being byte-identical to the shipped tutorial.

    A baseline that has been "improved" measures nothing, and the edit is
    invisible six months later.
    """
    body = code("stage1_dtchull")
    assert "sed -i" not in body, "stage-1 must not edit the tutorial case"
    assert "md5sum" in body, "the copy must be checksummed to prove it"


def test_stage1_does_not_rewrite_the_tutorial_decomposition(code, source):
    """decomposeParDict is part of "unmodified"; it is checked, not changed."""
    body = code("stage1_dtchull")
    assert not re.search(r"sed[^\n]*decomposeParDict", body)
    assert "numberOfSubdomains" in body, \
        "the tutorial's rank count must at least be read back and validated"


def test_stage1_refuses_to_destroy_an_existing_baseline(code):
    """The original opened with an unconditional `rm -rf "$CASE"`."""
    body = code("stage1_dtchull")
    assert not re.search(r'^\s*rm -rf "\$CASE"\s*$', body, re.M), \
        "unconditional removal of a previous baseline run"
    assert "DM_CFD_FORCE" in body, "removal must be gated behind an explicit opt-in"


def test_stage1_names_the_tutorial_and_geometry_as_parameters(code):
    """Which tutorial and which hull is the whole provenance of the baseline."""
    body = code("stage1_dtchull")
    assert "TUTORIAL_CASE=" in body
    assert "TUTORIAL_GEOMETRY=" in body
    assert "multiphase/interFoam/RAS/DTCHull" in body
    assert "REFINE_PASSES=" in body, \
        "the topoSet/refineMesh pass count must be named, not a bare 1..6 loop"


def test_stage1_reads_checkmesh_output_not_its_exit_code(source):
    """checkMesh exits 0 while reporting failed checks."""
    body = source("stage1_dtchull")
    assert "acceptance probe" in body, \
        "the note that checkMesh is NOT part of the tutorial Allrun is load-bearing"


# --------------------------------------------------------------------------- #
# setup_fine.sh — the third Richardson level
# --------------------------------------------------------------------------- #

def test_fine_scales_the_base_grid_from_a_named_ratio(code):
    """The destination divisions are a consequence, not a paste.

    The original carried four hand-computed literals in a sed expression, so
    the refinement ratio existed only in a comment and could not be checked.
    """
    body = code("setup_fine")
    assert "REFINEMENT_RATIO=" in body
    assert re.search(r"SRC_BASE_(NX|DIVISIONS_X)=", body)
    for literal in ("72 33 85", "72 33 7", "72 33 68", "72 33 34"):
        assert literal not in body, \
            f"destination divisions ({literal}) pasted instead of derived"


def test_fine_holds_everything_but_the_grid_constant(code):
    """LTS violates temporal conservation.

    Changing maxCo/maxAlphaCo between levels moves the answer rather than the
    path to it, and the Richardson estimate silently stops meaning anything.
    """
    body = code("setup_fine")
    for f in ("fvSchemes", "fvSolution", "turbulenceProperties"):
        assert f in body, f"{f} is not verified against the source case"
    assert re.search(r"held-constant|cfd_die", body), \
        "a held-constant file that moved must be fatal, not a warning"


def test_fine_reference_area_is_named_and_documented_as_half(source, code):
    """Aref = 4.71895 is HALF the published wetted surface S = 9.4379.

    The domain is cut at the centreplane and `forces` integrates the hull patch
    only, so the reported force is half-body. Halving the area is what makes Cd
    numerically equal to the true full-hull Ct. The factor-of-two trap cost a
    day on this issue; it has to stay visible as a parameter.
    """
    body = code("setup_fine")
    assert re.search(r"A_REF=.*4\.71895", body), \
        "the reference area must be a named parameter"
    assert re.search(r"9\.4379|half", source("setup_fine")), \
        "the half-body derivation must stay in the file"
    for key in ("MAG_U_INF=", "L_REF=", "RHO_INF="):
        assert key in body, f"{key} must be a named parameter"


def test_fine_convergence_stop_is_a_named_backstop(code):
    """runTimeControl only decides WHEN to stop; it cannot move the solution.

    Its window and start-up iteration count are what make the fine and medium
    levels comparable, so they are parameters rather than dictionary literals.
    """
    body = code("setup_fine")
    assert "CD_WINDOW=" in body and "CD_TOLERANCE=" in body
    assert "CD_N_ITER_STARTUP=" in body
    assert "runTimeControl" in body


def test_fine_maps_the_coarse_solution_explicitly(code):
    """A default source time silently maps time 0 — an empty prolongation."""
    body = code("setup_fine")
    assert "mapFieldsPar" in body
    assert "MAP_SOURCE_TIME=" in body, "the mapped time must be a parameter"
    assert "-consistent" in body, \
        "same domain, different mesh: the mapping is consistent by construction"
    assert "MAP_SOURCE_CASE" in body or "SRC_CASE=" in body, \
        "the mapFieldsPar source case must be named"


def test_fine_never_reinitialises_the_mapped_field(code, source):
    """setFields over a mapped IC destroys the prolongation and still solves.

    The host-side chain avoided this only by accident — runApplication refuses
    to repeat a stage whose log exists. The protection has to be deliberate:
    the mapped case is solved by invoking the solver DIRECTLY, never through
    the mesh/solve driver that re-runs restore0Dir + setFields.
    """
    body = code("setup_fine")
    assert not re.search(r"\bsetFields\b(?!Dict)", body), \
        "setFields is invoked somewhere in the fine-grid path"
    assert not re.search(r"\brestore0Dir\b", body), \
        "restore0Dir re-copies 0.orig over the mapped field"
    assert "interFoam -parallel" in body, \
        "the mapped case must be solved by a direct solver invocation"
    assert "mapped" in source("setup_fine"), \
        "the reason for the direct invocation must stay in the file"


def test_fine_refuses_to_overwrite_and_drops_the_inherited_mesh(code):
    """constant/polyMesh belongs to the source level, not this one."""
    body = code("setup_fine")
    assert "constant/polyMesh" in body
    assert re.search(r"refus|exists", body), \
        "an existing destination must not be silently rebuilt over"


# --------------------------------------------------------------------------- #
# setup_wallref.sh — RELATIVE near-wall refinement
# --------------------------------------------------------------------------- #

def test_wallref_keeps_relative_layer_sizing(code):
    """finalLayerThickness 0.4 is a FRACTION of the local cell size.

    Written into a dictionary with `relativeSizes false` it becomes 0.4 metres
    — on a 7.3 m hull — and snappyHexMesh will happily try. The wall-refinement
    variant must therefore never touch relativeSizes and never introduce an
    absolute thickness key.
    """
    body = code("setup_wallref")
    assert "firstLayerThickness" not in body, \
        "absolute thickness key introduced into the relative-sizing variant"
    assert not re.search(r"relativeSizes\s+false", body)
    assert "finalLayerThickness" in body
    assert re.search(r"relativeSizes", body), \
        "the script must at least assert the source is relatively sized"


def test_wallref_layer_stack_is_named_and_derived(code, source):
    """First-cell height = finalLayerThickness / expansionRatio^(n-1).

    Before: 0.7 / 1.5^2 = 0.311. After: 0.4 / 1.3^7 = 0.0637 — 4.88x thinner,
    and y+ scales linearly with it. The prediction is only checkable if the
    three numbers that produce it are named.
    """
    body = code("setup_wallref")
    for key in ("N_SURFACE_LAYERS=", "EXPANSION_RATIO=",
                "FINAL_LAYER_THICKNESS=", "MIN_THICKNESS="):
        assert key in body, f"{key} must be a named parameter"
    assert re.search(r"4\.88|0\.0637|154\.9|1715", source("setup_wallref")), \
        "the measured y+ range the change is designed against must survive"


def test_wallref_refuses_a_blind_sed_on_a_non_unique_key(code):
    """Each layer key must be unique or the sed edits the wrong block."""
    body = code("setup_wallref")
    assert re.search(r"grep -c", body)
    assert re.search(r"refusing blind sed|appears", body)


@pytest.mark.parametrize("name", ("setup_wallref", "setup_yplus", "setup_fine"))
def test_the_source_case_is_never_written_to(name: str, code):
    """kcs_companion and kcs_production hold converged multi-hour results.

    They are the V1/V2/V3 inputs. A variant builder that writes into its own
    source silently invalidates the baseline it is being compared against.
    """
    bad = [ln.strip() for ln in code(name).splitlines()
           if re.search(r'(sed -i|>\s*"?\$SRC|tee)[^\n]*\$SRC(?!_)', ln)]
    assert not bad, f"{name} writes into the source case: {bad}"


# --------------------------------------------------------------------------- #
# setup_yplus.sh — ABSOLUTE first-layer thickness from a target y+
# --------------------------------------------------------------------------- #

def test_yplus_switches_to_absolute_sizing_completely(code):
    """relativeSizes false and firstLayerThickness are one change, not two.

    Half-applying it parses cleanly and meshes: a `finalLayerThickness 0.4`
    left behind under `relativeSizes false` is 0.4 m, and a
    `firstLayerThickness` left under `relativeSizes true` is 0.0019 of the
    local cell. Both produce a mesh, a y+ and a C_v that look like results.
    """
    body = code("setup_yplus")
    assert re.search(r"relativeSizes\s+false", body)
    assert "firstLayerThickness" in body
    assert re.search(r"finalLayerThickness", body), \
        "the relative key must be explicitly replaced, not left to chance"
    assert re.search(r"(cfd_die|FATAL)[^\n]*(finalLayerThickness|absolute)",
                     body) or re.search(
        r"finalLayerThickness[^\n]*\n[^\n]*(cfd_die|FATAL)", body), \
        "a surviving finalLayerThickness after the flip must be fatal"


def test_yplus_thickness_is_derived_from_the_measurement(code, source):
    """t = 2 * y+_target * nu / u_tau_max.

    u_tau_max came from mag(wallShearStress) on production's own converged
    solution (max 0.059242 m^2/s^2). Pasting 1.8764e-03 as a literal makes the
    target y+ unrecoverable and the number unmaintainable at any other speed,
    fluid or grid.
    """
    body = code("setup_yplus")
    assert "1.8764" not in body, "the derived thickness is pasted as a literal"
    assert "YPLUS_TARGET=" in body
    assert re.search(r"NU(_KINEMATIC)?=", body)
    assert re.search(r"WALL_SHEAR|U_TAU", body)
    assert re.search(r"sqrt", body), "u_tau = sqrt(tau) must be computed"
    assert re.search(r"y\+\s*=|2 \* y|target y\+", source("setup_yplus")), \
        "the formula must stay in the file"


def test_yplus_layer_stack_is_named(code):
    """6 layers at 1.3 grow 1.876e-3 back to the existing outer layer size."""
    body = code("setup_yplus")
    for key in ("N_SURFACE_LAYERS=", "EXPANSION_RATIO=", "MIN_THICKNESS="):
        assert key in body, f"{key} must be a named parameter"


@pytest.mark.parametrize("name", ("setup_fine", "setup_yplus"))
def test_a_rank_change_rewrites_the_decomposition_vector(name: str, code):
    """hierarchical requires prod(n) == numberOfSubdomains.

    kcs_prod_yplus is registered at 16 ranks while its source case is
    decomposed for 8, so copying decomposeParDict unchanged and raising the
    rank count makes decomposePar exit fatally with "Wrong number of domain
    divisions" — after the mesh has been built.
    """
    body = code(name)
    assert "cfd_decompose_n" in body, \
        "the decomposition vector must come from the registry, not be assumed"
    assert "numberOfSubdomains" in body


# --------------------------------------------------------------------------- #
# Functional: the build phases are pure file manipulation, so they can be run
# --------------------------------------------------------------------------- #

def _write(p: Path, text: str) -> None:
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(text)


def _make_source_case(root: Path, name: str) -> Path:
    """A minimal but structurally faithful source case."""
    case = root / "kcs_cases" / name
    _write(case / "system" / "blockMeshDict", """\
FoamFile { version 2.0; format ascii; class dictionary; object blockMeshDict; }
scale 1;
blocks
(
    hex (0 1 2 3 4 5 6 7) (51 23 60) simpleGrading (1 1 1)
    hex (8 9 10 11 12 13 14 15) (51 23 5) simpleGrading (1 1 1)
    hex (16 17 18 19 20 21 22 23) (51 23 48) simpleGrading (1 1 1)
    hex (24 25 26 27 28 29 30 31) (51 23 24) simpleGrading (1 1 1)
);
""")
    _write(case / "system" / "snappyHexMeshDict", """\
FoamFile { version 2.0; format ascii; class dictionary; object snappyHexMeshDict; }
addLayersControls
{
    relativeSizes       true;
    layers
    {
        hull
        {
            nSurfaceLayers 3;
        }
    }
    expansionRatio      1.5;
    finalLayerThickness 0.7;
    minThickness        0.25;
    nGrow               0;
}
""")
    _write(case / "system" / "controlDict", """\
FoamFile { version 2.0; format ascii; class dictionary; object controlDict; }
application     interFoam;
endTime         25000;
writeInterval   2500;
functions
{
    #includeFunc residuals
}
""")
    _write(case / "system" / "decomposeParDict", """\
FoamFile { version 2.0; format ascii; class dictionary; object decomposeParDict; }
numberOfSubdomains 8;
method          hierarchical;
coeffs
{
    n           (2 2 2);
}
""")
    for f in ("fvSchemes", "fvSolution", "meshQualityDict",
              "surfaceFeatureExtractDict", "setFieldsDict", "refineMeshDict"):
        _write(case / "system" / f, f"// {f}\n")
    for i in range(1, 7):
        _write(case / "system" / f"topoSetDict.{i}", f"// pass {i}\n")
    for f in ("transportProperties", "turbulenceProperties", "g"):
        _write(case / "constant" / f, f"// {f}\n")
    _write(case / "constant" / "triSurface" / "KCS.stl", "solid\nendsolid\n")
    _write(case / "constant" / "polyMesh" / "points", "// inherited mesh\n")
    for f in ("U", "p_rgh", "alpha.water"):
        _write(case / "0.orig" / f, f"// {f}\n")
    return case


def _run(script: str, root: Path, tmp_path: Path, *args: str):
    env = dict(os.environ)
    env["DM_CFD_ROOT"] = str(root)
    env["DM_CFD_CONFIG"] = str(CONFIG)
    env["CFD_LOG"] = str(tmp_path / f"{script}.log")
    env["CFD_MARKER"] = str(tmp_path / f"{script}.marker")
    return subprocess.run(["bash", str(SCRIPTS[script]), *args],
                          env=env, cwd=str(tmp_path), capture_output=True,
                          text=True, timeout=300)


@pytest.fixture
def root(tmp_path: Path) -> Path:
    r = tmp_path / "cfd"
    (r / "kcs_cases").mkdir(parents=True)
    return r


@needs_bash
def test_fine_build_scales_every_block_and_leaves_no_source_divisions(
        root: Path, tmp_path: Path):
    _make_source_case(root, "kcs_production")
    r = _run("setup_fine", root, tmp_path, "build", "kcs_production", "kcs_fine")
    assert r.returncode == 0, r.stdout + r.stderr
    bm = (root / "kcs_cases" / "kcs_fine" / "system" / "blockMeshDict").read_text()
    for expected in ("(72 33 85)", "(72 33 7)", "(72 33 68)", "(72 33 34)"):
        assert expected in bm, f"{expected} missing:\n{bm}"
    assert "(51 23" not in bm, "an unscaled source block survived"


@needs_bash
def test_fine_build_inserts_the_force_coefficients_and_the_stop(
        root: Path, tmp_path: Path):
    _make_source_case(root, "kcs_production")
    r = _run("setup_fine", root, tmp_path, "build", "kcs_production", "kcs_fine")
    assert r.returncode == 0, r.stdout + r.stderr
    cd = (root / "kcs_cases" / "kcs_fine" / "system" / "controlDict").read_text()
    assert re.search(r"Aref\s+4\.71895", cd), cd
    assert re.search(r"magUInf\s+2\.1962", cd)
    assert re.search(r"lRef\s+7\.2786", cd)
    assert re.search(r"nIterStartUp\s+9000", cd)
    assert re.search(r"window\s+4000", cd)
    assert "runTimeControl" in cd


@needs_bash
def test_fine_build_drops_the_inherited_mesh_and_refuses_a_second_run(
        root: Path, tmp_path: Path):
    _make_source_case(root, "kcs_production")
    first = _run("setup_fine", root, tmp_path, "build",
                 "kcs_production", "kcs_fine")
    assert first.returncode == 0, first.stdout + first.stderr
    dst = root / "kcs_cases" / "kcs_fine"
    assert not (dst / "constant" / "polyMesh").exists()
    assert (dst / "constant" / "triSurface" / "KCS.stl").is_file(), \
        "the hull geometry must come along or the case cannot be meshed"
    marker = (tmp_path / "setup_fine.marker").read_text()
    assert marker.startswith("OK "), marker

    second = _run("setup_fine", root, tmp_path, "build",
                  "kcs_production", "kcs_fine")
    assert second.returncode != 0, "silently rebuilt over an existing case"
    assert (tmp_path / "setup_fine.marker").read_text().startswith("FAILED ")


@needs_bash
def test_wallref_build_changes_only_the_four_relative_layer_keys(
        root: Path, tmp_path: Path):
    src = _make_source_case(root, "kcs_companion")
    r = _run("setup_wallref", root, tmp_path,
             "kcs_companion", "kcs_companion_wallref")
    assert r.returncode == 0, r.stdout + r.stderr
    dst = root / "kcs_cases" / "kcs_companion_wallref"
    dict_text = (dst / "system" / "snappyHexMeshDict").read_text()
    assert re.search(r"nSurfaceLayers\s+8;", dict_text)
    assert re.search(r"expansionRatio\s+1\.3;", dict_text)
    assert re.search(r"finalLayerThickness\s+0\.4;", dict_text)
    assert re.search(r"minThickness\s+0\.05;", dict_text)
    assert re.search(r"relativeSizes\s+true;", dict_text), \
        "relative sizing must be untouched"
    assert "firstLayerThickness" not in dict_text
    # Everything outside the four keys is byte-identical.
    keys = re.compile(r"nSurfaceLayers|expansionRatio|finalLayerThickness"
                      r"|minThickness")
    before = [ln for ln in (src / "system" / "snappyHexMeshDict")
              .read_text().splitlines() if not keys.search(ln)]
    after = [ln for ln in dict_text.splitlines() if not keys.search(ln)]
    assert before == after
    assert (dst / "system" / "decomposeParDict").read_text() == \
        (src / "system" / "decomposeParDict").read_text()


@needs_bash
def test_wallref_refuses_an_absolutely_sized_source(root: Path, tmp_path: Path):
    """0.4 as a fraction is a near-wall cell; 0.4 as metres is a quarter hull."""
    src = _make_source_case(root, "kcs_companion")
    d = src / "system" / "snappyHexMeshDict"
    d.write_text(d.read_text().replace("relativeSizes       true;",
                                       "relativeSizes       false;"))
    r = _run("setup_wallref", root, tmp_path,
             "kcs_companion", "kcs_companion_wallref")
    assert r.returncode != 0, "relative thicknesses applied to an absolute dict"


@needs_bash
def test_yplus_build_derives_the_absolute_first_layer_thickness(
        root: Path, tmp_path: Path):
    """2 * 200 * 1.1418e-6 / sqrt(0.059242) = 1.8764e-03 m."""
    _make_source_case(root, "kcs_production")
    r = _run("setup_yplus", root, tmp_path, "kcs_production", "kcs_prod_yplus")
    assert r.returncode == 0, r.stdout + r.stderr
    d = (root / "kcs_cases" / "kcs_prod_yplus" / "system"
         / "snappyHexMeshDict").read_text()
    m = re.search(r"firstLayerThickness\s+([0-9.eE+-]+);", d)
    assert m, d
    assert abs(float(m.group(1)) - 1.8764e-03) / 1.8764e-03 < 0.01, m.group(1)
    assert re.search(r"relativeSizes\s+false;", d)
    assert "finalLayerThickness" not in d, \
        "a relative thickness survived the flip to absolute sizing"
    assert re.search(r"nSurfaceLayers\s+6;", d)
    assert re.search(r"minThickness\s+2\.0e-04;", d)


@needs_bash
def test_yplus_build_retargets_the_thickness_when_the_target_changes(
        root: Path, tmp_path: Path):
    """Halving the target y+ must halve the first layer — proof it is derived."""
    _make_source_case(root, "kcs_production")
    env = dict(os.environ)
    env.update(DM_CFD_ROOT=str(root), DM_CFD_CONFIG=str(CONFIG),
               CFD_LOG=str(tmp_path / "l"), CFD_MARKER=str(tmp_path / "m"),
               YPLUS_TARGET="100")
    r = subprocess.run(["bash", str(SCRIPTS["setup_yplus"]),
                        "kcs_production", "kcs_prod_yplus"],
                       env=env, cwd=str(tmp_path), capture_output=True,
                       text=True, timeout=300)
    assert r.returncode == 0, r.stdout + r.stderr
    d = (root / "kcs_cases" / "kcs_prod_yplus" / "system"
         / "snappyHexMeshDict").read_text()
    m = re.search(r"firstLayerThickness\s+([0-9.eE+-]+);", d)
    assert m and abs(float(m.group(1)) - 0.9382e-03) / 0.9382e-03 < 0.01, d


@needs_bash
def test_yplus_build_matches_the_decomposition_to_the_registered_ranks(
        root: Path, tmp_path: Path):
    """kcs_prod_yplus is registered at 16 ranks; the source is split for 8."""
    _make_source_case(root, "kcs_production")
    r = _run("setup_yplus", root, tmp_path, "kcs_production", "kcs_prod_yplus")
    assert r.returncode == 0, r.stdout + r.stderr
    dp = (root / "kcs_cases" / "kcs_prod_yplus" / "system"
          / "decomposeParDict").read_text()
    m = re.search(r"numberOfSubdomains\s+(\d+);", dp)
    assert m and int(m.group(1)) == 16, dp
    v = re.search(r"^\s*n\s+\(([\d\s]+)\);", dp, re.M)
    assert v, dp
    nums = [int(x) for x in v.group(1).split()]
    assert nums[0] * nums[1] * nums[2] == 16, \
        f"prod(n)={nums} != 16; decomposePar would exit fatally"
