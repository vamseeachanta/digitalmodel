"""TDD for the #1528 slice-7 outlet/vent placement study helpers.

Issue #1528 slice 7 recorded this open item:

    "the rendered outlet is a full-height pressure opening, so on a static tank
     it bleeds liquid under hydrostatic head - a clean tank-volume balance needs
     an above-waterline vent (or an outlet placed above the free surface). The
     inlet-flux integral is the robust transfer metric; the outlet placement is
     a modeling refinement to settle before the full 144-case coupled matrix."

These tests pin the *pure* helpers of the study driver
(``scripts/cfd/run_outlet_vent_study.py``): boundary-condition construction per
variant and post-run ``volFieldValue`` reduction. No OpenFOAM binary is invoked
here; the solver half of the study runs on the dedicated CFD node.
"""

from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.models import BoundaryType


_SCRIPT = (
    Path(__file__).resolve().parents[3] / "scripts" / "cfd" / "run_outlet_vent_study.py"
)


def _load_module():
    spec = importlib.util.spec_from_file_location("run_outlet_vent_study", _SCRIPT)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def study():
    return _load_module()


def _by_patch_field(bcs):
    return {(bc.patch_name, bc.field): bc for bc in bcs}


# ---------------------------------------------------------------------------
# Variant construction
# ---------------------------------------------------------------------------


def test_baseline_variant_is_the_frozen_full_height_pressure_outlet(study):
    """V0 is the control: the full-height pressure outlet, frozen as a literal.

    This deliberately does NOT compare against
    ``_exchange_boundary_conditions``.  That helper now emits the settled vent
    placement, so a control defined by it would become a copy of the treatment
    and this assertion would still pass while comparing the vent against itself.
    """
    actual = study.outlet_vent_boundary_conditions(19.2, study.VARIANT_BASELINE)

    assert {(b.patch_name, b.field, b.bc_type) for b in actual} == {
        ("inlet", "U", BoundaryType.FLOW_RATE_INLET_VELOCITY),
        ("inlet", "p_rgh", BoundaryType.ZERO_GRADIENT),
        ("inlet", "alpha.water", BoundaryType.INLET_OUTLET),
        ("outlet", "U", BoundaryType.PRESSURE_INLET_OUTLET_VELOCITY),
        ("outlet", "p_rgh", BoundaryType.TOTAL_PRESSURE),
        ("outlet", "alpha.water", BoundaryType.INLET_OUTLET),
    }


def test_the_control_and_the_treatment_are_not_the_same_configuration(study):
    """Guards the degenerate study: a control equal to the treatment proves nothing."""
    control = study.outlet_vent_boundary_conditions(19.2, study.VARIANT_BASELINE)
    treatment = study.outlet_vent_boundary_conditions(19.2, study.VARIANT_VENT_TOP)

    assert {(b.patch_name, b.field, b.bc_type) for b in control} != {
        (b.patch_name, b.field, b.bc_type) for b in treatment
    }


def test_vent_top_variant_removes_the_submerged_pressure_outlet(study):
    """V1 must close the full-height outlet so hydrostatic head cannot bleed."""
    bcs = _by_patch_field(
        study.outlet_vent_boundary_conditions(19.2, study.VARIANT_VENT_TOP)
    )

    assert bcs[("outlet", "U")].bc_type is BoundaryType.NO_SLIP
    assert bcs[("outlet", "p_rgh")].bc_type is BoundaryType.FIXED_FLUX_PRESSURE
    assert bcs[("outlet", "alpha.water")].bc_type is BoundaryType.ZERO_GRADIENT

    # No patch that sits below the free surface may carry a pressure opening.
    assert all(
        bc.bc_type is not BoundaryType.TOTAL_PRESSURE
        for (patch, _field), bc in bcs.items()
        if patch == "outlet"
    )


def test_vent_top_variant_puts_the_atmosphere_opening_on_the_top_patch(study):
    """The vent must live on ``top`` (z=max), which is above any fill < 1.0."""
    bcs = _by_patch_field(
        study.outlet_vent_boundary_conditions(19.2, study.VARIANT_VENT_TOP)
    )

    assert bcs[("top", "p_rgh")].bc_type is BoundaryType.TOTAL_PRESSURE
    assert bcs[("top", "U")].bc_type is BoundaryType.PRESSURE_INLET_OUTLET_VELOCITY
    assert bcs[("top", "alpha.water")].bc_type is BoundaryType.INLET_OUTLET
    # Air, not water, is drawn back in through the vent.
    assert bcs[("top", "alpha.water")].extra["inletValue"] == "uniform 0"


def test_vent_variant_preserves_the_area_independent_transfer_metric(study):
    """PR #1544's volumetric-flow-rate inlet must survive the vent change."""
    for variant in (study.VARIANT_BASELINE, study.VARIANT_VENT_TOP):
        bcs = _by_patch_field(study.outlet_vent_boundary_conditions(19.19576571, variant))
        inlet_u = bcs[("inlet", "U")]
        assert inlet_u.bc_type is BoundaryType.FLOW_RATE_INLET_VELOCITY
        assert inlet_u.extra["volumetricFlowRate"] == "constant 19.19576571"


def test_unknown_variant_fails_closed(study):
    with pytest.raises(ValueError, match="unknown outlet/vent variant"):
        study.outlet_vent_boundary_conditions(19.2, "not-a-variant")


# ---------------------------------------------------------------------------
# Post-run reduction
# ---------------------------------------------------------------------------


_VOL_DAT = """\
# Region type : cellZone all
# Faces  : 12800
# Time            volIntegrate(alpha.water)
0               600.0000000000
0.05            599.9999820000
0.10            599.9994000000
"""


def test_parse_vol_field_value_skips_comments_and_returns_series(study, tmp_path):
    dat = tmp_path / "volFieldValue.dat"
    dat.write_text(_VOL_DAT)

    series = study.parse_vol_field_value(dat)

    assert [t for t, _ in series] == [0.0, 0.05, 0.10]
    assert series[0][1] == pytest.approx(600.0)
    assert series[-1][1] == pytest.approx(599.9994)


def test_volume_drift_percent_is_signed_and_relative_to_the_initial_volume(study):
    series = [(0.0, 600.0), (1.0, 597.0)]
    drift = study.volume_drift_percent(series)
    assert drift == pytest.approx(-0.5)


def test_volume_drift_percent_fails_closed_on_a_degenerate_series(study):
    with pytest.raises(ValueError):
        study.volume_drift_percent([])
    with pytest.raises(ValueError):
        study.volume_drift_percent([(0.0, 0.0), (1.0, 1.0)])


def test_verdict_requires_the_vent_to_hold_volume_better_than_the_baseline(study):
    """The study's decision rule, pinned so the report cannot silently invert."""
    verdict = study.decide_outlet_placement(
        baseline_drift_pct=-4.2, vent_drift_pct=-0.001, tolerance_pct=0.05
    )
    assert verdict["vent_holds_volume"] is True
    assert verdict["baseline_holds_volume"] is False
    assert verdict["recommended_variant"] == study.VARIANT_VENT_TOP

    # If the baseline were fine, the study must not manufacture a change.
    verdict2 = study.decide_outlet_placement(
        baseline_drift_pct=-0.002, vent_drift_pct=-0.001, tolerance_pct=0.05
    )
    assert verdict2["baseline_holds_volume"] is True
    assert verdict2["recommended_variant"] == study.VARIANT_BASELINE


# ---------------------------------------------------------------------------
# Tolerance provenance
# ---------------------------------------------------------------------------
#
# The verdict must not be judged by a threshold invented for this study, and it
# must not be a number typed on a command line at run time.  It is bound to the
# tolerance the repo already applies to exactly these cases:
# ``ExtractionConfig.mass_balance_rtol`` in
# ``src/digitalmodel/solvers/openfoam/time_history.py``, which decides
# ``mass_balance_ok`` for the extracted time histories.
#
# The expected value below is a hand-written literal, NOT read back from
# ``ExtractionConfig``.  A test that computed its expectation from the same
# constant the implementation reads could never fail.  Pinning the literal means
# that if anyone retunes the repo's mass-balance tolerance, this test fails and
# forces a deliberate decision about the #1528 verdict rather than silently
# moving the goalposts under a published result.


def test_default_tolerance_pct_is_the_repo_mass_balance_rtol_in_percent(study):
    """Drift alarm: pins the shipped value against a hand-written literal."""
    assert study.DEFAULT_TOLERANCE_PCT == 0.1


def test_the_tolerance_is_computed_from_the_config_not_hardcoded(study):
    """Provenance: feed a DIFFERENT config and the tolerance must follow it.

    The pair of assertions above and below is deliberate. Pinning ``== 0.1``
    alone does not test provenance: replacing the derivation with a bare literal
    ``0.1`` keeps that assertion green, because the derived value happens to BE
    0.1. Only driving the helper with a config whose rtol is not 1.0e-3 can tell
    a real derivation from a magic number that coincides with it.
    """
    from digitalmodel.solvers.openfoam.time_history import ExtractionConfig

    other = ExtractionConfig(mass_balance_rtol=2.5e-3)
    assert study.default_tolerance_pct(other) == pytest.approx(0.25)


def test_parser_defaults_the_tolerance_so_it_is_not_typed_at_run_time(study):
    args = study.build_parser().parse_args(
        ["--work-root", "/tmp/wr", "--output", "/tmp/out.json"]
    )
    assert args.tolerance_pct == 0.1


def test_resonant_build_preserves_motion_and_zero_flow(study, tmp_path):
    """Two requested cycles last 2*T; T is the selected sweep input.

    Existing synthetic geometry and motion inputs are specified in the static
    study's _coupling_spec; no new empirical coefficient is introduced here.
    """
    row = study._select_sweep_case("worst-fill-resonant")
    case_dir, _ = study._build_case(
        tmp_path, study.VARIANT_VENT_TOP, 0.0, 2 * row.roll_period_s, 1,
        sweep_case=row, motion="resonant",
    )
    dynamic = (case_dir / "constant" / "dynamicMeshDict").read_text()
    assert "oscillatingRotatingMotion" in dynamic
    import re

    assert re.search(r"volumetricFlowRate\s+constant 0;", (case_dir / "0" / "U").read_text())


def test_static_build_still_removes_motion(study, tmp_path):
    case_dir, _ = study._build_case(tmp_path, study.VARIANT_VENT_TOP, 0, 10, 1)
    assert not (case_dir / "constant" / "dynamicMeshDict").exists()


def test_resonant_dry_run_records_inputs_without_claiming_solver_result(study, tmp_path):
    import json

    output = tmp_path / "manifest.json"
    assert study.main([
        "--work-root", str(tmp_path / "cases"), "--output", str(output),
        "--motion", "resonant", "--sweep-case", "worst-fill-resonant",
        "--cycles", "2", "--max-sample-gap-s", "0.1", "--ranks", "1",
        "--variants", "vent_top", "--dry-run",
    ]) == 0
    manifest = json.loads(output.read_text())
    assert manifest["motion"] == "resonant"
    assert manifest["selected_case"]["fill_fraction"] == 0.8
    assert manifest["selected_case"]["period_ratio"] == 1.0
    assert manifest["end_time_s"] == pytest.approx(
        2 * manifest["selected_case"]["roll_period_s"]
    )
    assert manifest["variants"]["vent_top"]["assessment"]["case_status"] == "NOT_EVALUATED"
    assert manifest["verdict"] is None


@pytest.mark.parametrize("options", [
    [], ["--cycles", "0", "--max-sample-gap-s", "0.1"],
    ["--cycles", "2"],
    ["--cycles", "2", "--max-sample-gap-s", "0.1", "--flow-rate", "1"],
    ["--cycles", "2", "--max-sample-gap-s", "nan"],
])
def test_resonant_mode_requires_explicit_run_plan(study, tmp_path, options):
    with pytest.raises(ValueError):
        study.main([
            "--work-root", str(tmp_path), "--output", str(tmp_path / "out.json"),
            "--motion", "resonant", "--dry-run", *options,
        ])


@pytest.mark.parametrize("with_runup", [False, True])
def test_completed_history_gate_and_offline_reduction(study, tmp_path, monkeypatch, with_runup):
    """Synthetic 20*6*10*.8=960 m3 hold; solver calls replaced by test doubles.

    Five samples span the requested duration; no elevation evidence exists.
    """
    import json

    row = study._select_sweep_case("worst-fill-resonant")
    duration = 2 * row.roll_period_s
    monkeypatch.setattr(study, "_require_openfoam_environment", lambda: None)
    monkeypatch.setattr(study, "_run_case", lambda *args: None)
    monkeypatch.setattr(study, "_volume_series", lambda *args: [
        (i * duration / 4, 960.0) for i in range(5)
    ])
    output = tmp_path / "manifest.json"
    args = [
        "--work-root", str(tmp_path / "cases"), "--output", str(output),
        "--motion", "resonant", "--sweep-case", "worst-fill-resonant",
        "--cycles", "2", "--max-sample-gap-s", str(duration / 2),
        "--ranks", "1", "--variants", "vent_top",
    ]
    result = study.main(args)
    assert result == 2
    manifest = json.loads(output.read_text())
    assert manifest["variants"]["vent_top"]["assessment"]["case_status"] == "NOT_EVALUATED"
    assert manifest["matrix_status"] == "NOT_EVALUATED"
    assert manifest["verdict"] is None
    if with_runup:
        evidence = tmp_path / "runup.json"
        evidence.write_text(json.dumps(dict(
            case_id=row.case_id, variant="vent_top", frame="tank",
            start_time_s=0, end_time_s=duration, peak_elevation_m=9.5,
            source="Synthetic spatial maximum from the independent geometry example",
        )))
        args += ["--runup-evidence", str(evidence)]

    def forbidden(*args, **kwargs):
        raise AssertionError("Offline reduction must not rebuild or run a solver")

    monkeypatch.setattr(study, "_build_case", forbidden)
    monkeypatch.setattr(study, "_run_case", forbidden)
    monkeypatch.setattr(study, "_require_openfoam_environment", forbidden)
    assert study.main([*args, "--reduce-only"]) == (0 if with_runup else 2)

    # A changed input must invalidate reuse, even if its old history still exists.
    case_dir = Path(manifest["variants"]["vent_top"]["case_directory"])
    (case_dir / "constant" / "dynamicMeshDict").write_text("changed input\n")
    with pytest.raises(ValueError, match="digest"):
        study.main([*args, "--reduce-only"])


@pytest.fixture
def completed_resonant_hold(study, tmp_path, monkeypatch):
    """Build both variants with synthetic histories and no solver execution."""
    import json

    row = study._select_sweep_case("worst-fill-resonant")
    duration = 2 * row.roll_period_s

    def synthetic_run(case_dir, ranks):
        import re

        alpha = case_dir / "0" / "alpha.water"
        text, count = re.subn(
            r"internalField\s+uniform 0;",
            "internalField nonuniform List<scalar>\n4\n(0 1 1 0)\n;", alpha.read_text(),
        )
        assert count == 1
        text = text.replace("    object", '    location "0";\n    object')
        alpha.write_text(text.replace("    ", "\t"))
        mesh = case_dir / "constant" / "polyMesh" / "points"
        mesh.parent.mkdir(parents=True)
        mesh.write_text("Synthetic initialization-generated mesh\n")
        history = case_dir / "postProcessing" / "liquidVolume" / "0" / "volFieldValue.dat"
        history.parent.mkdir(parents=True)
        history.write_text("".join(f"{i * duration / 4} 960.0\n" for i in range(5)))

    monkeypatch.setattr(study, "_require_openfoam_environment", lambda: None)
    monkeypatch.setattr(study, "_run_case", synthetic_run)
    evidence = tmp_path / "runup.json"
    evidence.write_text(json.dumps(dict(
        case_id=row.case_id, variant="vent_top", frame="tank",
        start_time_s=0, end_time_s=duration, peak_elevation_m=9.5,
        source="Synthetic spatial maximum; no measured solver result",
    )))
    output = tmp_path / "manifest.json"
    args = [
        "--work-root", str(tmp_path / "cases"), "--output", str(output),
        "--motion", "resonant", "--sweep-case", "worst-fill-resonant",
        "--cycles", "2", "--max-sample-gap-s", str(duration / 2),
        "--ranks", "1", "--runup-evidence", str(evidence),
    ]
    assert study.main(args) == 2  # Only the vent has synthetic clearance evidence.

    def forbidden(*args, **kwargs):
        raise AssertionError("Offline reduction must not rebuild or run a solver")

    for name in ("_build_case", "_run_case", "_require_openfoam_environment"):
        monkeypatch.setattr(study, name, forbidden)
    return args, output


@pytest.mark.parametrize("change", ["inlet_value", "boundary_type", "dimensions", "missing"])
def test_reduction_rejects_changed_liquid_field(study, completed_resonant_hold, change):
    import json
    import re

    args, output = completed_resonant_hold
    original = output.read_bytes()
    manifest = json.loads(original)
    case_dir = Path(manifest["variants"]["vent_top"]["case_directory"])
    alpha = case_dir / "0" / "alpha.water"
    text = alpha.read_text()
    if change == "inlet_value":
        text, count = re.subn(r"(inletValue\s+uniform\s+)0;", r"\g<1>1;", text)
        assert count > 0
    elif change == "boundary_type":
        assert "inletOutlet" in text
        text = text.replace("inletOutlet", "fixedValue")
    elif change == "dimensions":
        assert "[0 0 0 0 0 0 0]" in text
        text = text.replace("[0 0 0 0 0 0 0]", "[0 1 0 0 0 0 0]")
    else:
        alpha.unlink()
    if change != "missing":
        alpha.write_text(text)

    with pytest.raises(ValueError, match="case input digest mismatch"):
        study.main([*args, "--variants", "vent_top", "--reduce-only"])
    assert output.read_bytes() == original


def test_subset_reduction_preserves_other_completed_variant(study, completed_resonant_hold):
    import json

    args, output = completed_resonant_hold
    before = json.loads(output.read_text())
    baseline = study.VARIANT_BASELINE
    vent = study.VARIANT_VENT_TOP
    assert study.main([*args, "--variants", vent, "--reduce-only"]) == 0
    after_vent = json.loads(output.read_text())
    assert after_vent["variants"][baseline] == before["variants"][baseline]
    assert after_vent["variants"][vent]["assessment"]["case_status"] == "PASS"

    # The unselected completed history remains available for a later reduction.
    assert study.main([*args, "--variants", baseline, "--reduce-only"]) == 2
    after_baseline = json.loads(output.read_text())
    assert after_baseline["variants"][vent] == after_vent["variants"][vent]
    assert after_baseline["variants"][baseline] == before["variants"][baseline]


def test_reduction_rejects_unterminated_liquid_field_string(study, completed_resonant_hold):
    import json

    args, output = completed_resonant_hold
    original = output.read_bytes()
    manifest = json.loads(original)
    case_dir = Path(manifest["variants"]["vent_top"]["case_directory"])
    alpha = case_dir / "0" / "alpha.water"
    alpha.write_text(alpha.read_text() + '\n"\n')

    with pytest.raises(ValueError, match="invalid liquid field"):
        study.main([*args, "--variants", "vent_top", "--reduce-only"])
    assert output.read_bytes() == original
