# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Offline G4 regression cases; all example quantities are synthetic.

Criteria source: diffraction knowledge pack r4, section 5, AQG r0 section 8.
Expected arithmetic is written in individual docstrings; no expected result is
obtained from the implementation. These tests do not qualify a native solve.
"""

import json
import math
import subprocess
import sys
from pathlib import Path

import pytest

from digitalmodel.aqg.diffraction import (
    Artifact,
    BodySurface,
    InteriorLid,
    Check,
    NativeRemoval,
    Status,
    assess,
    check_mass,
    check_panels,
    check_resources,
    check_irregular_frequencies,
    reviewed_check,
)


@pytest.fixture
def artifact():
    return Artifact("synthetic-evidence", b"Synthetic example, not a native result.\n")


def test_empty_record_never_infers_acceptance():
    report = assess([])
    assert len(report.checks) == 26  # Knowledge pack r4, section 5: 26 rows.
    assert report.status == Status.NOT_RUN
    assert all(c.status == Status.NOT_RUN for c in report.checks)
    assert all(c.criterion and c.units and c.reason for c in report.checks)


def test_mass_uses_input_mass_denominator(artifact):
    """abs(100-99)/100 = 0.01 passes; /99 would exceed 0.01.

    Conversely, abs(100-101.01)/100 = 0.0101 fails, while normalization
    by 101.01 would pass. Density 1 makes volume equal displaced mass.
    Limit 0.01: source G4.1a; quantities are chosen hand examples.
    """
    assert check_mass(100, 99, 1, artifact).status == Status.PASS
    assert check_mass(100, 101.01, 1, artifact).status == Status.FAIL


@pytest.mark.parametrize("value", [0, -1, math.nan, math.inf, True])
def test_invalid_mass_input_is_rejected(value, artifact):
    with pytest.raises(ValueError):
        check_mass(value, 99, 1, artifact)


def test_panels_require_all_ratios_and_wavelength(artifact):
    """For lambda=12 m, dimension=2 m equals lambda/6.

    Aspect 3 is inclusive; 5 needs a second-order waterline note.
    Limits: source G4.4b and G4.4c. All geometry is synthetic.
    """
    checks = check_panels(
        [3, 5],
        [False, True],
        2,
        12,
        artifact,
        second_order=True,
        waterline_note="Refined waterline reviewed",
    )
    assert [c.status for c in checks] == [Status.PASS, Status.PASS]
    checks = check_panels([3, 5], [False, True], 2.1, 12, artifact)
    assert [c.status for c in checks] == [Status.FAIL, Status.FAIL]
    assert (
        check_panels(
            [5], [False], 2, 12, artifact, second_order=True, waterline_note="note"
        )[0].status
        == Status.FAIL
    )


@pytest.mark.parametrize(
    "ratios,flags",
    [([], []), ([3], []), ([math.nan], [True]), ([0], [True]), ([3], ["yes"])],
)
def test_invalid_panel_evidence_is_rejected(ratios, flags, artifact):
    with pytest.raises(ValueError):
        check_panels(ratios, flags, 2, 12, artifact)


def test_resource_limits_are_explicit(artifact):
    """10 panels=limit 10; 100 bytes=available 100 passes; 101 fails.

    Counts are synthetic caller limits, not universal program limits.
    Comparison criterion: source G4.4a and G4.4e.
    """
    checks = check_resources(10, 10, 100, 100, "installed-version manual", artifact)
    assert all(c.status == Status.PASS for c in checks)
    checks = check_resources(11, 10, 101, 100, "installed-version manual", artifact)
    assert all(c.status == Status.FAIL for c in checks)
    with pytest.raises(ValueError):
        check_resources(10, 10, 100, 100, "", artifact)


def proof(program="aqwa", enabled=True, text="REMOVE YES", setting="REMOVE YES"):
    native = Artifact(program, text.encode())
    return NativeRemoval(
        program,
        "synthetic-version",
        native,
        setting,
        enabled,
        "independent reviewer",
        result=Artifact("native-readback", b"Synthetic result linkage"),
        result_input_sha256=native.sha256,
    )


def test_frequency_triggers_and_paired_native_settings(artifact):
    """Supplied omega_i=10 rad/s gives lid trigger 0.9*10=9 rad/s.

    Source G4.7a/b: at 9 a lid is required; at 10 both programs need
    enabled native evidence. No estimate or program default is inferred.
    """
    assert (
        check_irregular_frequencies(
            8, 10, artifact, bodies=(BodySurface("body-1", False),)
        )[0].status
        == Status.PASS
    )
    assert check_irregular_frequencies(9, 10, artifact)[0].status == Status.NOT_RUN
    checks = check_irregular_frequencies(
        10,
        10,
        artifact,
        compared_programs=("aqwa", "orcawave"),
        bodies=surfaces(artifact),
        removal=(proof(), proof("orcawave")),
    )
    assert all(c.status == Status.PASS for c in checks)
    assert (
        check_irregular_frequencies(
            10,
            10,
            artifact,
            compared_programs=("aqwa", "orcawave"),
            bodies=surfaces(artifact),
        )[1].status
        == Status.NOT_RUN
    )
    assert (
        check_irregular_frequencies(
            10,
            10,
            artifact,
            compared_programs=("aqwa", "orcawave"),
            bodies=surfaces(artifact),
            removal=(proof(), proof("orcawave", False)),
        )[1].status
        == Status.FAIL
    )


def test_native_proof_requires_actual_line_and_distinct_programs(artifact):
    with pytest.raises(ValueError):
        proof(text="REMOVE NO")
    with pytest.raises(ValueError):
        proof(text="REMOVE YES_DISABLED")
    with pytest.raises(ValueError):
        check_irregular_frequencies(
            10,
            10,
            artifact,
            compared_programs=("aqwa", "orcawave"),
            bodies=surfaces(artifact),
            removal=(proof(), proof("AQWA")),
        )


def test_unreviewed_native_treatment_remains_not_run(artifact):
    unknown = proof("orcawave", enabled=None)
    result = check_irregular_frequencies(
        10,
        10,
        artifact,
        compared_programs=("aqwa", "orcawave"),
        bodies=surfaces(artifact),
        removal=(proof(), unknown),
    )
    assert result[1].status == Status.NOT_RUN


def test_reviewed_rows_need_explicit_applicability_and_evidence(artifact):
    with pytest.raises(ValueError):
        reviewed_check("G4.2a", True, True, (), "volume comparison", "reviewer")
    row = reviewed_check(
        "G4.2a", True, None, (artifact,), "Native value missing", "reviewer"
    )
    assert row.status == Status.NOT_RUN
    row = reviewed_check(
        "G4.1b",
        False,
        None,
        (artifact,),
        "Free flotation only",
        "reviewer",
        equilibrium_mode="free_floating",
    )
    assert row.status == Status.NOT_APPLICABLE
    assert not row.applicable


def test_complete_record_requires_every_row(artifact):
    """Synthetic review receipts exercise aggregation, not engineering acceptance."""
    rows = complete_rows(artifact)
    assert assess(rows).record_status == Status.PASS
    assert assess(rows).status == Status.NOT_RUN
    assert assess(rows[:-1]).status == Status.NOT_RUN
    rows[0] = check_mass(100, 102, 1, artifact)
    assert assess(rows[:-1]).status == Status.FAIL


def test_duplicate_and_packaging_records_are_rejected(artifact):
    row = check_mass(100, 99, 1, artifact)
    with pytest.raises(ValueError):
        assess([row, row])
    with pytest.raises((TypeError, ValueError)):
        assess([{"status": "PASS"}])
    with pytest.raises(ValueError):
        reviewed_check("mesh_status", True, True, (artifact,), "screen", "reviewer")
    with pytest.raises(ValueError):
        Check("G4.1a", True, Status.PASS, {}, (), "no evidence")


def test_json_roundtrip_carries_source_and_evidence_digests(artifact):
    """NIST SHA-256 worked example, message 'abc', final hash.

    https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/SHA256.pdf
    Opened source: the final hash is printed after round 63.
    """
    vector = Artifact("known-answer", b"abc")
    assert (
        vector.sha256
        == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )
    report = assess([check_mass(100, 99, 1, artifact)])
    payload = json.loads(json.dumps(report.to_dict(), allow_nan=False))
    assert payload["status"] == "NOT_RUN"
    assert payload["criteria_source"]["revision"] == "4"
    assert len(payload["criteria_source"]["sha256"]) == 64
    assert payload["checks"][0]["evidence"][0]["sha256"] == artifact.sha256
    assert payload["checks"][0]["units"] == "1"


def test_offline_package_never_imports_solver_dependencies():
    """Inspect loaded modules; the fresh process invokes no licensed import."""
    script = """
import sys
from digitalmodel.aqg.diffraction import assess
assert assess([]).status.value == 'NOT_RUN'
assert not any(n.startswith(('OrcFxAPI', 'digitalmodel.hydrodynamics')) for n in sys.modules)
"""
    subprocess.run([sys.executable, "-c", script], check=True, capture_output=True)


def test_worked_example():
    example = Path(__file__).resolve().parents[3] / "examples/aqg/diffraction_g4.py"
    run = subprocess.run(
        [sys.executable, str(example)], check=True, capture_output=True, text=True
    )
    payload = json.loads(run.stdout)
    assert payload["status"] == "NOT_RUN"
    assert len(payload["checks"]) == 26
    assert payload["checks"][0]["observed"]["relative_mass_residual"] == pytest.approx(
        0.01
    )


def test_unsupported_exclusions_cannot_hide_required_rows(artifact):
    for row in assess([]).checks:
        if row.check_id not in {"G4.1b", "G4.7b"}:
            with pytest.raises(ValueError):
                reviewed_check(
                    row.check_id, False, None, (artifact,), "Excluded", "reviewer"
                )


def test_native_settings_without_native_result_linkage_remain_not_run(artifact):
    native = NativeRemoval(
        "orcawave",
        "version",
        artifact,
        artifact.content.decode().strip(),
        True,
        "reviewer",
    )
    rows = check_irregular_frequencies(
        10,
        10,
        artifact,
        compared_programs=("aqwa", "orcawave"),
        bodies=surfaces(artifact),
        removal=(proof(), native),
    )
    assert rows[1].status == Status.NOT_RUN


@pytest.mark.parametrize(
    "line", ["# REMOVE YES", "! REMOVE YES", "// REMOVE YES", "; REMOVE YES"]
)
def test_commented_native_setting_is_rejected(line):
    with pytest.raises(ValueError):
        proof(text=line, setting=line)


def test_missing_source_fails_closed(monkeypatch, artifact):
    from digitalmodel.aqg import diffraction

    monkeypatch.setattr(diffraction, "SOURCE_DIGEST", "0" * 64)
    with pytest.raises(ValueError, match="criteria source"):
        check_mass(100, 99, 1, artifact)


def test_native_pair_must_match_the_actual_comparison(artifact):
    rows = check_irregular_frequencies(
        10,
        10,
        artifact,
        bodies=surfaces(artifact),
        removal=(proof(), proof("orcawave")),
    )
    assert rows[1].status == Status.NOT_RUN
    with pytest.raises(ValueError):
        proof("alternative-name")


def test_exclusion_requires_declared_free_flotation(artifact):
    with pytest.raises(ValueError):
        reviewed_check("G4.1b", False, None, (artifact,), "not needed", "reviewer")
    with pytest.raises(ValueError):
        Check("G4.7b", False, Status.NOT_APPLICABLE, {}, (artifact,), "excluded")


def complete_rows(artifact):
    numeric = [check_mass(100, 99, 1, artifact)]
    numeric.extend(check_panels([3], [False], 2, 12, artifact))
    numeric.extend(check_resources(10, 10, 100, 100, "synthetic limit", artifact))
    numeric.extend(
        check_irregular_frequencies(
            10,
            10,
            artifact,
            bodies=surfaces(artifact),
            removal=(proof(), proof("orcawave")),
            compared_programs=("aqwa", "orcawave"),
        )
    )
    computed = {row.check_id for row in numeric}
    return numeric + [
        reviewed_check(
            c.check_id, True, True, (artifact,), "Synthetic receipt only", "reviewer"
        )
        for c in assess([]).checks
        if c.check_id not in computed
    ]


def test_assessment_constructor_cannot_override_engineering_status(artifact):
    from digitalmodel.aqg.diffraction import Assessment

    with pytest.raises((ValueError, TypeError)):
        Assessment((), Status.PASS, Status.PASS)
    with pytest.raises(ValueError):
        Assessment(())
    assert (
        Assessment(tuple(assess(complete_rows(artifact)).checks)).status
        == Status.NOT_RUN
    )


@pytest.mark.parametrize(
    "check_id", ["G4.1a", "G4.4a", "G4.4b", "G4.4c", "G4.4e", "G4.7a", "G4.7b"]
)
def test_computed_rows_cannot_be_replaced_with_generic_reviews(check_id, artifact):
    with pytest.raises(ValueError):
        reviewed_check(check_id, True, True, (artifact,), "generic review", "reviewer")
    with pytest.raises(ValueError):
        Check(
            check_id,
            True,
            Status.PASS,
            {"reviewer_role": "reviewer"},
            (artifact,),
            "forged",
        )


def test_incomplete_evidence_is_validated_before_serialization(artifact):
    with pytest.raises(ValueError):
        reviewed_check("G4.2a", True, None, (object(),), "not run", "reviewer")
    evidence = (item for item in (artifact,))
    row = reviewed_check("G4.2a", True, True, evidence, "review", "reviewer")
    assert row.to_dict()["evidence"] == [artifact.to_dict()]


def test_inventory_matches_the_pinned_source():
    import re
    from digitalmodel.aqg.diffraction import criteria_source

    root = Path(__file__).resolve().parents[3]
    source = root / "src/digitalmodel" / criteria_source()["document"]
    expected = re.findall(r"^\| (G4\.\d[a-z])\s*\|", source.read_text(), re.MULTILINE)
    assert tuple(c.check_id for c in assess([]).checks) == tuple(expected)


def surfaces(artifact):
    return (BodySurface("body-1", False, InteriorLid((1,), artifact, "reviewer")),)


@pytest.mark.parametrize(
    "volume,expected",
    [(0.99, Status.PASS), (0.9901, Status.PASS), (0.9899, Status.FAIL)],
)
def test_decimal_mass_boundaries(volume, expected, artifact):
    """M=1,rho=1: residuals .01, .0099 and .0101 against .01.

    Source G4.1a. The input decimal representations define the arithmetic;
    no engineering tolerance is introduced to hide floating-point error.
    """
    assert check_mass(1, volume, 1, artifact).status == expected


@pytest.mark.parametrize(
    "dimension,expected",
    [(0.1, Status.PASS), (0.0999, Status.PASS), (0.1001, Status.FAIL)],
)
def test_decimal_resolution_boundaries(dimension, expected, artifact):
    """Source G4.4c: .6/6=.1 m; .0999 is below and .1001 above."""
    assert check_panels([1], [False], dimension, 0.6, artifact)[1].status == expected


@pytest.mark.parametrize(
    "maximum,expected",
    [(0.09, Status.NOT_RUN), (0.0899, Status.PASS), (0.0901, Status.NOT_RUN)],
)
def test_decimal_lid_trigger_boundaries(maximum, expected, artifact):
    """Source G4.7a: .9*.1=.09 rad/s, inclusive trigger, no lid."""
    rows = check_irregular_frequencies(
        maximum, 0.1, artifact, bodies=(BodySurface("body-1", False),)
    )
    assert rows[0].status == expected


def test_lid_and_control_surface_coverage_is_per_body(artifact):
    """Source G4.7a: each control-surface body needs a resolvable surface."""
    lid = InteriorLid((1,), artifact, "reviewer")
    bodies = (BodySurface("body-1", False, lid), BodySurface("body-2", True, lid))
    assert (
        check_irregular_frequencies(10, 10, artifact, bodies=bodies)[0].status
        == Status.NOT_RUN
    )
    complete = (bodies[0], BodySurface("body-2", True, lid, artifact))
    assert (
        check_irregular_frequencies(10, 10, artifact, bodies=complete)[0].status
        == Status.PASS
    )
    # Even below the lid trigger, the declared control-surface method requires evidence.
    assert (
        check_irregular_frequencies(8, 10, artifact, bodies=bodies)[0].status
        == Status.NOT_RUN
    )
    assert check_irregular_frequencies(8, 10, artifact)[0].status == Status.NOT_RUN
    with pytest.raises(ValueError):
        check_irregular_frequencies(8, 10, artifact, bodies=(bodies[0], bodies[0]))


def test_axis_volume_spread_criterion_is_dimensionless():
    assert (
        next(row for row in assess([]).checks if row.check_id == "G4.2b").units == "1"
    )
