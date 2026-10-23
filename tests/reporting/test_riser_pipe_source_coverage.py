# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Source qualification is distinct from repository-module coverage."""

import json
from copy import deepcopy
from importlib import import_module

import pytest


@pytest.fixture
def builder():
    return import_module("scripts.reporting.build_riser_pipe_comparison")


@pytest.fixture
def criteria(builder):
    return json.loads(builder.CRITERIA.read_text())


def test_coverage_matches_independently_read_module_inventory(builder, criteria):
    """Expected sets are read from each strategy's run_checks, not this helper.

    Sources: api_rp_1111.py, api_rp_2rd.py and api_std_2rd.py in
    src/digitalmodel/structural/analysis/wall_thickness_codes at the module
    digests recorded in riser_pipe_comparison.criteria.json, revision 2.
    No engineering equation or numerical acceptance limit is evaluated here.
    """
    expected = {
        "API RP 1111": {"burst", "collapse", "propagation"},
        "API RP 2RD": {"burst", "collapse", "hoop"},
        "API STD 2RD": {"burst", "collapse", "combined"},
    }
    states = {"burst", "collapse", "propagation", "hoop", "combined"}
    coverage = builder.source_coverage(criteria)
    assert set(coverage) == set(expected)
    for code, implemented in expected.items():
        assert set(coverage[code]) == states
        for state, cell in coverage[code].items():
            assert cell["module_status"] == (
                "IMPLEMENTED" if state in implemented else "NOT_IMPLEMENTED"
            )
            assert cell["standard_status"] == "NOT_EVALUATED"
            assert cell["reason"]
            assert cell["required_evidence"]
            assert "utilisation" not in cell and "within_module_limit" not in cell


def test_every_cell_retains_recorded_source_and_unqualified_locator(builder, criteria):
    """Repository criteria are metadata evidence, never standard qualification."""
    original = deepcopy(criteria)
    for code, cells in builder.source_coverage(criteria).items():
        reference = criteria["codes"][code]
        for state, cell in cells.items():
            assert cell["module"] == reference["module"]
            assert cell["module_sha256"] == reference["module_sha256"]
            assert cell["title"] == reference["title"]
            assert cell["edition"] == reference["revision"]
            if state in reference["checks"]:
                assert cell["section"] == reference["checks"][state]["section"]
                assert "not checked against standard text" in cell["reason"]
            else:
                assert cell["section"] == "Not established: no module check"
                assert "applicability" in cell["required_evidence"]
    assert criteria == original


def test_locator_text_cannot_promote_a_check_to_qualified(builder, criteria):
    """Even a detailed repository locator lacks checked standard evidence."""
    criteria["codes"]["API STD 2RD"]["checks"]["burst"]["section"] = "Claimed complete locator"
    cell = builder.source_coverage(criteria)["API STD 2RD"]["burst"]
    assert cell["standard_status"] == "NOT_EVALUATED"


def test_source_matrix_is_written_to_page_sidecar_and_manifest(builder, tmp_path):
    """Read actual artifacts; missing checks must remain explicit, never zero."""
    manifest = builder.build(builder.INPUT, tmp_path)
    sidecar = json.loads((tmp_path / f"{builder.STEM}.citations.json").read_text())
    stored = json.loads((tmp_path / f"{builder.STEM}.manifest.json").read_text())
    assert stored["check_coverage"] == manifest["check_coverage"] == sidecar["check_coverage"]
    page = (tmp_path / f"{builder.STEM}.html").read_text()
    assert '<h2 id="source-coverage">Source qualification by limit state</h2>' in page
    for code, cells in sidecar["check_coverage"].items():
        for state, cell in cells.items():
            marker = f'data-coverage="{builder._code_key(code)}-{state}"'
            row = page.split(marker)[1].split("</tr>")[0]
            assert cell["module_status"] in row
            assert cell["standard_status"] in row
            assert cell["reason"] in row
            assert cell["required_evidence"] in row
    assert "An absent module check does not establish that the standard omits it" in page


def test_source_metadata_is_escaped_in_html(builder, criteria):
    """Supplied metadata must remain text in the generated report."""
    criteria["codes"]["API STD 2RD"]["revision"] = '<script>edition</script>'
    criteria["codes"]["API STD 2RD"]["checks"]["burst"]["section"] = '<b>clause</b>'
    table = builder._coverage_table(criteria)
    assert '<script>' not in table and '<b>clause</b>' not in table
    assert '&lt;script&gt;edition&lt;/script&gt;' in table
    assert '&lt;b&gt;clause&lt;/b&gt;' in table
