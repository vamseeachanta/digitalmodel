"""Test shards that read no docs/domains path check out without that tree.

``docs/domains`` holds about 1.3 GB of built HTML and solver files. A shard
listed in the ``Choose checkout scope`` step fetches file contents on demand
(``filter: blob:none``) and leaves that tree out of its work tree; every other
shard, including a shard added later, keeps the full checkout.
"""

import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

import pytest

from scripts.ci.detect_touched_domains import parse_domains
from tests.workflows.test_ci_triggers import load_workflow


ROOT = Path(__file__).resolve().parents[2]
JOBS = (("quality-gates-by-domain.yml", "domain-tests"), ("full-matrix-sweep.yml", "sweep-tests"))
DOMAINS = [domain.name for domain in parse_domains(ROOT / "tests/DOMAINS.md")]
# Any spelling of the path: "docs/domains", "docs" / "domains", ("docs", "domains").
DOCS_DOMAINS = re.compile(r"docs\W{1,6}domains")
SPARSE = ["/*", "!/docs/domains/"]


def scope_step(filename, job_id):
    steps = load_workflow(filename)["jobs"][job_id]["steps"]
    names = [step["name"] for step in steps]
    index = names.index("Choose checkout scope")
    assert names[index + 1] == "Checkout code", "the scope must be chosen just before the checkout"
    return steps[index], steps[index + 1]


def light_shards(filename, job_id):
    step, _ = scope_step(filename, job_id)
    arms = re.findall(r"^\s*([\w|-]+)\)\s*$", step["run"], re.M)
    assert len(arms) == 1, "expected one case arm listing the light shards"
    return arms[0].split("|")


def run_scope(step, domain, tmp_path):
    bash = shutil.which("bash")
    if bash is None:
        pytest.skip("bash is not available")
    output = tmp_path / f"output-{domain}"
    output.write_text("")
    env = {key: value for key, value in os.environ.items() if not key.startswith("GIT_")}
    env.update(GITHUB_OUTPUT=str(output), **{
        key: value.replace("${{ matrix.domain }}", domain) for key, value in step["env"].items()
    })
    subprocess.run([bash, "-c", step["run"]], env=env, check=True, capture_output=True, text=True)
    text = output.read_text()
    values, lines = {}, iter(text.splitlines())
    for line in lines:
        if "<<" in line:
            key, marker = line.split("<<", 1)
            block = []
            for item in lines:
                if item == marker:
                    break
                block.append(item)
            values[key] = block
        else:
            key, _, value = line.partition("=")
            values[key] = value
    return values


@pytest.mark.parametrize("filename,job_id", JOBS)
def test_checkout_reads_the_chosen_scope(filename, job_id):
    step, checkout = scope_step(filename, job_id)
    assert step["id"] == "checkout-scope"
    assert "if" not in step and "uses" not in step
    options = checkout["with"]
    assert str(options["fetch-depth"]) == "0", "history is still needed by the citation and subsea checks"
    assert options["filter"] == "${{ steps.checkout-scope.outputs.filter }}"
    assert options["sparse-checkout"] == "${{ steps.checkout-scope.outputs.sparse-checkout }}"
    assert options["sparse-checkout-cone-mode"] is False


def test_both_workflows_list_the_same_light_shards():
    lists = [light_shards(*job) for job in JOBS]
    assert lists[0] == lists[1]
    assert lists[0] == sorted(set(lists[0]))
    assert set(lists[0]) <= set(DOMAINS), "a light shard must name an existing domain"


@pytest.mark.parametrize("filename,job_id", JOBS)
@pytest.mark.parametrize("domain", DOMAINS)
def test_only_listed_shards_leave_docs_domains_out(filename, job_id, domain, tmp_path):
    step, _ = scope_step(filename, job_id)
    values = run_scope(step, domain, tmp_path)
    if domain in light_shards(filename, job_id):
        assert values == {"filter": "blob:none", "sparse-checkout": SPARSE}
    else:
        # Empty outputs leave actions/checkout at its defaults: every file, every blob.
        assert values == {}


def shard_inputs(domain):
    """Test files a shard selects, the helper modules beside them, the conftest files
    above them, and every file under a directory operand (fixtures included)."""
    sys.path.insert(0, str(ROOT / "scripts/ci"))
    try:
        import orphan_tests
    finally:
        sys.path.remove(str(ROOT / "scripts/ci"))
    text = (ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text(encoding="utf-8")
    command = dict(orphan_tests.domain_commands(ROOT, text))[domain]
    files, ignored = orphan_tests.candidates(ROOT)
    selected = orphan_tests.selected_paths(command, files, ignored, ROOT)
    inputs = {ROOT / path for path in selected}
    for path in selected:
        parent = (ROOT / path).parent
        # Helper modules beside a selected test; sibling test files belong to other shards.
        inputs.update(helper for helper in parent.glob("*.py")
                      if not helper.name.startswith("test_") and not helper.name.endswith("_test.py"))
        while parent != ROOT.parent:
            inputs.update(parent.glob("conftest.py"))
            parent = parent.parent
    for token in command.split():
        if token.startswith("tests/") and (ROOT / token).is_dir():
            inputs.update(path for path in (ROOT / token).rglob("*") if path.is_file())
    return inputs


@pytest.mark.parametrize("domain", light_shards(*JOBS[0]))
def test_light_shards_read_no_docs_domains_path(domain):
    readers = sorted(
        path.relative_to(ROOT).as_posix() for path in shard_inputs(domain)
        if DOCS_DOMAINS.search(path.read_text(encoding="utf-8", errors="ignore"))
    )
    assert readers == [], f"{domain} reads docs/domains; remove it from the light list"
