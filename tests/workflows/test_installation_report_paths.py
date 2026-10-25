"""Path expectations are derived by directory traversal, without solver inputs."""

from copy import deepcopy
from hashlib import sha256
from html.parser import HTMLParser
import json
import shutil
from urllib.parse import unquote, urljoin, urlsplit
from urllib.request import url2pathname

import pytest

from digitalmodel.workflows import installation_partial_report as partial
from digitalmodel.workflows import vessel_capability_layout as capability
from digitalmodel.workflows.installation_full_report import _display_cases, generate_report


class Links(HTMLParser):
    def __init__(self):
        super().__init__()
        self.targets = []

    def handle_starttag(self, tag, attrs):
        if tag == "a":
            self.targets.extend(value for key, value in attrs if key == "href")


def summary(run_dir):
    """Synthetic path-only fixture; no engineering result is qualified."""
    return dict(
        created_utc="retained snapshot", counts={"VERIFIED": 1}, envelopes=[],
        design_basis={}, matrix_sha256="matrix", campaign_sha256="campaign",
        cases=[dict(index=0, hs_m=1, tp_s=8, status="VERIFIED", channels={},
                    run_dir=run_dir, simulation_sha256="simulation",
                    trace_sha256="traces")],
    )


def render(kind, data, config, base):
    if kind == "partial":
        return partial.render_html(data, {}, base, config=config)
    if kind == "capability":
        return capability._appendices(data, base, config)
    return partial._case_details(_display_cases(data, config, base), base)


@pytest.mark.parametrize("kind", ["partial", "capability", "full"])
@pytest.mark.parametrize("retained", [r"Z:\retired\runs\case", "/retired/runs/case"])
def test_relocated_links_preserve_pinned_evidence(tmp_path, kind, retained):
    """Reports -> parent dataset -> runs/case -> extracted/report.html.

    This manual traversal gives ../runs/case/extracted/report.html, regardless
    of the recorded source drive. Source SHA-256 is compared before and after.
    """
    original = tmp_path / "original"
    target = original / "runs/case/extracted/report.html"
    target.parent.mkdir(parents=True)
    target.write_text("retained extraction", encoding="utf-8")
    source = original / "summary.json"
    source.write_text(json.dumps(summary(retained)), encoding="utf-8")
    digest = sha256(source.read_bytes()).hexdigest()
    relocated = tmp_path / "relocated"
    shutil.copytree(original, relocated)
    base = relocated / "reports"
    base.mkdir()
    data = json.loads((relocated / "summary.json").read_bytes())
    frozen = deepcopy(data)
    config = {"dataset_root": "..", "case_run_dirs": {"0": "runs/case"}}
    frozen_config = deepcopy(config)
    document = render(kind, data, config, base)
    links = Links()
    links.feed(document)
    expected = "../runs/case/extracted/report.html"
    assert expected in links.targets
    assert (base / unquote(expected)).read_text() == "retained extraction"
    assert retained not in document
    assert data == frozen
    assert config == frozen_config
    assert sha256(source.read_bytes()).hexdigest() == digest
    assert sha256((relocated / "summary.json").read_bytes()).hexdigest() == digest


@pytest.mark.parametrize("kind", ["partial", "capability", "full"])
@pytest.mark.parametrize("entry, expected", [
    ("runs/%2e%2e/%2e%2e/outside",
     "../runs/%252e%252e/%252e%252e/outside/extracted/report.html"),
    ("runs/case#1", "../runs/case%231/extracted/report.html"),
    ("runs/50%25load", "../runs/50%2525load/extracted/report.html"),
])
def test_mapped_links_encode_url_characters(tmp_path, kind, entry, expected):
    """Literal directory names remain inside the dataset after URL readback."""
    target = tmp_path / entry / "extracted/report.html"
    target.parent.mkdir(parents=True)
    target.write_text("retained extraction", encoding="utf-8")
    base = tmp_path / "reports"
    base.mkdir()
    data = summary("retained")
    frozen = deepcopy(data)
    config = {"dataset_root": "..", "case_run_dirs": {"0": entry}}
    frozen_config = deepcopy(config)
    links = Links()
    links.feed(render(kind, data, config, base))
    assert expected in links.targets
    resolved = urlsplit(urljoin((base / "full.html").as_uri(), expected))
    assert resolved.query == resolved.fragment == ""
    assert url2pathname(resolved.path) == str(target)
    assert (base / unquote(expected)).read_text() == "retained extraction"
    assert data == frozen
    assert config == frozen_config


@pytest.mark.parametrize("kind", ["partial", "capability", "full"])
def test_links_through_symlinked_dataset_remain_portable(tmp_path, kind):
    """A linked dataset and its copied tree share the same relative report link."""
    dataset = tmp_path / "physical"
    target = dataset / "runs/case/extracted/report.html"
    target.parent.mkdir(parents=True)
    target.write_text("retained extraction", encoding="utf-8")
    (dataset / "reports").mkdir()
    linked = tmp_path / "linked"
    try:
        linked.symlink_to(dataset, target_is_directory=True)
    except OSError as error:
        pytest.skip(f"Directory symlink unavailable: {error}")
    data = summary("retained")
    frozen = deepcopy(data)
    config = {"dataset_root": "..", "case_run_dirs": {"0": "runs/case"}}
    document = render(kind, data, config, linked / "reports")
    links = Links()
    links.feed(document)
    expected = "../runs/case/extracted/report.html"
    assert expected in links.targets
    assert str(dataset) not in document
    relocated = tmp_path / "relocated"
    shutil.copytree(linked, relocated)
    assert (relocated / "reports" / unquote(expected)).read_text() == "retained extraction"
    assert data == frozen


@pytest.mark.parametrize("kind", ["partial", "capability", "full"])
@pytest.mark.parametrize("config", [
    {"dataset_root": "..", "case_run_dirs": {}},
    {"dataset_root": "..", "case_run_dirs": {"1": "runs/case"}},
    {"dataset_root": "..", "case_run_dirs": {"0": "../outside"}},
    {"dataset_root": "..", "case_run_dirs": {"0": "/absolute"}},
    {"dataset_root": "..", "case_run_dirs": {"0": "Q:/absolute"}},
    {"dataset_root": "..", "case_run_dirs": {"0": r"runs\case"}},
    {"dataset_root": "Q:/dataset", "case_run_dirs": {"0": "runs/case"}},
    {"dataset_root": r"..\dataset", "case_run_dirs": {"0": "runs/case"}},
    {"case_run_dirs": {"0": "runs/case"}},
])
def test_unsafe_or_incomplete_mapping_rejected(tmp_path, kind, config):
    data = summary("retained")
    frozen = deepcopy(data)
    with pytest.raises(ValueError, match="mapping|relative"):
        render(kind, data, config, tmp_path)
    assert data == frozen


def test_mapping_rejects_symlink_escape(tmp_path):
    root = tmp_path / "dataset"
    root.mkdir()
    outside = tmp_path / "outside"
    outside.mkdir()
    try:
        (root / "case").symlink_to(outside, target_is_directory=True)
    except OSError as error:
        pytest.skip(f"Directory symlink unavailable: {error}")
    with pytest.raises(ValueError, match="outside dataset"):
        _display_cases(summary("retained"),
                       {"dataset_root": "dataset", "case_run_dirs": {"0": "case"}},
                       tmp_path)


@pytest.mark.parametrize("kind", ["partial", "capability", "full"])
@pytest.mark.parametrize("escaping", [True, False])
def test_symlinked_reports_validate_the_lexical_dataset(tmp_path, kind, escaping):
    """Both report locations must resolve the link to the same retained case."""
    dataset = tmp_path / "dataset"
    dataset.mkdir()
    physical = tmp_path / "physical"
    (physical / "reports").mkdir(parents=True)
    outside = tmp_path / "outside"
    target = outside / "case/extracted/report.html"
    target.parent.mkdir(parents=True)
    target.write_text("outside extraction", encoding="utf-8")
    linked_root, plain_root = (dataset, physical) if escaping else (physical, dataset)
    target = plain_root / "runs/case/extracted/report.html"
    target.parent.mkdir(parents=True)
    target.write_text("retained extraction", encoding="utf-8")
    base = dataset / "reports"
    try:
        base.symlink_to(physical / "reports", target_is_directory=True)
        (linked_root / "runs").symlink_to(outside, target_is_directory=True)
    except OSError as error:
        pytest.skip(f"Directory symlink unavailable: {error}")
    data = summary("retained")
    frozen = deepcopy(data)
    config = {"dataset_root": "..", "case_run_dirs": {"0": "runs/case"}}
    if escaping:
        with pytest.raises(ValueError, match="outside dataset"):
            render(kind, data, config, base)
    else:
        expected = "../runs/case/extracted/report.html"
        resolved = urlsplit(urljoin((base / "full.html").as_uri(), expected))
        assert url2pathname(resolved.path) == str(target)
        assert target.read_text() == "retained extraction"
        assert (physical / "reports" / unquote(expected)).read_text() == "outside extraction"
        with pytest.raises(ValueError, match="different case directories"):
            render(kind, data, config, base)
    assert data == frozen


@pytest.mark.parametrize("kind", ["partial", "capability", "full"])
def test_symlinked_reports_with_shared_dataset_remain_portable(tmp_path, kind):
    """Two parent steps reach the same dataset from either report location."""
    target = tmp_path / "runs/case/extracted/report.html"
    target.parent.mkdir(parents=True)
    target.write_text("retained extraction", encoding="utf-8")
    physical = tmp_path / "physical/reports"
    physical.mkdir(parents=True)
    base = tmp_path / "linked/reports"
    base.parent.mkdir()
    try:
        base.symlink_to(physical, target_is_directory=True)
    except OSError as error:
        pytest.skip(f"Directory symlink unavailable: {error}")
    data = summary("retained")
    frozen = deepcopy(data)
    config = {"dataset_root": "../..", "case_run_dirs": {"0": "runs/case"}}
    frozen_config = deepcopy(config)
    links = Links()
    links.feed(render(kind, data, config, base))
    expected = "../../runs/case/extracted/report.html"
    assert expected in links.targets
    for location in (base, physical):
        resolved = urlsplit(urljoin((location / "full.html").as_uri(), expected))
        assert url2pathname(resolved.path) == str(target)
        assert (location / unquote(expected)).read_text() == "retained extraction"
    assert data == frozen
    assert config == frozen_config


def test_no_mapping_keeps_existing_link_behavior(tmp_path):
    data = summary(str(tmp_path / "run"))
    assert _display_cases(data, {}, tmp_path) is data["cases"]
    assert 'href="run/extracted/report.html"' in render("partial", data, {}, tmp_path)


def test_full_generation_keeps_source_digest_and_retained_locator(tmp_path):
    """From reports, one parent step reaches runs/case; no drive is inferred."""
    retained = summary(r"Z:\retired\runs\case")
    source = tmp_path / "summary.json"
    source.write_text(json.dumps(retained), encoding="utf-8")
    digest = sha256(source.read_bytes()).hexdigest()
    # Synthetic dashboard samples satisfy the renderer's 120 s horizon contract;
    # they are not predicted engineering results (installation_envelope_dashboard).
    channel = dict(id="wave_elevation", label="Wave", units="m",
                   history=dict(times=[0, 1], values=[0, 1]),
                   forecast=dict(times=[2, 121], values=[0, 1]), assumed_limit=None)
    payload = dict(
        title="Example", created_utc="payload time", criteria=[], limitations=[],
        cases=[dict(index=0, hs_m=1, tp_s=8, status="NOT_EVALUATED", checks=[])],
        demo=dict(case_index=0, hs_m=1, tp_s=8, source_label="Synthetic",
                  frames=[dict(now_s=1, channels=[channel])]), boundaries=[],
        provenance={"summary": {"sha256": digest}},
    )
    payload_path = tmp_path / "payload.json"
    payload_path.write_text(json.dumps(payload), encoding="utf-8")
    payload_digest = sha256(payload_path.read_bytes()).hexdigest()
    output = tmp_path / "reports/full.html"
    record = generate_report(
        source, payload_path, output,
        {"dataset_root": "..", "case_run_dirs": {"0": "runs/case"}},
    )
    assert '../runs/case/extracted/report.html' in output.read_text(encoding="utf-8")
    assert record["demand_summary"] == retained
    assert record["evidence"]["Demand summary"]["sha256"] == digest
    assert sha256(source.read_bytes()).hexdigest() == digest
    assert sha256(payload_path.read_bytes()).hexdigest() == payload_digest
    sidecar = json.loads(output.with_suffix(".json").read_bytes())
    assert sidecar["demand_summary"] == retained
