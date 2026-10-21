# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Public input claims must agree with the implemented measured sources."""

import ast
import json
import re
import shlex
import sys
from html import unescape
from html.parser import HTMLParser
from pathlib import Path

import pytest
import yaml


ROOT = Path(__file__).resolve().parents[2]
MODULE = ROOT / "src/digitalmodel/motion_forecast"
MODULE_ORIGINAL = MODULE
DOCUMENTS = (
    "docs/api/hydro/short-horizon-motion-forecast.html",
    "src/digitalmodel/motion_forecast/docs/agent-pack.md",
    "README.md",
    "docs/capability-map/capability-cards.json",
    "scripts/capabilities/build_onepagers.py",
)


def _offline_source_names(source, *, require_readers=True):
    """Require each concrete reader to declare its offline input capability."""
    tree = ast.parse(source)
    readers = []
    function_types = (ast.FunctionDef, ast.AsyncFunctionDef)
    parents = {child: parent for parent in ast.walk(tree)
               for child in ast.iter_child_nodes(parent)}
    protocols = {node for node in ast.walk(tree) if isinstance(node, ast.ClassDef)
                 and any(ast.unparse(base).endswith("Protocol") for base in node.bases)}
    for node in ast.walk(tree):
        ancestor = parents.get(node)
        lineage = []
        while ancestor is not None:
            lineage.append(ancestor)
            ancestor = parents.get(ancestor)
        if any(parent in protocols for parent in lineage) or node in protocols:
            continue
        if isinstance(node, ast.ClassDef):
            if any(isinstance(method, function_types) and method.name == "read"
                   for method in node.body):
                readers.append((node, node.body, "LIVE_INPUT"))
        elif isinstance(node, function_types) and node.returns is not None:
            if not re.search(r"\b(?:MeasuredMotion|WaveForecast)\b", ast.unparse(node.returns)):
                continue
            parent = parents[node]
            if isinstance(parent, ast.ClassDef) and node.name == "read":
                continue  # The concrete reader class supplies this declaration.
            scope = next(parent for parent in lineage
                         if isinstance(parent, (ast.Module, ast.ClassDef, *function_types)))
            readers.append((node, scope.body, node.name + ".LIVE_INPUT"))
    assert readers or not require_readers, "No concrete measured sources discovered; review the claim guard"
    for reader, body, attribute in readers:
        flags = []
        for statement in body:
            if isinstance(statement, (ast.Assign, ast.AnnAssign)):
                targets = statement.targets if isinstance(statement, ast.Assign) else [statement.target]
                if any(ast.unparse(target) == attribute for target in targets):
                    flags.append(statement.value)
        assert flags and all(isinstance(value, ast.Constant) and value.value is False
                             for value in flags), (
            f"Review input claims for new source {reader.name}; declare LIVE_INPUT = False "
            "only for offline inputs"
        )
    return [reader.name for reader, _body, _attribute in readers]


def _package_source_names(package):
    """Inspect every Python module, including nested input adapters, without imports."""
    readers = []
    for path in sorted(package.rglob("*.py")):
        readers.extend(_offline_source_names(path.read_text(encoding="utf-8"), require_readers=False))
    assert readers, "No concrete measured sources discovered; review the claim guard"
    return readers


class _ClaimText(HTMLParser):
    """Retain visible prose and descriptive attributes, excluding executable text."""

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.parts = []
        self.hidden = None

    def handle_starttag(self, tag, attrs):
        if tag in {"style", "script"}:
            self.hidden = tag
        if not self.hidden:
            for name, value in attrs:
                if name in {"aria-label", "title", "alt", "content"} and value:
                    self.parts.append(". " + value + ". ")
            if tag in {"p", "div", "li", "br", "tr", "h1", "h2", "h3"}:
                self.parts.append(". ")

    def handle_endtag(self, tag):
        if self.hidden == tag:
            self.hidden = None
        if tag in {"p", "div", "li", "tr", "h1", "h2", "h3"}:
            self.parts.append(". ")

    def handle_data(self, data):
        if not self.hidden:
            self.parts.append(data)


def _document_text(relative):
    """Select this capability's authored and generated metadata without execution."""
    document = (ROOT / relative).read_text(encoding="utf-8")
    if relative == "README.md":
        return _motion_readme_text(document)
    if relative.endswith("capability-cards.json"):
        card = next(card for card in json.loads(document)
                    if card["id"] == "short-horizon-motion-forecast")
        return json.dumps(card)
    if relative.endswith("build_onepagers.py"):
        specs = next(node.value for node in ast.parse(document).body
                     if isinstance(node, ast.AnnAssign) and ast.unparse(node.target) == "SPECS")
        spec = next(spec for spec in specs.elts
                    if any(keyword.arg == "id" and isinstance(keyword.value, ast.Constant)
                           and keyword.value.value == "short-horizon-motion-forecast"
                           for keyword in spec.keywords))
        return ". ".join(node.value for node in ast.walk(spec)
                         if isinstance(node, ast.Constant) and isinstance(node.value, str))
    return document


def _motion_readme_text(document):
    """Select motion-forecast sections, including their nested headings."""
    selected = []
    level = None
    for line in document.splitlines():
        heading = re.match(r"^(#{1,6})\s+(.*)", line)
        if heading:
            depth, title = len(heading[1]), heading[2]
            if level is not None and depth <= level:
                level = None
            if re.search(r"motion[-_ ]forecast", title, re.I):
                level = depth if level is None else level
        if level is not None:
            selected.append(line)
    return "\n".join(selected)


def _unsupported_live_claims(document):
    """Check claims per clause; a limitation cannot excuse a subsequent claim."""
    # Canvas labels are executable strings rather than visible HTML text.
    label_claims = re.findall(r"\b(?:mms\s+measured|measured\s+mms)\b", unescape(document), re.I)
    # A site-availability prefix is exempt only when followed by a real link.
    document = re.sub(r"\bLive:\s*\[[^\]]+\]\(https?://[^)]+\)", "", document, flags=re.I)
    # Inline repository references are evidence locations, not sensor claims.
    document = re.sub(r"`(?:[\w.-]+/)+[\w.-]+\.(?:md|py|html)`", "", document)
    parser = _ClaimText()
    parser.feed(document)
    text = re.sub(r"\[([^\]]+)\]\([^)]*\)", r"\1", "".join(parser.parts))
    text = re.sub(r"\s+", " ", unescape(text)).lower()
    claims = list(label_claims)
    for sentence in re.split(r"[.!?;]", text):
        clauses = re.split(r"\b(?:and|or|but|however|yet)\b", sentence)
        if re.search(r"\b(?:live|real[- ]?time|onboard|mms|mru)\b", sentence):
            for continuation in clauses[1:]:
                current_use = re.search(r"\b(?:already|currently|now|drives?|use[sd]?|driven|fed|runs?|powers?|feeds?)\b", continuation)
                offline = re.search(r"\b(?:synthetic|csv|stub)\b", continuation)
                future = re.search(r"\b(?:planned|not (?:implemented|supported|available)|will)\b", continuation)
                if current_use and not (offline or future):
                    claims.append(continuation.strip())
        if re.search(r"\b(?:mms|mru)\b", sentence):
            limitation = re.search(
                r"\b(?:planned|not (?:implemented|supported|available)|no (?:live )?mms|without (?:live )?mms)\b",
                sentence,
            )
            if not limitation:
                claims.append(sentence.strip())
        for clause in clauses:
            for live in re.finditer(r"\b(?:live|real[- ]?time|onboard)\b", clause):
                before, after = clause[:live.start()], clause[live.end():]
                negated = re.search(r"\b(?:no|without|not(?: a)?)\s+$", before)
                planned = re.match(
                    r"\s+(?:mms|mru|sensor|feed|input)[\w\s/&-]{0,60}?\s+"
                    r"(?:is|are|remains?)\s+(?:planned|not (?:implemented|supported|available))\b",
                    after,
                )
                if not (negated or planned):
                    claims.append(clause.strip())
    return claims


@pytest.mark.parametrize("relative", DOCUMENTS)
def test_public_input_claims_match_implemented_sources(relative):
    sources = _package_source_names(MODULE)
    claims = _unsupported_live_claims(_document_text(relative))
    assert not claims, f"{relative}: only {sources} are implemented; claims: {claims}"


@pytest.mark.parametrize("relative", DOCUMENTS)
@pytest.mark.parametrize("claim", (
    'Anchored to live <strong>vessel &amp; structure</strong> motions from an MMS.',
    '<span class="pill">Live MMS / MRU feed &middot; phase-resolved forecast</span>',
    'Measured — live vessel &amp; structure motions from an onboard MMS / MRU.',
    'This demo uses LIVE motion data.',
    'The demo uses real-time MMS / MRU feed data.',
    'This is a live demo.',
    'Runs on live vessel data.',
    'Anchored to live vessel measurements from onboard instruments.',
    'Realtime MMS / MRU feed.',
    '<meta content="Live MMS / MRU feed">',
    'aria-label="Live MMS motion trace"',
    'Live MMS feed is planned and already drives this demo.',
    'MMS measured',
    'measured MMS',
    '<script>ctx.fillText("MMS MEASURED", 0, 0);</script>',
    'Live MMS feed is planned and this demo already uses onboard measurements.',
    '<img alt="Onboard measured motion data">',
    '<span title="Real-time vessel measurements">trace</span>',
    'Live MMS ingest is planned, but this demo uses a live MRU feed.',
    'No live MMS feed is implemented; this demo uses live sensor data.',
    'Measured: vessel & structure motions from the MMS / MRU give the now ground truth.',
    'Anchored to vessel motions measured by the MRU.',
    'Live MMS / MRU ingest is planned and is used by this demo.',
    '<p title="MMS-driven forecast">',
    '`MMS-driven forecast`',
    'MMS input is planned but the forecast is fed by it.',
))
def test_negative_control_rejects_injected_claim_on_each_surface(relative, claim):
    document = _document_text(relative)
    assert _unsupported_live_claims(claim)
    assert _unsupported_live_claims(document + "\n\n" + claim)


@pytest.mark.parametrize("description", (
    'Illustrative: synthetic sea and simplified analytic RAOs.',
    'Measured-motion input uses a synthetic stub or a CSV reader.',
    'Live MMS / MRU ingest is planned.',
    'Live MRU feeds are not supported.',
    'A live feed is not implemented.',
    'Real-time MMS / MRU input is planned.',
    'No live MMS feed is implemented.',
    'Live: [engineering capabilities](https://example.org/capabilities/)',
    'MMS / MRU integration and hardware time synchronisation are planned.',
    'MMS / MRU input planned',
    '`docs/plans/mms-ingest.md`',
))
def test_accurate_limitations_and_site_links_are_permitted(description):
    assert not _unsupported_live_claims(description)


def test_source_inventory_requires_review_when_a_reader_is_added():
    source = (MODULE / "measured_source.py").read_text()
    unsupported = '\nclass NetworkReader:\n    def read(self):\n        pass\n'
    with pytest.raises(AssertionError, match="new source NetworkReader"):
        _offline_source_names(source + unsupported)


def test_source_inventory_cannot_pass_when_no_reader_is_discovered():
    with pytest.raises(AssertionError, match="No concrete measured sources"):
        _offline_source_names("class Interface: pass")


def test_claim_guard_covers_card_and_generator():
    assert {
        "docs/capability-map/capability-cards.json",
        "scripts/capabilities/build_onepagers.py",
    } <= set(DOCUMENTS)


def test_chart_label_discloses_synthetic_measurement():
    page = (ROOT / DOCUMENTS[0]).read_text()
    assert 'ctx.fillText("MEASURED (SYNTHETIC)"' in page
    assert "measured MMS vs forecast" not in page
    assert "AQWA phase sign pending validation" in page


@pytest.mark.parametrize("reader", (
    'class NetworkReader:\n    """Live NMEA reader; replaces the synthetic stub."""\n    def read(self): pass\n',
    'class NetworkReader:\n    """Synthetic stub replacement."""\n    async def read(self): pass\n',
    'async def NetworkReader() -> MeasuredMotion:\n    """CSV replacement."""\n    pass\n',
))
def test_docstrings_cannot_approve_new_sources(reader):
    source = (MODULE / "measured_source.py").read_text()
    with pytest.raises(AssertionError, match="new source NetworkReader"):
        _offline_source_names(source + "\n" + reader)


@pytest.mark.parametrize("reader", (
    'def NetworkReader() -> MeasuredMotion | None: pass',
    'def NetworkReader() -> "Optional[MeasuredMotion]": pass',
    'def NetworkReader() -> models.WaveForecast: pass',
    'class Factory:\n    @staticmethod\n    def NetworkReader() -> WaveForecast: pass',
    'def factory():\n    async def NetworkReader() -> MeasuredMotion: pass',
))
def test_wrapped_and_nested_source_annotations_require_flags(reader):
    with pytest.raises(AssertionError, match="new source NetworkReader"):
        _offline_source_names(reader)


def test_readme_scan_is_limited_to_motion_forecast():
    document = (
        '# Library\n\n## Other capability\nLive telemetry.\n\n'
        '## Motion forecast\nSynthetic inputs.\n'
        '### Inputs\nMMS input is planned.\n'
        '## Another capability\nReal-time telemetry.\n'
    )
    assert not _unsupported_live_claims(_motion_readme_text(document))
    document = document.replace('Synthetic inputs.', 'MMS-driven forecast.')
    assert _unsupported_live_claims(_motion_readme_text(document))


def test_readme_without_motion_section_has_no_motion_claims():
    assert _motion_readme_text('# Library\n\nLive telemetry.\n') == ""


@pytest.mark.parametrize("declaration", ("True", "False or True", "None", "0"))
def test_source_flag_requires_literal_false(declaration):
    reader = (
        'class NetworkReader:\n    """Synthetic replacement."""\n'
        f'    LIVE_INPUT = {declaration}\n    def read(self): pass\n'
    )
    with pytest.raises(AssertionError, match="new source NetworkReader"):
        _offline_source_names(reader)


@pytest.mark.parametrize("reader", (
    'class FileReader:\n    LIVE_INPUT = False\n    async def read(self): pass\n',
    'class FileReader:\n    LIVE_INPUT: bool = False\n    def read(self): pass\n',
    'async def FileReader() -> MeasuredMotion:\n    pass\nFileReader.LIVE_INPUT = False\n',
    'def FileReader() -> "MeasuredMotion":\n    pass\nFileReader.LIVE_INPUT: bool = False\n',
    'def FileReader() -> "MeasuredMotion | None": pass\nFileReader.LIVE_INPUT = False\n',
    'class Factory:\n    @staticmethod\n    def FileReader() -> WaveForecast: pass\n    FileReader.LIVE_INPUT = False\n',
    'def factory():\n    def FileReader() -> WaveForecast: pass\n    FileReader.LIVE_INPUT = False\n',
))
def test_offline_source_flag_is_read_from_code(reader):
    assert _offline_source_names(reader) == ["FileReader"]


@pytest.mark.parametrize("relative", ("network.py", "inputs/network.py"))
def test_package_inventory_detects_reader_in_another_module(tmp_path, monkeypatch, relative):
    module = sys.modules[__name__]
    monkeypatch.setattr(module, "MODULE", tmp_path)
    (tmp_path / "measured_source.py").write_text((MODULE_ORIGINAL / "measured_source.py").read_text())
    extra = tmp_path / relative
    extra.parent.mkdir(parents=True, exist_ok=True)
    extra.write_text('class NetworkReader:\n    """CSV stub replacement."""\n    async def read(self): pass\n')
    with pytest.raises(AssertionError, match="new source NetworkReader"):
        test_public_input_claims_match_implemented_sources(DOCUMENTS[0])


def test_hosted_shard_runs_documented_motion_root():
    from scripts.ci.detect_touched_domains import parse_domains

    domains = parse_domains(ROOT / "tests/DOMAINS.md")
    domain = next(domain for domain in domains if domain.name == "hydrodynamics-other")
    assert "tests/motion_forecast/" in domain.roots
    gates = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text())
    gate = gates["gates"]["tests-hydrodynamics-other"]
    assert gate["enabled"] is True
    assert gate["failure_action"] == "block"
    assert "tests/motion_forecast/" in gate["test_roots"]
    assert "tests/motion_forecast/" in shlex.split(gate["command"])
