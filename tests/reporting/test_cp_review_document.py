"""Portfolio presentation must preserve calculation evidence and honest review state."""
import copy
import importlib.util
import hashlib
import json
import re
import shutil
import subprocess
from pathlib import Path
from types import ModuleType

import pytest

from digitalmodel.reporting.provenance import Provenance
from digitalmodel.reporting.engine import render_html
from digitalmodel.reporting.spec import (
    DocumentMeta, FigureBlock, ReportSpec, RevisionRow, Section, StatusBlock, TableBlock,
)

MODULE = Path(__file__).resolve().parents[2] / "scripts/reporting/cp_review_document.py"


def api() -> ModuleType:
    spec = importlib.util.spec_from_file_location("cp_review_document", MODULE)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def source() -> ReportSpec:
    return ReportSpec(
        document=DocumentMeta(
            number="X0000-CP-001-00", revision="00", title="Jacket regression",
            project="CP module", client="Internal", checked_by="Pending",
            approved_by="Pending", revision_history=[RevisionRow(
                rev="00", description="Internal review draft", checked="Pending",
                approved="Pending",
            )],
        ),
        sections=[Section(key="demand", title="Current demand", blocks=[TableBlock(
            title="Demand", columns=["Zone", "Current"], units=["-", "A"],
            rows=[["Submerged", 66.499]],
        )])],
        provenance=Provenance().add("file", "input.yml", digest="sha256:abc"),
    )


def test_render_retains_evidence_and_ten_sections_without_mutating_source() -> None:
    module = api()
    spec = source()
    before = spec.model_dump()
    html = module.render_review(spec, "S01")
    assert spec.model_dump() == before
    assert "66.499" in html and 'id="demand"' in html
    for title in module.SECTION_TITLES:
        assert title in html
    assert "deferred by the owner" in html
    assert "No engineering acceptance" in html
    assert '<script src=' not in html
    assert 'id="cp-review-ui"' in html
    config_match = re.search(
        r'<script id="cp-review-config" type="application/json">(.*?)</script>', html,
    )
    assert config_match is not None
    config = json.loads(config_match.group(1))
    core = re.sub(r'<!--CP_REVIEW_START-->[\s\S]*?<!--CP_REVIEW_END-->', '', html)
    assert hashlib.sha256(core.encode()).hexdigest() == config["content_sha256"]


def test_comment_seed_binds_exact_document_and_pending_review() -> None:
    module = api()
    html = module.render_review(source(), "S01")
    seed = module.comment_seed(html, "S01", "00")
    module.validate_comments(html, seed)
    assert seed["reviewer_assignment"] == "pending"
    assert all(row["decision"] == "pending" for row in seed["results"])
    assert seed["comments"] == []
    with pytest.raises(ValueError, match="digest"):
        module.validate_comments(html + " ", seed)


@pytest.mark.parametrize("field,value", [("report_id", "S02"), ("revision", "01")])
def test_wrong_report_or_revision_cannot_be_loaded(field: str, value: str) -> None:
    module = api()
    html = module.render_review(source(), "S01")
    seed = module.comment_seed(html, "S01", "00")
    seed[field] = value
    with pytest.raises(ValueError):
        module.validate_comments(html, seed)


def test_malformed_comments_and_issued_metadata_fail_closed() -> None:
    module = api()
    html = module.render_review(source(), "S01")
    seed = module.comment_seed(html, "S01", "00")
    bad = copy.deepcopy(seed)
    bad["results"][0]["decision"] = "APPROVED"
    with pytest.raises(ValueError):
        module.validate_comments(html, bad)
    bad = copy.deepcopy(seed)
    bad["comments"] = [{"id": "c1", "section": "review-1", "quote": "Evidence",
                        "reviewer": "Reviewer", "text": "Check", "disposition": "deferred"}]
    with pytest.raises(ValueError):
        module.validate_comments(html, bad)
    spec = source()
    spec.document.revision_history[0].description = "Issued"
    with pytest.raises(ValueError, match="draft"):
        module.render_review(spec, "S01")


def test_embedded_json_cannot_close_script() -> None:
    module = api()
    with pytest.raises(ValueError):
        module.render_review(source(), '</script><script>alert(1)</script>')


def test_summary_and_findings_include_actual_governing_result() -> None:
    spec = source()
    spec.sections[0].blocks.append(StatusBlock(
        label="Output", status="FAIL", governing_case="final",
        detail="55 anodes supply 66.499 A against 208 A demand",
    ))
    rendered = api().render_review(spec, "S01")
    assert rendered.count("55 anodes supply 66.499 A against 208 A demand") == 4
    assert "Governing case: final" in rendered
    assert 'id="result-demand-1"' in rendered
    seed = api().comment_seed(rendered, "S01", "00")
    assert {"id": "result-demand-1", "decision": "pending"} in seed["results"]


def test_numeric_tables_and_plot_payloads_are_preserved() -> None:
    spec = source()
    spec.sections[0].blocks.append(FigureBlock(
        title="Demand", figure_id="demand-curve", caption="Regression curve",
        plotly={"data": [{"x": [0, 25], "y": [10, 160], "type": "scatter"}],
                "layout": {"title": "Demand"}},
    ))
    before, after = render_html(spec), api().render_review(spec, "S01")
    tables = re.findall(r'<table>.*?</table>', before, re.S)
    assert any("66.499" in table for table in tables)
    assert all(table in after for table in tables)
    pattern = r'<script[^>]*>(.*?)</script>'
    def plots(text: str) -> list[str]:
        return [s for s in re.findall(pattern, text, re.S)
                if "Plotly.newPlot(" in s and len(s) < 100000]
    assert plots(before) == plots(after)
    assert plots(before), "Must exercise a real plot payload"


UI_HARNESS = '''
const fs=require('fs'),vm=require('vm'),assert=require('assert');
const elements={};
const get=id=>elements[id]||(elements[id]={value:'',textContent:'',files:[],appendChild(){}});
get('cp-review-config').textContent=JSON.stringify({report_id:'S01',revision:'00',
 content_sha256:require('crypto').createHash('sha256').update('exact HTML').digest('hex'),
 sections:[{id:'review-1',title:'Executive Summary'}]});
global.document={getElementById:get,createElement:()=>({click(){}})};
global.crypto=require('crypto').webcrypto;
Object.defineProperty(global,'navigator',{value:{clipboard:{writeText:async()=>{global.copied=true;}}}});
vm.runInThisContext(fs.readFileSync(process.argv[2],'utf8'));
(async()=>{
 await get('cp-copy').onclick(); assert(!global.copied);
 const data=Buffer.from('exact HTML');
 get('cp-html').files=[{arrayBuffer:async()=>data}];await get('cp-html').onchange();
 const state={schema_version:1,report_id:'S01',revision:'00',
 report_sha256:require('crypto').createHash('sha256').update(data).digest('hex'),
 standard_revision:'c955b86159acc4f9e9f5964690c24dd1411e5b48',
 results:[{id:'review-1',decision:'pending'}],comments:[],prior_rounds:[],decision_conflicts:[]};
 get('cp-load').files=[{size:100,text:async()=>JSON.stringify(state)}];
 await get('cp-load').onchange();await get('cp-copy').onclick();assert(global.copied);
 get('cp-reviewer').value='Reviewer';get('cp-comment').value='Check output';
 get('cp-section').value='review-1';get('cp-decision').value='revise';
 get('cp-disposition').value='deferred';get('cp-add').onclick();
 assert(!JSON.parse(get('cp-preview').textContent).comments.length);
 get('cp-reason').value='Independent check needed';get('cp-add').onclick();
 const comment=JSON.parse(get('cp-preview').textContent).comments[0];
 assert(comment.id&&comment.disposition==='deferred'&&comment.reason);
 global.copied=false;await get('cp-copy').onclick();assert(!global.copied);
 const incoming={...state,results:[{id:'review-1',decision:'accept'}]};
 get('cp-load').files=[{size:100,text:async()=>JSON.stringify(incoming)}];
 await get('cp-load').onchange();await get('cp-load').onchange();
 const merged=JSON.parse(get('cp-preview').textContent);
 assert.equal(merged.comments.length,1);assert.equal(merged.decision_conflicts.length,1);
 assert(merged.prior_rounds.every(round=>round.prior_rounds.length===0));
 get('cp-html').files=[{arrayBuffer:async()=>data}];await get('cp-html').onchange();
 let saved='';global.window={showSaveFilePicker:async()=>({
 createWritable:async()=>({write:async value=>{saved=value;},close:async()=>{}}),
 getFile:async()=>({text:async()=>saved})})};
 await get('cp-save').onclick();assert.equal(JSON.parse(saved).comments.length,1);
 assert(get('cp-message').textContent.includes('read back successfully'));
 get('cp-load').files=[{size:saved.length,text:async()=>saved}];
 await get('cp-load').onchange();
 assert.equal(JSON.parse(get('cp-preview').textContent).comments.length,1);
 get('cp-html').files=[{arrayBuffer:async()=>Buffer.from('edited HTML')}];
 await get('cp-html').onchange();await get('cp-copy').onclick();assert(!global.copied);
})().catch(e=>{console.error(e);process.exit(1)});
'''


def test_offline_ui_rejects_unbound_and_reused_exports(tmp_path: Path) -> None:
    node = shutil.which("node")
    if not node:
        pytest.skip("Node is required for offline UI unit execution")
    script = MODULE.with_name("cp_review_ui.js")
    harness = tmp_path / "check.cjs"
    harness.write_text(UI_HARNESS, encoding="utf-8")
    result = subprocess.run([node, str(harness), str(script)], capture_output=True,
                            text=True, creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
    assert result.returncode == 0, result.stderr
