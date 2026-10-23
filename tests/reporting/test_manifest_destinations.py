"""A relocated manifest must reach each hand-enumerated bundle artifact."""

import json
from pathlib import Path
import runpy

from digitalmodel.reporting import write_report


def test_relocated_standard_report_manifest_links_resolve(tmp_path):
    spec = runpy.run_path(str(Path(__file__).with_name("test_output_destinations.py")))["_spec"]()
    bundle = tmp_path / "bundle.html.json"
    bundle.mkdir()
    actual_manifest = tmp_path / "details" / "manifest.json"
    actual_manifest.parent.mkdir()
    (bundle / "analysis_manifest.json").symlink_to(actual_manifest)
    write_report(spec, bundle, "analysis", pdf="off")
    manifest = json.loads(actual_manifest.read_text())
    for kind, expected in {
        "html": bundle / "analysis.html",
        "citations": bundle / "analysis_citations.json",
        "manifest": actual_manifest,
    }.items():
        assert (actual_manifest.parent / manifest["artifacts"][kind]).resolve() == expected
