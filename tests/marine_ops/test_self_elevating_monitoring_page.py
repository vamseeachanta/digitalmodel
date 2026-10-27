# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Ensure plotted warnings come from the tested evaluator and remain reproducible."""

import json
import os
from pathlib import Path
import re
import subprocess
import sys

from digitalmodel.marine_ops.self_elevating_monitoring import evaluate, synthetic_cases
from scripts.capabilities.build_self_elevating_monitoring import render_page


ROOT = Path(__file__).resolve().parents[2]
PAGE = ROOT / "docs/api/hydro/self-elevating-monitoring.html"
FORECAST = ROOT / "docs/api/hydro/short-horizon-motion-forecast.html"


def test_committed_page_reproduces_and_links_both_ways():
    page = render_page()
    assert PAGE.read_text(encoding="utf-8") == page
    assert 'href="short-horizon-motion-forecast.html"' in page
    assert 'href="self-elevating-monitoring.html"' in FORECAST.read_text(encoding="utf-8")
    assert 'href="../_assets/brand.css"' in page
    assert "Revision history" in page
    assert "@@" not in page


def test_page_alerts_and_lead_times_match_python_results():
    page = render_page()
    match = re.search(r'<script id="monitor-data" type="application/json">(.*?)</script>',
                      page, re.S)
    payload = json.loads(match[1])
    for name, case in synthetic_cases().items():
        plotted = payload[name]
        alerts = evaluate(case.time_s, case.channels, case.rules)
        assert plotted["time_s"] == list(case.time_s)
        assert plotted["event_s"] == case.event_s
        assert [(a["rule"], a["time_s"]) for a in plotted["alerts"]] == [
            (a.rule, a.time_s) for a in alerts
        ]
        for row, alert in zip(plotted["alerts"], alerts):
            assert row["lead_min"] == round((case.event_s - alert.time_s) / 60, 1)
    assert payload["settled"]["alerts"] == []
    assert payload["storm"]["charts"][0]["limit"] == 450
    assert payload["storm"]["charts"][0]["threshold"] == 300


def test_new_generator_and_page_have_registry_ownership():
    from scripts.check_generated_html import GENERATORS

    entry, = [g for g in GENERATORS
              if g.script == "scripts/capabilities/build_self_elevating_monitoring.py"]
    assert entry.outputs == ("docs/api/hydro/self-elevating-monitoring.html",)
    assert "src/digitalmodel/marine_ops/self_elevating_monitoring.py" in entry.inputs
    assert "scripts/capabilities/self_elevating_monitoring.html.template" in entry.inputs


def test_generator_cli_writes_reproducibly_outside_checkout(tmp_path):
    output = tmp_path / "monitoring.html"
    env = {**os.environ, "PYTHONPATH": str(ROOT / "src")}
    subprocess.run(
        [sys.executable, str(ROOT / "scripts/capabilities/build_self_elevating_monitoring.py"),
         "--output", str(output)],
        env=env, check=True, capture_output=True, text=True,
    )
    assert output.read_bytes() == PAGE.read_bytes()
