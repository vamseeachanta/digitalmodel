"""Demonstrate report links and collision rejection in a new output directory.

Run with PYTHONPATH=src and pass --output-dir pointing to a new scratch folder.
See docs/domains/reporting/output-destinations.md for inputs, method and answer.
"""

import argparse
from hashlib import sha256
import json
from pathlib import Path
import re

from digitalmodel.report_pack.workflow import router


def _config(root: Path, directory: str) -> dict:
    return {
        "_config_dir_path": str(root),
        "_config_file_path": str(root / "analysis"),
        "report_pack": {
            "document": {
                "number": "B0000-RPT-001-00", "revision": "00",
                "title": "Output destination example", "project": "Example",
                "client": "Example",
            },
            "sections": [{"title": "Results", "content": "Synthetic display fixture."}],
            "results": {"figures": [{"title": "Fixture", "path": "inputs/figure.svg"}]},
            "manifest": {
                "issue": "https://example.invalid/issues/1", "project": "Example",
                "artifact_class": "example", "privacy_classification": "public",
                "publishability_decision": "example only", "input_source_ids": ["fixture"],
                "raw_output_path": "inputs", "final_output_path": directory,
            },
            "output_dir": directory, "pdf": "off",
        },
    }


def run(root: Path) -> dict:
    """Create disposable fixtures; refuse an existing example directory."""
    root.mkdir(parents=True, exist_ok=False)
    inputs = root / "inputs"
    inputs.mkdir()
    figure = inputs / "figure.svg"
    figure.write_text('<svg xmlns="http://www.w3.org/2000/svg"></svg>', encoding="utf-8")
    before = sha256(figure.read_bytes()).digest()
    router(_config(root, "reports.html.json"))
    markdown = root / "reports.html.json" / "analysis_report.md"
    figure_link = re.search(r"!\[Fixture\]\(([^)]+)\)", markdown.read_text()).group(1)
    assert figure_link == "../inputs/figure.svg"
    assert (markdown.parent / figure_link).resolve() == figure

    rejected = root / "rejected"
    rejected.mkdir()
    # Two output names identify the same existing input. Preflight must reject
    # them before rendering, leaving that input's SHA-256 digest unchanged.
    (rejected / "analysis_report.md").hardlink_to(figure)
    (rejected / "analysis_report.html").hardlink_to(figure)
    try:
        router(_config(root, "rejected"))
    except ValueError as error:
        if "distinct" not in str(error):
            raise
    else:
        raise AssertionError("Expected collision rejection")
    unchanged = sha256(figure.read_bytes()).digest() == before
    assert unchanged
    return {
        "figure_link": figure_link,
        "collision_rejected": True,
        "retained_bytes_unchanged": unchanged,
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    print(json.dumps(run(parser.parse_args().output_dir), indent=2))
