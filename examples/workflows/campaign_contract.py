# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Solver-free worked example for issue 2298 (2026), contract items 1–5.

Inputs are synthetic: three cases; age limit 10 clock units; counter 4 at time
10, then 5 at time 20. At time 31 the age is 31 - 20 = 11, exceeding 10.
Acceptance decisions are explicitly supplied; no engineering limit is assumed.
"""

import argparse
import json
from pathlib import Path

from digitalmodel.workflows.campaign_contract import Campaign


def run_example(output_dir):
    """Write a checkpoint, announced migration, workbook CSV and report payload."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    criteria = dict(criteria_id="synthetic-screen", criteria_version="v1")
    campaign = Campaign.launch(
        ["a", "b", "c"], **criteria,
        criteria_source="Synthetic caller-supplied pass decisions",
        stall_after=10, authority={"covers_untouched_cases": True},
        retention=dict(inputs=["synthetic-inputs"], plotted_arrays=["synthetic-arrays"],
                       metrics=["synthetic-metrics"], manifest_digest_algorithm="sha256",
                       archive_replica="archive/synthetic-campaign"))
    checkpoint = output_dir / "campaign.json"
    campaign.save(checkpoint)  # Launch declaration precedes any observation.
    campaign.observe("a", counter=4, now=10, process_alive=True)
    campaign.save(checkpoint)
    campaign.observe("a", counter=5, now=20, process_alive=True)
    campaign.save(checkpoint)
    campaign = Campaign.load(checkpoint)
    campaign.observe("a", counter=5, now=31, process_alive=True)
    stalled = campaign.health(now=31)["a"].value
    campaign.save(checkpoint)
    campaign.collect("a", **criteria, exit_code=0, native_readback_ok=True,
                     criterion_met=True)
    campaign.save(checkpoint)
    campaign.collect("b", **criteria, exit_code=1, native_readback_ok=False,
                     criterion_met=None, failure_class="synthetic-input-error")
    campaign.stop()
    campaign.save(checkpoint)
    campaign = Campaign.load(checkpoint)
    resumed = campaign.next_cases()
    def execute(case_id, governing):
        # The runner calls this only for untouched authorized c. Supply evidence
        # here; a real adapter supplies native readback without changing guards.
        return dict(criteria_id=governing["criteria_id"],
                    criteria_version=governing["criteria_version"],
                    exit_code=0, native_readback_ok=True, criterion_met=True)

    campaign.run_untouched(execute, checkpoint=checkpoint)
    campaign.mark_reported(["a", "c"], **criteria)
    campaign.save(checkpoint)
    preview = campaign.preview_criteria_change(
        criteria_id="synthetic-screen", criteria_version="v2",
        criteria_source="Revised synthetic caller-supplied pass decisions",
        qualifications={"a": False, "c": True})

    def announce(message):
        # The affected rows are printed and retained before application.
        print(message)
        (output_dir / "criteria-preview.json").write_text(message + "\n", encoding="utf-8")

    campaign.apply_criteria_change(preview, emit=announce)
    campaign.save(checkpoint)
    criteria["criteria_version"] = "v2"
    campaign.mark_reported(["a", "c"], **criteria)
    campaign.save(checkpoint)
    report = campaign.report(**criteria, next_action="Review withheld a and failed b")
    (output_dir / "report.json").write_text(json.dumps(report, indent=2) + "\n",
                                            encoding="utf-8")
    (output_dir / "workbook.csv").write_text(campaign.workbook_csv(**criteria),
                                             encoding="utf-8")
    return dict(stalled=stalled, resumed=resumed, status=report["status"])


def main():
    """Require an explicit output directory; no solver or project environment."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    print(json.dumps(run_example(args.output), indent=2))


if __name__ == "__main__":
    main()
