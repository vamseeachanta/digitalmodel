"""Retirement contract for workspace-hub issue 3936.

Parse active hook/CI wiring; archived scanner libraries are not active gates.
"""
from pathlib import Path
import unittest

import yaml


ROOT = Path(__file__).resolve().parents[2]
RETIRED = (
    "legal-sanity-scan",
    "check_identifiers.py",
    "check_protected_identifiers.py",
    "verify_public_surface.sh",
    "check-no-abs-paths.sh",
)


def hook_entries():
    config = yaml.safe_load((ROOT / ".pre-commit-config.yaml").read_text())
    return [hook for repo in config["repos"] for hook in repo["hooks"]]


class RetirementContract(unittest.TestCase):
    def test_commits_do_not_run_identifier_gates(self):
        for hook in hook_entries():
            command = str(hook.get("entry", "")) + " " + hook["id"]
            for retired in RETIRED:
                with self.subTest(hook=hook["id"], retired=retired):
                    self.assertNotIn(retired, command)

    def test_ci_does_not_run_identifier_gates(self):
        for path in (ROOT / ".github" / "workflows").glob("*.y*ml"):
            workflow = yaml.safe_load(path.read_text(encoding="utf-8"))
            for job in workflow.get("jobs", {}).values():
                for step in job.get("steps", []):
                    for retired in RETIRED:
                        with self.subTest(workflow=path.name, retired=retired):
                            self.assertNotIn(retired, str(step.get("run", "")))

    def test_existing_secret_hooks_are_preserved(self):
        hooks = {hook["id"]: hook for hook in hook_entries()}
        self.assertIn("gitleaks", hooks)
        self.assertEqual(
            hooks["gitleaks"]["args"], ["--config", "../.gitleaks.toml"]
        )
        if (ROOT / "packages" / "worldenergydata-core").exists():
            self.assertIn("detect-private-key", hooks)
            workflow = (ROOT / ".github/workflows/ci.yml").read_text()
            self.assertIn("uses: gitleaks/gitleaks-action@v3", workflow)
        if (ROOT / "src/digitalmodel").exists():
            self.assertIn("detect-private-key", hooks)


if __name__ == "__main__":
    unittest.main()
