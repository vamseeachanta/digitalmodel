# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Keep structure validation active in local hooks and pull requests."""

from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
CHECKER = "scripts/maintenance/verify_repo_structure.py"


def workflow(name):
    # BaseLoader preserves the GitHub Actions event key as a string.
    return yaml.load((ROOT / ".github/workflows" / name).read_text(), Loader=yaml.BaseLoader)


def test_local_structure_hook_checks_all_paths_on_every_commit():
    config = yaml.safe_load((ROOT / ".pre-commit-config.yaml").read_text())
    hooks = [hook for repo in config["repos"] if repo["repo"] == "local"
             for hook in repo["hooks"] if hook["id"] == "verify-repo-structure"]
    assert len(hooks) == 1
    hook = hooks[0]
    assert CHECKER in hook["entry"]
    assert hook["language"] == "python"
    assert "PyYAML" in hook["additional_dependencies"]
    assert hook["pass_filenames"] is False
    assert hook["always_run"] is True
    assert hook["stages"] == ["pre-commit"]
    assert not any(key in hook for key in ("files", "exclude", "types", "types_or"))


def test_structure_workflow_runs_for_ready_pull_requests():
    config = workflow("repo-structure.yml")
    assert set(config["on"]) == {"pull_request", "push", "workflow_dispatch"}
    assert config["on"]["pull_request"] == {
        "types": ["opened", "synchronize", "reopened", "edited", "ready_for_review"],
    }
    assert config["on"]["push"]["branches"] == ["main"]
    job = config["jobs"]["repo-structure"]
    assert job["if"] == (
        "github.event_name != 'pull_request' || github.event.pull_request.draft == false"
    )
    assert job.get("continue-on-error", "false") == "false"
    steps = job["steps"]
    commands = [step for step in steps if "run" in step]
    assert any(CHECKER in step["run"] for step in commands)
    assert any("tests/repo_structure" in step["run"] for step in commands)
    for step in commands:
        assert "if" not in step
        assert step.get("continue-on-error", "false") == "false"
    assert any(step.get("uses", "").startswith("actions/checkout@") for step in steps)


def test_docs_workflow_covers_structure_and_domain_docs_with_routing_tests():
    config = workflow("docs.yml")
    for event in ("push", "pull_request"):
        assert {"docs/standards/**", "docs/domains/**"} <= set(config["on"][event]["paths"])
    steps = config["jobs"]["docs"]["steps"]
    validations = [step for step in steps
                   if "tests/docs/test_digitalmodel_routing_contract.py" in step.get("run", "")]
    assert len(validations) == 1
    assert "if" not in validations[0]
    assert validations[0].get("continue-on-error", "false") == "false"
