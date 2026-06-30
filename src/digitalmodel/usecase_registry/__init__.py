"""Solver use-case registry — the single source of truth for every analysis run
we can prepare/dispatch across OrcaWave / AQWA / OrcaFlex / ANSYS / OpenFOAM
(#938 b, OpenFOAM CFD added #1161).

Load with :func:`load_usecases`; check integrity (ready entries must have a
resolvable template, ids unique, solvers/readiness valid) with
:func:`validate_registry`. The validator is a CI-able invariant: it fails if a
use-case is marked ``ready`` but its template path no longer resolves.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

import yaml

REGISTRY_PATH = Path(__file__).with_name("registry.yaml")
REPO_ROOT = Path(__file__).resolve().parents[3]

SOLVERS = {"orcawave", "aqwa", "orcaflex", "ansys", "openfoam"}
READINESS = {"ready", "partial", "planned"}


@dataclass(frozen=True)
class UseCase:
    id: str
    domain: str
    solver: str
    basename: str
    operation: Optional[str]
    template: Optional[str]  # repo-relative path, or None
    analysis: tuple[str, ...] = field(default_factory=tuple)
    readiness: str = "planned"
    notes: str = ""

    def template_path(self, repo_root: Path = REPO_ROOT) -> Optional[Path]:
        return (repo_root / self.template) if self.template else None


def load_usecases(path: Path = REGISTRY_PATH) -> list[UseCase]:
    data = yaml.safe_load(Path(path).read_text()) or {}
    out: list[UseCase] = []
    for raw in data.get("usecases", []):
        out.append(
            UseCase(
                id=raw["id"],
                domain=raw["domain"],
                solver=raw["solver"],
                basename=raw["basename"],
                operation=raw.get("operation"),
                template=raw.get("template"),
                analysis=tuple(raw.get("analysis") or ()),
                readiness=raw.get("readiness", "planned"),
                notes=raw.get("notes", ""),
            )
        )
    return out


def by_solver(usecases: Optional[list[UseCase]] = None) -> dict[str, list[UseCase]]:
    cases = usecases if usecases is not None else load_usecases()
    grouped: dict[str, list[UseCase]] = {}
    for case in cases:
        grouped.setdefault(case.solver, []).append(case)
    return grouped


def readiness_counts(usecases: Optional[list[UseCase]] = None) -> dict[str, int]:
    cases = usecases if usecases is not None else load_usecases()
    counts = {r: 0 for r in READINESS}
    for case in cases:
        counts[case.readiness] = counts.get(case.readiness, 0) + 1
    return counts


def validate_registry(
    repo_root: Path = REPO_ROOT, path: Path = REGISTRY_PATH
) -> list[str]:
    """Return a list of integrity issues; empty means the registry is sound."""
    issues: list[str] = []
    cases = load_usecases(path)
    seen: set[str] = set()
    for case in cases:
        if case.id in seen:
            issues.append(f"duplicate use-case id: {case.id}")
        seen.add(case.id)
        if case.solver not in SOLVERS:
            issues.append(f"{case.id}: unknown solver {case.solver!r}")
        if case.readiness not in READINESS:
            issues.append(f"{case.id}: unknown readiness {case.readiness!r}")
        # A "ready" use-case must have a committed, resolvable template.
        if case.readiness == "ready":
            if not case.template:
                issues.append(f"{case.id}: readiness=ready but no template")
            elif not (repo_root / case.template).exists():
                issues.append(f"{case.id}: template not found: {case.template}")
        # A committed template path (any readiness) must resolve.
        if case.template and not (repo_root / case.template).exists():
            issues.append(f"{case.id}: template path does not exist: {case.template}")
    return issues
