"""Reconciliation test: code-level skip-lists must be documented in the taxonomy.

This is the load-bearing automated enforcement of
``SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md`` §3.1. When any of the four
code-level skip sets drifts away from ``SEMANTIC_DIFF_TAXONOMY.md``, when
``MODEL_CLAIM_REGISTRY.yaml`` references a test file that doesn't exist on
disk, or when ``MODEL_CLAIM_INVENTORY.yaml`` carries a malformed entry, this
test fails with a human-readable diff naming the offending key.

Issue: https://github.com/vamseeachanta/digitalmodel/issues/515 (iteration 3/7)
Follow-up: #515 CF1 (registry/inventory split) — see PR body for details.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pytest
import yaml

# tests/solvers/orcaflex/foo.py -> parents[3] = repo root
REPO_ROOT = Path(__file__).parents[3]
DOCS_DIR = REPO_ROOT / "docs" / "domains" / "orcaflex"
TAXONOMY_PATH = DOCS_DIR / "SEMANTIC_DIFF_TAXONOMY.md"
REGISTRY_PATH = DOCS_DIR / "MODEL_CLAIM_REGISTRY.yaml"
INVENTORY_PATH = DOCS_DIR / "MODEL_CLAIM_INVENTORY.yaml"
BOUNDARY_PATH = DOCS_DIR / "SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md"

# ``scripts/semantic_validate.py`` is a tool, not a package — inject path.
sys.path.insert(0, str(REPO_ROOT / "scripts"))


def _taxonomy_text() -> str:
    return TAXONOMY_PATH.read_text(encoding="utf-8")


def _registry_models() -> list[dict]:
    data = yaml.safe_load(REGISTRY_PATH.read_text(encoding="utf-8"))
    return data["models"]


def _inventory_entries() -> list[dict]:
    data = yaml.safe_load(INVENTORY_PATH.read_text(encoding="utf-8"))
    return data["inventory"]


def _in_taxonomy(prop: str) -> bool:
    """Substring presence is sufficient — the taxonomy doc lists properties
    in backticks, in code blocks, and in comma-separated table cells. A
    strict markdown parse would catch positional drift but reject legitimate
    formatting variations; substring is the looser, more durable check."""
    return prop in _taxonomy_text()


class TestArtifactsExist:
    """The three contract artifacts must all be present."""

    def test_taxonomy_doc_exists(self) -> None:
        assert TAXONOMY_PATH.exists(), f"missing {TAXONOMY_PATH}"

    def test_boundary_doc_exists(self) -> None:
        assert BOUNDARY_PATH.exists(), f"missing {BOUNDARY_PATH}"

    def test_registry_exists_and_parses(self) -> None:
        assert REGISTRY_PATH.exists(), f"missing {REGISTRY_PATH}"
        data = yaml.safe_load(REGISTRY_PATH.read_text(encoding="utf-8"))
        assert "models" in data and isinstance(data["models"], list)
        assert len(data["models"]) >= 3, (
            "registry has fewer than 3 models — plan acceptance criterion requires "
            "a01_catenary_riser, c03_fpso, rigid_jumper at minimum"
        )

    def test_inventory_exists_and_parses(self) -> None:
        """Inventory file (CF1 follow-up split) must parse and carry an
        ``inventory`` list, not a ``models`` list — registry vs inventory
        keying must stay distinct to prevent accidental conflation."""
        assert INVENTORY_PATH.exists(), f"missing {INVENTORY_PATH}"
        data = yaml.safe_load(INVENTORY_PATH.read_text(encoding="utf-8"))
        assert "inventory" in data and isinstance(data["inventory"], list), (
            "inventory file must carry ``inventory`` key (not ``models``) "
            "to prevent conflation with the attested-claims registry"
        )
        assert "models" not in data, (
            "inventory file must NOT carry ``models`` key — that's the "
            "registry's schema. Found ``models`` in inventory file: drift "
            "between the two files."
        )
        assert data.get("purpose") == "inventory_not_claim", (
            "inventory file must carry ``purpose: inventory_not_claim`` so "
            "any reader can disambiguate at a glance from the registry"
        )


class TestSkipListReconciliation:
    """Every code-level skip-list entry must be documented in the taxonomy."""

    def test_skip_general_keys_documented(self) -> None:
        from digitalmodel.solvers.orcaflex.modular_generator.builders.generic_builder import (
            _SKIP_GENERAL_KEYS,
        )

        missing = sorted(k for k in _SKIP_GENERAL_KEYS if not _in_taxonomy(k))
        assert not missing, (
            f"{len(missing)} _SKIP_GENERAL_KEYS entries are NOT documented in "
            f"{TAXONOMY_PATH.name}: {missing}\n"
            f"Fix: add each to the taxonomy doc under §C1 or §C3, or "
            f"remove from generic_builder._SKIP_GENERAL_KEYS."
        )

    def test_skip_object_keys_documented(self) -> None:
        from digitalmodel.solvers.orcaflex.modular_generator.builders.generic_builder import (
            _SKIP_OBJECT_KEYS,
        )

        missing = sorted(k for k in _SKIP_OBJECT_KEYS if not _in_taxonomy(k))
        assert not missing, (
            f"{len(missing)} _SKIP_OBJECT_KEYS entries are NOT documented in "
            f"{TAXONOMY_PATH.name}: {missing}"
        )

    def test_wind_speed_dormant_documented(self) -> None:
        from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import (
            EnvironmentBuilder,
        )

        dormant_values = EnvironmentBuilder._WIND_SPEED_DORMANT
        doc = _taxonomy_text()
        assert "_WIND_SPEED_DORMANT" in doc, (
            "_WIND_SPEED_DORMANT variable name not mentioned in taxonomy doc"
        )
        missing = sorted(v for v in dormant_values if v not in doc)
        assert not missing, (
            f"_WIND_SPEED_DORMANT WindType values not in taxonomy: {missing}"
        )

    def test_allowed_diff_props_classified(self) -> None:
        """Properties in ALLOWED_DIFF_PROPS must be either skip-list entries
        (C3) or mentioned in the taxonomy doc (C1 cosmetic table). Soft
        threshold tolerates legitimate in-flight taxonomy work."""
        try:
            from semantic_validate import ALLOWED_DIFF_PROPS  # type: ignore
        except ImportError as exc:
            pytest.skip(f"scripts/semantic_validate.py not importable: {exc}")

        from digitalmodel.solvers.orcaflex.modular_generator.builders.generic_builder import (
            _SKIP_GENERAL_KEYS,
        )

        unclassified = sorted(
            p
            for p in ALLOWED_DIFF_PROPS
            if p not in _SKIP_GENERAL_KEYS and not _in_taxonomy(p)
        )
        # Tightened 2026-05-11 (#515 follow-up CSF1): post-iter-5 OQ
        # classifications landed, so the taxonomy is now caught up. Threshold
        # dropped from `max(5, len // 10)` to `max(2, len // 20)` to catch
        # smaller drift that would have silently slipped through at 10 %.
        threshold = max(2, len(ALLOWED_DIFF_PROPS) // 20)
        assert len(unclassified) <= threshold, (
            f"{len(unclassified)} ALLOWED_DIFF_PROPS properties are neither "
            f"skip-list entries nor documented in the taxonomy "
            f"(threshold: {threshold}). First 10: {unclassified[:10]}"
        )


class TestRegistryReconciliation:
    """Every claim-registry entry must back-reference a real test file."""

    def test_attested_entries_have_resolvable_tests(self) -> None:
        missing = []
        for m in _registry_models():
            if m.get("pending"):
                continue
            path = m.get("test_enforcing")
            assert path, (
                f"attested model {m['name']!r} has no test_enforcing path"
            )
            file_path = REPO_ROOT / path.split("::")[0]
            if not file_path.exists():
                missing.append((m["name"], path))
        assert not missing, (
            f"{len(missing)} attested registry entries point at non-existent "
            f"tests:\n  " + "\n  ".join(f"{n}: {p}" for n, p in missing)
        )

    def test_attested_entries_have_required_fields(self) -> None:
        required = {
            "name",
            "family",
            "builder_track",
            "highest_validated_level",
            "test_enforcing",
        }
        for m in _registry_models():
            if m.get("pending"):
                continue
            missing_fields = required - set(m.keys())
            assert not missing_fields, (
                f"attested model {m.get('name', '<unnamed>')!r} missing "
                f"required fields: {missing_fields}"
            )
            assert m["highest_validated_level"] in {"L1", "L2", "L3"}, (
                f"{m['name']!r} has invalid level "
                f"{m['highest_validated_level']!r} (must be L1/L2/L3)"
            )

    def test_no_pending_entries_in_registry(self) -> None:
        """Per CF1 follow-up split, the registry MUST NOT carry pending
        entries — they live in MODEL_CLAIM_INVENTORY.yaml. Any ``pending``
        marker or ``highest_validated_level: pending`` in the registry is
        either drift from the pre-split shape or an attempt to slip a
        non-attested entry into the claim surface."""
        for m in _registry_models():
            assert not m.get("pending"), (
                f"{m['name']!r} carries ``pending: true`` in the registry — "
                f"move it to MODEL_CLAIM_INVENTORY.yaml. Per #515 CF1, the "
                f"registry is attested-claims-only."
            )
            assert m.get("highest_validated_level") != "pending", (
                f"{m['name']!r} has ``highest_validated_level: pending`` in "
                f"the registry — move it to MODEL_CLAIM_INVENTORY.yaml."
                )


class TestCrossReferences:
    """The three contract artifacts must cross-reference each other."""

    def test_boundary_doc_links_taxonomy(self) -> None:
        body = BOUNDARY_PATH.read_text(encoding="utf-8")
        assert "SEMANTIC_DIFF_TAXONOMY.md" in body

    def test_boundary_doc_links_registry(self) -> None:
        body = BOUNDARY_PATH.read_text(encoding="utf-8")
        assert "MODEL_CLAIM_REGISTRY.yaml" in body

    def test_taxonomy_carries_adoption_header(self) -> None:
        """Iteration 1 added an 'Adopted by #515' header. If it's removed
        (e.g. by a force-rewrite or merge accident), the taxonomy is no
        longer ratified as the claim-boundary classification policy."""
        body = TAXONOMY_PATH.read_text(encoding="utf-8")
        assert "Adopted by:" in body
        assert "#515" in body
