# ABOUTME: Engine router for the DNV-OS-E301 mooring MBL safety-factor pilot under a
# ABOUTME: NEW basename "mooring_mbl" (workspace-hub#3285). Does NOT touch reserved "mooring".
"""Engine route for the mooring MBL safety-factor calc-citation pilot (#2685).

The ``orcaflex/mooring_design.py`` ``MooringLineDesign.check_mbl_with_safety_factor``
pilot is the live DNV-OS-E301 calc-citation surface. ``engine.py``'s existing
``mooring`` basename is RESERVED for the separate ``subsea/mooring_analysis/``
subsystem (it raises a redirect ``NotImplementedError``), so this pilot gets a NEW
non-colliding basename ``mooring_mbl`` -- the reserved ``mooring`` arm is left
untouched.

``kind: in_memory`` -- the result (per-segment utilisation + DNV-OS-E301 citation
sidecar) is parked on ``cfg["mooring_mbl"]``. Citations are serialised to plain
dicts so the in-memory payload is JSON-clean for the determinism hash, and the
digitalmodel runner lifts them into ``provenance.standard_revisions``.
"""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from pathlib import Path
from typing import Any, Optional

from digitalmodel.orcaflex.mooring_design import MooringLineDesign


class MooringMBLWorkflow:
    """Engine adapter for the mooring MBL pilot (NEW basename ``mooring_mbl``)."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        mbl_cfg = cfg.get("mooring_mbl_design")
        if not mbl_cfg:
            raise KeyError(
                "mooring_mbl workflow requires a 'mooring_mbl_design' block with "
                "'design' and 'max_tension_kn' keys"
            )

        design = MooringLineDesign(**mbl_cfg.get("design", {}))
        repo_root = mbl_cfg.get("repo_root")
        out = design.check_mbl_with_safety_factor(
            float(mbl_cfg["max_tension_kn"]),
            condition=mbl_cfg.get("condition", "intact"),
            repo_root=Path(repo_root) if repo_root else None,
        )

        # Serialise Citation sidecars to plain dicts -> JSON-clean in_memory payload.
        out = dict(out)
        out["citations"] = [_citation_to_dict(c) for c in out.get("citations", [])]

        cfg[cfg["basename"]] = out
        return cfg


def _citation_to_dict(citation: Any) -> dict[str, Optional[str]]:
    """Normalise a Citation (frozen dataclass) or dict to a plain dict."""
    if isinstance(citation, dict):
        src = citation
    elif is_dataclass(citation):
        src = asdict(citation)
    else:  # pragma: no cover - defensive
        src = {
            k: getattr(citation, k, None)
            for k in ("code_id", "publisher", "revision", "section", "wiki_path", "note")
        }
    return {
        "code_id": src.get("code_id"),
        "publisher": src.get("publisher"),
        "revision": src.get("revision"),
        "section": src.get("section"),
        "wiki_path": src.get("wiki_path"),
        "note": src.get("note", ""),
    }
