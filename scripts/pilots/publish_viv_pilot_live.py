#!/usr/bin/env python3
# ABOUTME: GATED LIVE publish of the #1505 VIV pilot to Hugging Face.
# ABOUTME: This DOES publish a PUBLIC dataset to aceengineer/digitalmodel-runs.
"""Live publish entry point for the digitalmodel #1505 VIV parametric pilot.

Unlike ``run_viv_pilot.py`` (InMemoryHfPort, no network), this drives the SAME
verified pipeline through the real ``HuggingFaceHubHfPort`` — it creates/updates
the public dataset ``aceengineer/digitalmodel-runs`` and pushes immutable
revisions. Auth is environment-backed (huggingface_hub resolves ``HF_TOKEN`` /
the cached login itself); the token is never read, logged, or passed here.

Run ONLY with explicit owner go-ahead and a WRITE-scope HF token::

    uv run --python 3.11 --with huggingface_hub python scripts/pilots/publish_viv_pilot_live.py

On a read-only token it fails at ``create_commit`` with an auth error (nothing
partially published — the promotion ledger only accepts after verification).
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import viv_pilot  # noqa: E402
from assetutilities.workflow_api.publication import hf_port as hp  # noqa: E402


def main() -> int:
    print(f"LIVE publish → {viv_pilot.DATASET_REPO} (real HuggingFaceHubHfPort)")
    port = hp.HuggingFaceHubHfPort(repo_id=viv_pilot.DATASET_REPO, private=False)
    try:
        summary = viv_pilot.run_pilot(hf_port=port)
    except Exception as e:  # pragma: no cover - live path
        msg = str(e)
        print("LIVE PUBLISH FAILED:", type(e).__name__)
        print("detail:", msg[:400])
        low = msg.lower()
        if any(k in low for k in ("401", "403", "unauthorized", "forbidden",
                                  "permission", "write", "authentication", "token")):
            print(">>> A WRITE-scope HF token is required. Create one at "
                  "https://huggingface.co/settings/tokens (Write) and retry.")
        return 1
    print("LIVE PUBLISH OK")
    print("dataset :", summary.get("dataset_repo"))
    print("revisions:", summary.get("revisions"))
    print("accepted :", summary.get("accepted_count"))
    print("Verify: https://huggingface.co/datasets/%s" % viv_pilot.DATASET_REPO)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
