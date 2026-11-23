#!/usr/bin/env python3
"""publish_atlas_explorer.py — publish digitalmodel parametric atlases as a public
Hugging Face dataset, so they surface on the aceengineer.com capabilities showcase
(epic workspace-hub#3485, C8 = dm#1551).

digitalmodel's FIRST HF integration. Deterministic + reproducible (dispatch rule): it
reads already-committed atlas grids (atlases/<basename>/<default-hash>/grid.parquet) and
their manifests, then publishes a multi-config dataset via huggingface_hub (the `datasets`
lib is not required). Config layout mirrors aceengineer/worldenergydata-explorer.

  python publish_atlas_explorer.py --dry-run          # validate + print plan, no upload
  python publish_atlas_explorer.py                     # publish (needs cached HF token)

Configs published: mooring_fatigue, synthetic_rope_mooring_fatigue, code_check.
"""

import argparse
import io
import os
import sys

import pandas as pd
import yaml

REPO_ID = "aceengineer/digitalmodel-atlas-explorer"
LICENSE = "cc-by-4.0"

# (config_name on HF, atlas basename under atlases/) — declared order = display order.
CONFIGS = [
    ("mooring_fatigue", "mooring_fatigue"),
    ("synthetic_rope_mooring_fatigue", "synthetic_rope_mooring_fatigue"),
    ("code_check", "code_check"),
]

# One-line human descriptions per config for the dataset card.
BLURB = {
    "mooring_fatigue": "Chain/steel mooring fatigue damage (S-N) across stress range, cycle count, and DNV S-N curve class.",
    "synthetic_rope_mooring_fatigue": "Synthetic-rope mooring fatigue damage (T-N) across tension range, cycle count, and minimum breaking load.",
    "code_check": "Riser/pipe code-check utilisation across outer diameter, wall thickness, tension, bending moment, and steel grade.",
}


def resolve_grid(atlases_root, basename):
    """basename -> (parquet_path, manifest_dict) using the atlas's default.txt pointer."""
    default_txt = os.path.join(atlases_root, basename, "default.txt")
    with open(default_txt) as fh:
        atlas_id = fh.read().strip()
    atlas_dir = os.path.join(atlases_root, basename, atlas_id)
    parquet = os.path.join(atlas_dir, "grid.parquet")
    with open(os.path.join(atlas_dir, "manifest.yaml")) as fh:
        manifest = yaml.safe_load(fh)
    if not os.path.exists(parquet):
        raise FileNotFoundError(parquet)
    return parquet, manifest


def std_str(manifest):
    stds = (manifest.get("provenance") or {}).get("standards") or []
    return ", ".join(f"{s.get('id')} ({s.get('edition')})" for s in stds) or "—"


def build_card(entries):
    """entries = [(config_name, df, manifest)]. Returns README.md text with configs YAML."""
    fm = {
        "license": LICENSE,
        "pretty_name": "digitalmodel Parametric Atlas Explorer",
        "tags": [
            "engineering",
            "offshore",
            "fatigue",
            "riser",
            "mooring",
            "parametric",
            "reduced-order-model",
        ],
        "configs": [
            {
                "config_name": name,
                "data_files": [{"split": "train", "path": f"{name}/train.parquet"}],
            }
            for name, _df, _m in entries
        ],
    }
    front = yaml.safe_dump(fm, sort_keys=False, default_flow_style=False).strip()

    lines = [
        "---",
        front,
        "---",
        "",
        "# digitalmodel — Parametric Atlas Explorer",
        "",
        "Pre-computed parametric result grids from **digitalmodel**, each generated from "
        "standard-traceable engineering code and validated against a held-out sample. Every "
        "config is a synthetic parametric sweep — no client or project-specific data.",
        "",
        "These power the live capability pages on "
        "[aceengineer.com/capabilities](https://www.aceengineer.com/capabilities/).",
        "",
        "## Configs",
        "",
    ]
    for name, df, m in entries:
        v = m.get("validation") or {}
        lines += [
            f"### `{name}`",
            "",
            BLURB.get(name, ""),
            "",
            f"- **Standard:** {std_str(m)}",
            f"- **Response:** `{m.get('response')}` · **Rows:** {len(df):,} · **Columns:** {', '.join('`%s`' % c for c in df.columns)}",
            f"- **Validation:** {v.get('metric')} = {v.get('max_rel_error'):.4g} "
            f"(threshold {v.get('threshold')}, {'PASS' if v.get('passes') else 'FAIL'})",
            "",
        ]
    lines += [
        "## Provenance & limits",
        "",
        "Each config is a deterministic sweep over the axes in its `manifest.yaml` (uploaded "
        "alongside the data). Values are model outputs from the cited standard's formulation, "
        "intended for screening and capability demonstration — not a substitute for a "
        "project-specific engineering check. Source: "
        "[digitalmodel](https://github.com/vamseeachanta/digitalmodel).",
        "",
    ]
    return "\n".join(lines)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument(
        "--atlases-root",
        default=os.path.join(
            os.path.dirname(os.path.abspath(__file__)), "..", "..", "atlases"
        ),
    )
    ap.add_argument("--repo-id", default=REPO_ID)
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    atlases_root = os.path.abspath(args.atlases_root)
    print(f"atlases root: {atlases_root}")
    print(f"target repo : {args.repo_id} (dataset)")

    entries = []
    for config_name, basename in CONFIGS:
        parquet, manifest = resolve_grid(atlases_root, basename)
        df = pd.read_parquet(parquet)
        entries.append((config_name, df, manifest, parquet))
        print(
            f"  · {config_name:32s} {len(df):5,} rows  cols={list(df.columns)}  std={std_str(manifest)}"
        )

    card = build_card([(n, df, m) for (n, df, m, _p) in entries])

    if args.dry_run:
        print("\n--- DRY RUN: dataset card preview (head) ---")
        print("\n".join(card.splitlines()[:28]))
        print("--- (no upload performed) ---")
        return

    from huggingface_hub import HfApi

    api = HfApi()
    api.create_repo(args.repo_id, repo_type="dataset", exist_ok=True)
    print(f"\nrepo ready: https://huggingface.co/datasets/{args.repo_id}")

    for config_name, df, manifest, parquet in entries:
        api.upload_file(
            path_or_fileobj=parquet,
            path_in_repo=f"{config_name}/train.parquet",
            repo_id=args.repo_id,
            repo_type="dataset",
        )
        # provenance sidecar: the manifest travels with the data
        buf = io.BytesIO(yaml.safe_dump(manifest, sort_keys=False).encode())
        api.upload_file(
            path_or_fileobj=buf,
            path_in_repo=f"{config_name}/manifest.yaml",
            repo_id=args.repo_id,
            repo_type="dataset",
        )
        print(
            f"  uploaded {config_name}/train.parquet ({len(df):,} rows) + manifest.yaml"
        )

    api.upload_file(
        path_or_fileobj=io.BytesIO(card.encode()),
        path_in_repo="README.md",
        repo_id=args.repo_id,
        repo_type="dataset",
    )
    print("  uploaded README.md (dataset card)")
    print(f"\nDONE → https://huggingface.co/datasets/{args.repo_id}")


if __name__ == "__main__":
    sys.exit(main())
