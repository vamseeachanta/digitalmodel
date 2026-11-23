# ABOUTME: Assemble the dynacard example-card library (synthetic + field archive).
# ABOUTME: Writes src/.../dynacard/data/example_cards.json consumed by example_cards.py.
"""Assemble the **dynacard example-card library** — every identified example
card, from three sources:

1. **Synthetic variants**: every generator mode swept over several seeds and
   severity parameters; only variants that classify back to their own mode
   are kept (verified against the shipped classifier at build time).
2. **Field wells (measured)**: surface cards parsed from dynamometer test
   files (.dyn / spreadsheet exports) with SPM / stroke context — calibrated
   lbs vs inches. Well identities are anonymized.
3. **Field archive (digitized)**: pump-card shapes digitized from a
   hand-labeled card-image archive (phenomenon = archive folder label:
   butterfly, incomplete fillage, fillage collapse, full card) by
   ``extract_dynacard_archive_pngs.py``. Shapes are normalized (unitless)
   marker positions; well names are anonymized.

The field sources live on a **private archive** that must never be named in
the public repo: source paths and real well identifiers are read from the
uncommitted side files ``_measured_cards_manifest.json`` and
``_digitized_cards_stage.json`` next to this script. When those are absent
(any checkout without the private share), the measured/digitized entries
already committed in the library JSON are preserved as-is, so the build is
still reproducible for the synthetic source.

Run (repo root):

    uv run python scripts/capabilities/build_dynacard_example_library.py
"""

from __future__ import annotations

import json
from pathlib import Path

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    ALL_GENERATORS,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
    PumpDiagnostics,
)

_REPO = Path(__file__).resolve().parents[2]
_DATA = (
    _REPO / "src" / "digitalmodel" / "marine_ops" / "artificial_lift"
    / "dynacard" / "data" / "example_cards.json"
)
# Private, uncommitted side files (leading underscore = never git-added).
_MEASURED_MANIFEST = Path(__file__).with_name("_measured_cards_manifest.json")
_DIGITIZED_STAGE = Path(__file__).with_name("_digitized_cards_stage.json")

SEEDS = [711, 42, 7, 2026, 101]
SEVERITY_SWEEPS = {"severity": [0.3, 0.7], "drop_position": [0.45, 0.75]}


def synthetic_cards() -> list[dict]:
    import inspect

    cards = []
    for mode, gen in ALL_GENERATORS.items():
        sig = inspect.signature(gen)
        variants: list[tuple[int, tuple | None]] = [(s, None) for s in SEEDS]
        for pname, vals in SEVERITY_SWEEPS.items():
            if pname in sig.parameters:
                variants += [(711, (pname, v)) for v in vals]
        for seed, param in variants:
            kwargs = {param[0]: param[1]} if param else {}
            card = gen(seed=seed, **kwargs)
            if PumpDiagnostics.classify_card(card) != mode:
                continue  # only verified variants are published
            suffix = f"-{param[0]}{param[1]}" if param else ""
            cards.append(dict(
                id=f"synthetic-{mode.lower()}-s{seed}{suffix}",
                phenomenon=mode,
                source="synthetic-verified",
                units="oilfield",
                render="line",
                position=[round(p, 2) for p in card.position],
                load=[round(v, 1) for v in card.load],
                meta=dict(seed=seed, **({param[0]: param[1]} if param else {})),
            ))
    return cards


def _preserved(source: str) -> list[dict]:
    """Previously committed entries for a field source (no private share)."""
    if not _DATA.exists():
        return []
    prior = json.loads(_DATA.read_text(encoding="utf-8"))["cards"]
    return [c for c in prior if c["source"] == source]


def measured_cards() -> list[dict]:
    if not _MEASURED_MANIFEST.exists():
        return _preserved("field-measured")
    import pandas as pd

    manifest = json.loads(_MEASURED_MANIFEST.read_text(encoding="utf-8"))
    root = Path(manifest["archive_root"])
    cards = []
    for spec in manifest["measured"]:
        path = root / spec["path"]
        if path.suffix == ".dyn":
            lines = path.read_text().replace("\r", "").strip().splitlines()
            pts = []
            for line in lines[1:]:
                try:
                    a, b = line.split(",")
                    pts.append((float(a), float(b)))
                except ValueError:
                    break  # trailer block reached
            pos = [p[0] for p in pts]
            load = [p[1] for p in pts]
        else:
            df = pd.read_excel(path, sheet_name="Full Card", header=None)
            pos = df[0].astype(float).tolist()
            load = df[1].astype(float).tolist()
        cards.append(dict(
            id=spec["id"],
            phenomenon=spec["phenomenon"],
            source="field-measured",
            units="oilfield",
            render="line",
            position=[round(p, 2) for p in pos],
            load=[round(v, 1) for v in load],
            meta=dict(spm=spec["spm"], stroke_in=spec["stroke_in"],
                      card_type="surface"),
        ))
    return cards


def digitized_cards() -> list[dict]:
    if not _DIGITIZED_STAGE.exists():
        return _preserved("field-archive-digitized")
    staged = json.loads(_DIGITIZED_STAGE.read_text(encoding="utf-8"))["cards"]
    for c in staged:
        c.setdefault("render", "dots")
        c.setdefault("meta", {})
        c["meta"].setdefault("n_markers", c.pop("n_markers", None))
        c["meta"].setdefault("chain_q", c.pop("chain_q", None))
        c["meta"]["card_type"] = "pump"
    return staged


def main() -> None:
    cards = synthetic_cards() + measured_cards() + digitized_cards()
    ids = [c["id"] for c in cards]
    assert len(ids) == len(set(ids)), "duplicate example-card ids"
    by_source: dict[str, int] = {}
    for c in cards:
        by_source[c["source"]] = by_source.get(c["source"], 0) + 1
    _DATA.parent.mkdir(parents=True, exist_ok=True)
    _DATA.write_text(
        json.dumps({"version": 1, "cards": cards}, separators=(",", ":")) + "\n",
        encoding="utf-8",
    )
    print(f"Wrote {len(cards)} example cards -> {_DATA.relative_to(_REPO)} "
          f"({_DATA.stat().st_size / 1024:.0f} KB)")
    for src, n in sorted(by_source.items()):
        print(f"  {src:24s} {n}")


if __name__ == "__main__":
    main()
