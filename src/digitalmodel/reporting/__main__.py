#!/usr/bin/env python3
"""Skeleton-first report CLI (#1021).

    uv run python -m digitalmodel.reporting <skeleton.yml> [-o out.html] [--check]

Instantiate a report backbone from a declarative skeleton so analysis fills
slots instead of being written up last. Optionally supply already-filled slots
via ``--content <map.yml>`` (block-key -> HTML/text); empty required slots render
as pending placeholders and are reported by ``--check``.
"""

from __future__ import annotations

import argparse
import json
import sys
from typing import Optional

from digitalmodel.reporting.provenance import Provenance
from digitalmodel.reporting.skeleton import ReportSkeleton


def _load_content(path: Optional[str]) -> dict[str, str]:
    if not path:
        return {}
    import yaml

    with open(path) as f:
        data = yaml.safe_load(f) or {}
    if not isinstance(data, dict):
        raise SystemExit("--content must be a mapping of block-key -> HTML/text")
    return {str(k): str(v) for k, v in data.items()}


def _load_provenance(path: Optional[str]) -> Optional[Provenance]:
    if not path:
        return None
    import yaml

    with open(path) as f:
        data = yaml.safe_load(f) or {}
    return Provenance.model_validate(data)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="python -m digitalmodel.reporting",
        description="Skeleton-first report: build a backbone from a declarative "
        "skeleton; analysis fills the slots.",
    )
    parser.add_argument("skeleton", help="Path to the report skeleton YAML.")
    parser.add_argument(
        "--content", help="Optional YAML map of block-key -> filled HTML/text."
    )
    parser.add_argument(
        "--provenance", help="Optional YAML Provenance ({sources: [...]})."
    )
    parser.add_argument("-o", "--out", help="Write the rendered HTML here.")
    parser.add_argument(
        "--check",
        action="store_true",
        help="Print completeness JSON; exit non-zero if the report is incomplete.",
    )
    args = parser.parse_args(argv)

    skeleton = ReportSkeleton.from_yaml(args.skeleton)
    content = _load_content(args.content)
    provenance = _load_provenance(args.provenance)

    status = skeleton.completeness(content, provenance=provenance)

    if args.out:
        out = skeleton.build_html(content, provenance=provenance, output_path=args.out)
        print(f"wrote {out}")
    elif not args.check:
        # Default: emit the HTML to stdout.
        print(skeleton.build_html(content, provenance=provenance))

    if args.check:
        print(json.dumps(status.model_dump(), indent=2))
    else:
        print(status.summary(), file=sys.stderr)

    return 0 if status.complete else (1 if args.check else 0)


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
