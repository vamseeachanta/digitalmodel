"""Build ten offline worked dynacard examples from the retained CC0 plate."""
from __future__ import annotations

import argparse
import importlib.util
import json
from pathlib import Path

from digitalmodel.marine_ops.artificial_lift.dynacard.worked_examples import (
    build_examples, results_document,
)

ROOT = Path(__file__).resolve().parents[3]
DEFAULT_OUT = ROOT / "docs/domains/artificial_lift/dynacard-examples"
WORKBOOK = (ROOT / "tests/marine_ops/artificial_lift/dynacard/testdata/"
            "mendeley_plate/Working condition diagnosis plate.xls")


def _helper(name):
    """Load adjacent reporting helpers in both CLI and importlib contexts."""
    path = Path(__file__).with_name(f"dynacard_example_{name}.py")
    spec = importlib.util.spec_from_file_location(f"dynacard_example_{name}", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def text_artifacts(document):
    """Regenerate all non-figure bytes exclusively from a results document."""
    return _helper("text").text_artifacts(document)


def build(out=DEFAULT_OUT, check=False):
    """Build once; compare text without modifying output when checking drift."""
    document = results_document(build_examples(WORKBOOK))
    artifacts = text_artifacts(document)
    out = Path(out)
    if check:
        drift = [name for name, data in artifacts.items()
                 if not (out / name).is_file() or (out / name).read_bytes() != data]
        for name in drift:
            print(f"DRIFT: {name}")
        if not drift:
            print(f"Verified {len(artifacts)} text artifacts; figures excluded.")
        return 1 if drift else 0
    out.mkdir(parents=True, exist_ok=True)
    for name, data in artifacts.items():
        (out / name).write_bytes(data)
    # Readback makes the saved document the sole reporting input.
    saved = json.loads((out / "results.json").read_text(encoding="utf-8"))
    if text_artifacts(saved) != artifacts:
        raise ValueError("Saved results document does not reproduce text artifacts")
    _helper("plots").render_figures(saved, out)
    print(f"Built {len(artifacts)} text artifacts and 22 figures.")
    return 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", type=Path, default=DEFAULT_OUT)
    parser.add_argument("--check", action="store_true",
                        help="Compare fresh text artifacts; figures excluded")
    args = parser.parse_args(argv)
    return build(args.out, check=args.check)


if __name__ == "__main__":
    raise SystemExit(main())
