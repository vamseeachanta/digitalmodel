"""User-run AISC workbook fetch/import into a git-ignored local directory.

Examples (from the repository root):
    PYTHONPATH=src python scripts/integrations/fetch_steel_shapes.py --url PUBLISHER_XLSX_URL
    PYTHONPATH=src python scripts/integrations/fetch_steel_shapes.py --workbook source.xlsx

The repository ships a synthetic sample for tests only. For real section
properties, obtain the shapes database from the publisher yourself, under the
publisher's terms, then run the importer on your local copy; nothing imported
is committed.

The current download URL shall be obtained from the publisher's download page.
Authentication/access errors stop the fetch. No alternate mirror is attempted.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import subprocess
import urllib.request
from collections import Counter
from pathlib import Path
from urllib.parse import urlparse

from digitalmodel.steel_shapes_import import import_workbook

PUBLISHER_PAGE = "https://www.aisc.org/aisc/publications/steel-construction-manual/aisc-shapes-database-v160/"
_PUBLISHER_HOSTS = {"www.aisc.org", "aisc.org", "cloud.aisc.org"}


def publisher_url(url: str) -> str:
    parsed = urlparse(url)
    if (
        parsed.scheme != "https"
        or parsed.hostname not in _PUBLISHER_HOSTS
        or parsed.username
        or parsed.password
        or parsed.port not in {None, 443}
    ):
        raise ValueError("Fetch requires an AISC publisher HTTPS URL")
    return url


class PublisherRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        publisher_url(newurl)
        return super().redirect_request(req, fp, code, msg, headers, newurl)


def output_directory() -> Path:
    """Resolve the owning checkout and verify the fixed destination is ignored."""
    root = Path(__file__).resolve().parents[2]
    output = root / "data" / "steel_shapes_local"
    if output.is_symlink() or output.resolve() != output:
        raise ValueError("Local catalogue destination must not be a symlink")
    # Clear caller Git bindings so check-ignore cannot inspect another checkout.
    environment = {
        key: value
        for key, value in os.environ.items()
        if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}
    }
    check = subprocess.run(
        ["git", "check-ignore", "-q", "data/steel_shapes_local/catalog.json"],
        cwd=root,
        env=environment,
        check=False,
    )
    if check.returncode:
        raise ValueError("Local catalogue destination must be git-ignored")
    if output.exists():
        raise FileExistsError(
            "Local catalogue directory already exists; existing data will be preserved"
        )
    return output


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument(
        "--url", help="Current publisher XLSX download URL; review its terms first"
    )
    source.add_argument(
        "--workbook", type=Path, help="Already obtained AISC v16 workbook"
    )
    parser.add_argument("--strict", action="store_true",
                        help="Reject the complete import if any supported row is invalid")
    args = parser.parse_args()
    output = output_directory()
    if args.url:
        url = publisher_url(args.url)
        opener = urllib.request.build_opener(PublisherRedirect())
        with opener.open(url) as response:
            publisher_url(response.url)
            raw = response.read()
    else:
        url = PUBLISHER_PAGE
        raw = args.workbook.read_bytes()
    # Exclusive creation preserves evidence and refuses a racing second writer.
    output.mkdir(parents=True, exist_ok=False)
    workbook = output / "source.xlsx"
    with workbook.open("xb") as target:
        target.write(raw)
    try:
        catalog = import_workbook(workbook, source_url=url, strict=args.strict)
    except ValueError as exc:
        parser.exit(1, f"Import failed: {exc}\n")
    saved = output / "catalog.json"
    with saved.open("x", encoding="utf-8") as target:
        target.write(json.dumps(catalog, indent=2, allow_nan=False) + "\n")
    if json.loads(saved.read_text()) != catalog:
        raise RuntimeError("Catalogue readback failed")
    if hashlib.sha256(workbook.read_bytes()).hexdigest() != catalog["source"]["sha256"]:
        raise RuntimeError("Raw workbook readback failed")
    print(
        json.dumps(
            {
                "status": "imported; source and engineering qualification remain user-owned",
                "counts": dict(Counter(item["family"] for item in catalog["sections"])),
                "imported": catalog["import_report"]["imported"],
                "rejected": catalog["import_report"]["rejected"],
                "skipped": catalog["import_report"]["skipped"],
                "sha256": catalog["source"]["sha256"],
            }
        )
    )


if __name__ == "__main__":
    main()
