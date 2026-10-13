# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Retrieve reference documents outside Git; factual readback remains manual."""

import argparse
import hashlib
import json
import shutil
import subprocess
import time
from copy import deepcopy
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable
from urllib.error import HTTPError, URLError
from urllib.parse import urlsplit
from urllib.request import HTTPRedirectHandler, Request, build_opener
from urllib.robotparser import RobotFileParser

USER_AGENT = (
    "PublicVesselSpecResearch/1.1 "
    "(+https://github.com/vamseeachanta/digitalmodel; "
    "public specification curation; sequential requests)"
)
ROOT = Path(__file__).resolve().parents[1]
Transport = Callable[[str, str, int], tuple[int, str, bytes]]
Response = tuple[str, bytes]


class NoRedirect(HTTPRedirectHandler):
    """A redirect requires policy review rather than an alternate access route."""

    def redirect_request(
        self, req: Request, fp: Any, code: int, msg: str, headers: Any, newurl: str
    ) -> None:
        """Refuse automatic redirects pending access-policy review."""
        return None


def request_document(url: str, user_agent: str, timeout: int) -> tuple[int, str, bytes]:
    """Request one document without following redirects."""
    request = Request(url, headers={"User-Agent": user_agent})
    try:
        with build_opener(NoRedirect()).open(request, timeout=timeout) as response:
            return response.status, response.url, response.read()
    except HTTPError as exc:
        return exc.code, exc.url, b""


def prepare_output(output: Path, checkout: Path) -> Path:
    """Require an empty output directory outside the checkout."""
    output = Path(output).resolve()
    checkout = Path(checkout).resolve()
    if output == checkout or checkout in output.parents:
        raise ValueError("Output directory must be outside the checkout")
    if output.exists() and (not output.is_dir() or any(output.iterdir())):
        raise ValueError("Output directory must be empty; nothing is overwritten")
    output.mkdir(parents=True, exist_ok=True)
    return output


def is_challenge(body: bytes) -> bool:
    """Detect access-challenge markers in the response sample."""
    sample = body[:262144].lower()
    return any(
        token in sample
        for token in (
            b"verify you are human",
            b"just a moment...",
            b"cf-chl-",
            b"captcha",
            b"access denied",
        )
    )


def fetch_with_policy(
    url: str,
    policy: dict[str, Any],
    transport: Transport,
    pause: Callable[[float], None],
    last_request: set[str],
) -> tuple[Response | None, str | None]:
    """Apply the recorded retry and pause limits to one request."""
    site = urlsplit(url).netloc
    for attempt in range(policy["retry_limit"] + 1):
        if site in last_request:
            pause(policy["same_site_pause_seconds"])
        last_request.add(site)
        try:
            status, resolved, body = transport(
                url, policy["user_agent"], policy["timeout_seconds"]
            )
        except (URLError, TimeoutError, OSError) as exc:
            if attempt < policy["retry_limit"]:
                continue
            return None, f"Transport failure: {type(exc).__name__}"
        if status != 200:
            return None, f"HTTP refusal or redirect: {status}"
        if resolved != url:
            return None, "Redirect requires access-policy review"
        if is_challenge(body):
            return None, "Access challenge"
        return (resolved, body), None
    raise AssertionError("Retry bound exhausted")


def extract_pdf(document: Path, output: Path) -> None:
    """Extract PDF text externally when the tool is available."""
    tool = shutil.which("pdftotext")
    if tool is None:
        print(f"PDF text extraction skipped: pdftotext unavailable ({document.name})")
        return
    text = output / f"{document.stem}.txt"
    if text.exists():
        raise ValueError("Extracted text already exists; nothing is overwritten")
    result = subprocess.run(
        [tool, "-layout", str(document), str(text)],
        capture_output=True,
        check=False,
    )
    if result.returncode:
        print(f"PDF text extraction failed ({document.name}); readback required")


def save_source(
    source: dict[str, Any], response: Response, output: Path, index: int
) -> dict[str, Any]:
    """Save one document and record its retrieval observations."""
    resolved, body = response
    document = output / f"{index:02d}.document"
    with document.open("xb") as stream:
        stream.write(body)
    updated = dict(source)
    updated.update(
        resolved_url=resolved,
        retrieved_utc=datetime.now(timezone.utc).isoformat(),
        sha256=hashlib.sha256(body).hexdigest(),
        size_bytes=len(body),
        status="retrieved",
    )
    if source["role"] == "specification":
        extract_pdf(document, output)
    return updated


def source_policy(
    source: dict[str, Any],
    robots: dict[str, RobotFileParser],
    stopped: set[str],
    policy: dict[str, Any],
) -> tuple[str, str | None]:
    """Check HTTPS and robots permission before content access."""
    url = source["source_url"]
    parsed = urlsplit(url)
    if parsed.scheme != "https" or not parsed.netloc:
        raise ValueError("Sources require HTTPS URLs")
    site = parsed.netloc
    if site in stopped:
        return site, "Site already stopped"
    if source["role"] != "access_policy":
        parser = robots.get(site)
        if parser is None:
            return site, "Robots policy not retrieved before content"
        if not parser.can_fetch(policy["user_agent"], url):
            return site, "Robots disallow"
    return site, None


def write_manifest(
    template: dict[str, Any], result: dict[str, Any], stopped: set[str], output: Path
) -> None:
    """Write observations while retaining prior evidence for stopped sites."""
    if stopped:
        result["sources"] = [
            original if urlsplit(original["source_url"]).netloc in stopped else new
            for original, new in zip(
                template["sources"], result["sources"], strict=True
            )
        ]
        result["assembly"] = (
            "Entries for stopped sites are hand-assembled from "
            "the first retrieval; their original observations are retained."
        )
    with (output / "vessel_specs_provenance.json").open(
        "x", encoding="utf-8"
    ) as stream:
        stream.write(json.dumps(result, indent=2) + "\n")


def retrieve(
    template: dict[str, Any],
    output: Path,
    checkout: Path = ROOT,
    *,
    transport: Transport = request_document,
    pause: Callable[[float], None] = time.sleep,
) -> dict[str, Any]:
    """Emit the input contract with observations; retain first evidence on refusal."""
    output = prepare_output(output, checkout)
    result = deepcopy(template)
    result.pop("assembly", None)
    policy = result["retrieval_policy"]
    policy["user_agent"] = USER_AGENT
    result["refused_sites"] = []
    robots: dict[str, RobotFileParser] = {}
    stopped: set[str] = set()
    last_request: set[str] = set()
    for index, source in enumerate(result["sources"]):
        site, reason = source_policy(source, robots, stopped, policy)
        if site in stopped:
            continue
        response = None
        if reason is None:
            response, reason = fetch_with_policy(
                source["source_url"], policy, transport, pause, last_request
            )
        if reason:
            stopped.add(site)
            result["refused_sites"].append({"site": site, "reason": reason})
            continue
        assert response is not None
        result["sources"][index] = save_source(source, response, output, index)
        if source["role"] == "access_policy":
            parser = RobotFileParser(source["source_url"])
            parser.parse(response[1].decode("utf-8", errors="replace").splitlines())
            robots[site] = parser
    write_manifest(template, result, stopped, output)
    return result


def main() -> None:
    """Read command arguments and emit the retrieval manifest."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "output", type=Path, help="New empty directory outside checkout"
    )
    parser.add_argument(
        "--input",
        type=Path,
        default=ROOT / "assets/data/vessel_specs_provenance.json",
    )
    args = parser.parse_args()
    template = json.loads(args.input.read_text(encoding="utf-8"))
    manifest = retrieve(template, args.output)
    print(f"Manifest emitted; stopped sites: {len(manifest['refused_sites'])}")


if __name__ == "__main__":
    main()
