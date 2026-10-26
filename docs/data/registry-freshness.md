# Registry freshness checker

The standalone checker accepts an explicit YAML file and reports HEAD outcomes.
Dry-run is the default. The production registry is absent; this change supplies
no live inventory and changes no schedule.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-26 | Document explicit input, offline examples and opt-in updates. |
| Update | 2026-10-26 | Document per-URL protocol failures and explicit example text encoding. |

## Input and output

The public input contract is a YAML mapping containing `resources`, a list of
mappings with an HTTP(S) `url`. This is a supported fixture schema, not a claim
about the shape or contents of an unavailable production registry. Empty lists
are valid. Unrelated fields and resource ordering are retained as YAML values.
Duplicate mapping keys, YAML aliases, credential-bearing URLs and malformed
input are rejected before requests are sent.

Each distinct URL is checked once, in lexical order, with bounded additional
attempts. JSON contains `records`, `summary` and `applied`. Each record carries
`url`, `status`, `http_status`, `location`, `attempts`, `error` and `last_checked`.
The summary counts distinct URLs: `total`, `alive`, `redirected`, `dead`, `timeout`.
Repeated resource rows receive the same result when applying updates.

HTTP classification uses Python's `HTTPStatus` classes, described in RFC 9110,
section 15: successful responses are `alive`, redirects are `redirected`, and
other HTTP responses are `dead`. Redirects are not followed and no GET fallback
is made. Authentication failures and servers that reject HEAD are reported as
HTTP failures; this does not establish that their content is unavailable to
an authenticated browser.

Connection errors, HTTP protocol failures and timeouts are retried. The checker
also retries `TOO_MANY_REQUESTS` and server errors, stopping after the supplied
retry count is exhausted. Retries are immediate; backoff and Retry-After handling
are not implemented. The supplied timeout is a socket timeout per attempt, not a strict
wall-clock limit on DNS resolution or the complete run. Timeout records have
`status: timeout` and `error: timeout`; connection failures have `status: dead`
and `error: connection_error`. Malformed HTTP responses have `status: dead` and
`error: protocol_error`; later URLs are still checked. Exception text is not
included in reports.

With `--apply`, `last_checked` becomes the UTC check date and `download_status`
becomes `alive`, `redirected`, `dead_link` or `timeout`. All records use a single
clock reading. File permissions are retained. Writes stage a complete YAML file
beside the input, flush it and replace the input atomically. Failed staging or
replacement preserves original bytes. Ordinary concurrent edits and symlinks
are rejected; there is no interprocess lock, and the final comparison and rename
are not a transaction against other writers. Comments and scalar formatting may
change on apply. Successful dry-runs preserve original bytes and modification
time. Atomic replacement does not promise recovery from every power failure.

## Offline CLI example

Requires Python with PyYAML. From the repository root, the following generates
a synthetic fixture in the supplied temporary directory and invokes the CLI
with fake HTTP and a fixed clock. It performs no network request. The reserved
`example.test` namespace follows RFC 2606, section 2.

```sh
python - "$TMPDIR/registry-example.yaml" <<'PY'
from datetime import datetime
from http import HTTPStatus
import importlib.util
from pathlib import Path
import sys
import yaml

path = Path(sys.argv[1])
path.write_text(yaml.safe_dump({
    "note": "Synthetic fixture, not a production inventory",
    "resources": [{"url": "https://example.test/resource", "title": "Example"}],
}), encoding="utf-8", newline="\n")
spec = importlib.util.spec_from_file_location(
    "freshness", "scripts/data/check-registry-freshness.py"
)
checker = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = checker
spec.loader.exec_module(checker)
checker.head = lambda url, timeout: checker.HTTPResult(HTTPStatus.OK)
checker.utc_now = lambda: datetime.fromisoformat("2025-02-03T12:00:00+00:00")
raise SystemExit(checker.main([str(path), "--timeout", "0.25", "--retries", "2"]))
PY
```

The fixed date and policy values come from the synthetic contract examples in
`tests/data/test_registry_freshness.py`: the quarter-second timeout is arbitrary,
and two additional retries allow at most three attempts by hand count. The
example produces one alive record, zero records in each other category and
`applied: false`. Add `--apply` to the argument list to exercise opt-in updates.
No values in this example are live measurements or recommended service settings.

The same test file exercises argparse and JSON output in subprocesses with
socket creation blocked. Its fake transport and injected clock validate retries,
HTTP failures, summaries, dry-run purity and interrupted writes:

```sh
python -m pytest tests/data/test_registry_freshness.py -q -p no:cacheprovider
```

For an actual supplied registry, the entry point is
`python scripts/data/check-registry-freshness.py REGISTRY --timeout SECONDS --retries COUNT`.
That command contacts the URLs in the supplied file even in dry-run mode.
A successful check, including reported dead links, exits successfully; malformed
input, invalid policy or file errors use argparse's command-line error status.
The production inventory and periodic scheduling require separate follow-up.
