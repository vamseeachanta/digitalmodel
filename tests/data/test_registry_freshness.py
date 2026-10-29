# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline contract examples for the standalone registry freshness checker.

Provenance: every date and policy value below is synthetic test data. The fixed
UTC date is an arbitrary calendar example, not an observation. HTTP values use
Python's HTTPStatus names. Retry expectations are hand counts: an initial call
plus the specified additional retries. Summary expectations count the literal
fixture rows independently of the checker. The example.test namespace is
reserved for testing by RFC 2606, section 2.
"""

import copy
from contextlib import nullcontext
from datetime import datetime
from http import HTTPStatus
from http.client import BadStatusLine, LineTooLong
import importlib.util
import json
import os
from pathlib import Path
import socket
import subprocess
import sys
from types import SimpleNamespace
from urllib.error import HTTPError, URLError

import pytest
import yaml

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/data/check-registry-freshness.py"
NOW = datetime.fromisoformat("2025-02-03T12:00:00+00:00")
DATE = "2025-02-03"
URL = "https://example.test/resource"
TIMEOUT = 0.25  # Synthetic short per-attempt socket timeout, seconds.
RETRIES = 2  # Synthetic policy: initial call + two retries = three attempts.


@pytest.fixture
def checker():
    spec = importlib.util.spec_from_file_location("registry_freshness", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


@pytest.fixture(autouse=True)
def forbid_network(monkeypatch):
    def denied(*args, **kwargs):
        pytest.fail("Tests must never open a network connection")

    monkeypatch.setattr(socket, "socket", denied)
    monkeypatch.setattr(socket, "create_connection", denied)
    monkeypatch.setattr(socket, "getaddrinfo", denied)


@pytest.fixture
def registry(tmp_path):
    path = tmp_path / "registry.yaml"
    document = {
        "description": "Synthetic public input-schema example; no inventory claim",
        "metadata": {"retain": ["alpha", "beta"]},
        "resources": [{"url": URL, "title": "Example", "download_status": "unknown",
                       "last_checked": "2024-01-01", "tags": ["fixture"]}],
    }
    path.write_text("# Preserve exact bytes in dry-run\n" + yaml.safe_dump(document),
                    encoding="utf-8", newline="\n")
    return path, document


class FakeHead:
    def __init__(self, *outcomes):
        self.outcomes = iter(outcomes)
        self.calls = []

    def __call__(self, url, timeout):
        self.calls.append((url, timeout))
        outcome = next(self.outcomes)
        if isinstance(outcome, BaseException):
            raise outcome
        return outcome


def run(checker, path, head, **kwargs):
    return checker.check_registry(path, timeout=TIMEOUT, retries=RETRIES,
                                  transport=head, clock=lambda: NOW, **kwargs)


@pytest.mark.parametrize("code,status,download_status", [
    (HTTPStatus.OK, "alive", "alive"),
    (HTTPStatus.NO_CONTENT, "alive", "alive"),
    (HTTPStatus.MOVED_PERMANENTLY, "redirected", "redirected"),
    (HTTPStatus.TEMPORARY_REDIRECT, "redirected", "redirected"),
    (HTTPStatus.NOT_FOUND, "dead", "dead_link"),
    (HTTPStatus.GONE, "dead", "dead_link"),
    (HTTPStatus.FORBIDDEN, "dead", "dead_link"),
    (HTTPStatus.METHOD_NOT_ALLOWED, "dead", "dead_link"),
])
def test_head_classification(checker, registry, code, status, download_status):
    path, _ = registry
    location = "/moved" if status == "redirected" else None
    head = FakeHead(checker.HTTPResult(code, location))
    report = run(checker, path, head, apply=True)
    assert report["records"] == [{
        "url": URL, "status": status, "http_status": code, "location": location,
        "attempts": 1, "error": None, "last_checked": DATE,
    }]
    assert head.calls == [(URL, TIMEOUT)]
    row = yaml.safe_load(path.read_text(encoding="utf-8"))["resources"][0]
    assert row["download_status"] == download_status


@pytest.mark.parametrize("error,status,reason", [
    (TimeoutError(), "timeout", "timeout"),
    (URLError(socket.timeout()), "timeout", "timeout"),
    (URLError("connection refused"), "dead", "connection_error"),
    (ConnectionError(), "dead", "connection_error"),
])
def test_bounded_error_retries(checker, registry, error, status, reason):
    path, _ = registry
    head = FakeHead(error, error, error)
    record = run(checker, path, head, apply=True)["records"][0]
    assert record["status"] == status
    assert record["error"] == reason
    assert record["http_status"] is None
    assert record["attempts"] == 3  # Initial + two retries, hand counted.
    assert head.calls == [(URL, TIMEOUT)] * 3
    row = yaml.safe_load(path.read_text(encoding="utf-8"))["resources"][0]
    assert row["download_status"] == ("timeout" if status == "timeout" else "dead_link")


@pytest.mark.parametrize("code", [HTTPStatus.SERVICE_UNAVAILABLE, HTTPStatus.TOO_MANY_REQUESTS])
def test_transient_http_exhaustion(checker, registry, code):
    head = FakeHead(*(checker.HTTPResult(code) for _ in range(3)))
    record = run(checker, registry[0], head)["records"][0]
    assert (record["status"], record["http_status"], record["attempts"]) == ("dead", code, 3)


def test_retry_recovery(checker, registry):
    head = FakeHead(TimeoutError(), checker.HTTPResult(HTTPStatus.SERVICE_UNAVAILABLE),
                    checker.HTTPResult(HTTPStatus.OK))
    record = run(checker, registry[0], head)["records"][0]
    assert (record["status"], record["attempts"], record["error"]) == ("alive", 3, None)


def test_zero_retries(checker, registry):
    head = FakeHead(TimeoutError())
    result = checker.check_registry(registry[0], timeout=TIMEOUT, retries=0,
                                    transport=head, clock=lambda: NOW)
    assert result["records"][0]["attempts"] == 1


def test_summary_deduplication_and_order(checker, registry):
    path, document = registry
    urls = [f"https://example.test/{suffix}" for suffix in ("a", "b", "c", "d")]
    document["resources"] = [{"url": url} for url in reversed(urls)] + [{"url": urls[0]}]
    path.write_text(yaml.safe_dump(document), encoding="utf-8", newline="\n")
    head = FakeHead(checker.HTTPResult(HTTPStatus.OK),
                    checker.HTTPResult(HTTPStatus.FOUND, "/elsewhere"),
                    checker.HTTPResult(HTTPStatus.NOT_FOUND),
                    TimeoutError(), TimeoutError(), TimeoutError())
    report = run(checker, path, head, apply=True)
    assert [row["url"] for row in report["records"]] == urls
    # Four distinct fixture URLs: one in each status; duplicate adds no URL.
    assert report["summary"] == {"total": 4, "alive": 1, "redirected": 1, "dead": 1, "timeout": 1}
    rows = yaml.safe_load(path.read_text(encoding="utf-8"))["resources"]
    assert [row["url"] for row in rows] == [row["url"] for row in document["resources"]]
    assert rows[-1]["download_status"] == "alive"


def test_dry_run_is_default_and_byte_pure(checker, registry):
    path, _ = registry
    original = path.read_bytes()
    before = path.stat()
    report = run(checker, path, FakeHead(checker.HTTPResult(HTTPStatus.OK)))
    assert report["applied"] is False
    assert path.read_bytes() == original
    assert path.stat().st_mtime_ns == before.st_mtime_ns
    assert sorted(p.name for p in path.parent.iterdir()) == [path.name]


def test_apply_preserves_unrelated_fields_and_permissions(checker, registry):
    path, document = registry
    path.chmod(0o640)  # Synthetic POSIX fixture mode: user read/write, group read.
    run(checker, path, FakeHead(checker.HTTPResult(HTTPStatus.OK)), apply=True)
    expected = copy.deepcopy(document)
    expected["resources"][0].update(last_checked=DATE, download_status="alive")
    assert yaml.safe_load(path.read_text(encoding="utf-8")) == expected
    assert path.stat().st_mode & 0o777 == 0o640


@pytest.mark.parametrize("text", [
    "", "[", "[]", "resources: null", "resources: {}", "resources: [null]",
    "resources: [{}]", "resources: [{url: 42}]",
    "resources: [{url: 'file:///fixture'}]", "resources: [{url: 'https://'}]",
    "resources: [{url: 'https://example.test/has space'}]",
    "resources: []\nresources: []", "resources: &rows [*rows]",
])
def test_malformed_input_is_rejected_before_requests(checker, tmp_path, text):
    path = tmp_path / "bad.yaml"
    path.write_text(text, encoding="utf-8", newline="\n")
    original = path.read_bytes()
    head = FakeHead()
    with pytest.raises(ValueError):
        run(checker, path, head, apply=True)
    assert path.read_bytes() == original
    assert not head.calls


@pytest.mark.parametrize("policy", [
    {"timeout": 0}, {"timeout": -1}, {"timeout": float("nan")},
    {"timeout": float("inf")}, {"retries": -1}, {"retries": 0.5},
    {"timeout": True}, {"retries": True},
])
def test_invalid_policy_preserves_input(checker, registry, policy):
    path, _ = registry
    original = path.read_bytes()
    args = {"timeout": TIMEOUT, "retries": RETRIES, **policy}
    with pytest.raises(ValueError):
        checker.check_registry(path, transport=FakeHead(), clock=lambda: NOW, apply=True, **args)
    assert path.read_bytes() == original


@pytest.mark.parametrize("failure", [OSError("synthetic replacement failure"), KeyboardInterrupt()])
def test_interrupted_replace_preserves_original_bytes(checker, registry, monkeypatch, failure):
    path, _ = registry
    original = path.read_bytes()

    def interrupted(source, target):
        assert Path(source).parent == path.parent
        assert Path(target) == path
        assert path.read_bytes() == original
        raise failure

    monkeypatch.setattr(checker.os, "replace", interrupted)
    with pytest.raises(type(failure)):
        run(checker, path, FakeHead(checker.HTTPResult(HTTPStatus.OK)), apply=True)
    assert path.read_bytes() == original
    assert list(path.parent.iterdir()) == [path]


def test_interrupted_fsync_preserves_original(checker, registry, monkeypatch):
    path, _ = registry
    original = path.read_bytes()

    def interrupted(*args):
        raise OSError("synthetic flush failure")

    monkeypatch.setattr(checker.os, "fsync", interrupted)
    with pytest.raises(OSError):
        run(checker, path, FakeHead(checker.HTTPResult(HTTPStatus.OK)), apply=True)
    assert path.read_bytes() == original


def test_empty_registry(checker, tmp_path):
    path = tmp_path / "empty.yaml"
    path.write_text("resources: []\n", encoding="utf-8", newline="\n")
    result = run(checker, path, FakeHead())
    assert result["records"] == []
    assert result["summary"] == dict(total=0, alive=0, redirected=0, dead=0, timeout=0)


def test_clock_uses_utc_date(checker, registry):
    head = FakeHead(checker.HTTPResult(HTTPStatus.OK))
    # Hand conversion: evening at UTC-02:00 is the following UTC calendar day.
    clock = lambda: datetime.fromisoformat("2025-02-02T23:30:00-02:00")
    report = checker.check_registry(registry[0], timeout=TIMEOUT, retries=RETRIES,
                                    transport=head, clock=clock)
    assert report["records"][0]["last_checked"] == DATE


def test_real_adapter_sends_head_without_following_redirects(checker, monkeypatch):
    calls = []

    class Opener:
        def open(self, request, timeout):
            calls.append((request.full_url, request.get_method(), timeout))
            raise HTTPError(URL, HTTPStatus.FOUND, "synthetic redirect",
                            {"Location": "/new"}, None)

    monkeypatch.setattr(checker, "build_opener", lambda *handlers: Opener())
    assert checker.head(URL, TIMEOUT) == checker.HTTPResult(HTTPStatus.FOUND, "/new")
    assert calls == [(URL, "HEAD", TIMEOUT)]
    assert checker.NoRedirect().redirect_request(None, None, None, None, None, None) is None


@pytest.mark.parametrize("error_type", [BadStatusLine, LineTooLong])
def test_real_adapter_protocol_failure_uses_retry_budget(checker, registry, monkeypatch, error_type):
    class Opener:
        def open(self, request, timeout):
            raise error_type("synthetic protocol detail")

    monkeypatch.setattr(checker, "build_opener", lambda *handlers: Opener())
    report = run(checker, registry[0], checker.head)
    assert report["records"] == [{
        "url": URL, "status": "dead", "http_status": None, "location": None,
        "attempts": 3, "error": "protocol_error", "last_checked": DATE,
    }]  # Initial attempt + two additional retries = three failed attempts.
    assert "synthetic protocol detail" not in json.dumps(report)


@pytest.mark.parametrize("error_type", [BadStatusLine, LineTooLong])
@pytest.mark.parametrize("apply", [False, True])
def test_main_protocol_failure_emits_json_and_checks_later_url(
        checker, registry, monkeypatch, capsys, error_type, apply):
    path, document = registry
    urls = ["https://example.test/a", "https://example.test/b"]
    document["resources"] = [{"url": url} for url in urls]
    path.write_text(yaml.safe_dump(document), encoding="utf-8", newline="\n")
    original = path.read_bytes()

    class Opener:
        def open(self, request, timeout):
            if request.full_url == urls[0]:
                raise error_type("synthetic protocol detail")
            return nullcontext(SimpleNamespace(status=HTTPStatus.OK, headers={}))

    monkeypatch.setattr(checker, "build_opener", lambda *handlers: Opener())
    monkeypatch.setattr(checker, "utc_now", lambda: NOW)
    args = [path.as_posix(), "--timeout", str(TIMEOUT), "--retries", str(RETRIES)]
    if apply:
        args.append("--apply")
    assert checker.main(args) == 0
    output = capsys.readouterr()
    assert not output.err
    assert "synthetic protocol detail" not in output.out
    report = json.loads(output.out)
    # Two distinct URLs: one exhausted protocol failure and one alive result.
    assert report["summary"] == dict(total=2, alive=1, redirected=0, dead=1, timeout=0)
    assert [record["url"] for record in report["records"]] == urls
    assert [record["attempts"] for record in report["records"]] == [3, 1]
    assert [record["error"] for record in report["records"]] == ["protocol_error", None]
    assert report["applied"] is apply
    if apply:
        rows = yaml.safe_load(path.read_text(encoding="utf-8"))["resources"]
        assert [row["download_status"] for row in rows] == ["dead_link", "alive"]
    else:
        assert path.read_bytes() == original


@pytest.fixture
def cli(tmp_path):
    # Subprocess bootstrap substitutes HTTP only; production argparse, main,
    # serialization and atomic write code run unchanged. Socket calls also fail.
    bootstrap = tmp_path / "offline_cli.py"
    bootstrap.write_text(
        "import importlib.util, socket, ssl, sys\n"
        "from datetime import datetime\n"
        "def denied(*a, **kw):\n    raise AssertionError('network forbidden')\n"
        "socket.socket = socket.create_connection = socket.getaddrinfo = denied\n"
        "spec = importlib.util.spec_from_file_location('checker', "
        "'scripts/data/check-registry-freshness.py')\n"
        "m = importlib.util.module_from_spec(spec)\nsys.modules[spec.name] = m\n"
        "spec.loader.exec_module(m)\n"
        "m.head = lambda url, timeout: m.HTTPResult(m.HTTPStatus.OK)\n"
        "m.utc_now = lambda: datetime.fromisoformat('2025-02-03T12:00:00+00:00')\n"
        "raise SystemExit(m.main(sys.argv[1:]))\n",
        encoding="utf-8", newline="\n",
    )

    def invoke(*args):
        env = os.environ.copy()
        for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
            env.pop(key, None)
        return subprocess.run([sys.executable, str(bootstrap), *map(str, args)],
                              capture_output=True, text=True, env=env,
                              cwd=SCRIPT.parents[2], check=False)

    return invoke


@pytest.mark.parametrize("apply", [False, True])
def test_subprocess_cli(cli, registry, apply):
    path, document = registry
    original = path.read_bytes()
    args = [path, "--timeout", str(TIMEOUT), "--retries", str(RETRIES)]
    if apply:
        args.append("--apply")
    result = cli(*args)
    assert result.returncode == 0, result.stderr
    report = json.loads(result.stdout)
    assert report["applied"] is apply
    assert report["summary"] == dict(total=1, alive=1, redirected=0, dead=0, timeout=0)
    if apply:
        document["resources"][0].update(last_checked=DATE, download_status="alive")
        assert yaml.safe_load(path.read_text(encoding="utf-8")) == document
    else:
        assert path.read_bytes() == original


@pytest.mark.parametrize("arguments", [[], ["--timeout", "nan", "--retries", "0"],
                                       ["--timeout", "1", "--retries", "-1"]])
def test_subprocess_cli_rejects_missing_or_invalid_policy(cli, registry, arguments):
    path, _ = registry
    original = path.read_bytes()
    result = cli(path, *arguments)
    assert result.returncode == 2  # argparse's documented command-line error exit.
    assert not result.stdout
    assert path.read_bytes() == original


def test_subprocess_cli_rejects_malformed_yaml(cli, tmp_path):
    path = tmp_path / "bad.yaml"
    path.write_text("[", encoding="utf-8", newline="\n")
    result = cli(path, "--timeout", str(TIMEOUT), "--retries", str(RETRIES), "--apply")
    assert result.returncode == 2
    assert path.read_bytes() == b"["
    assert "invalid registry" in result.stderr.lower()


def test_apply_detects_concurrent_edit(checker, registry):
    path, _ = registry
    changed = b"resources: []\nmetadata: concurrent edit\n"

    def head(url, timeout):
        path.write_bytes(changed)
        return checker.HTTPResult(HTTPStatus.OK)

    with pytest.raises(ValueError, match="changed during checking"):
        run(checker, path, head, apply=True)
    assert path.read_bytes() == changed


def test_apply_rejects_symlink(checker, registry, tmp_path):
    path, _ = registry
    original = path.read_bytes()
    link = tmp_path / "link.yaml"
    link.symlink_to(path)
    with pytest.raises(ValueError, match="symlink"):
        run(checker, link, FakeHead(checker.HTTPResult(HTTPStatus.OK)), apply=True)
    assert link.is_symlink()
    assert path.read_bytes() == original


def test_naive_clock_rejected_before_http(checker, registry):
    with pytest.raises(ValueError, match="timezone-aware"):
        checker.check_registry(registry[0], timeout=TIMEOUT, retries=RETRIES,
                               transport=FakeHead(), clock=lambda: NOW.replace(tzinfo=None))


def test_real_adapter_success_closes_response(checker, monkeypatch):
    class Response:
        status = HTTPStatus.NO_CONTENT
        headers = {}
        closed = False

        def __enter__(self):
            return self

        def __exit__(self, *args):
            self.closed = True

    response = Response()

    class Opener:
        def open(self, request, timeout):
            assert request.get_method() == "HEAD"
            assert timeout == TIMEOUT
            return response

    monkeypatch.setattr(checker, "build_opener", lambda *handlers: Opener())
    assert checker.head(URL, TIMEOUT) == checker.HTTPResult(HTTPStatus.NO_CONTENT)
    assert response.closed


@pytest.mark.parametrize("url", ["https://example.test:bad", "https://[bad", "https://example.test/\x00"])
def test_invalid_url_rejected(checker, registry, url):
    path, document = registry
    document["resources"][0]["url"] = url
    path.write_text(yaml.safe_dump(document), encoding="utf-8", newline="\n")
    original = path.read_bytes()
    with pytest.raises(ValueError, match="invalid registry"):
        run(checker, path, FakeHead(), apply=True)
    assert path.read_bytes() == original


def test_nested_duplicate_keys_rejected(checker, registry):
    path, _ = registry
    path.write_text(f"resources: [{{url: '{URL}', title: first, title: second}}]\n",
                    encoding="utf-8", newline="\n")
    original = path.read_bytes()
    with pytest.raises(ValueError, match="duplicate"):
        run(checker, path, FakeHead(), apply=True)
    assert path.read_bytes() == original


def test_real_entrypoint_empty_fixture(tmp_path):
    # An empty generated fixture exercises __main__ without requiring transport.
    path = tmp_path / "empty.yaml"
    path.write_text("resources: []\n", encoding="utf-8", newline="\n")
    result = subprocess.run([sys.executable, str(SCRIPT), str(path), "--timeout",
                             str(TIMEOUT), "--retries", str(RETRIES)],
                            capture_output=True, text=True, check=False)
    assert result.returncode == 0, result.stderr
    assert json.loads(result.stdout) == {
        "applied": False, "records": [],
        "summary": dict(total=0, alive=0, redirected=0, dead=0, timeout=0),
    }
