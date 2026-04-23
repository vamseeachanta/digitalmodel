"""End-to-end pipeline tests for prospect_adapter + branded_report.

Covers plan section §3 (dual-delivery state machine) + §E (branded
report wrapper) + §G (fallback sidecar never ships). Per plan v3/Q8,
the full 5-demo happy-path E2E runs against cached outputs and is
marked `@pytest.mark.slow` so the PR gate skips them unless
`--run-slow` is passed. The fast tests here exercise the state machine
+ branding logic with mocks and should each complete in <5s.
"""
from __future__ import annotations

import sys
import time
from pathlib import Path

import pytest

_HERE = Path(__file__).resolve().parent
_GTM_DIR = _HERE.parent
if str(_GTM_DIR) not in sys.path:
    sys.path.insert(0, str(_GTM_DIR))

from branded_report import (  # noqa: E402
    BrandConfig,
    wrap_with_client_branding,
)
from prospect_adapter import (  # noqa: E402
    DeliveryState,
    ProspectInput,
    compute_gated_url_hash,
    deliver,
    load_and_validate,
    unpublish_url,
)

FIXTURES_DIR = _HERE / "fixtures"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _valid_prospect() -> ProspectInput:
    return load_and_validate(FIXTURES_DIR / "prospect-valid.yaml")


def _fake_html(tmp_path: Path, body: str = "<h1>report</h1>") -> Path:
    html = tmp_path / "report.html"
    html.write_text(
        f"<html><head><title>demo</title></head><body>{body}</body></html>",
        encoding="utf-8",
    )
    return html


# ---------------------------------------------------------------------------
# Dual-delivery state machine — plan §3
# ---------------------------------------------------------------------------


def test_delivery_email_first_then_url(tmp_path: Path) -> None:
    calls: list[str] = []

    def email(_p: ProspectInput, _html: Path) -> bool:
        calls.append("email")
        return True

    def url(_p: ProspectInput, _html: Path, _hash: str) -> bool:
        calls.append("url")
        return True

    result = deliver(
        _valid_prospect(),
        _fake_html(tmp_path),
        email_sender=email,
        url_publisher=url,
        sleep=lambda _s: None,
    )
    assert calls == ["email", "url"]
    assert result.state == DeliveryState.DELIVERED
    assert result.gated_url_hash is not None
    assert len(result.gated_url_hash) == 16


def test_delivery_email_fail_aborts_url(tmp_path: Path) -> None:
    url_calls: list[str] = []

    def email(_p: ProspectInput, _html: Path) -> bool:
        return False

    def url(_p: ProspectInput, _html: Path, _hash: str) -> bool:
        url_calls.append("url")
        return True

    result = deliver(
        _valid_prospect(),
        _fake_html(tmp_path),
        email_sender=email,
        url_publisher=url,
        sleep=lambda _s: None,
    )
    assert result.state == DeliveryState.FAILED_EMAIL
    assert result.email_attempt_count == 4  # initial + 3 retries
    assert result.url_publish_attempt_count == 0
    assert url_calls == []  # URL channel MUST NOT be attempted
    assert result.gated_url_hash is None


def test_delivery_url_fail_records_email_only_state(tmp_path: Path) -> None:
    def email(_p: ProspectInput, _html: Path) -> bool:
        return True

    def url(_p: ProspectInput, _html: Path, _hash: str) -> bool:
        return False

    result = deliver(
        _valid_prospect(),
        _fake_html(tmp_path),
        email_sender=email,
        url_publisher=url,
        sleep=lambda _s: None,
    )
    assert result.state == DeliveryState.DELIVERED_EMAIL_ONLY
    assert result.email_sent_utc is not None
    assert result.url_publish_attempt_count == 4
    assert result.gated_url_hash is not None  # hash computed, publish failed


def test_delivery_retry_backoff_bounds(tmp_path: Path) -> None:
    # Spy on the sleep sequence to confirm the [30s, 2min, 10min] schedule.
    sleeps: list[float] = []

    def email(_p: ProspectInput, _html: Path) -> bool:
        return False  # force all retries

    result = deliver(
        _valid_prospect(),
        _fake_html(tmp_path),
        email_sender=email,
        url_publisher=None,  # URL unused when email fails
        retry_backoff_s=(30.0, 120.0, 600.0),
        sleep=sleeps.append,
    )
    assert result.state == DeliveryState.FAILED_EMAIL
    assert sleeps == [30.0, 120.0, 600.0]


def test_delivery_unpublish_records_state() -> None:
    deleted: list[str] = []

    def deleter(h: str) -> bool:
        deleted.append(h)
        return True

    result = unpublish_url("abcdef0123456789deadbeef", deleter=deleter, reason="NDA violation")
    assert result.state == DeliveryState.UNPUBLISHED
    assert deleted == ["abcdef0123456789deadbeef"]
    assert "NDA violation" in result.notes


def test_delivery_publish_url_false_is_email_only(tmp_path: Path) -> None:
    # A prospect with publish_private_url=false should not attempt URL
    # publish even if the publisher callable is provided.
    fixture = FIXTURES_DIR / "prospect-valid.yaml"
    yaml_text = fixture.read_text(encoding="utf-8").replace(
        "publish_private_url: true",
        "publish_private_url: false",
    )
    # purge_after_utc is required only when publish_private_url=true,
    # so drop it to satisfy the schema allOf.
    yaml_text = yaml_text.replace(
        '  purge_after_utc: "2026-05-20T00:00:00Z"\n',
        "",
    )
    intake = tmp_path / "opt-out.yaml"
    intake.write_text(yaml_text, encoding="utf-8")
    prospect = load_and_validate(intake)

    url_calls: list[str] = []

    def email(_p: ProspectInput, _html: Path) -> bool:
        return True

    def url(_p: ProspectInput, _html: Path, _hash: str) -> bool:
        url_calls.append("url")
        return True

    result = deliver(
        prospect,
        _fake_html(tmp_path),
        email_sender=email,
        url_publisher=url,
        sleep=lambda _s: None,
    )
    assert result.state == DeliveryState.DELIVERED
    assert url_calls == []
    assert result.gated_url_hash is None


# ---------------------------------------------------------------------------
# Gated-URL hash determinism — plan §Gated URL mechanics
# ---------------------------------------------------------------------------


def test_gated_url_hash_is_deterministic() -> None:
    h1 = compute_gated_url_hash("acme-2026-04-21", "salt-x", "2026-04-21")
    h2 = compute_gated_url_hash("acme-2026-04-21", "salt-x", "2026-04-21")
    assert h1 == h2


def test_gated_url_hash_changes_with_salt() -> None:
    h1 = compute_gated_url_hash("acme-2026-04-21", "salt-a", "2026-04-21")
    h2 = compute_gated_url_hash("acme-2026-04-21", "salt-b", "2026-04-21")
    assert h1 != h2


# ---------------------------------------------------------------------------
# Branded report wrapper — plan §E
# ---------------------------------------------------------------------------


def test_e2e_report_contains_brand_strings(tmp_path: Path) -> None:
    src = _fake_html(tmp_path)
    cfg = BrandConfig(
        header="Prepared for Acme Marine",
        footer="Confidential — NDA 2026-04-23",
    )
    out = wrap_with_client_branding(src, cfg)
    assert "Prepared for Acme Marine" in out
    assert "Confidential — NDA 2026-04-23" in out


def test_e2e_report_has_nda_watermark_when_requested(tmp_path: Path) -> None:
    src = _fake_html(tmp_path)
    cfg = BrandConfig(
        header="Prepared for Acme Marine",
        footer="NDA 2026-04-23",
        nda_watermark=True,
    )
    out = wrap_with_client_branding(src, cfg)
    assert "nda-watermark" in out
    assert "CONFIDENTIAL" in out


def test_e2e_report_has_canonical_class_disclaimer(tmp_path: Path) -> None:
    src = _fake_html(tmp_path)
    cfg = BrandConfig(
        header="Prepared for Acme Marine",
        footer="NDA",
        canonical_vessel_name="heavy-lift-csv",
        canonical_citations=(
            "Subsea7 public fleet page (accessed 2026-04-23)",
            "Bai & Bai, Subsea Engineering Handbook 2e (ISBN 978-0-12-812622-6)",
        ),
    )
    out = wrap_with_client_branding(src, cfg)
    assert "canonical-class-disclaimer" in out
    assert "heavy-lift-csv" in out
    assert "Subsea Engineering Handbook" in out


def test_e2e_branded_report_does_not_break_existing_body(tmp_path: Path) -> None:
    src = _fake_html(tmp_path, body="<section id='engineering-body'>content</section>")
    cfg = BrandConfig(header="H", footer="F")
    out = wrap_with_client_branding(src, cfg)
    assert "id='engineering-body'" in out
    assert "content" in out


# ---------------------------------------------------------------------------
# Fallback sidecar never ships — plan §G
# ---------------------------------------------------------------------------


def test_fallback_sidecar_never_in_email_attachment(tmp_path: Path) -> None:
    # Simulate a bundle that happens to contain a fallback sidecar and
    # assert the packager filters it.
    from prospect_adapter import package_delivery_files

    bundle = tmp_path / "bundle"
    bundle.mkdir()
    (bundle / "report.html").write_text("<html></html>", encoding="utf-8")
    sidecar_dir = bundle / "private-log"
    sidecar_dir.mkdir()
    (sidecar_dir / "fallback-applied.json").write_text("[]", encoding="utf-8")

    files = package_delivery_files(bundle)
    paths = {str(p) for p in files}
    assert not any("private-log" in p for p in paths)
    assert any(p.endswith("report.html") for p in paths)


def test_fallback_sidecar_never_in_url_publish_set(tmp_path: Path) -> None:
    # Same guarantee, framed as the URL publish set.
    from prospect_adapter import package_delivery_files

    bundle = tmp_path / "bundle-url"
    bundle.mkdir()
    (bundle / "index.html").write_text("<html></html>", encoding="utf-8")
    sidecar_dir = bundle / "private-log"
    sidecar_dir.mkdir()
    (sidecar_dir / "fallback-applied.json").write_text("[]", encoding="utf-8")

    files = package_delivery_files(bundle)
    paths = {str(p) for p in files}
    assert not any("fallback-applied" in p for p in paths)


# ---------------------------------------------------------------------------
# Refuse-fast guarantee — plan acceptance criterion
# ---------------------------------------------------------------------------


def test_refuse_on_malformed_yaml_under_5s(tmp_path: Path) -> None:
    bad = tmp_path / "bad.yaml"
    bad.write_text("prospect:\n  company: \"unterminated\n", encoding="utf-8")

    from prospect_adapter import ProspectIntakeError

    start = time.monotonic()
    with pytest.raises(ProspectIntakeError):
        load_and_validate(bad)
    elapsed = time.monotonic() - start
    assert elapsed < 5.0, f"refuse-fast violated: took {elapsed:.2f}s"
