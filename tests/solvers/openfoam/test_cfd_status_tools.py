from __future__ import annotations

import os
import subprocess
from pathlib import Path

import pytest


REPO = Path(__file__).resolve().parents[3]
STATUS = REPO / "scripts" / "cfd" / "status"


def _cache(path: Path, age: int = 30) -> None:
    now = 2_000_000_000
    path.write_text(
        f"stamp|{now - age}|2033-05-18T03:33:20Z\n"
        "probe|lane-A|fine|running|125/500|2.4|1.0 h|120|1e-4/1|2e-4/1|1/0.5|+0.0123|1/2/3|+1/+2 kN|1e-5\n"
        "probe|lane-B|coarse|unreachable|-|-|-|-|-|-|-|-|-|-|-\n"
    )


def _render(cache: Path, width: int, now: int = 2_000_000_000) -> str:
    env = dict(os.environ, DM_CFD_STATUS_CACHE=str(cache), DM_CFD_STATUS_NOW=str(now))
    result = subprocess.run(
        ["bash", str(STATUS / "cfd_statusline.sh"), "--width", str(width)],
        input='{"model":{"display_name":"test"}}', capture_output=True,
        text=True, env=env, check=True,
    )
    return result.stdout.rstrip("\n")


def test_statusline_renders_fresh_and_unreachable_cache(tmp_path: Path) -> None:
    cache = tmp_path / "latest.cache"
    _cache(cache)
    line = _render(cache, 240)
    assert "lane-A:fine 125/500 2.4s w120 m+0.0123%" in line
    assert "lane-B:coarse unreachable" in line
    assert line.endswith("age 30s")
    assert "test" not in line


def test_statusline_marks_stale_cache(tmp_path: Path) -> None:
    cache = tmp_path / "latest.cache"
    _cache(cache, age=3601)
    assert "STALE 1h" in _render(cache, 240)


@pytest.mark.parametrize("width", [24, 48, 72])
def test_statusline_never_exceeds_width(tmp_path: Path, width: int) -> None:
    cache = tmp_path / "latest.cache"
    _cache(cache)
    line = _render(cache, width)
    assert len(line) <= width
    assert line.endswith("…")


def test_collect_dry_run_prints_local_and_ssh_probe_commands(tmp_path: Path) -> None:
    config = tmp_path / "status.yml"
    config.write_text(
        "campaign: resistance-demo\n"
        "cache: /tmp/not-written.cache\n"
        "lanes:\n"
        "  - name: lane-A\n"
        "    ssh: local\n"
        "    cases:\n"
        "      - {name: fine, path: /cfd/cases/fine, force_divisor: 1000}\n"
        "  - name: lane-B\n"
        "    ssh: cfd-b\n"
        "    cases:\n"
        "      - {name: coarse, path: /cfd/cases/coarse}\n"
    )
    env = dict(os.environ, DM_CFD_STATUS_CONFIG=str(config))
    result = subprocess.run(
        ["bash", str(STATUS / "cfd_status_collect.sh"), "--dry-run"],
        capture_output=True, text=True, env=env, check=True,
    )
    assert "lane_probe.sh lane-A /cfd/cases/fine 1000" in result.stdout
    assert "ssh -o BatchMode=yes -o ConnectTimeout=8 cfd-b" in result.stdout
    assert "cfd/resistance-demo/scripts/lane_probe.sh" in result.stdout
    assert "${" not in result.stdout
    assert "lane_probe.sh lane-B /cfd/cases/coarse 1000" in result.stdout
    assert not Path("/tmp/not-written.cache").exists()


@pytest.mark.parametrize("ssh_line", ["", "    ssh:\n", "    ssh: ''\n", "    ssh: local\n"])
def test_collect_dry_run_treats_missing_or_empty_ssh_as_local(
    tmp_path: Path, ssh_line: str,
) -> None:
    config = tmp_path / "status.yml"
    config.write_text(
        "lanes:\n"
        "  - name: local-lane\n"
        f"{ssh_line}"
        "    cases:\n"
        "      - {name: first-case, path: ~/cfd/demo/cases/first-case}\n"
    )
    result = subprocess.run(
        ["bash", str(STATUS / "cfd_status_collect.sh"), "--dry-run"],
        capture_output=True, text=True,
        env=dict(os.environ, DM_CFD_STATUS_CONFIG=str(config)), check=True,
    )
    assert "ssh " not in result.stdout
    assert "lane_probe.sh local-lane" in result.stdout


def test_statusline_uses_cache_from_collector_config(tmp_path: Path) -> None:
    cache = tmp_path / "configured.cache"
    config = tmp_path / "status.yml"
    _cache(cache)
    config.write_text(f"cache: {cache}\nlanes: []\n")
    env = dict(os.environ, DM_CFD_STATUS_CONFIG=str(config), DM_CFD_STATUS_NOW="2000000000")
    env.pop("DM_CFD_STATUS_CACHE", None)
    result = subprocess.run(
        ["bash", str(STATUS / "cfd_statusline.sh"), "--width", "240"],
        input="{}", capture_output=True, text=True, env=env, check=True,
    )
    assert "lane-A:fine 125/500 2.4s w120 m+0.0123%" in result.stdout


def test_combined_statusline_uses_cache_from_collector_config(tmp_path: Path) -> None:
    cache = tmp_path / "configured.cache"
    config = tmp_path / "status.yml"
    _cache(cache)
    config.write_text(f"cache: {cache}\nlanes: []\n")
    env = dict(os.environ, DM_CFD_STATUS_CONFIG=str(config), DM_CFD_STATUS_NOW="2000000000")
    env.pop("DM_CFD_STATUS_CACHE", None)
    result = subprocess.run(
        ["bash", str(STATUS / "cfd_statusline_combined.sh"), "--width", "240"],
        input="{}", capture_output=True, text=True, env=env, check=True,
    )
    assert "lane-A:fine 125/500 2.4s w120 m+0.0123%" in result.stdout


def test_collect_infers_campaign_from_first_case_path(tmp_path: Path) -> None:
    config = tmp_path / "status.yml"
    config.write_text(
        "lanes:\n"
        "  - name: remote\n"
        "    ssh: cfd-remote\n"
        "    cases: [{name: fine, path: ~/cfd/inferred-campaign/cases/fine}]\n"
    )
    result = subprocess.run(
        ["bash", str(STATUS / "cfd_status_collect.sh"), "--dry-run"],
        capture_output=True, text=True,
        env=dict(os.environ, DM_CFD_STATUS_CONFIG=str(config)), check=True,
    )
    assert "cfd/inferred-campaign/scripts/lane_probe.sh" in result.stdout
