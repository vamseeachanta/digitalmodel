# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Capability cards replace the retired print build without a browser."""

import json
import os
import subprocess
import sys
from pathlib import Path
from urllib.parse import urlsplit
from urllib.request import Request, urlopen

import pytest

from scripts.capabilities import build_onepagers as builder

ROOT = Path(__file__).resolve().parents[2]


def test_build_writes_one_deterministic_card_file_without_browser(tmp_path):
    target = tmp_path / "capability-cards.json"
    env = dict(os.environ, PATH="", CHROME="/nonexistent")
    command = [sys.executable, str(ROOT / "scripts/capabilities/build_onepagers.py"),
               "--output", str(target)]
    subprocess.run(command, env=env, check=True, capture_output=True)
    first = target.read_bytes()
    subprocess.run(command, env=env, check=True, capture_output=True)
    assert target.read_bytes() == first
    assert list(tmp_path.iterdir()) == [target]
    assert b"\r" not in first
    cards = json.loads(first)
    ids = [card["id"] for card in cards]
    assert len(ids) == len(set(ids)) == len(builder.SPECS)
    assert set(ids) == {spec["id"] for spec in builder.SPECS}
    for card, spec in zip(cards, builder.SPECS, strict=True):
        assert set(card) == {"id", "title", "summary", "key_figure", "link"}
        assert card["title"] == spec["title"]
        assert card["summary"] == " ".join(spec["blurb"].split())
        expected = dict(zip(("value", "label"), spec["figures"][0])) if spec["figures"] else None
        assert card["key_figure"] == expected


def test_committed_cards_are_fresh():
    assert json.loads(builder._OUT.read_text(encoding="utf-8")) == builder.build_cards()


def test_pressure_explorer_card_keeps_its_identity_and_reference_figures():
    cards = {card["id"]: card for card in builder.build_cards()}
    card = cards["wall-thickness-explorer"]
    assert card["title"] == "Wall-thickness multi-code explorer"
    assert card["link"] == "docs/api/structural/wall-thickness-explorer.html"
    spec = next(spec for spec in builder.SPECS if spec["id"] == card["id"])
    assert spec["figures"] == [
        ("9", "design codes"), ("369", "engine evaluations"),
        ("~19.5 mm", "min wall, pipeline codes"),
    ]


def test_load_explorer_card_has_its_own_grid_figures_and_limits():
    cards = {card["id"]: card for card in builder.build_cards()}
    card = cards["wall-thickness-load-explorer"]
    assert card["link"] == "docs/api/structural/wall-thickness-3d-explorer.html"
    assert "design qualification is not evaluated" in card["summary"]
    spec = next(spec for spec in builder.SPECS if spec["id"] == card["id"])
    assert spec["figures"] == [
        ("9", "design codes"), ("354,609", "study rows"),
        ("22,869", "display points"),
    ]
    assert all("369" not in bullet and "19.5" not in bullet for bullet in spec["bullets"])


def test_section_cards_link_to_the_published_capabilities_page():
    cards = {card["id"]: card for card in builder.build_cards()}
    for spec in builder.SPECS:
        if spec["path"].startswith("capabilities/#"):
            assert cards[spec["id"]]["link"] == builder._CANONICAL


def test_every_card_link_is_local_or_uses_an_approved_prefix():
    for card in builder.build_cards():
        link = card["link"]
        url = urlsplit(link)
        path = link
        if url.scheme:
            assert link.startswith((builder._CANONICAL,
                                    "https://github.com/vamseeachanta/digitalmodel/"))
            if url.netloc != "github.com":
                continue
            prefix = "/vamseeachanta/digitalmodel/"
            kind, ref, path = url.path.removeprefix(prefix).split("/", 2)
            assert kind in {"blob", "tree"} and ref == "main", card
            assert ((ROOT / path).is_file() if kind == "blob"
                    else (ROOT / path).is_dir()), card
        else:
            assert (ROOT / path).is_file(), card
        assert subprocess.check_output(
            ["git", "ls-files", "--", path], cwd=ROOT, text=True,
        ).strip(), card


def test_offline_github_link_check_rejects_an_untracked_file(monkeypatch):
    card = {"link": "https://github.com/vamseeachanta/digitalmodel/blob/main/AGENTS.md"}
    monkeypatch.setattr(builder, "build_cards", lambda: [card])
    monkeypatch.setattr(subprocess, "check_output", lambda *args, **kwargs: "")
    with pytest.raises(AssertionError):
        test_every_card_link_is_local_or_uses_an_approved_prefix()


@pytest.mark.network
@pytest.mark.skipif(os.environ.get("CAPABILITY_NETWORK_CHECK") != "1",
                    reason="Public-site check runs in the scheduled workflow")
def test_every_card_links_to_a_committed_page_or_a_live_url():
    fetched = {}
    for card in json.loads(builder._OUT.read_text(encoding="utf-8")):
        url = urlsplit(card["link"])
        path = url.path.lstrip("/")
        if url.netloc == "github.com":
            prefix = "vamseeachanta/digitalmodel/"
            assert path.startswith(prefix)
            path = path.removeprefix(prefix).split("/", 2)[2]
        elif url.scheme:
            assert url.scheme == "https"
            base = url._replace(fragment="").geturl()
            if base not in fetched:
                request = Request(base, headers={"User-Agent": "capability-card-check"})
                with urlopen(request, timeout=30) as response:
                    assert response.status == 200
                    fetched[base] = response.read().decode("utf-8")
            if url.fragment:
                assert f'id="{url.fragment}"' in fetched[base], card
            continue
        assert (ROOT / path).exists(), card
        tracked = subprocess.check_output(
            ["git", "ls-files", "--", path], cwd=ROOT, text=True,
        )
        assert tracked.strip(), card


def test_retired_print_directory_is_empty():
    assert not list((ROOT / "docs/api/capabilities/pdf").rglob("*"))


def test_brand_option_is_removed(tmp_path):
    result = subprocess.run(
        [sys.executable, str(ROOT / "scripts/capabilities/build_onepagers.py"),
         "--brand", "missing.css", "--output", str(tmp_path / "cards.json")],
        capture_output=True, text=True,
    )
    assert result.returncode == 2
    assert "unrecognized arguments" in result.stderr
    assert not list(tmp_path.iterdir())
