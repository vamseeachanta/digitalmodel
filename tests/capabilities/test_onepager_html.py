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
    assert json.loads(builder._OUT.read_text()) == builder.build_cards()


def test_every_card_links_to_a_committed_page_or_a_live_url():
    fetched = {}
    for card in json.loads(builder._OUT.read_text()):
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
