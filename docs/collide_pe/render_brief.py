# /// script
# requires-python = ">=3.10"
# dependencies = ["pyyaml"]
# ///
"""
Render a Collide PE problem (YAML) into a Deckhand-letterhead HTML brief.

The brief is a single self-contained HTML file (inline CSS, no assets) so it can
be published anywhere and shared as a link — that was the explicit ask: Deckhand
header, no Aceengineer branding, low overhead, "send it as a rendered HTML link".

Usage (Linux: use `uv run`):
    uv run docs/collide_pe/render_brief.py docs/collide_pe/problems/pe-2026-06-19.yml
    # -> writes docs/collide_pe/briefs/pe-2026-06-19.html

    uv run docs/collide_pe/render_brief.py --all     # render every problem YAML
"""
from __future__ import annotations

import argparse
import datetime as _dt
import html
import sys
from pathlib import Path

import yaml

HERE = Path(__file__).resolve().parent
TEMPLATE = HERE / "templates" / "letterhead.html"
PROBLEMS = HERE / "problems"
BRIEFS = HERE / "briefs"


def _esc(x) -> str:
    return html.escape(str(x))


def _chips(tags) -> str:
    return "".join(f'<span class="chip">{_esc(t)}</span>' for t in (tags or []))


def _given_table(given: dict) -> str:
    if not given:
        return "<em>None stated.</em>"
    rows = "".join(
        f"<tr><td>{_esc(k.replace('_', ' '))}</td><td>{_esc(v)}</td></tr>"
        for k, v in given.items()
    )
    return f'<table class="given">{rows}</table>'


def _find_list(find) -> str:
    items = "".join(f"<li>{_esc(f)}</li>" for f in (find or []))
    return f"<ul>{items}</ul>" if items else "<em>See problem statement.</em>"


def _steps(steps) -> str:
    return "".join(f"<li>{_esc(s)}</li>" for s in (steps or []))


def _answer_block(answer) -> str:
    if isinstance(answer, dict):
        parts = [
            f'<div class="big">{_esc(k.replace("_", " "))}: {_esc(v)}</div>'
            for k, v in answer.items()
        ]
        return "".join(parts)
    return f'<div class="big">{_esc(answer)}</div>'


def render(problem: dict) -> str:
    tpl = TEMPLATE.read_text(encoding="utf-8")
    sol = problem.get("solution") or {}
    repl = {
        "{{TITLE}}": _esc(problem.get("title", problem.get("id", "PE Problem"))),
        "{{ID}}": _esc(problem.get("id", "")),
        "{{DATE}}": _esc(problem.get("date", "")),
        "{{SOURCE_URL}}": _esc(problem.get("source_url", "https://app.collide.io")),
        "{{CHIPS}}": _chips(problem.get("tags")),
        "{{STATEMENT}}": _esc((problem.get("statement") or "").strip()),
        "{{GIVEN}}": _given_table(problem.get("given") or {}),
        "{{FIND}}": _find_list(problem.get("find")),
        "{{METHOD}}": _esc(sol.get("method", "Pending solver run.")),
        "{{STEPS}}": _steps(sol.get("steps")) or "<li>Pending solver run.</li>",
        "{{ANSWER}}": _answer_block(sol.get("answer", "Pending solver run.")),
        "{{CAVEATS}}": _esc(sol.get("caveats", "")),
        "{{SOLVED_BY}}": _esc(sol.get("solved_by", "—")),
        "{{CONFIDENCE}}": _esc(sol.get("confidence", "—")),
        "{{GENERATED}}": _dt.datetime.now().strftime("%Y-%m-%d %H:%M"),
    }
    out = tpl
    for k, v in repl.items():
        out = out.replace(k, v)
    return out


def render_file(yml_path: Path) -> Path:
    problem = yaml.safe_load(yml_path.read_text(encoding="utf-8"))
    BRIEFS.mkdir(parents=True, exist_ok=True)
    out_path = BRIEFS / f"{problem.get('id', yml_path.stem)}.html"
    out_path.write_text(render(problem), encoding="utf-8")
    return out_path


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("problem", nargs="?", help="Path to a problem YAML")
    ap.add_argument("--all", action="store_true", help="Render every YAML in problems/")
    args = ap.parse_args(argv)

    targets: list[Path] = []
    if args.all:
        targets = sorted(PROBLEMS.glob("*.yml"))
    elif args.problem:
        targets = [Path(args.problem)]
    else:
        ap.error("provide a problem YAML path or --all")

    for t in targets:
        out = render_file(t)
        print(f"rendered {t.name} -> {out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
