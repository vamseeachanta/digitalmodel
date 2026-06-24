# /// script
# requires-python = ">=3.10"
# dependencies = ["anthropic>=0.49", "pyyaml"]
# ///
"""
Solve a Collide "PE Problem of the Day" with the Claude API and record the worked
solution back into the problem YAML.

Design (ties into the two epics):
  * This module is the *engine*. `solve_problem(problem) -> dict` does one Claude
    API call and returns a structured solution. It is intentionally transport-free.
  * Epic B ("Deckhand-routed solver, Telegram transport") wraps this: a Deckhand
    `solve <slug>` command runs on the Hermes gateway host, calls solve_problem(),
    renders the Deckhand-letterhead brief, and replies to the Telegram channel with
    the answer + brief link. The API call itself stays here so it is testable and
    reusable regardless of transport.

Model: claude-opus-4-8 (per the claude-api skill — do not downgrade).
Auth:  ANTHROPIC_API_KEY in the environment, or an `ant auth login` profile.

Usage (Linux: use `uv run`):
    uv run docs/collide_pe/solve.py docs/collide_pe/problems/pe-2026-06-19.yml
    uv run docs/collide_pe/solve.py --all          # solve every unsolved problem
    uv run docs/collide_pe/solve.py <yml> --dry-run # print, don't write back
"""
from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path

import yaml

HERE = Path(__file__).resolve().parent
PROBLEMS = HERE / "problems"
MODEL = "claude-opus-4-8"

SYSTEM = (
    "You are a licensed petroleum engineer answering a 'PE Problem of the Day' "
    "from the Collide engineering community. Solve rigorously and concisely. "
    "For quantitative problems, show each step with the formula and units and give "
    "a numeric answer. For conceptual/definition problems, give the precise term and "
    "a one-line justification. State any assumption that changes the answer (e.g. "
    "above vs below the bubble point). Do not pad."
)

# Structured-outputs schema (output_config.format). Keep it simple: structured
# outputs reject numeric/length constraints, so `answer` is free-form text.
SCHEMA = {
    "type": "object",
    "properties": {
        "method": {"type": "string", "description": "The governing principle/formula in one line."},
        "steps": {"type": "array", "items": {"type": "string"}, "description": "Ordered worked steps."},
        "answer": {"type": "string", "description": "Final answer with units; one line per sub-question."},
        "caveats": {"type": "string", "description": "Assumptions/limits that would change the answer."},
        "confidence": {"type": "string", "enum": ["high", "medium", "low"]},
    },
    "required": ["method", "steps", "answer", "caveats", "confidence"],
    "additionalProperties": False,
}


def build_prompt(problem: dict) -> str:
    lines = [problem.get("statement", "").strip()]
    given = problem.get("given") or {}
    if given:
        lines.append("\nGiven:")
        lines += [f"  - {k.replace('_', ' ')}: {v}" for k, v in given.items()]
    find = problem.get("find") or []
    if find:
        lines.append("\nFind:")
        lines += [f"  - {f}" for f in find]
    return "\n".join(lines)


def solve_problem(problem: dict, model: str = MODEL) -> dict:
    """One Claude API call -> validated structured solution dict.

    This is the function Epic B's Deckhand command and Telegram router call.
    """
    import anthropic  # imported lazily so render-only workflows need no SDK

    client = anthropic.Anthropic()
    resp = client.messages.create(
        model=model,
        max_tokens=8000,
        thinking={"type": "adaptive"},               # let the model decide depth
        output_config={"format": {"type": "json_schema", "schema": SCHEMA}},
        system=SYSTEM,
        messages=[{"role": "user", "content": build_prompt(problem)}],
    )
    if resp.stop_reason == "refusal":
        raise RuntimeError("Claude refused this request; not a solvable PE problem as posed.")
    text = next((b.text for b in resp.content if b.type == "text"), None)
    if not text:
        raise RuntimeError("No text block in response.")
    sol = json.loads(text)
    sol["solved_by"] = f"Claude API ({model})"
    sol["_request_id"] = getattr(resp, "_request_id", None)
    return sol


def solve_file(yml_path: Path, *, dry_run: bool = False, force: bool = False) -> dict:
    problem = yaml.safe_load(yml_path.read_text(encoding="utf-8"))
    if problem.get("solution") and not force:
        print(f"skip {yml_path.name}: already solved (use --force to re-solve)")
        return problem["solution"]
    sol = solve_problem(problem)
    print(f"\n=== {problem.get('id', yml_path.stem)} ===")
    print(json.dumps(sol, indent=2))
    if not dry_run:
        problem["solution"] = sol
        yml_path.write_text(
            yaml.safe_dump(problem, sort_keys=False, allow_unicode=True, width=88),
            encoding="utf-8",
        )
        print(f"wrote solution back to {yml_path}")
    return sol


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("problem", nargs="?", help="Path to a problem YAML")
    ap.add_argument("--all", action="store_true", help="Solve every YAML in problems/")
    ap.add_argument("--dry-run", action="store_true", help="Print solution, don't write back")
    ap.add_argument("--force", action="store_true", help="Re-solve even if a solution exists")
    args = ap.parse_args(argv)

    if not (os.environ.get("ANTHROPIC_API_KEY") or os.environ.get("ANTHROPIC_AUTH_TOKEN")):
        print("warning: no ANTHROPIC_API_KEY / ANTHROPIC_AUTH_TOKEN set; "
              "relying on an `ant auth login` profile.", file=sys.stderr)

    targets = sorted(PROBLEMS.glob("*.yml")) if args.all else (
        [Path(args.problem)] if args.problem else None)
    if targets is None:
        ap.error("provide a problem YAML path or --all")

    for t in targets:
        try:
            solve_file(t, dry_run=args.dry_run, force=args.force)
        except Exception as e:  # noqa: BLE001 — surface per-file errors, keep going
            print(f"ERROR solving {t.name}: {e}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
