#!/usr/bin/env python3
"""Brand drift guard — digitalmodel#1476 (epic #1471).

Fails CI if a public capability page under docs/api/ hard-codes a CORE brand
colour token (--brand/--navy/--accent/--teal) with a literal hex, instead of
inheriting it from the canonical docs/api/_assets/brand.css.

This locks in the #1474 migration: identity colours come from ONE place, so brand
drift (the original problem — --accent across 6+ values) cannot creep back.

Run in CI (see .github/workflows/docs.yml) and locally:
    python scripts/brand_guard.py
Exit 0 = clean, 1 = violations.
"""
from __future__ import annotations
import re, sys, glob, os

ROOT = "docs/api"

# identity colours that MUST be inherited from brand.css, never hard-coded in a page.
CORE = ("brand", "navy", "accent", "teal")
CORE_RE = re.compile(r"--(?:%s)\s*:\s*#[0-9a-fA-F]{3,8}" % "|".join(CORE))

# pages intentionally self-themed (documented exceptions). Keep this list tiny and justified.
ALLOWLIST = {
    "docs/api/ffs/build-wave-2026-07.html",  # ships its own complete dark-mode design (#1474)
}

# sanctioned container widths (soft check — informational only, never fails the build)
SANCTIONED_WIDTHS = {"var(--wrap)", "1240px"}
WIDTH_RE = re.compile(r"max-width\s*:\s*([0-9]+px|var\(--wrap\))")


def iter_pages():
    for f in sorted(glob.glob(f"{ROOT}/**/*.html", recursive=True)):
        f = f.replace("\\", "/")
        if "/_assets/" in f:      # brand.css legitimately DEFINES the tokens
            continue
        yield f


def selftest() -> int:
    ok = bool(CORE_RE.search("--accent:#0b6e99"))
    ok &= bool(CORE_RE.search("  --navy: #1a365d;"))
    ok &= CORE_RE.search("color:var(--accent)") is None      # usage must NOT flag
    ok &= CORE_RE.search("--soft:#f4f8fc") is None           # page-specific must NOT flag
    print("selftest:", "PASS" if ok else "FAIL")
    return 0 if ok else 1


def main() -> int:
    if "--selftest" in sys.argv:
        return selftest()

    violations, widths = [], {}
    for f in iter_pages():
        s = open(f, encoding="utf-8", errors="ignore").read()
        if f not in ALLOWLIST:
            m = CORE_RE.search(s)
            if m:
                violations.append((f, m.group(0)))
        for w in WIDTH_RE.findall(s):
            if w not in SANCTIONED_WIDTHS:
                widths.setdefault(w, 0)
                widths[w] += 1

    if widths:
        total = sum(widths.values())
        print(f"note (non-failing): {len(widths)} non-sanctioned max-width value(s) across "
              f"{total} occurrence(s) — consider var(--wrap). Top: "
              + ", ".join(f"{w}×{n}" for w, n in sorted(widths.items(), key=lambda kv: -kv[1])[:3]))
        print()

    if violations:
        print("BRAND GUARD FAILED — pages hard-code core brand tokens instead of "
              "linking ../_assets/brand.css:\n")
        for f, tok in violations:
            print(f"  ✗ {f}   ({tok})")
        print(f"\n{len(violations)} violation(s). Use `var(--brand)`/`var(--accent)` and "
              "`<link rel=\"stylesheet\" href=\"…/_assets/brand.css\">` — see "
              "docs/api/_assets/LAYOUT.md. If a page is intentionally self-themed, add it "
              "to ALLOWLIST in scripts/brand_guard.py with a reason.")
        return 1

    n = sum(1 for _ in iter_pages())
    print(f"brand guard OK — no core-token drift across {n} pages in {ROOT}/** "
          f"(allowlist: {len(ALLOWLIST)})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
