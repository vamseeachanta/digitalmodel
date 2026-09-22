"""Verify that an OrcaFlex campaign rebuilds from its tracked source.

A campaign keeps its model as a chain of small YAML files and its results as
large solved ``.sim`` binaries. Deleting the binaries is only defensible if the
chain rebuilds the model that was actually run, so this compares the two.

**Why there is no list of properties to compare.** An earlier version of this
check compared thirteen hand-picked properties and reported "26 of 26". Three
of those thirteen took a single value across the whole campaign, so they
distinguished no run from any other, and a property that did vary between runs
-- the current profile factor -- was not compared at all. Removing it from a
rebuilt input moved the surface current by 54% while every compared property
stayed bit-identical. A count of properties is not evidence; a property is
evidence only if it varies across the population being verified, and a list
chosen by hand repeats the mistake it is meant to prevent.

So the comparison is property-agnostic. Both models are saved as data and the
text is compared. Whatever the solver considers part of the model is in that
text, including anything a chosen list would have missed.

Two checks with different jobs, reported separately because they prove
different things:

**A. Per-run overrides.** Saving a model that carries a ``BaseFile`` writes the
delta against that base -- exactly the per-run overrides, which is where a
recovered or edited environment file lives. This is the check that can detect a
wrong environment. It is verified to be deterministic for the same model and to
change when the model changes.

**B. Shared base.** The delta says nothing about the base both sides resolve
against, and a ``.sim`` carries its own copy of the base internally. If the
tracked base has drifted since the run, two deltas can match while the resolved
models differ. So a set of base-derived properties is compared as well. These
are constant across the campaign by design: their job is drift detection, not
telling runs apart, and they are not counted as evidence about the per-run
files.

**What this does not establish.** It compares model INPUT. The solver is not
run and results are not compared. It also cannot detect solver-version drift,
because both sides are opened by the same library and any data migration is
applied to both and cancels. The version in use is recorded so a later reader
can see what the comparison was blind to.

Usage::

    python verify_orcaflex_campaign.py <source-dir> <sim-dir> [-o report.json]
"""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import os
import sys
import tempfile
import warnings
from collections import Counter

warnings.filterwarnings("ignore")

try:
    import OrcFxAPI as O
except ImportError:  # pragma: no cover
    sys.exit("OrcFxAPI is not available; this check needs an OrcaFlex install")

#: Base-derived properties, for check B. Constant across a campaign by design.
_BASE_PROBES = (
    ("#object_count", lambda m: len(list(m.objects))),
    ("#object_names", lambda m: tuple(sorted(o.name for o in m.objects))),
    ("pipeline.CumulativeLength", lambda m: _r(m["pipeline"].CumulativeLength[-1])),
    ("pipeline.Length", lambda m: _r(tuple(m["pipeline"].Length))),
    ("pipeline.LineType", lambda m: tuple(str(v) for v in m["pipeline"].LineType)),
    ("pipeline.TargetSegmentLength",
     lambda m: _r(tuple(m["pipeline"].TargetSegmentLength))),
    ("general.StageDuration", lambda m: _r(tuple(m.general.StageDuration))),
    ("general.ImplicitConstantTimeStep",
     lambda m: _r(m.general.ImplicitConstantTimeStep)),
    ("vessel_winch.StageValue",
     lambda m: _r(tuple(m["vessel_winch"].StageValue))),
    ("vessel_winch.StageMode",
     lambda m: tuple(str(v) for v in m["vessel_winch"].StageMode)),
)


def _r(value):
    if isinstance(value, (list, tuple)):
        return tuple(_r(v) for v in value)
    if isinstance(value, float):
        return round(value, 10)
    return value


def _save_body(model, path: str) -> list[str]:
    """The model saved as data, header removed.

    The header carries the machine name, the user name and a timestamp, none of
    which is part of the model.
    """
    if os.path.exists(path):
        os.remove(path)
    model.SaveData(path)
    with open(path, encoding="utf-8-sig", errors="replace") as fh:
        text = fh.read()
    return [ln.rstrip() for ln in text.splitlines()
            if not ln.startswith("#") and not ln.startswith("%YAML")]


def _base_probe(model) -> dict:
    out = {}
    for name, fn in _BASE_PROBES:
        try:
            out[name] = fn(model)
        except Exception as exc:                           # noqa: BLE001
            out[name] = f"<unreadable: {type(exc).__name__}>"
    return out


def digest(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 22), b""):
            h.update(chunk)
    return h.hexdigest()


def main() -> int:
    ap = argparse.ArgumentParser(description="Verify an OrcaFlex campaign")
    ap.add_argument("source", help="directory holding runs/<case>.yml")
    ap.add_argument("sims", help="directory holding <case>.sim")
    ap.add_argument("-o", "--out", default="campaign-verification.json")
    ap.add_argument("--limit", type=int, default=0)
    args = ap.parse_args()

    runs_dir = os.path.join(args.source, "runs")
    cases = sorted(f[:-4] for f in os.listdir(args.sims)
                   if f.lower().endswith(".sim"))
    if args.limit:
        cases = cases[:args.limit]
    if not cases:
        sys.exit(f"no .sim files in {args.sims}")

    try:
        version = str(O.DLLVersion())
    except Exception:                                      # noqa: BLE001
        version = "unknown"
    print(f"OrcFxAPI DLL version {version}")
    print(f"{len(cases)} case(s)")
    print()

    tmp = tempfile.mkdtemp(prefix="ofxverify_")
    built_path = os.path.join(tmp, "built.yml")
    solved_path = os.path.join(tmp, "solved.yml")
    solved_again = os.path.join(tmp, "solved2.yml")

    per_case: dict[str, dict] = {}
    deltas: dict[str, tuple] = {}
    base_values: dict[str, dict] = {}
    failures: list[dict] = []

    for i, case in enumerate(cases, start=1):
        yml = os.path.join(runs_dir, case + ".yml")
        sim = os.path.join(args.sims, case + ".sim")
        print(f"[{i}/{len(cases)}] {case}", flush=True)
        if not os.path.isfile(yml):
            failures.append(dict(case=case, error="no run YAML"))
            print("    no run YAML")
            continue

        try:
            built = O.Model()
            built.LoadData(yml)
            a = _save_body(built, built_path)
            base_a = _base_probe(built)
            del built
        except Exception as exc:                           # noqa: BLE001
            failures.append(dict(case=case, error=f"rebuild failed: {exc}"))
            print(f"    rebuild failed: {exc}")
            continue

        solved = O.Model(sim)
        b = _save_body(solved, solved_path)
        b2 = _save_body(solved, solved_again)
        base_b = _base_probe(solved)
        del solved

        deterministic = b == b2
        delta_match = a == b
        base_diff = sorted(k for k in base_b if base_a.get(k) != base_b.get(k))

        deltas[case] = tuple(b)
        base_values[case] = base_b

        diff_lines = []
        if not delta_match:
            for n, (x, y) in enumerate(zip(a, b)):
                if x != y:
                    diff_lines.append({"line": n, "rebuild": x, "sim": y})
            if len(a) != len(b):
                diff_lines.append({"line": "length",
                                   "rebuild": len(a), "sim": len(b)})

        per_case[case] = dict(
            delta_lines=len(b),
            delta_matches=delta_match,
            delta_differences=diff_lines,
            save_is_deterministic=deterministic,
            base_properties_differing=base_diff,
            rebuilds=delta_match and not base_diff and deterministic,
            sim_sha256=digest(sim),
            sim_bytes=os.path.getsize(sim),
            source=os.path.relpath(yml, args.source).replace("\\", "/"),
        )
        status = "rebuilds" if per_case[case]["rebuilds"] else "DIFFERS"
        print(f"    {len(b)} delta lines, {status}"
              + ("" if deterministic else "  [save NOT deterministic]")
              + (f"  base differs on {base_diff}" if base_diff else ""))

    # How much can the per-run check actually tell apart?
    distinct_deltas = len(set(deltas.values()))
    varying_base = {
        k: len({repr(v[k]) for v in base_values.values()})
        for k in (base_values[next(iter(base_values))] if base_values else {})
    }
    base_that_vary = {k: n for k, n in varying_base.items() if n > 1}

    ok = sum(1 for c in per_case.values() if c["rebuilds"])
    print()
    print(f"rebuilt and matched      : {ok} / {len(per_case)}")
    print(f"distinct per-run deltas  : {distinct_deltas} across "
          f"{len(deltas)} cases")
    print(f"  (a check that cannot tell runs apart proves little; this is "
          f"how many it can)")
    print(f"base properties compared : {len(varying_base)}, of which "
          f"{len(base_that_vary)} vary across the campaign")
    if failures:
        print(f"failures: {len(failures)}")

    report = dict(
        generated=dt.datetime.now().isoformat(timespec="seconds"),
        orcfxapi_dll_version=version,
        method=dict(
            per_run="full saved-data delta compared as text, property-agnostic",
            shared_base=[name for name, _ in _BASE_PROBES],
        ),
        caveats=[
            "Compares model input only. The solver was not run and results "
            "were not compared.",
            "Cannot detect solver-version drift: both sides are opened by the "
            "same library, so any data migration applies to both and cancels.",
            "Base properties are constant across the campaign by design; they "
            "detect drift in the shared base, not a wrong per-run override.",
        ],
        source_dir=args.source,
        sim_dir=args.sims,
        summary=dict(
            cases=len(per_case),
            rebuilt=ok,
            distinct_per_run_deltas=distinct_deltas,
            base_properties_that_vary=base_that_vary,
        ),
        cases=per_case,
        failures=failures,
    )
    with open(args.out, "w", encoding="utf-8") as fh:
        json.dump(report, fh, indent=2, default=str)
    print(f"written {args.out}")
    return 0 if ok == len(per_case) and not failures else 1


if __name__ == "__main__":
    raise SystemExit(main())
