"""Offline L01 request generation, gated native preparation and export comparison.

No solver imports, subprocesses, or licence operations.
See docs/domains/orcawave/L01_aqwa_benchmark/solver_request.html for the contract.
"""
import argparse
import cmath
import hashlib
import json
import math
from pathlib import Path
import re

# Issue 714 scope and API_EXECUTION_SUMMARY.md: case-count diagnostic ladder.
STAGES = ((4, 4), (10, 9), (20, 9))
# API_EXECUTION_SUMMARY.md, Hydrodynamic Data: six motion DOFs and 6 x 6 matrices.
DOFS = 6
SOURCE_RIGHTS = (
    "Source rights: internal use only. The L01 source model files are held in "
    "the project repositories; runs use the project licences; results are kept "
    "in the private data store; nothing derived from the source model is "
    "published in a public repository or page until its provenance is recorded."
)
RECONCILIATION_FIELDS = {
    "geometry", "mass", "centre_of_gravity", "reference_point", "water_depth",
    "restraints",
}
BASIS_FIELDS = {
    "geometry", "mesh", "mass", "inertia", "centre_of_gravity", "reference_point",
    "water_depth", "density", "gravity", "units", "axes", "heading_convention",
    "phase_convention", "normalization", "dof_order", "solve_type", "source_rights",
    "restraints",
}


def number(value, *, positive=False):
    if type(value) not in (int, float) or not math.isfinite(value):
        raise ValueError("finite numeric value required")
    if positive and value <= 0:
        raise ValueError("positive value required")
    return value


def text(value):
    if not isinstance(value, str) or not value.strip():
        raise ValueError("nonempty citation or declaration required")
    return value


def vector(values, length, *, positive=False):
    if not isinstance(values, list) or len(values) != length:
        raise ValueError("invalid vector dimensions")
    return [number(v, positive=positive) for v in values]


def matrix(values, *, positive=False):
    if not isinstance(values, list) or len(values) != DOFS:
        raise ValueError("invalid matrix dimensions")
    return [vector(row, DOFS, positive=positive) for row in values]


def allocation_policy(request, cap):
    """Require a cited resource policy; declarations do not authenticate approval."""
    policy = request.get("allocation_policy")
    if not isinstance(policy, dict):
        raise ValueError("cited allocation policy required")
    try:
        maximum = number(policy.get("stage1_max_licensed_minutes"), positive=True)
        multiplier = number(policy.get("later_stage_multiplier"), positive=True)
        if multiplier < 1:
            raise ValueError("contingency multiplier cannot reduce measured time")
        for key in ("citation", "contingency_rationale"):
            value = text(policy.get(key)).strip().casefold()
            normalized = re.sub(r"[\W_]+", "", value)
            if normalized in {"", "notevaluated", "tbd", "na", "none", "todo",
                              "tbc", "unknown", "pending", "unverified"}:
                raise ValueError("evaluated citation and contingency rationale required")
        citation = policy["citation"].strip()
        # Require a retrievable document and locator, rather than an approval word.
        if not (re.fullmatch(r"[\w./-]+\.[\w]+:[1-9][0-9]*", citation)
                or re.fullmatch(r"https?://[^\s/#]+/[^\s]*#[^\s]+", citation)
                or re.fullmatch(r".+\S,\s*(?:revision|section|clause)\s+\S.*",
                                citation, flags=re.IGNORECASE)):
            raise ValueError("allocation citation requires a document reference and locator")
    except ValueError as error:
        raise ValueError(f"allocation policy: {error}") from error
    if cap > maximum:
        raise ValueError(f"stage-1 cap {cap} minutes exceeds allocation policy "
                         f"maximum {maximum} minutes")
    return policy


def validate_config(config):
    frequencies = config["frequencies_rad_s"]
    headings = config["headings_deg"]
    vector(frequencies, len(frequencies), positive=True)
    vector(headings, len(headings))
    if len(frequencies) < STAGES[-1][0] or len(headings) < STAGES[-1][1]:
        raise ValueError("grid cannot supply the diagnostic ladder")
    if any(b <= a for a, b in zip(frequencies, frequencies[1:])):
        raise ValueError("frequencies must be strictly increasing")
    # Canonical headings, no implicit wrapping; 360 degrees is one revolution.
    # Source: multi_solver_comparator.py, _compute_phase_statistics wrap formula.
    if len(set(headings)) != len(headings) or any(not 0 <= h < 360 for h in headings):
        raise ValueError("headings must be unique canonical degrees")
    gap = number(config["max_relative_gap"], positive=True)
    text(config["sampling_citation"])
    # Source: benchmark_abscissa.py MAX_RELATIVE_GAP / half-power-band contract.
    if any((b - a) / a > gap for a, b in zip(frequencies, frequencies[1:])):
        raise ValueError("sampling gap exceeds the supplied criterion")
    threads = config["threads"]
    if (not isinstance(threads, list) or not threads
            or any(type(t) is not int or t <= 0 for t in threads)
            or len(set(threads)) != len(threads)):
        raise ValueError("unique positive integer thread counts required")
    number(config["budget_minutes"], positive=True)
    allocation_policy(config, config["budget_minutes"])
    text(config["resource_citation"])
    basis = config["basis"]
    if not isinstance(basis, dict) or set(basis) != BASIS_FIELDS:
        raise ValueError("complete explicit physical basis required")
    for value in basis.values():
        text(value)
    criteria = config["criteria"]
    for key in ("citation", "aqg_band"):
        text(criteria[key])
    number(criteria["max_normalized_error"], positive=True)
    vector(criteria["rao_scales"], DOFS, positive=True)
    vector(criteria["phase_amplitude_floors"], DOFS, positive=True)
    for key in ("added_mass_scales", "damping_scales"):
        matrix(criteria[key], positive=True)


def subset(values, count):
    """Deterministic endpoint-inclusive indices; no invented frequency values."""
    return [values[i * (len(values) - 1) // (count - 1)] for i in range(count)]


def later_allocation(cases, policy):
    # Resource cap and contingency come from the caller's cited allocation policy.
    return (f"allocation = measured stage-1 minutes per case x {cases} "
            f"x {policy['later_stage_multiplier']}, "
            "set by the solver manager from the measured time")


def build_request(config):
    validate_config(config)
    runs = []
    for nf, nh in STAGES:
        for threads in config["threads"]:
            # Issue 714 requires DivideNonPlanarPanels sensitivity, not a default fix.
            for divide in (False, True):
                runs.append({
                    "cases": nf * nh, "threads": threads,
                    "divide_non_planar_panels": divide,
                    "frequencies_rad_s": subset(config["frequencies_rad_s"], nf),
                    "headings_deg": subset(config["headings_deg"], nh),
                    **({"budget_minutes": config["budget_minutes"],
                        "budget_scope": "shared_stage1_all_solvers_and_variants"}
                       if nf * nh == 16 else {
                           "allocation": later_allocation(nf * nh, config["allocation_policy"])}),
                    "scientific_comparison": False,
                })
    return {
        "config_sha256": digest(config), "scaling_runs": runs,
        "comparison_grid": {key: config[key] for key in
                            ("frequencies_rad_s", "headings_deg")},
        "basis": config["basis"], "criteria": config["criteria"],
        "sampling_citation": config["sampling_citation"],
        "max_relative_gap": config["max_relative_gap"],
        "resource_citation": config["resource_citation"],
        "allocation_policy": config["allocation_policy"],
        "estimated_licensed_minutes": "NOT_EVALUATED",
        "native_inputs": "NOT_EVALUATED", "engineering_qualification": "NOT_EVALUATED",
    }


def validate_reconciliation(request):
    """Check supplied reconciliations; declarations are not native readback."""
    rows = request.get("reconciliation", [])
    if (len(rows) != len(RECONCILIATION_FIELDS)
            or {row.get("quantity") for row in rows} != RECONCILIATION_FIELDS):
        raise ValueError("complete unique physical reconciliation required")
    for row in rows:
        if row.get("status") != "AGREES":
            raise ValueError(f"reconciliation: {row.get('quantity')} {row.get('status')}")
        if not isinstance(row.get("unit"), str) or not row["unit"].strip():
            raise ValueError("reconciliation units required")
        a, b = (row.get(solver, {}).get("value") for solver in ("AQWA", "OrcaWave"))
        if a is None or b is None or a != b:
            raise ValueError("reconciliation values disagree or are not evaluated")
        pending = [a, b]
        while pending:
            value = pending.pop()
            if isinstance(value, list) and value:
                pending.extend(value)
            elif type(value) not in (int, float, bool):
                raise ValueError("reconciliation requires evaluated numeric values")
            elif type(value) is bool and row["quantity"] != "restraints":
                raise ValueError("reconciliation booleans describe restraints only")
        # Reject NaN, including nested normalized vectors, before equality is used.
        json.dumps([a, b], allow_nan=False)


def native_sources(request, root):
    """Pin both deck bytes and resolve every citation inside the supplied root."""
    root = Path(root).resolve(strict=True)
    sources = request.get("sources", {})
    if set(sources) != {"AQWA", "OrcaWave"}:
        raise ValueError("both native sources required")
    contents = {}
    for solver, source in sources.items():
        path = root / source["file"]
        if Path(source["file"]).is_absolute() or not path.resolve().is_relative_to(root):
            raise ValueError("native source must be relative to the evidence root")
        raw = path.read_bytes()
        if hashlib.sha256(raw).hexdigest() != source["sha256"]:
            raise ValueError("native source digest mismatch")
        contents[solver] = raw.decode("utf-8-sig").replace("\r\n", "\n")
    for row in request["reconciliation"]:
        for solver in sources:
            path, line = row[solver]["file_line"].rsplit(":", 1)
            if (path != sources[solver]["file"] or not line.isdigit()
                    or not 1 <= int(line) <= len(contents[solver].splitlines())):
                raise ValueError("reconciliation file:line citation required")
    return contents


def native_stage1(request, contents):
    """Select the declared four-by-four grid without loading a solver library."""
    stage = request["stage1"]
    periods = vector(stage["periods_s"], 4, positive=True)
    headings = vector(stage["headings_deg"], 4)
    if (len(set(periods)) != 4 or len(set(headings)) != 4
            or any(not 0 <= h < 360 for h in headings)):
        raise ValueError("unique stage-1 periods and canonical headings required")
    ow = contents["OrcaWave"]
    if (not re.search(r"(?m)^WavesReferredToBy: period \(s\)$", ow)
            or not re.search(r"(?m)^\s*BodyMeshFormat: Aqwa dat$", ow)):
        raise ValueError("explicit period convention and AQWA mesh format required")
    mesh = re.findall(r"(?m)^\s*BodyMeshFileName: ([^\n]+)$", ow)
    if mesh != [Path(request["sources"]["AQWA"]["file"]).name]:
        raise ValueError("OrcaWave must reference the pinned AQWA mesh source")
    # These anchors describe the checked-in L01 YAML and AQWA FDR1 deck only.
    period_pattern = r"(?m)^PeriodOrFrequency:\n((?:[ \t]+- [^\n]+\n)+)"
    heading_pattern = r"(?m)^WaveHeading:\n((?:[ \t]+- [^\n]+\n)+)"
    for pattern, values in ((period_pattern, periods), (heading_pattern, headings)):
        match = re.search(pattern, ow)
        if match is None or not set(values) <= {
                float(line.strip()[2:]) for line in match[1].splitlines()}:
            raise ValueError("stage-1 grid must be read from the OrcaWave candidate")
        field = match[0].splitlines()[0]
        ow, count = re.subn(pattern, field + "\n" + "".join(
            f"  - {value}\n" for value in values), ow)
        if count != 1:
            raise ValueError("ambiguous native grid block")
    ow, count = re.subn(r"(?m)^(\s*BodyMeshFileName:) [^\n]+$",
                        r"\1 stage1.dat", ow)
    if count != 1:
        raise ValueError("one AQWA mesh reference required")
    # AQWA cards retain the source's fixed columns and ascending frequency order.
    cards = "".join(f"     1HRTZ {i:4d} {i:4d}{1 / p:10.7g}\n"
                    for i, p in enumerate(sorted(periods, reverse=True), 1))
    cards += "".join(f"     1DIRN {i:4d} {i:4d}{h:10.7g}\n"
                     for i, h in enumerate(headings, 1))
    aqwa, count = re.subn(r"(?m)^(          FDR1\n)(?:(?! END\n)[^\n]*\n)*",
                          lambda m: m[1] + cards, contents["AQWA"])
    if count != 1:
        raise ValueError("one AQWA frequency and direction deck required")
    aqwa = select_ah1_output(aqwa)
    return {"stage1.yml": ow.encode("utf-8"), "stage1.dat": aqwa.encode("utf-8")}


def select_ah1_output(aqwa):
    """Select AH1 before Deck 0 closes, preserving REST/RESTART adjacency.

    Card-order precedent: AqwaBackend.build_deck0 in aqwa_backend.py.
    Native interpretation remains subject to the required solver readback.
    """
    lines = aqwa.splitlines(keepends=True)
    options = [(i, line.split()[1:]) for i, line in enumerate(lines)
               if line.startswith("OPTIONS ")]
    ends = [i for i, tokens in options if "END" in tokens]
    if (sum(tokens.count("END") for _, tokens in options) != 1
            or any(i > ends[0] for i, _ in options)):
        raise ValueError("AQWA Deck 0 requires one final OPTIONS END card")
    for i, tokens in options:
        if "END" in tokens and tokens[-1] != "END":
            raise ValueError("AQWA Deck 0 option follows END")
        if "REST" in tokens and (i + 1 >= len(lines)
                                   or not lines[i + 1].startswith("RESTART ")):
            raise ValueError("AQWA Deck 0 requires RESTART immediately after REST")
    restarts = [i for i, line in enumerate(lines) if line.startswith("RESTART ")]
    if any(i != ends[0] + 1 or "REST" not in lines[i - 1].split()[1:]
           for i in restarts):
        raise ValueError("AQWA Deck 0 requires RESTART immediately after OPTIONS END")
    if not any("AHD1" in tokens for _, tokens in options):
        insertion = next(i for i, tokens in options if "REST" in tokens or "END" in tokens)
        lines.insert(insertion, "OPTIONS AHD1\n")
    return "".join(lines)


def stage1_manifest(stage):
    cases = []
    for i, period in enumerate(stage["periods_s"], 1):
        for j, heading in enumerate(stage["headings_deg"], 1):
            cases.append({"id": f"L01-S1-F{i:02d}-H{j:02d}",
                          "period_s": period, "heading_deg": heading,
                          "solvers": ["AQWA", "OrcaWave"],
                          "expected_outputs": {"AQWA": ["stage1.LIS", "stage1.AH1"],
                                               "OrcaWave": ["stage1.owr"]}})
    if "cases" in stage and stage["cases"] != cases:
        raise ValueError("declared stage-1 cases differ from the native grid")
    return {"cap_licensed_minutes": stage["cap_licensed_minutes"], "cases": cases,
            "budget_scope": "shared_stage1_all_solvers_and_variants",
            "stop_rule": (f"stop at {stage['cap_licensed_minutes']} minutes; "
                          "the solver manager stops both solvers when cumulative "
                          "licensed time reaches the cap, including failed attempts")}


def issue_native_inputs(request, root, output):
    """Issue candidate decks offline only after all release controls hold."""
    if request.get("source_rights") != SOURCE_RIGHTS:
        raise ValueError("complete internal-use source rights record required")
    cap = request.get("stage1", {}).get("cap_licensed_minutes")
    if type(cap) not in (int, float) or not math.isfinite(cap) or cap <= 0:
        raise ValueError("positive finite stage-1 licensed-minute cap required")
    validate_reconciliation(request)
    policy = allocation_policy(request, cap)
    contents = native_sources(request, root)
    payloads = native_stage1(request, contents)
    manifest = {
        "native_inputs": "ISSUED", "source_rights": request["source_rights"],
        "request_sha256": digest(request), "sources": request["sources"],
        "reconciliation": request["reconciliation"],
        "allocation_policy": policy,
        "stage1": stage1_manifest(request["stage1"]),
        "later_stages": [{"cases": n, "allocation": later_allocation(n, policy)}
                         for n in (90, 180)],
        "files": [{"file": path, "sha256": hashlib.sha256(raw).hexdigest()}
                  for path, raw in payloads.items()],
        "engineering_qualification": "NOT_EVALUATED",
        "native_readback": "NOT_EVALUATED",
        "native_to_request": [
            {"OrcaWave_period_s": p, "AQWA_frequency_hz": float(f"{1 / p:.7g}"),
             "requested_frequency_rad_s": math.tau / p}
            for p in request["stage1"]["periods_s"]],
        "native_heading_to_request": [
            {"OrcaWave_heading_deg": h, "AQWA_heading_deg": float(f"{h:.7g}"),
             "requested_heading_deg": h} for h in request["stage1"]["headings_deg"]],
    }
    serialized = json.dumps(manifest, indent=2, allow_nan=False) + "\n"
    output = private_output_directory(root, output)
    output.mkdir()  # Exclusive; never overwrite a previous issuance.
    for name, raw in payloads.items():
        with (output / name).open("xb") as stream:
            stream.write(raw)
    with (output / "manifest.json").open("x", encoding="utf-8", newline="\n") as stream:
        stream.write(serialized)
    for item in manifest["files"]:
        if hashlib.sha256((output / item["file"]).read_bytes()).hexdigest() != item["sha256"]:
            raise ValueError("issued native input readback digest mismatch")
    if read_json(output / "manifest.json") != manifest:
        raise ValueError("issued manifest readback mismatch")
    return manifest


def private_output_directory(root, output):
    """Refuse source-tree and Git-worktree derivatives, resolving symlinks first."""
    root = Path(root).resolve(strict=True)
    output = Path(output).resolve()
    # A work tree has a gitfile or a metadata directory containing HEAD.
    # Empty marker directories do not establish a Git repository.
    if output.is_relative_to(root) or any(
            (parent / ".git").is_file() or (parent / ".git/HEAD").is_file()
            or (parent / ".git").is_symlink()
            for parent in (output, *output.parents)):
        raise ValueError("private output must be outside the source root and Git work trees")
    return output


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True, allow_nan=False,
                                     separators=(",", ":")).encode("utf-8")).hexdigest()


def validate_export(config, payload, solver):
    if payload["solver"] != solver or payload["source"] != "solver":
        raise ValueError("distinct named solver exports required; no placeholders")
    if payload["native_readback"] is not True:
        raise ValueError("native readback attestation required")
    if not re.fullmatch(r"[0-9a-f]{64}", text(payload["native_sha256"])):
        raise ValueError("native artifact SHA-256 required")
    if payload["basis"] != config["basis"]:
        raise ValueError("physical basis or conventions differ")
    raos, matrices = {}, {}
    for row in payload["raos"]:
        key = (number(row["frequency_rad_s"], positive=True), number(row["heading_deg"]))
        if key in raos:
            raise ValueError("duplicate RAO key")
        values = row["values"]
        if not isinstance(values, list) or len(values) != DOFS:
            raise ValueError("invalid RAO dimensions")
        # Complex pair cardinality is algebraic, not an engineering constant.
        raos[key] = [complex(*vector(pair, 2)) for pair in values]
    for row in payload["matrices"]:
        key = number(row["frequency_rad_s"], positive=True)
        if key in matrices:
            raise ValueError("duplicate matrix frequency")
        matrices[key] = {name: matrix(row[name]) for name in ("added_mass", "damping")}
    expected = {(f, h) for f in config["frequencies_rad_s"] for h in config["headings_deg"]}
    if set(raos) != expected or set(matrices) != set(config["frequencies_rad_s"]):
        raise ValueError("full declared grid coverage required; no inner joins")
    return raos, matrices


def compare(config, aqwa, orcawave):
    """Conditional numerical band check; supplied evidence is not authenticated."""
    validate_config(config)
    a_raos, a_matrices = validate_export(config, aqwa, "AQWA")
    b_raos, b_matrices = validate_export(config, orcawave, "OrcaWave")
    criteria = config["criteria"]
    errors, phases = [], []
    excluded = 0
    for key in sorted(a_raos):
        for dof, (a, b) in enumerate(zip(a_raos[key], b_raos[key])):
            # Supplied engineering scale, never the measured response denominator.
            # Criteria explicitly define this pointwise norm in solver_request.html.
            errors.append({"quantity": "rao", "frequency_rad_s": key[0],
                           "heading_deg": key[1], "component": dof,
                           "normalized_error": abs(a - b) / criteria["rao_scales"][dof]})
            if min(abs(a), abs(b)) <= criteria["phase_amplitude_floors"][dof]:
                excluded += 1
            else:
                # Circular phase, as in multi_solver_comparator.py phase statistics.
                phase = abs(math.degrees(math.remainder(
                    cmath.phase(b) - cmath.phase(a), math.tau)))
                if not math.isfinite(phase):
                    raise ValueError("phase calculation overflow")
                phases.append(phase)
    for frequency in sorted(a_matrices):
        for quantity in ("added_mass", "damping"):
            for i in range(DOFS):
                for j in range(DOFS):
                    error = abs(a_matrices[frequency][quantity][i][j]
                                - b_matrices[frequency][quantity][i][j])
                    errors.append({"quantity": quantity, "frequency_rad_s": frequency,
                                   "component": [i, j], "normalized_error":
                                   error / criteria[f"{quantity}_scales"][i][j]})
    if any(not math.isfinite(row["normalized_error"]) for row in errors):
        raise ValueError("comparison overflow")
    governing = max(errors, key=lambda row: row["normalized_error"])
    failures = [row for row in errors
                if row["normalized_error"] > criteria["max_normalized_error"]]
    return {
        "config_sha256": digest(config), "aqwa_export_sha256": digest(aqwa),
        "orcawave_export_sha256": digest(orcawave), "criteria": criteria,
        "band_check": "OUTSIDE_BAND" if failures else "WITHIN_BAND",
        "governing": governing, "failures": failures, "rao_samples": len(a_raos),
        "matrix_frequencies": len(a_matrices), "phase_not_evaluated": excluded,
        "max_phase_difference_deg": max(phases) if phases else None,
        "phase_role": "diagnostic_only", "engineering_qualification": "NOT_EVALUATED",
        "evidence_status": "caller_supplied_not_independently_authenticated",
    }


def unique_pairs(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("duplicate JSON key")
        result[key] = value
    return result


def read_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8"), object_pairs_hook=unique_pairs)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("prepare", "compare", "issue"))
    parser.add_argument("config", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--aqwa", type=Path)
    parser.add_argument("--orcawave", type=Path)
    parser.add_argument("--source-root", type=Path)
    args = parser.parse_args(argv)
    try:
        config = read_json(args.config)
        if args.mode == "issue":
            if args.source_root is None:
                raise ValueError("explicit source root required for native issuance")
            issue_native_inputs(config, args.source_root, args.output)
            return 0
        if args.mode == "prepare":
            result = build_request(config)
        else:
            if args.aqwa is None or args.orcawave is None:
                raise ValueError("both normalized solver exports required")
            result = compare(config, read_json(args.aqwa), read_json(args.orcawave))
        serialized = json.dumps(result, indent=2, allow_nan=False) + "\n"
        # Serialize before exclusive creation so invalid numbers leave no partial file.
        with args.output.open("x", encoding="utf-8", newline="\n") as stream:
            stream.write(serialized)
        return int(result.get("band_check") == "OUTSIDE_BAND")
    except (ValueError, KeyError, TypeError, OSError, OverflowError) as error:
        parser.exit(1, f"Investigation refused: {error}\n")


if __name__ == "__main__":
    raise SystemExit(main())
