"""Run LibreDWG in isolated staging and report DXF facts without qualifying a hull.

Requires ezdxf>=1.3,<2. A successful open is not dimension/scale verification.
The caller must retain the report and independently qualify lines-plan geometry.
"""

import argparse
import hashlib
import json
import os
import shutil
import subprocess
import tempfile
from collections import Counter
from pathlib import Path

import ezdxf
from ezdxf import bbox


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def drawing_texts(entities):
    texts = []
    for entity in entities:
        if entity.dxftype() in ("TEXT", "MTEXT"):
            value = (
                entity.dxf.text if entity.dxftype() == "TEXT" else entity.plain_text()
            )
            texts.append(
                {
                    "handle": entity.dxf.handle,
                    "layer": entity.dxf.layer,
                    "text": value,
                }
            )
    return texts


def inspect_dxf(path):
    report = {
        "opens": False,
        "readiness": "blocked",
        "ezdxf_version": ezdxf.__version__,
    }
    try:
        doc = ezdxf.readfile(path)
        entities = list(doc.modelspace())
        report.update(
            opens=True,
            readiness="unverified",
            dxf_version=doc.dxfversion,
            insunits=doc.header.get("$INSUNITS"),
            entities_by_type=dict(Counter(e.dxftype() for e in entities)),
            entities_by_layer=dict(Counter(e.dxf.layer for e in entities)),
            layers=[layer.dxf.name for layer in doc.layers],
        )
        report["texts"] = drawing_texts(entities)
        bounds = bbox.extents(entities, fast=False)
        report["modelspace_bbox"] = (
            {"minimum": list(bounds.extmin), "maximum": list(bounds.extmax)}
            if bounds.has_data
            else None
        )
        # Counts/bounds above precede audit; audit may repair the in-memory copy.
        audit = doc.audit()
        report["audit_errors"] = [
            {"code": e.code, "message": e.message} for e in audit.errors
        ]
        report["audit_fixes"] = [
            {"code": e.code, "message": e.message} for e in audit.fixes
        ]
        report["qualification_required"] = (
            "Identify hull view, units, scale, stated dimensions and station assignments; compare source fidelity."
        )
    except (ezdxf.DXFError, OSError, UnicodeError, ValueError) as error:
        report["readiness"] = "blocked"
        report["error"] = f"{type(error).__name__}: {error}"
    return report


def invoke(command, env, cwd, timeout):
    try:
        result = subprocess.run(
            command,
            capture_output=True,
            text=True,
            env=env,
            cwd=cwd,
            timeout=timeout,
            check=False,
        )
        return {
            "returncode": result.returncode,
            "stdout": result.stdout,
            "stderr": result.stderr,
        }
    except (subprocess.TimeoutExpired, OSError) as error:

        def text_value(value):
            return (
                value.decode(errors="replace")
                if isinstance(value, bytes)
                else value or ""
            )

        return {
            "returncode": None,
            "stdout": text_value(getattr(error, "stdout", "")),
            "stderr": text_value(getattr(error, "stderr", "")),
            "error": f"{type(error).__name__}: {error}",
        }


def run_converter(executable, staged_source, staged_dxf, env, source_hash, timeout):
    version = invoke(
        [executable, "--version"], env, staged_source.parent, min(timeout, 10)
    )
    report = {
        "source": staged_source.name,
        "source_sha256": source_hash,
        "converter_version": version["stdout"].strip(),
        "converter_sha256": digest(executable),
        "command": ["dwg2dxf", "-v1", "-o", "<staged-output.dxf>", staged_source.name],
    }
    if version["returncode"] != 0:
        return (
            report | version | {"error": version.get("error", "Version command failed")}
        )
    command = [executable, "-v1", "-o", str(staged_dxf), str(staged_source)]
    return report | invoke(command, env, staged_source.parent, timeout)


def convert(source, output, converter, timeout=300):
    source, output = Path(source).resolve(), Path(output).resolve()
    if output.exists():
        raise FileExistsError(output)
    if source.suffix.lower() != ".dwg" or not source.is_file():
        raise ValueError("Source must be an existing DWG file")
    executable = shutil.which(str(converter))
    if executable is None:
        raise FileNotFoundError(converter)
    executable = str(Path(executable).resolve())
    source_hash = digest(source)
    env = {
        key: value for key, value in os.environ.items() if not key.startswith("GIT_")
    }
    with tempfile.TemporaryDirectory(prefix="dwg-probe-") as staging:
        staged_source = Path(staging) / source.name
        shutil.copyfile(source, staged_source)
        if digest(staged_source) != source_hash:
            raise RuntimeError("Staged source differs from recorded source")
        staged_dxf = Path(staging) / "converted.dxf"
        report = run_converter(
            executable, staged_source, staged_dxf, env, source_hash, timeout
        )
        if digest(source) != source_hash:
            raise RuntimeError("Source changed during conversion")
        if staged_dxf.is_file():
            report["dxf_sha256"] = digest(staged_dxf)
            report["dxf"] = inspect_dxf(staged_dxf)
            output.parent.mkdir(parents=True, exist_ok=True)
            # Exclusive creation preserves existing evidence even if it appears mid-run.
            with output.open("xb") as target, staged_dxf.open("rb") as original:
                shutil.copyfileobj(original, target)
        else:
            report["dxf"] = {
                "opens": False,
                "readiness": "blocked",
                "error": "Converter produced no DXF",
            }
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument(
        "output",
        type=Path,
        help="Unqualified staging DXF; never publish without review",
    )
    parser.add_argument("--converter", default="dwg2dxf")
    args = parser.parse_args()
    report = convert(args.source, args.output, args.converter)
    print(json.dumps(report, indent=2, allow_nan=False))
    complete = report["dxf"].get("readiness") == "unverified"
    return 0 if report["returncode"] == 0 and complete else 1


if __name__ == "__main__":
    raise SystemExit(main())
