"""Run LibreDWG in isolated staging and report DXF facts without qualifying a hull.

Requires ezdxf>=1.3,<2. A successful open is not dimension/scale verification.
The caller must retain the report and independently qualify lines-plan geometry.
"""

import argparse
import hashlib
import io
import json
import logging
import os
import shutil
import subprocess
import tempfile
from collections import Counter
from contextlib import contextmanager, redirect_stderr, redirect_stdout
from pathlib import Path

import ezdxf
from ezdxf import bbox


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def extents(entities):
    bounds = bbox.extents(entities, fast=False)
    return (
        {"minimum": list(bounds.extmin), "maximum": list(bounds.extmax)}
        if bounds.has_data
        else None
    )


def public_identifier(value):
    # Arbitrary CAD names can contain restricted identities. Preserve only
    # reviewed generic identifiers; pseudonyms allow cross-file comparison.
    if value in {"0", "A", "SECTIONS", "BORDER", "Defpoints"} or value.isdecimal():
        return value
    return "withheld-" + hashlib.sha256(value.encode()).hexdigest()[:16]


def insert_metadata(doc, entities):
    inserts, tags = [], set()
    for entity in entities:
        if entity.dxftype() != "INSERT":
            continue
        block = doc.blocks.get(entity.dxf.name)
        inserts.append(
            {
                "block_name": public_identifier(entity.dxf.name),
                "scales": [entity.dxf.xscale, entity.dxf.yscale, entity.dxf.zscale],
                "block_extents": extents(block) if block is not None else None,
                "insert_extents": extents([entity]),
            }
        )
        for attribute in entity.attribs:
            tag = attribute.dxf.tag
            if tag.startswith("TITLE") or tag == "DWG#":
                tags.add(tag)
    return inserts, sorted(tags)


def viewport_metadata(doc):
    viewports = []
    for layout in doc.layouts:
        if layout.name == "Model":
            continue
        for entity in layout.query("VIEWPORT"):
            height = entity.dxf.view_height
            viewports.append(
                {
                    "viewport_id": entity.dxf.id,
                    "status": entity.dxf.status,
                    "paper_height": entity.dxf.height,
                    "view_height": height,
                    "scale": entity.dxf.height / height if height else None,
                }
            )
    return viewports


@contextmanager
def private_diagnostics():
    """Suppress decoder diagnostics without writing restricted payloads to disk."""
    previous = logging.root.manager.disable
    logging.disable(logging.CRITICAL)
    try:
        with redirect_stdout(io.StringIO()), redirect_stderr(io.StringIO()):
            yield
    finally:
        logging.disable(previous)


def layout_metadata(doc, entities, model):
    return {
        "opens": True,
        "readiness": "unverified",
        "dxf_version": doc.dxfversion,
        "insunits": doc.header.get("$INSUNITS"),
        "header": {
            key: doc.header.get(key)
            for key in ("$INSUNITS", "$MEASUREMENT", "$LUNITS", "$DIMSCALE")
        },
        "dimension_entity_count": sum(e.dxftype() == "DIMENSION" for e in entities),
        "entities_by_type": dict(Counter(e.dxftype() for e in model)),
        "entities_by_layer": dict(
            Counter(public_identifier(e.dxf.layer) for e in model)
        ),
        "layers": [public_identifier(layer.dxf.name) for layer in doc.layers],
    }


def inspect_dxf(path):
    report = {
        "opens": False,
        "readiness": "blocked",
        "ezdxf_version": ezdxf.__version__,
    }
    try:
        with private_diagnostics():
            doc = ezdxf.readfile(path)
            entities = [entity for layout in doc.layouts for entity in layout]
            model = list(doc.modelspace())
            report.update(layout_metadata(doc, entities, model))
            report["inserts"], report["title_attribute_tags"] = insert_metadata(
                doc, entities
            )
            report["paperspace_viewports"] = viewport_metadata(doc)
            report["modelspace_bbox"] = extents(model)
            audit = doc.audit()
            # Audit messages, drawing texts and attribute VALUES may contain
            # restricted identities. Only codes and allowlisted tags are emitted.
            report["audit_errors"] = [e.code for e in audit.errors]
            report["audit_fixes"] = [e.code for e in audit.fixes]
            report["qualification_required"] = (
                "Establish units, scale, dimensions and station assignments independently."
            )
    except Exception as error:  # noqa: BLE001 - each file must fail closed
        report["readiness"] = "blocked"
        report["error"] = f"{type(error).__name__}: inspection failed; message withheld"
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
            "diagnostics": {
                "stdout_bytes": len(result.stdout.encode()),
                "stderr_bytes": len(result.stderr.encode()),
            },
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
            "diagnostics": {
                "stdout_bytes": len(text_value(getattr(error, "stdout", "")).encode()),
                "stderr_bytes": len(text_value(getattr(error, "stderr", "")).encode()),
            },
            "error": f"{type(error).__name__}: converter failed; message withheld",
        }


def run_converter(executable, staged_source, staged_dxf, env, source_hash, timeout):
    version = invoke(
        [executable, "--version"], env, staged_source.parent, min(timeout, 10)
    )
    report = {
        "source": staged_source.name,
        "source_sha256": source_hash,
        "converter_version": "not emitted; binary digest identifies converter",
        "converter_sha256": digest(executable),
        "command": ["dwg2dxf", "-v1", "-o", "<staged-output.dxf>", staged_source.name],
    }
    if version["returncode"] != 0:
        return (
            report | version | {"error": version.get("error", "Version command failed")}
        )
    command = [executable, "-v1", "-o", str(staged_dxf), str(staged_source)]
    return report | invoke(command, env, staged_source.parent, timeout)


def convert(source, output, converter, timeout=300, inspection_only=False):
    source = Path(source).resolve()
    output = Path(output).resolve() if output is not None else None
    if not inspection_only and output is None:
        raise ValueError("Output is required unless inspection-only")
    if output is not None and output.exists():
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
            if not inspection_only:
                output.parent.mkdir(parents=True, exist_ok=True)
                # Exclusive creation preserves evidence appearing mid-run.
                with output.open("xb") as target, staged_dxf.open("rb") as original:
                    shutil.copyfileobj(original, target)
        else:
            report["dxf"] = {
                "opens": False,
                "readiness": "blocked",
                "error": "Converter produced no DXF",
            }
    report["inspection_only"] = inspection_only
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument(
        "output",
        type=Path,
        nargs="?",
        help="Unqualified staging DXF; never publish without review",
    )
    parser.add_argument("--converter", default="dwg2dxf")
    parser.add_argument(
        "--inspection-only",
        action="store_true",
        help="Inspect in temporary staging; retain no DXF",
    )
    args = parser.parse_args()
    if args.output is None and not args.inspection_only:
        parser.error("output is required unless --inspection-only")
    report = convert(
        args.source, args.output, args.converter, inspection_only=args.inspection_only
    )
    print(json.dumps(report, indent=2, allow_nan=False))
    complete = report["dxf"].get("readiness") == "unverified"
    return 0 if report["returncode"] == 0 and complete else 1


if __name__ == "__main__":
    raise SystemExit(main())
