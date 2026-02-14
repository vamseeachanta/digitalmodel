#!/usr/bin/env python3
"""
Extract comparison plot images from the OrcaWave Validation Report PDF.

Reads the PDF, extracts all embedded images, and saves them as PNG files
organized by page number. Uses PyPDF2 v3.x built-in image extraction.

Usage:
    uv run python scripts/benchmark/extract_pdf_plots.py
    uv run python scripts/benchmark/extract_pdf_plots.py --pdf-path path/to/report.pdf
    uv run python scripts/benchmark/extract_pdf_plots.py --output-dir custom/output/dir
    uv run python scripts/benchmark/extract_pdf_plots.py --min-size 50000

Key pages for Phase 1 cases:
    Pages 7-8:   Case 2.1 - Haskind load RAOs + PI mean drift loads
    Pages 31-32: Case 2.7 - CS and PI mean drift loads (pyramid)
    Pages 35-36: Case 2.8 - Ellipsoid convergence study
    Pages 45-46: Case 3.2 - QTF surge and heave (hemisphere)
    Pages 49-50: Case 3.3 - Full QTFs (multi-body)
"""

from __future__ import annotations

import argparse
import io
import struct
import sys
import zlib
from pathlib import Path

from PyPDF2 import PdfReader
from PyPDF2.generic import ArrayObject, IndirectObject

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DEFAULT_PDF = (
    Path(__file__).parent.parent.parent
    / "docs"
    / "modules"
    / "orcawave"
    / "L00_validation_wamit"
    / "OrcaWave Validation Report.pdf"
)

DEFAULT_OUTPUT = (
    Path(__file__).parent.parent.parent
    / "benchmark_output"
    / "validation"
    / "pdf_plots"
)

# Minimum image data size in bytes to filter out logos/icons
DEFAULT_MIN_SIZE = 10_000

# Key pages annotated with figure descriptions (1-indexed for display)
KEY_PAGES: dict[int, str] = {
    5: "Case 2.1 - geometry views",
    7: "Case 2.1 - Figure 2: Haskind load RAOs, beta=0",
    8: "Case 2.1 - Figure 3: PI mean drift loads, beta=27",
    9: "Case 2.2 - geometry views",
    11: "Case 2.2 - Figure 5: Haskind load RAOs",
    12: "Case 2.2 - Figure 6: PI mean drift loads",
    13: "Case 2.3 - Figures 7-8: Haskind + mean drift",
    14: "Case 2.3 - geometry views",
    16: "Case 2.4 - Figure 10: Haskind load RAOs",
    17: "Case 2.4 - Figure 11: PI mean drift loads",
    18: "Case 2.5 - geometry views (ISSC TLP)",
    19: "Case 2.5 - Figure 13: Haskind load RAOs",
    20: "Case 2.5 - Figure 14: PI mean drift loads",
    21: "Case 2.6 - geometry views",
    23: "Case 2.6 - Figure 17: Haskind load RAOs (cylinder)",
    24: "Case 2.6 - Figure 18: Haskind load RAOs (spheroid)",
    25: "Case 2.6 - Figure 19: PI mean drift loads",
    26: "Case 2.6 - geometry views (spheroid variant)",
    27: "Case 2.6 - Figure 21: Mean drift loads (re-run)",
    29: "Case 2.7 - Figure 22: pyramid mesh views",
    31: "Case 2.7 - Figure 23: CS and PI mean drift loads, beta=0",
    32: "Case 2.7 - Figure 24: convergence study T=5s",
    33: "Case 2.8 - Figure 25: ellipsoid mesh views",
    35: "Case 2.8 - Figure 26: convergence study T=2s",
    36: "Case 2.8 - Figure 27: wall-sided ellipsoid convergence",
    37: "Case 2.9 - geometry views (moonpool)",
    39: "Case 2.9 - Figure 29: heave added mass/damping",
    42: "Case 3.1 - Figure 31: bottom-mounted cylinder components",
    43: "Case 3.2 - Figure 32: hemisphere mesh views",
    45: "Case 3.2 - Figure 33: surge QTF components",
    46: "Case 3.2 - Figure 34: heave QTF components",
    49: "Case 3.3 - Figure 36: full QTFs (cylinder)",
    50: "Case 3.3 - Figure 37: full QTFs (ellipsoid)",
}


# ---------------------------------------------------------------------------
# Extraction
# ---------------------------------------------------------------------------


def _make_png(width: int, height: int, raw_rgb: bytes) -> bytes:
    """Encode raw RGB pixel data as a PNG file.

    Constructs a valid PNG byte stream from raw interleaved RGB pixels.
    This avoids depending on Pillow for the simple FlateDecode case.
    """

    def _chunk(chunk_type: bytes, data: bytes) -> bytes:
        c = chunk_type + data
        crc = struct.pack(">I", zlib.crc32(c) & 0xFFFFFFFF)
        return struct.pack(">I", len(data)) + c + crc

    # PNG signature
    sig = b"\x89PNG\r\n\x1a\n"

    # IHDR: width, height, bit_depth=8, color_type=2 (RGB)
    ihdr_data = struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0)
    ihdr = _chunk(b"IHDR", ihdr_data)

    # IDAT: filtered scanlines (filter byte 0 = None for each row)
    row_size = width * 3
    raw_lines = []
    for y in range(height):
        raw_lines.append(b"\x00")  # filter type None
        raw_lines.append(raw_rgb[y * row_size : (y + 1) * row_size])
    compressed = zlib.compress(b"".join(raw_lines))
    idat = _chunk(b"IDAT", compressed)

    # IEND
    iend = _chunk(b"IEND", b"")

    return sig + ihdr + idat + iend


def _try_pillow_decode(
    raw_data: bytes,
    width: int,
    height: int,
    color_space: str,
    bits_per_component: int,
    filter_type: str,
) -> bytes | None:
    """Attempt to use Pillow for JPEG (DCTDecode) or complex images."""
    try:
        from PIL import Image
    except ImportError:
        return None

    if filter_type == "/DCTDecode":
        # raw_data is JPEG bytes
        img = Image.open(io.BytesIO(raw_data))
        buf = io.BytesIO()
        img.save(buf, format="PNG")
        return buf.getvalue()

    if filter_type == "/JPXDecode":
        # JPEG2000
        img = Image.open(io.BytesIO(raw_data))
        buf = io.BytesIO()
        img.save(buf, format="PNG")
        return buf.getvalue()

    return None


def extract_images(
    pdf_path: Path,
    output_dir: Path,
    min_size: int,
) -> tuple[list[dict], int, int]:
    """Extract all images from the PDF and save as PNG files.

    Uses the lower-level XObject approach for robust handling of
    different image formats and corrupt data.

    Returns (results, skipped_small, skipped_error) where results is a
    list of metadata dicts for each extracted image.
    """
    reader = PdfReader(str(pdf_path))
    results: list[dict] = []
    skipped_small = 0
    skipped_error = 0

    for page_idx, page in enumerate(reader.pages):
        page_num = page_idx + 1

        try:
            resources = page["/Resources"]
        except (KeyError, TypeError):
            continue

        try:
            x_objects = resources["/XObject"].get_object()
        except (KeyError, TypeError, AttributeError):
            continue

        img_idx = 0
        for obj_name in x_objects:
            try:
                obj = x_objects[obj_name]
                if isinstance(obj, IndirectObject):
                    obj = obj.get_object()

                if obj.get("/Subtype") != "/Image":
                    continue

                width = int(obj["/Width"])
                height = int(obj["/Height"])
                bpc = int(obj.get("/BitsPerComponent", 8))

                # Resolve color space
                cs = obj.get("/ColorSpace", "/DeviceRGB")
                if isinstance(cs, ArrayObject):
                    cs = str(cs[0])
                else:
                    cs = str(cs)

                # Determine filter type
                filt = obj.get("/Filter", "")
                if isinstance(filt, ArrayObject):
                    filt = str(filt[-1]) if filt else ""
                else:
                    filt = str(filt)

                # Get decompressed pixel data
                raw_data = obj.get_data()

                # Handle different encoding formats
                png_data = None

                if filt in ("/DCTDecode", "/JPXDecode"):
                    # JPEG / JPEG2000 - try Pillow
                    png_data = _try_pillow_decode(
                        raw_data, width, height, cs, bpc, filt
                    )
                    if png_data is None:
                        # Save as JPEG directly if Pillow unavailable
                        ext = ".jpg" if filt == "/DCTDecode" else ".jp2"
                        out_name = (
                            f"p{page_num:02d}_{img_idx:02d}"
                            f"_{obj_name.lstrip('/')}{ext}"
                        )
                        out_path = output_dir / out_name
                        out_path.write_bytes(raw_data)
                        png_data = raw_data  # track size

                elif "RGB" in cs and bpc == 8:
                    # FlateDecode RGB - most common in this PDF
                    expected = width * height * 3
                    if len(raw_data) == expected:
                        png_data = _make_png(width, height, raw_data)
                    else:
                        # Try Pillow as fallback
                        png_data = _try_pillow_decode(
                            raw_data, width, height, cs, bpc, filt
                        )

                elif "Gray" in cs and bpc == 8:
                    # Grayscale - convert to RGB for PNG
                    expected = width * height
                    if len(raw_data) == expected:
                        rgb = b"".join(
                            bytes([b, b, b]) for b in raw_data
                        )
                        png_data = _make_png(width, height, rgb)

                else:
                    # Unknown format - try Pillow
                    png_data = _try_pillow_decode(
                        raw_data, width, height, cs, bpc, filt
                    )

                if png_data is None:
                    skipped_error += 1
                    print(
                        f"  WARNING: Could not decode page {page_num} "
                        f"{obj_name} ({width}x{height} {cs} {filt})"
                    )
                    img_idx += 1
                    continue

                # Size filter
                if len(png_data) < min_size:
                    skipped_small += 1
                    img_idx += 1
                    continue

                # Write PNG
                obj_label = obj_name.lstrip("/")
                out_name = f"p{page_num:02d}_{img_idx:02d}_{obj_label}.png"
                out_path = output_dir / out_name
                out_path.write_bytes(png_data)

                annotation = KEY_PAGES.get(page_num, "")
                results.append({
                    "page": page_num,
                    "index": img_idx,
                    "name": obj_label,
                    "width": width,
                    "height": height,
                    "output": str(out_path),
                    "size_bytes": len(png_data),
                    "annotation": annotation,
                })

            except Exception as exc:
                skipped_error += 1
                print(
                    f"  WARNING: Error extracting {obj_name} from "
                    f"page {page_num}: {exc}"
                )

            img_idx += 1

    return results, skipped_small, skipped_error


def print_summary(
    results: list[dict],
    skipped_small: int,
    skipped_error: int,
    output_dir: Path,
) -> None:
    """Print a readable summary of what was extracted."""
    print(f"\n{'=' * 70}")
    print("OrcaWave Validation Report - Image Extraction Summary")
    print(f"{'=' * 70}")
    print(f"Output directory: {output_dir}")
    print(f"Images extracted: {len(results)}")
    print(f"Small images skipped (logos/icons): {skipped_small}")
    if skipped_error:
        print(f"Images skipped (decode errors): {skipped_error}")

    if not results:
        print("No images extracted.")
        return

    total_bytes = sum(r["size_bytes"] for r in results)
    print(f"Total data: {total_bytes / 1024:.1f} KB")

    # Group by page
    pages_with_images: dict[int, list[dict]] = {}
    for r in results:
        pages_with_images.setdefault(r["page"], []).append(r)

    print(f"\nPages with plot images: {len(pages_with_images)}")
    print(f"\n{'Page':>6}  {'Count':>5}  {'Size (KB)':>10}  Description")
    print(f"{'-' * 6}  {'-' * 5}  {'-' * 10}  {'-' * 40}")

    for page_num in sorted(pages_with_images.keys()):
        imgs = pages_with_images[page_num]
        page_bytes = sum(i["size_bytes"] for i in imgs)
        annotation = KEY_PAGES.get(page_num, "")
        print(
            f"  {page_num:4d}  {len(imgs):5d}  {page_bytes / 1024:10.1f}  {annotation}"
        )

    # Highlight Phase 1 key pages
    phase1_pages = {7, 8, 31, 32, 35, 36, 45, 46, 49, 50}
    phase1_imgs = [r for r in results if r["page"] in phase1_pages]
    print(f"\nPhase 1 key images: {len(phase1_imgs)}")
    for r in phase1_imgs:
        print(f"  {Path(r['output']).name:40s}  {r['annotation']}")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Extract images from the OrcaWave Validation Report PDF.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "--pdf-path",
        type=Path,
        default=DEFAULT_PDF,
        help=f"Path to the PDF file (default: {DEFAULT_PDF.name})",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=DEFAULT_OUTPUT,
        help=f"Output directory for extracted images (default: {DEFAULT_OUTPUT})",
    )
    parser.add_argument(
        "--min-size",
        type=int,
        default=DEFAULT_MIN_SIZE,
        help=(
            f"Minimum image data size in bytes to include "
            f"(default: {DEFAULT_MIN_SIZE}, filters logos/icons)"
        ),
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)

    if not args.pdf_path.is_file():
        print(f"ERROR: PDF not found at {args.pdf_path}", file=sys.stderr)
        return 1

    args.output_dir.mkdir(parents=True, exist_ok=True)
    print(f"Extracting images from: {args.pdf_path.name}")
    print(f"Output directory: {args.output_dir}")
    print(f"Minimum image size: {args.min_size} bytes")

    results, skipped_small, skipped_error = extract_images(
        args.pdf_path, args.output_dir, args.min_size
    )

    print_summary(results, skipped_small, skipped_error, args.output_dir)
    return 0


if __name__ == "__main__":
    sys.exit(main())
