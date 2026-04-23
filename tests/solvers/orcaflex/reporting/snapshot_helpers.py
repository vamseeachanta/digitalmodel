from __future__ import annotations

import re
from pathlib import Path


VOLATILE_PATTERNS: list[tuple[str, str]] = [
    # Example placeholders; tighten once real generated HTML is available.
    (r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z", "<TIMESTAMP>"),
    (r"/mnt/[^"]+", "<PATH>"),
    (r"[A-Fa-f0-9]{32,}", "<HEX>"),
]


def normalize_report_html(html_text: str) -> str:
    text = html_text.replace("\r\n", "\n")
    for pattern, replacement in VOLATILE_PATTERNS:
        text = re.sub(pattern, replacement, text)

    # Collapse excessive whitespace between tags while keeping visible text intact.
    text = re.sub(r">\s+<", "><", text)
    text = text.strip()
    return text


def load_normalized_snapshot(path: Path) -> str:
    return normalize_report_html(path.read_text(encoding="utf-8"))


def normalized_report_text(report_path: Path) -> str:
    return normalize_report_html(report_path.read_text(encoding="utf-8"))
