from __future__ import annotations

import re
from pathlib import Path


VOLATILE_PATTERNS: list[tuple[str, str]] = [
    # Example placeholders; tighten once real generated HTML is available.
    (r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z', '<TIMESTAMP>'),
    (r'/mnt/[^"\s>]+', '<PATH>'),
    (r'[A-Fa-f0-9]{32,}', '<HEX>'),
]


def _fold_json_unicode_escapes(text: str) -> str:
    """Fold JSON \\uXXXX escapes into literal characters.

    Plotly's embedded-figure JSON is ASCII-escaped (``\\u00b7``) when orjson is
    absent but keeps literal UTF-8 (``·``) when orjson is installed, so the
    same report renders byte-differently across environments. Comparing on the
    folded form makes snapshots environment-agnostic. Surrogate pairs are not
    expected in these reports and are left untouched.
    """
    return re.sub(
        r"\\u([0-9a-fA-F]{4})",
        lambda m: chr(int(m.group(1), 16))
        if not 0xD800 <= int(m.group(1), 16) <= 0xDFFF
        else m.group(0),
        text,
    )


def normalize_report_html(html_text: str) -> str:
    text = html_text.replace("\r\n", "\n")
    text = _fold_json_unicode_escapes(text)
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
