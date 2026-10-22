"""Canonical UTF-8 text for portable fingerprints, never binary payloads."""
from pathlib import Path


def normalized_text_bytes(path: Path) -> bytes:
    """Read strict UTF-8, reject NUL, and map CRLF and CR to LF.

    Extends the CRLF replacement used by parametric refresh with universal
    newline handling. No BOM stripping, Unicode normalization or whitespace
    folding is performed. NUL-containing data is outside the text contract.
    """
    try:
        text = Path(path).read_bytes().decode("utf-8")
    except UnicodeDecodeError as exc:
        raise ValueError(f"Expected UTF-8 text: {Path(path).name}") from exc
    if "\x00" in text:
        raise ValueError(f"Expected UTF-8 text without NUL: {Path(path).name}")
    return text.replace("\r\n", "\n").replace("\r", "\n").encode("utf-8")
