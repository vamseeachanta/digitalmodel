"""Branded-report wrapper (plan #2346 section E).

Post-processes a GTMReportBuilder-emitted HTML file by injecting
prospect branding into the <head> and <body> without replacing the
engineering body. Returns the modified HTML as a string AND writes it
back to disk when a target path is provided.

Design choices:

- **BeautifulSoup-free fallback.** The implementation uses plain string
  anchor-splicing instead of depending on BeautifulSoup because the
  existing report_template already emits well-formed sections and
  parsing the whole document doubles runtime + adds a dep. If
  BeautifulSoup is present and preferred later, swap
  `_splice_into_head` / `_splice_into_body` for a bs4 pass — the rest
  of the API is stable.

- **NDA watermark is a fixed-position CSS overlay**, not a DOM element
  on every page. It prints cleanly in browser-PDF export because its
  container inherits `position: fixed; top: 50%;` + `transform:
  rotate(-30deg);` — common PDF engines keep it.

- **Canonical-class disclaimer** auto-emits when
  `canonical_vessel_name` is supplied. Plan acceptance criterion
  "report cover page auto-emits the disclaimer when a canonical vessel
  is used" is enforced here.
"""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Sequence


@dataclass(frozen=True)
class BrandConfig:
    header: str
    footer: str
    nda_watermark: bool = False
    canonical_vessel_name: str | None = None
    canonical_citations: Sequence[str] = ()
    logo_inline_svg: str | None = None


def _client_header_html(cfg: BrandConfig) -> str:
    logo_block = ""
    if cfg.logo_inline_svg:
        logo_block = f'<span class="client-logo">{cfg.logo_inline_svg}</span>'
    return (
        '<div class="client-header" '
        'style="background:#0a2540;color:#fff;padding:14px 28px;'
        'font-family:system-ui,sans-serif;font-weight:600;'
        'display:flex;align-items:center;gap:18px;">'
        f'{logo_block}<span>{cfg.header}</span></div>'
    )


def _client_footer_html(cfg: BrandConfig) -> str:
    return (
        '<div class="client-footer" '
        'style="background:#0a2540;color:#cbd5e1;padding:12px 28px;'
        'font-family:system-ui,sans-serif;font-size:0.85rem;'
        'border-top:1px solid #1e3a5f;">'
        f'{cfg.footer}</div>'
    )


def _watermark_html(nda_text: str) -> str:
    return (
        '<div class="nda-watermark" '
        'style="position:fixed;top:50%;left:50%;'
        'transform:translate(-50%,-50%) rotate(-30deg);'
        'font-size:6rem;color:rgba(220,38,38,0.12);'
        'font-weight:800;font-family:system-ui,sans-serif;'
        'pointer-events:none;z-index:9999;'
        'white-space:nowrap;">'
        f'{nda_text}</div>'
    )


def _canonical_disclaimer_html(name: str, citations: Sequence[str]) -> str:
    citation_items = "".join(f"<li>{c}</li>" for c in citations)
    citation_block = (
        f'<ul style="margin:6px 0 0 18px;padding:0;">{citation_items}</ul>'
        if citation_items
        else ""
    )
    return (
        '<aside class="canonical-class-disclaimer" '
        'style="background:#fef3c7;border:1px solid #f59e0b;color:#78350f;'
        'padding:14px 18px;margin:12px 0;border-radius:6px;'
        'font-family:system-ui,sans-serif;font-size:0.9rem;">'
        '<strong>Class-typical reference used.</strong> '
        f'Vessel specification supplied by the ACE canonical library '
        f'({name}). Values are class-typical and do not represent any '
        f'specific commercial asset.'
        f'{citation_block}'
        '</aside>'
    )


def _splice_into_head(html: str, injection: str) -> str:
    """Insert before </head>, or prepend to <body> if no </head> found."""
    lower = html.lower()
    idx = lower.rfind("</head>")
    if idx >= 0:
        return html[:idx] + injection + html[idx:]
    body_idx = lower.find("<body")
    if body_idx < 0:
        return injection + html
    body_close = lower.find(">", body_idx)
    return html[: body_close + 1] + injection + html[body_close + 1 :]


def _splice_header_footer(html: str, header: str, footer: str, watermark: str) -> str:
    lower = html.lower()
    body_idx = lower.find("<body")
    if body_idx >= 0:
        body_close = lower.find(">", body_idx)
        # Insert header immediately after <body ...>.
        html = html[: body_close + 1] + header + watermark + html[body_close + 1 :]
    else:
        html = header + watermark + html

    footer_idx = html.lower().rfind("</body>")
    if footer_idx >= 0:
        html = html[:footer_idx] + footer + html[footer_idx:]
    else:
        html = html + footer
    return html


def wrap_with_client_branding(
    report_html_path: Path,
    cfg: BrandConfig,
    *,
    target_path: Path | None = None,
) -> str:
    """Inject client branding into a generated report HTML.

    Reads `report_html_path`, writes the branded HTML back to
    `target_path` (or the same path if omitted), and returns the HTML
    string.
    """
    html = Path(report_html_path).read_text(encoding="utf-8")

    # Inject a minimal <style> into <head> so the client-header/footer
    # CSS does not get overwritten by existing report styles.
    style_block = (
        '<style>'
        '.client-header, .client-footer {'
        '  width: 100%; box-sizing: border-box;'
        '}'
        '.client-header { position: sticky; top: 0; z-index: 100; }'
        '.canonical-class-disclaimer { max-width: 900px; }'
        '@media print {'
        '  .nda-watermark { color: rgba(220,38,38,0.10) !important; }'
        '}'
        '</style>'
    )
    html = _splice_into_head(html, style_block)

    header_html = _client_header_html(cfg)
    footer_html = _client_footer_html(cfg)
    watermark_html = (
        _watermark_html(f"CONFIDENTIAL — {cfg.footer}")
        if cfg.nda_watermark
        else ""
    )

    html = _splice_header_footer(html, header_html, footer_html, watermark_html)

    # Insert canonical-class disclaimer near the start of <body> (after
    # the client header) so it is visible on the cover page.
    if cfg.canonical_vessel_name:
        disclaimer = _canonical_disclaimer_html(
            cfg.canonical_vessel_name,
            cfg.canonical_citations,
        )
        header_idx = html.find('class="client-header"')
        if header_idx >= 0:
            close_idx = html.find("</div>", header_idx)
            if close_idx >= 0:
                insertion_point = close_idx + len("</div>")
                html = html[:insertion_point] + disclaimer + html[insertion_point:]
        else:  # pragma: no cover — the client-header was always injected above.
            html = disclaimer + html

    output_path = Path(target_path) if target_path else Path(report_html_path)
    output_path.write_text(html, encoding="utf-8")
    return html
