#!/usr/bin/env python3
"""Render reviewable NAIP preview frames for address manifest.

This is a fallback renderer for issue #2538 when Earth Engine is not authenticated.
It uses Microsoft Planetary Computer NAIP STAC previews/thumbnails to prove the
multi-address artifact loop, with explicit limitations in the generated report.
"""
from __future__ import annotations

import io
import json
import subprocess
from collections import OrderedDict
from datetime import datetime, timezone
from pathlib import Path

import imageio.v2 as imageio
import requests
from PIL import Image, ImageDraw, ImageFont

WORKDIR = Path('/mnt/local-analysis/ace2-gis-timelapse')
MANIFEST = WORKDIR / 'addresses.json'
STAC_URL = 'https://planetarycomputer.microsoft.com/api/stac/v1/search'


def load_font(size: int = 24):
    for p in ['/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf', '/usr/share/fonts/truetype/liberation2/LiberationSans-Regular.ttf']:
        if Path(p).exists():
            return ImageFont.truetype(p, size)
    return ImageFont.load_default()


def label_image(img: Image.Image, lines: list[str]) -> Image.Image:
    img = img.convert('RGB').resize((960, 720))
    draw = ImageDraw.Draw(img, 'RGBA')
    font = load_font(24)
    small = load_font(18)
    pad = 14
    box_h = 34 + 28 * len(lines)
    draw.rectangle([0, 0, img.width, box_h], fill=(0, 0, 0, 170))
    y = pad
    for i, line in enumerate(lines):
        draw.text((pad, y), line, fill=(255, 255, 255, 255), font=font if i == 0 else small)
        y += 30 if i == 0 else 26
    return img


def stac_naip_items(bbox: list[float]) -> list[dict]:
    payload = {'collections': ['naip'], 'bbox': bbox, 'datetime': '2003-01-01/2026-12-31', 'limit': 100}
    r = requests.post(STAC_URL, json=payload, timeout=60)
    r.raise_for_status()
    return r.json().get('features', [])


def choose_representative_by_year(items: list[dict], max_years: int = 8) -> list[dict]:
    by_year: OrderedDict[str, dict] = OrderedDict()
    for item in sorted(items, key=lambda f: f.get('properties', {}).get('datetime', '')):
        dt = item.get('properties', {}).get('datetime', '')
        year = dt[:4]
        if year and year not in by_year:
            by_year[year] = item
    selected = list(by_year.values())
    if len(selected) > max_years:
        # Keep an even-ish spread including first and last.
        idxs = sorted(set(round(i * (len(selected) - 1) / (max_years - 1)) for i in range(max_years)))
        selected = [selected[i] for i in idxs]
    return selected


def download_preview(item: dict) -> Image.Image:
    assets = item.get('assets', {})
    href = assets.get('rendered_preview', {}).get('href') or assets.get('thumbnail', {}).get('href')
    if not href:
        raise RuntimeError(f'No preview/thumbnail asset for {item.get("id")}')
    r = requests.get(href, timeout=90)
    r.raise_for_status()
    return Image.open(io.BytesIO(r.content))


def render_address(addr: dict, output_root: Path) -> dict:
    slug = addr['slug']
    meta_path = output_root / slug / 'manifests' / f'{slug}-aoi-metadata.json'
    meta = json.loads(meta_path.read_text())
    bbox = meta['aoi']['neighborhood']['bounds_wgs84']
    out_dir = output_root / slug
    frame_dir = out_dir / 'frames' / 'neighborhood'
    media_dir = out_dir / 'media'
    report_dir = out_dir / 'reports'
    manifest_dir = out_dir / 'manifests'
    for d in [frame_dir, media_dir, report_dir, manifest_dir]:
        d.mkdir(parents=True, exist_ok=True)

    items = stac_naip_items(bbox)
    selected = choose_representative_by_year(items)
    frame_records = []
    frames = []
    for n, item in enumerate(selected, 1):
        dt = item.get('properties', {}).get('datetime', '')
        year = dt[:4] or 'unknown'
        item_id = item.get('id', 'unknown')
        img = download_preview(item)
        labeled = label_image(img, [
            f"{addr['label']} — NAIP preview {year}",
            f"Scale: neighborhood fallback preview | radius: {meta['aoi']['neighborhood']['radius_miles']} mi",
            f"Source item: {item_id}",
            "Limitation: STAC preview/thumbnail proves pipeline; final frame should crop COG/tiles consistently.",
        ])
        frame_path = frame_dir / f'{slug}-naip-{year}-{n:02d}.png'
        labeled.save(frame_path)
        frames.append(labeled)
        frame_records.append({
            'frame': str(frame_path),
            'source': 'NAIP via Microsoft Planetary Computer STAC rendered_preview/thumbnail',
            'item_id': item_id,
            'datetime': dt,
            'year': year,
            'scale': 'neighborhood',
            'radius_miles': meta['aoi']['neighborhood']['radius_miles'],
            'asset_used': 'rendered_preview' if item.get('assets', {}).get('rendered_preview') else 'thumbnail',
        })

    if not frames:
        raise RuntimeError('No NAIP frames selected')

    gif_path = media_dir / f'{slug}-naip-neighborhood-preview.gif'
    mp4_path = media_dir / f'{slug}-naip-neighborhood-preview.mp4'
    contact_path = report_dir / f'{slug}-naip-contact-sheet.png'
    imageio.mimsave(gif_path, [f.copy() for f in frames], duration=1.25)
    ffmpeg_list = media_dir / f'{slug}-ffmpeg-frames.txt'
    ffmpeg_list.write_text(''.join(f"file '{Path(r['frame']).resolve()}'\nduration 1.25\n" for r in frame_records) + f"file '{Path(frame_records[-1]['frame']).resolve()}'\n")
    subprocess.run([
        'ffmpeg', '-y', '-hide_banner', '-loglevel', 'error',
        '-f', 'concat', '-safe', '0', '-i', str(ffmpeg_list),
        '-vf', 'fps=1,format=yuv420p', str(mp4_path),
    ], check=True)

    cols = min(3, len(frames))
    rows = (len(frames) + cols - 1) // cols
    thumb_w, thumb_h = 480, 360
    contact = Image.new('RGB', (cols * thumb_w, rows * thumb_h), 'white')
    for i, f in enumerate(frames):
        contact.paste(f.resize((thumb_w, thumb_h)), ((i % cols) * thumb_w, (i // cols) * thumb_h))
    contact.save(contact_path)

    frame_manifest = {
        'generated_at': datetime.now(timezone.utc).isoformat(),
        'slug': slug,
        'address': addr['address'],
        'scale': 'neighborhood',
        'bbox_wgs84': bbox,
        'renderer': 'naip_stac_preview_fallback_v1',
        'limitations': [
            'Earth Engine is not authenticated on this machine, so this renderer uses public STAC previews/thumbnails.',
            'Preview frames are review artifacts, not final measurement-grade orthomosaic crops.',
            'Final production rendering should use NAIP COG/tile crop/mosaic with consistent AOI extent.',
        ],
        'frames': frame_records,
        'artifacts': {'gif': str(gif_path), 'mp4': str(mp4_path), 'contact_sheet': str(contact_path)},
    }
    frame_manifest_path = manifest_dir / f'{slug}-frame-manifest.json'
    frame_manifest_path.write_text(json.dumps(frame_manifest, indent=2))

    report_path = report_dir / f'{slug}-review-report.md'
    report_path.write_text(
        f"# {addr['label']} historical imagery review artifact\n\n"
        f"Address: `{addr['address']}`\n\n"
        f"Generated: `{frame_manifest['generated_at']}`\n\n"
        f"## Artifacts\n\n"
        f"- GIF: `{gif_path}`\n"
        f"- MP4: `{mp4_path}`\n"
        f"- Contact sheet: `{contact_path}`\n"
        f"- Frame manifest: `{frame_manifest_path}`\n\n"
        f"## Source and date coverage\n\n"
        + ''.join(f"- {r['year']}: {r['item_id']} ({r['datetime']})\n" for r in frame_records)
        + "\n## Limitations and next step\n\n"
        + "Earth Engine auth is currently missing on ace-linux-2. These are NAIP public STAC preview frames to validate the multi-address pipeline. Next step is to switch renderer from preview assets to NAIP COG/tile crop/mosaic for consistent AOI extent and parcel/neighborhood scale.\n"
    )

    return {
        'slug': slug,
        'status': 'complete',
        'frames': len(frame_records),
        'years': [r['year'] for r in frame_records],
        'artifacts': frame_manifest['artifacts'],
        'frame_manifest': str(frame_manifest_path),
        'report': str(report_path),
    }


def main() -> None:
    manifest = json.loads(MANIFEST.read_text())
    output_root = Path(manifest['output_root'])
    status = {'generated_at': datetime.now(timezone.utc).isoformat(), 'stage': 'render_preview_artifacts', 'addresses': []}
    for addr in manifest['addresses']:
        try:
            status['addresses'].append(render_address(addr, output_root))
        except Exception as e:
            status['addresses'].append({'slug': addr.get('slug'), 'status': 'failed', 'error_type': type(e).__name__, 'error': str(e)[:1000]})
    status_path = WORKDIR / 'batch-render-status.json'
    status_path.write_text(json.dumps(status, indent=2))
    print(json.dumps(status, indent=2))


if __name__ == '__main__':
    main()
