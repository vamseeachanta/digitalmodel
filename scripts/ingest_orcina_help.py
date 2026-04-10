#!/usr/bin/env python3
"""Download and convert Orcina help documentation to LLM-friendly markdown.

Usage:
    python scripts/ingest_orcina_help.py --product orcaflex --output docs/domains/orcaflex/reference/
    python scripts/ingest_orcina_help.py --product orcawave --output docs/domains/orcawave/reference/
    python scripts/ingest_orcina_help.py --product orcfxapi --output docs/domains/orcfxapi/reference/
    python scripts/ingest_orcina_help.py --all

Requires: pip install beautifulsoup4 markdownify requests
"""

import argparse
import os
import re
import sys
import time
from pathlib import Path
from urllib.parse import urljoin

try:
    import requests
    from bs4 import BeautifulSoup
    from markdownify import markdownify as md
except ImportError:
    print("Missing dependencies. Install with:")
    print("  pip install beautifulsoup4 markdownify requests")
    sys.exit(1)


REPO_ROOT = Path(__file__).resolve().parent.parent

BASE_URLS = {
    "orcaflex": "https://www.orcina.com/webhelp/OrcaFlex/",
    "orcawave": "https://www.orcina.com/webhelp/OrcaWave/",
    "orcfxapi": "https://www.orcina.com/webhelp/OrcFxAPI/",
}

# Priority pages per product -- (slug, output_filename, description)
PRIORITY_PAGES = {
    "orcaflex": [
        # Text data files (YAML format)
        ("Content/html/Textdatafiles.htm", "text-data-files", "YAML text data file format and syntax"),
        ("Content/html/Textdatafiles,Examplesofsettingdata.htm", "text-data-files-examples", "Examples of setting data in YAML files"),
        ("Content/html/Textdatafiles,Automatinggeneration.htm", "text-data-files-automation", "Automating YAML file generation"),
        # General data
        ("Content/html/Generaldata.htm", "general-data", "General model data properties"),
        ("Content/html/Generaldata,Statics.htm", "general-data-statics", "Statics solver configuration"),
        ("Content/html/Generaldata,Dynamics.htm", "general-data-dynamics", "Dynamics solver configuration"),
        ("Content/html/Generaldata,Logging.htm", "general-data-logging", "Logging and output configuration"),
        ("Content/html/Generaldata,Analysis.htm", "general-data-analysis", "Analysis options"),
        # Environment
        ("Content/html/Environment.htm", "environment", "Environment overview"),
        ("Content/html/Environment,Seadata.htm", "environment-sea-data", "Sea properties (density, viscosity, temperature)"),
        ("Content/html/Environment,Wavedata.htm", "environment-wave-data", "Wave configuration and types"),
        ("Content/html/Environment,Currentdata.htm", "environment-current-data", "Current profiles and models"),
        ("Content/html/Environment,Seabeddata.htm", "environment-seabed-data", "Seabed shape and soil models"),
        ("Content/html/Environment,Winddata.htm", "environment-wind-data", "Wind models and spectra"),
        ("Content/html/Environment,Dataforrandomwaves.htm", "environment-random-waves", "Random wave spectrum parameters"),
        ("Content/html/Environment,DataforJONSWAPandISSCspectra.htm", "environment-jonswap-issc", "JONSWAP and ISSC spectra data"),
        ("Content/html/Environment,Dataforregularwaves.htm", "environment-regular-waves", "Regular wave parameters"),
        # Vessel types
        ("Content/html/Vesseltypes.htm", "vessel-types", "Vessel type overview and draught system"),
        ("Content/html/Vesseltypes,Structure.htm", "vessel-types-structure", "Vessel type structure (mass, inertia, CoG)"),
        ("Content/html/Vesseltypes,Conventions.htm", "vessel-types-conventions", "RAO and QTF conventions"),
        ("Content/html/Vesseltypes,RAOs.htm", "vessel-types-raos", "Displacement and load RAOs"),
        ("Content/html/Vesseltypes,Stiffness,addedmassanddamping.htm", "vessel-types-stiffness-amd", "Stiffness, added mass, and damping"),
        ("Content/html/Vesseltypes,Currentandwindloads.htm", "vessel-types-current-wind", "Current and wind load coefficients"),
        ("Content/html/Vesseltypes,WavedriftandsumfrequencyQTFs.htm", "vessel-types-qtfs", "Wave drift and sum frequency QTFs"),
        # Vessels
        ("Content/html/Vesseldata.htm", "vessel-data", "Vessel instance data and connections"),
        ("Content/html/Vesseldata,Calculationdata.htm", "vessel-data-calculation", "Vessel calculation and motion data"),
        # Line types
        ("Content/html/Linetypes,Data.htm", "line-types-data", "Line type categories and overview"),
        ("Content/html/Linetypes,Geometry,massexpansiondata.htm", "line-types-geometry-mass", "Line type geometry, mass, expansion"),
        ("Content/html/Linetypes,Structuredata.htm", "line-types-structure", "Line type structural properties"),
        ("Content/html/Linetypes,Dragliftdata.htm", "line-types-drag-lift", "Line type drag and lift coefficients"),
        ("Content/html/Linetypes,Addedmass,inertia,slamdata.htm", "line-types-added-mass", "Line type added mass, inertia, slam"),
        ("Content/html/Linetypes,Contactdata.htm", "line-types-contact", "Line type contact data"),
        ("Content/html/Linetypes,Stressdata.htm", "line-types-stress", "Line type stress calculation data"),
        # Lines
        ("Content/html/Linedata.htm", "line-data", "Line object overview and properties"),
        ("Content/html/Linedata,Structure.htm", "line-data-structure", "Line sections and segmentation"),
        ("Content/html/Linedata,Endconnections.htm", "line-data-end-connections", "Line end connections and positions"),
        ("Content/html/Linedata,Contents.htm", "line-data-contents", "Line contents (internal fluid)"),
        ("Content/html/Linedata,Statics.htm", "line-data-statics", "Line statics configuration"),
        # 6D Buoys
        ("Content/html/6Dbuoys.htm", "6d-buoys", "6D buoy overview"),
        ("Content/html/6Dbuoys,Commondata.htm", "6d-buoys-common-data", "6D buoy common properties"),
        ("Content/html/6Dbuoys,Lumpedbuoyproperties.htm", "6d-buoys-lumped-properties", "6D lumped buoy mass and inertia"),
        # 3D Buoys
        ("Content/html/3Dbuoys.htm", "3d-buoys", "3D buoy overview"),
        ("Content/html/3Dbuoys,Data.htm", "3d-buoys-data", "3D buoy data properties"),
        # Shapes
        ("Content/html/Shapes.htm", "shapes", "Shape objects overview"),
        ("Content/html/Shapes,Data.htm", "shapes-data", "Shape data properties"),
        # Constraints
        ("Content/html/Constraints.htm", "constraints", "Constraint objects overview"),
        ("Content/html/Constraints,Commondata.htm", "constraints-common-data", "Constraint common properties"),
        # Links
        ("Content/html/Links,Data.htm", "links-data", "Link data properties"),
        # Winches
        ("Content/html/Winches.htm", "winches", "Winch objects overview"),
        ("Content/html/Winches,Data.htm", "winches-data", "Winch data properties"),
        ("Content/html/Winches,Wireproperties.htm", "winches-wire-properties", "Winch wire properties"),
    ],
    "orcawave": [
        ("Content/html/Data,Model.htm", "data-model", "OrcaWave model configuration"),
        ("Content/html/Data,Calculationandoutput.htm", "data-calculation-output", "Calculation and output settings"),
        ("Content/html/Data,Environment.htm", "data-environment", "Environment settings (depth, density)"),
        ("Content/html/Data,Bodies.htm", "data-bodies", "Body definitions and properties"),
        ("Content/html/Data,Inertia.htm", "data-inertia", "Body inertia properties"),
        ("Content/html/Data,Constraints.htm", "data-constraints", "Constraints between bodies"),
        ("Content/html/Data,Morisonelements.htm", "data-morison", "Morison element definitions"),
        ("Content/html/Data,Springdampers.htm", "data-spring-dampers", "Spring/damper connections"),
        ("Content/html/Data,QTFs.htm", "data-qtfs", "QTF calculation settings"),
        ("Content/html/Data,Wavespectrum.htm", "data-wave-spectrum", "Wave spectrum configuration"),
        ("Content/html/Userinterface,Textdatafiles.htm", "text-data-files", "OrcaWave text data file format"),
    ],
    "orcfxapi": [
        ("Content/html/Pythoninterface,Introduction.htm", "python-introduction", "Python interface overview"),
        ("Content/html/Pythoninterface,Runningmodels.htm", "python-running-models", "Running and controlling simulations"),
        ("Content/html/Pythoninterface,Results.htm", "python-results", "Extracting simulation results"),
        ("Content/html/Pythoninterface,Creatingobjects.htm", "python-creating-objects", "Creating model objects"),
        ("Content/html/Pythoninterface,Objectdata.htm", "python-object-data", "Reading and writing object data"),
        ("Content/html/Pythonreference,Model.htm", "python-ref-model", "Model class reference"),
        ("Content/html/Pythonreference,OrcaFlexObject.htm", "python-ref-object", "OrcaFlexObject class reference"),
        ("Content/html/Modelbuilding,Introduction.htm", "model-building-intro", "Model building introduction"),
        ("Content/html/Modelbuilding,Settingthedata.htm", "model-building-setting-data", "Setting model data programmatically"),
        ("Content/html/Modelbuilding,Morecomplexdata.htm", "model-building-complex-data", "Complex data patterns"),
        ("Content/html/Modelbuilding,Anexample.htm", "model-building-example", "Complete model building example"),
    ],
}


def html_to_markdown(html_content: str, page_title: str = "") -> str:
    """Convert Orcina WebHelp HTML to clean LLM-friendly markdown."""
    soup = BeautifulSoup(html_content, "html.parser")

    # Extract the title from <h1> or <title>
    title = page_title
    h1 = soup.find("h1")
    if h1:
        title = h1.get_text(strip=True)
    elif not title:
        title_tag = soup.find("title")
        if title_tag:
            title = title_tag.get_text(strip=True)

    # Extract the body content
    body = soup.find("body")
    if not body:
        return f"# {title}\n\n*Content could not be extracted.*\n"

    # Remove navigation, header images, scripts, and style elements
    for tag in body.find_all(["script", "style", "img", "iframe"]):
        tag.decompose()

    # Remove the header div (logo + h1)
    header_div = body.find("div", class_="Header")
    if header_div:
        header_div.decompose()

    # Remove MadCap-specific elements
    for tag in body.find_all(attrs={"MadCap:conditions": True}):
        tag.decompose()

    # Convert remaining HTML to markdown
    content = md(str(body), heading_style="ATX", strip=["img"])

    # Clean up the markdown
    # Remove excessive blank lines
    content = re.sub(r"\n{4,}", "\n\n\n", content)
    # Remove trailing whitespace
    content = "\n".join(line.rstrip() for line in content.split("\n"))
    # Remove leading/trailing whitespace
    content = content.strip()

    # Build final document
    result = f"# {title}\n\n"
    result += f"> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)\n\n"
    result += content
    result += "\n"

    return result


def fetch_page(base_url: str, page_path: str) -> str | None:
    """Fetch a page from Orcina WebHelp. Returns HTML content or None."""
    url = urljoin(base_url, page_path)
    try:
        resp = requests.get(url, timeout=30, allow_redirects=True)
        # Check if we got redirected to the frameset (means page doesn't exist)
        if "gSubsystemFile" in resp.text and "gDefaultStartTopic" in resp.text:
            if '<body>' not in resp.text or resp.text.count('<body>') == 0:
                return None
            # Frameset page, not actual content
            if '<frameset' in resp.text:
                return None
        if resp.status_code == 200 and "<body>" in resp.text:
            return resp.text
    except requests.RequestException as e:
        print(f"  Error fetching {url}: {e}")
    return None


def fetch_from_local(local_dir: str, page_path: str) -> str | None:
    """Try to load page from locally downloaded HTML files."""
    # Convert page path to local filename
    fname = page_path.replace("Content/html/", "").replace(".htm", "").replace(",", "_")
    local_path = os.path.join(local_dir, f"{fname}.htm")
    if os.path.exists(local_path):
        with open(local_path, "r", encoding="utf-8-sig") as f:
            return f.read()
    return None


def process_product(
    product: str,
    output_dir: str,
    local_html_dir: str | None = None,
    fetch_online: bool = True,
) -> list[dict]:
    """Process all priority pages for a product and write markdown files."""
    base_url = BASE_URLS[product]
    pages = PRIORITY_PAGES[product]
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    results = []
    for page_path, filename, description in pages:
        print(f"  Processing: {filename} ({description})")

        html = None

        # Try local first
        if local_html_dir:
            subdir = ""
            if product == "orcawave":
                subdir = "orcawave/"
            elif product == "orcfxapi":
                subdir = "orcfxapi/"
            html = fetch_from_local(os.path.join(local_html_dir, subdir), page_path)
            if html:
                print(f"    -> Loaded from local cache")

        # Fall back to online fetch
        if not html and fetch_online:
            html = fetch_page(base_url, page_path)
            if html:
                print(f"    -> Fetched from web")
            time.sleep(0.5)  # Rate limiting

        if html:
            markdown = html_to_markdown(html, description)
            out_file = output_path / f"{filename}.md"
            with open(out_file, "w", encoding="utf-8") as f:
                f.write(markdown)
            results.append({"file": filename, "description": description, "status": "ok"})
            print(f"    -> Wrote {out_file}")
        else:
            results.append({"file": filename, "description": description, "status": "missing"})
            print(f"    -> SKIPPED (not available)")

    return results


def write_index(product: str, output_dir: str, results: list[dict]):
    """Write an INDEX.md file listing all converted topics."""
    product_names = {
        "orcaflex": "OrcaFlex",
        "orcawave": "OrcaWave",
        "orcfxapi": "OrcFxAPI",
    }
    output_path = Path(output_dir)
    index_file = output_path / "INDEX.md"

    lines = [
        f"# {product_names[product]} Reference Documentation\n",
        f"> Converted from Orcina WebHelp: {BASE_URLS[product]}\n",
        "",
    ]

    ok_results = [r for r in results if r["status"] == "ok"]
    missing = [r for r in results if r["status"] == "missing"]

    if ok_results:
        lines.append("## Available Topics\n")
        lines.append("| File | Description |")
        lines.append("|------|-------------|")
        for r in ok_results:
            lines.append(f"| [{r['file']}.md]({r['file']}.md) | {r['description']} |")
        lines.append("")

    if missing:
        lines.append("## Not Yet Available\n")
        lines.append("These pages could not be fetched. Run the script again or download manually:\n")
        for r in missing:
            lines.append(f"- {r['description']} (`{r['file']}`)")
        lines.append("")

    with open(index_file, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))
    print(f"  -> Wrote index: {index_file}")


def main():
    parser = argparse.ArgumentParser(description="Ingest Orcina help docs to markdown")
    parser.add_argument("--product", choices=["orcaflex", "orcawave", "orcfxapi"],
                        help="Which product to process")
    parser.add_argument("--output", type=str, help="Output directory for markdown files")
    parser.add_argument("--all", action="store_true", help="Process all products")
    parser.add_argument("--local-html", type=str, default="/tmp/orcina_html",
                        help="Directory with pre-downloaded HTML files")
    parser.add_argument("--no-fetch", action="store_true",
                        help="Skip online fetching, use only local files")
    args = parser.parse_args()

    if args.all:
        products = [
            ("orcaflex", "docs/domains/orcaflex/reference/"),
            ("orcawave", "docs/domains/orcawave/reference/"),
            ("orcfxapi", "docs/domains/orcfxapi/reference/"),
        ]
    elif args.product and args.output:
        products = [(args.product, args.output)]
    else:
        parser.error("Specify --product and --output, or use --all")
        return

    for product, output_rel in products:
        output_dir = str(REPO_ROOT / output_rel)
        print(f"\n{'='*60}")
        print(f"Processing {product.upper()} -> {output_dir}")
        print(f"{'='*60}")

        results = process_product(
            product,
            output_dir,
            local_html_dir=args.local_html,
            fetch_online=not args.no_fetch,
        )
        write_index(product, output_dir, results)

    print("\nDone!")


if __name__ == "__main__":
    main()
