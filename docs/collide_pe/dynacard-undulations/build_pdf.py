# ABOUTME: Renders the Collide data request markdown to a client-facing PDF.
# ABOUTME: Self-contained HTML (no external assets) so headless Chrome prints it faithfully.
import subprocess
import sys
from pathlib import Path

import markdown

SRC = Path("/mnt/local-analysis/digitalmodel/docs/collide_pe/"
           "dynacard-undulations/DATA-REQUEST.md")
OUT = Path("/tmp/claude-1000/-mnt-local-analysis/"
           "bef7bcd3-75db-455f-8193-f247ec2c5754/scratchpad")
HTML = OUT / "data-request.html"
PDF = OUT / "AceEngineer-dynacard-data-request.pdf"

CSS = """
@page { size: Letter; margin: 18mm 16mm 16mm 16mm; }
* { box-sizing: border-box; }
body { font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
       font-size: 10.5pt; line-height: 1.5; color: #1a2332; margin: 0; }
h1 { font-size: 19pt; color: #0b2545; margin: 0 0 2px 0; letter-spacing: -0.3px; }
h2 { font-size: 13pt; color: #0b2545; margin: 20px 0 6px 0;
     padding-bottom: 4px; border-bottom: 2px solid #1d7a8c; page-break-after: avoid; }
h3 { font-size: 11pt; color: #1d7a8c; margin: 14px 0 4px 0; page-break-after: avoid; }
p { margin: 6px 0; }
table { border-collapse: collapse; width: 100%; margin: 8px 0 12px 0;
        font-size: 9.5pt; page-break-inside: avoid; }
th { background: #0b2545; color: #fff; text-align: left; padding: 6px 8px;
     font-weight: 600; }
td { border-bottom: 1px solid #d8dee6; padding: 5px 8px; vertical-align: top; }
tr:nth-child(even) td { background: #f5f8fa; }
code { background: #eef2f6; padding: 1px 4px; border-radius: 3px;
       font-family: "SF Mono", Menlo, Consolas, monospace; font-size: 9pt; }
blockquote { border-left: 3px solid #1d7a8c; background: #f5f8fa;
             margin: 8px 0; padding: 8px 12px; color: #33475b; }
hr { border: 0; border-top: 1px solid #d8dee6; margin: 18px 0; }
ul, ol { margin: 6px 0 6px 20px; padding: 0; }
li { margin: 3px 0; }
strong { color: #0b2545; }
.masthead { border-bottom: 3px solid #1d7a8c; padding-bottom: 10px;
            margin-bottom: 16px; }
.brand { font-size: 9pt; letter-spacing: 2px; text-transform: uppercase;
         color: #1d7a8c; font-weight: 700; }
.sub { color: #5a6b7d; font-size: 9.5pt; margin-top: 2px; }
.footer { margin-top: 24px; padding-top: 10px; border-top: 1px solid #d8dee6;
          font-size: 8.5pt; color: #5a6b7d; }
"""

body = markdown.markdown(
    SRC.read_text(encoding="utf-8"),
    extensions=["tables", "fenced_code", "sane_lists"],
)

# The markdown H1 becomes the document title; wrap it in a masthead instead.
html = f"""<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<title>Dynacard data request</title><style>{CSS}</style></head>
<body>
<div class="masthead">
  <div class="brand">AceEngineer</div>
  <div class="sub">Rod-pump surface-card analysis &middot; prepared for Reed Goodman
  &middot; 27 July 2026</div>
</div>
{body}
<div class="footer">
Prepared by AceEngineer. Analysis code and method are public:
https://github.com/vamseeachanta/digitalmodel &mdash; API RP 11L rod-pump module
and the SPE 18189 surface-to-downhole solver.
</div>
</body></html>"""

HTML.write_text(html, encoding="utf-8")
print(f"html: {HTML} ({len(html)} bytes)")

cmd = [
    "google-chrome", "--headless=new", "--disable-gpu", "--no-sandbox",
    "--password-store=basic",          # else Chrome hangs on the keyring here
    "--no-pdf-header-footer",          # else Chrome stamps a timestamp + file://
    "--virtual-time-budget=12000",
    f"--print-to-pdf={PDF}", str(HTML),
]
res = subprocess.run(cmd, capture_output=True, timeout=180)
print("chrome rc:", res.returncode)
if PDF.exists():
    print(f"pdf: {PDF} ({PDF.stat().st_size:,} bytes)")
else:
    print("PDF NOT CREATED", res.stderr.decode()[:400])
    sys.exit(1)
