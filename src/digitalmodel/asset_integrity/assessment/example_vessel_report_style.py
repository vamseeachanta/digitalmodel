"""Restrained screen and print presentation for a controlled engineering example."""

STYLE = '''
:root{--navy:#102f48;--ink:#203549;--muted:#586d7e;--rule:#d8e2eb;--teal:#076a73}
*{box-sizing:border-box}html{scroll-behavior:smooth;scroll-padding-top:24px}
body{margin:0;background:#eef3f7;color:var(--ink);font:15px/1.65 system-ui,-apple-system,Segoe UI,sans-serif}
main{max-width:1200px;margin:auto;padding:36px 28px 64px}
.cover{background:var(--navy);color:#fff;padding:42px 44px;border-radius:6px;border-top:6px solid #278a94}
.brand{font-size:18px;letter-spacing:.02em;font-weight:700}.eyebrow{font-size:11px;letter-spacing:.15em;text-transform:uppercase;margin:26px 0 10px;color:#bcd5df}
h1{font-size:38px;line-height:1.15;font-weight:650;max-width:850px;margin:0 0 20px}
.subtitle{font-size:18px;max-width:820px;color:#dce9ef}.cover-meta{font-size:12px;border-top:1px solid #527087;margin-top:26px;padding-top:18px}
section,.frontmatter{background:#fff;border:1px solid var(--rule);border-radius:5px;padding:30px 34px;margin-top:22px}
h2{font-size:25px;line-height:1.3;color:var(--navy);margin:0 0 22px;padding-bottom:12px;border-bottom:2px solid #dce7ee}
h3{font-size:18px;line-height:1.4;color:var(--navy);margin:28px 0 12px}h4{font-size:15px;margin:18px 0 8px}
p{margin:12px 0;max-width:100%}a{color:#00638b;text-underline-offset:3px}a:hover{color:var(--navy)}
.notice{border-left:4px solid #b88025;background:#fff7e8;padding:15px 20px;margin:20px 0}
.finding{border-left:4px solid var(--teal);background:#edf7f7;padding:16px 20px;margin:18px 0}
nav ol{columns:2;column-gap:40px;list-style:none;padding:0;margin:0}nav li{break-inside:avoid;margin:8px 0}
nav a{text-decoration:none;font-size:14px}.table-wrap{overflow-x:auto;margin-top:14px}
table{border-collapse:collapse;width:100%;font-size:13px;line-height:1.5}td,th{text-align:left;vertical-align:top;padding:10px 12px;border-bottom:1px solid var(--rule)}
th{background:#edf3f7;font-weight:650;color:var(--navy)}tbody tr:nth-child(even){background:#fafcfd}
.caption,figcaption{font-size:12px;line-height:1.5;color:var(--muted);margin:9px 0 22px}
.two-column{display:grid;grid-template-columns:1fr 1fr;gap:26px}.two-column h3{margin-top:12px}
figure{margin:22px 0 30px;break-inside:avoid}figure img{display:block;width:100%;height:auto;border:1px solid #e4eaf0;background:white}
.native-figure{background:#f7f9fb;border:1px solid var(--rule);padding:14px}.native-figure figcaption{margin-bottom:0}
svg{width:100%;height:auto;display:block}svg:not(.grid) rect{fill:#e7f0f5;stroke:#315c76}svg:not(.grid) path{fill:none;stroke:#315c76}
svg text{font:12px system-ui;fill:#183044}.grid rect{stroke:none}.case-block{border-top:1px solid var(--rule);padding-top:10px;margin-top:28px}
.case-block h3{margin-top:10px}.case-note{font-size:13px;color:var(--muted)}.status{font-weight:650}
details{border-top:1px solid var(--rule);padding:14px 0}summary{cursor:pointer;font-weight:650;color:var(--navy)}
.hash{font:11px/1.6 ui-monospace,Consolas,monospace;overflow-wrap:anywhere}#appendix-b td:last-child{overflow-wrap:anywhere}.source-list{padding-left:22px}
.source-list li{margin:9px 0}.small{font-size:12px;color:var(--muted)}footer{padding:25px 4px;color:var(--muted);font-size:12px}
.doc-control td:first-child{width:26%;font-weight:600}.no-image{border:1px dashed #9aaebb;padding:20px;color:var(--muted)}
@media(max-width:720px){main{padding:14px 10px 30px}.cover{padding:28px 24px}h1{font-size:29px}section,.frontmatter{padding:22px 18px}
nav ol{columns:1}.two-column{grid-template-columns:1fr;gap:8px}td,th{padding:8px;font-size:12px}}
@page{size:A4;margin:17mm 15mm 19mm}
@media print{body{background:#fff;font-size:10pt;color:#152e40}main{max-width:none;padding:0}.cover{border-radius:0;min-height:190mm}
section,.frontmatter{border:0;border-radius:0;padding:0;margin:18pt 0;break-before:page}.cover,.notice,.finding,th{-webkit-print-color-adjust:exact;print-color-adjust:exact}
h1{font-size:30pt}h2{font-size:18pt}h3{font-size:13pt;break-after:avoid}p{orphans:3;widows:3}table{font-size:9pt}thead{display:table-header-group}
tr,figure,.case-block h3{break-inside:avoid}.table-wrap{overflow:visible}table{table-layout:fixed}td,th{overflow-wrap:anywhere;padding:6px}.two-column{grid-template-columns:1fr}figure img{max-height:190mm;object-fit:contain}
details{display:block}details>summary{display:block;margin:12px 0}details>*{display:block!important}a{color:inherit}nav ol{columns:2}footer{border-top:1px solid #bccbd4}}
'''

PRINT_SCRIPT = '''<script>
let closedForPrint=[];
window.addEventListener('beforeprint',()=>{
  closedForPrint=[...document.querySelectorAll('details:not([open])')];
  closedForPrint.forEach(item=>item.open=true);
});
window.addEventListener('afterprint',()=>{
  closedForPrint.forEach(item=>item.open=false);
  closedForPrint=[];
});
</script>'''
