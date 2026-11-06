# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Negative probes for the FE10 deck checker: tampered copies (in a scratch folder) must FAIL.

Usage: python -I negative_probes.py <package dir> <scratch dir>   (the scratch dir is replaced)
"""
import hashlib
import shutil
import subprocess
import sys
from pathlib import Path

PKG, W = Path(sys.argv[1]).resolve(), Path(sys.argv[2]).resolve()
if PKG in W.parents or W == PKG:
    raise SystemExit("scratch dir must lie outside the package")
if W.exists():
    shutil.rmtree(W)
W.mkdir(parents=True)
src = PKG / "inputs" / "le1_16x48.bdf"
psrc = PKG / "inputs" / "patch.bdf"
before = [hashlib.sha256(p.read_bytes()).hexdigest() for p in (src, psrc)]
t = src.read_text(encoding="ascii")
p = psrc.read_text(encoding="ascii")
first_force = next(l for l in t.splitlines() if l.startswith("FORCE,"))
first_quad = next(l for l in t.splitlines() if l.startswith("CQUAD4,"))
q = first_quad.split(",")
flipped = ",".join(q[:3] + [q[3], q[6], q[5], q[4]])
cases = [
    ("le1", "force removed", t.replace(first_force + "\n", "", 1)),
    ("le1", "modulus changed", t.replace("MAT1,1,210000.0,", "MAT1,1,200000.0,")),
    ("le1", "element reversed", t.replace(first_quad + "\n", flipped + "\n", 1)),
    ("le1", "symmetry dropped", t.replace("SPC1,1,2,", "SPC1,1,3,", 1)),
    ("patch", "boundary value changed", p.replace("SPC,1,3,1,0.0025", "SPC,1,3,1,0.0026")),
]
ok = True
for kind, name, txt in cases:
    if txt in (t, p):
        print("probe %s did not change the deck" % name)
        ok = False
        continue
    (W / "probe.bdf").write_text(txt, encoding="ascii", newline="\n")
    r = subprocess.run([sys.executable, "-I", str(PKG / "tools" / "check_bdf.py"), str(W / "probe.bdf"), "--kind", kind], capture_output=True, text=True)
    fails = [ln.split("  ")[0].strip() for ln in r.stdout.splitlines() if " FAIL " in ln]
    print("deck probe %-24s exit %d; failing checks %s" % (name, r.returncode, fails))
    ok &= r.returncode == 1
after = [hashlib.sha256(x.read_bytes()).hexdigest() for x in (src, psrc)]
print("package decks unchanged by the probes: %s" % (before == after))
print("NEGATIVE PROBES %s" % ("PASS: every tampered copy failed" if ok and before == after else "FAIL"))
sys.exit(0 if ok and before == after else 1)
