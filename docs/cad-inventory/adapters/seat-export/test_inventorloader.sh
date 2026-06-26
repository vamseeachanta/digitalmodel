#!/bin/bash
# ============================================================================
#  Evaluate the OSS Inventor reader (FreeCAD InventorLoader) — RUN ON ace-linux-2.
#  This installs an EXTERNAL FreeCAD addon (jmplonka/InventorLoader) into the
#  FreeCAD Mod auto-load dir, then headless-tests reading a sample .ipt.
#  It is provided as a USER-initiated script (an agent was not permitted to
#  auto-install external auto-load code). Review before running.
#
#  Usage:  ./test_inventorloader.sh [/path/to/sample.ipt]
#  If it prints INVENTORLOADER_OK with shapes>0, the OSS path can read that part;
#  then a1 can convert .ipt -> STEP/glTF via FreeCAD headless WITHOUT a seat
#  (parts only; assemblies .iam are typically not supported by the addon).
# ============================================================================
set -e
MOD="$HOME/.local/share/FreeCAD/Mod"
mkdir -p "$MOD"; cd "$MOD"
if [ ! -d InventorLoader ]; then
  echo ">> cloning InventorLoader (external addon)…"
  git clone --depth 1 https://github.com/jmplonka/InventorLoader
fi
SAMPLE="${1:-/tmp/test_part.ipt}"
cat > /tmp/il_test.py <<'PY'
import FreeCAD, sys
p = sys.argv[-1]
try:
    d = FreeCAD.open(p)
    shapes = [o for o in d.Objects if getattr(o, "Shape", None) is not None and o.Shape.Solids]
    nsolid = sum(len(o.Shape.Solids) for o in shapes)
    print("INVENTORLOADER_OK file=%s objects=%d solids=%d" % (p.split('/')[-1], len(d.Objects), nsolid))
except Exception as e:
    print("INVENTORLOADER_FAIL %s: %s" % (type(e).__name__, str(e)[:140]))
PY
echo ">> testing on: $SAMPLE"
freecadcmd /tmp/il_test.py "$SAMPLE" 2>&1 | grep -E "INVENTORLOADER_(OK|FAIL)"
echo ">> If OK: install pattern proven; we can batch .ipt -> STEP via freecadcmd on a2,"
echo ">>        feeding the same dedup/glTF/index pipeline, no Inventor seat for parts."
