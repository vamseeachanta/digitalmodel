"""
Test if Excel uses SAG formulation instead of vertical span.

For a catenary from anchor (0,0) to fairlead (X, Y_fairlead):
- Sag = maximum vertical distance below the straight line connecting endpoints
- This is different from Y_fairlead

If the "vertical span" input is actually the SAG, not the elevation difference,
the problem becomes different.

Actually, re-reading the problem:
- "Horizontal span: 800m"
- "Vertical span: 100m"

Could this mean the fairlead is 100m BELOW the anchor (not above)?
Let's try Y = -100 (fairlead below anchor).
