# Formatter migration scope

Formatter migrations shall preserve lint behavior for untouched files.
Additional global Ruff rules shall be introduced through a separate reviewed rollout.
Regression checks shall include an unchanged CI lint target: global selectors affect
that target even when its files are outside the change. Owner board O04 exposed this
failure class in the CFD bridge lint job; its 19-file command passes with main's rules.
