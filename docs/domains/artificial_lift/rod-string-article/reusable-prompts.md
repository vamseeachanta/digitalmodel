# Reusable prompts: rod string article

Rod string article ("Rod string mechanics: what stretch, load and damping do to the dynamometer card"), revision 2, 19 Nov 2026. This page holds in full what Appendix D of the article summarises.

This prompt lets your team rebuild the article's rod string results from public code and data, check them against the published values, and then run the same steps on a well of your own: the stretch of each taper, the stress at the top of each taper and its Goodman loading, the effect of damping and the sampling limit. To have a study of your own rod string done for you instead, see below.

## What you need to run this

- Data required: none beyond the public repository; the example well is in it.
- Data required to adapt: one surface card (position and load, evenly spaced in time over one stroke), the pumping speed, the rod taper (diameter, length, modulus, weight per foot and grade of each section), the pump diameter and depth, the fluid density; for damping, the fluid viscosity and the tubing inside diameter.
- Paid or behind an account: nothing.
- Changes over time: the library changes; use the pinned commit below to reproduce the published values.

## Reproduce the published result

Give this prompt to your assistant:

```text
Clone https://github.com/vamseeachanta/digitalmodel and check out commit <the commit that holds this page>.
Install the package with its test extra, then run from the repository root:
  PYTHONPATH=src python examples/domains/artificial_lift/build_rod_string_article.py --refresh --check
The command reruns the analysis and reports any page that differs from the committed one.
Check these values in docs/domains/artificial_lift/rod-string-article/results.json:
  pump stroke 39.57 in; static stretch 12.91 in (89.5 % of the stroke lost);
  peak stress at the top of the 3/4 in rods 21,570 psi; at the polished rod 20,360 psi;
  Goodman loading at the top of the 3/4 in rods 46.0 %;
  damping share 0.7 %; sampling limit 6,380 samples a stroke.
Then work the static stretch, the polished-rod stress and the Goodman loading by hand from the inputs and compare.
```

## Adapt it to your project

- Write your well in the layout of the example file and load it with `load_from_json_file`.
- Change the rod taper, the pumping speed, the pump and the fluid density to your well's values; replace the assumed viscosity, tubing inside diameter and coupling size where you know them.
- Checks that still apply: the static stretch by hand against the stroke loss; the pump card area with no damping against the surface card area; the card's samples a stroke against the sampling limit; the stress at the top of each taper against the Goodman allowable for your rod grade and service factor.

Public data set: the example well, file [cleansed_well_001.json](https://github.com/vamseeachanta/digitalmodel/blob/a26efaa413c962105cdd49ce6b5122b5d0ab0b58/tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json) of the digitalmodel library, MIT licence, version of 19 Nov 2026.

To have a study of your own rod string done for you, write to support@aceengineer.com.
