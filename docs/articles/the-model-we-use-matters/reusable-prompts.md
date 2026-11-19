# Reusable prompts

Article: "The model we use matters", The Deckhand Chronicles, revision 5 of 19 Nov 2026. This page holds in full what Appendix B of the article summarises.

These prompts recompute the article's worked example and turn your own problems into a repeatable check of each model and provider.

## What you need to run this

- Data required: the public article's illustrative rectangular tank, length 1.0 m, fill depths 0.15 m and 0.70 m, and gravity 9.81 m/s^2.
- Data required to adapt: your own problem set, one problem per line, each with its known answer or the check you will apply; inputs, units, assumptions, acceptance criteria and permission to use the data.
- Paid or behind an account: nothing for the arithmetic; Python is free. Testing a model may need the provider's account and paid usage. The textbook in References may need purchase or library access.
- Changes over time: model releases, provider settings, prices and available usage change; record the model, provider, date, prompt and answers. The illustrative tank inputs and equations below are fixed.

## Reproduce the published result

Give this prompt to an assistant that can run Python, or run the script yourself:

```text
Recompute the worked example in AceEngineer's public article:
https://www.aceengineer.com/newsletter/topics/why-the-model-you-use-matters/?utm_source=linkedin&utm_medium=article&utm_campaign=model&utm_content=model
Use the inputs and equations written below if the page has changed.
1. Install Python 3 from https://www.python.org/downloads/ if needed.
   No additional packages, clone or repository code are required.
2. Create a file named sloshing_check.py with this Python code:

from math import pi, sqrt, tanh
L = 1.0  # m, tank length in the sloshing direction
g = 9.81  # m/s^2
k = pi / L  # wave number, per m
for h in (0.15, 0.70):  # m, fill depth
    kh = k * h
    depth_factor = tanh(kh)
    omega_full = sqrt(g * k * depth_factor)  # rad/s
    omega_deep = sqrt(g * k)  # rad/s
    f_full = omega_full / (2 * pi)  # Hz
    f_deep = omega_deep / (2 * pi)  # Hz
    error_pct = (f_deep / f_full - 1) * 100
    print(f"h={h:.2f} m, k={k:.9f} per m, kh={kh:.9f}, "
          f"tanh(kh)={depth_factor:.9f}, "
          f"omega_full={omega_full:.9f} rad/s, "
          f"omega_deep={omega_deep:.9f} rad/s, "
          f"f_full={f_full:.9f} Hz, f_deep={f_deep:.9f} Hz, "
          f"shortcut_error={error_pct:.9f} %")

3. Run: python sloshing_check.py
4. Use full precision in all calculations. Round only the displayed results.
   State the assumptions: small motions, linear potential-flow theory, the
   first mode of a two-dimensional rectangular tank, and a deep-liquid
   approximation that replaces tanh(kh) with 1. Define each symbol and unit.
5. Compare the shortcut with the full answer, using the full answer as the
   percentage denominator. Explain why the shallow fill exposes the error.
6. Label this an illustrative arithmetic check, not a measured model test.
   Retain the script, console output, inputs and comparison.
Check: at L=1.0 m and h=0.15 m, f_full rounds to 0.586 Hz and
f_deep to 0.884 Hz, with the shortcut about 51 % high; k rounds to
3.14 per m, kh to 0.471, tanh(kh) to 0.439, omega_full to 3.68 rad/s
and omega_deep to 5.55 rad/s. At h=0.70 m the shortcut is about 1 %
high, using the full answer as the denominator.
```

## Adapt it to your project

```text
Before you start, ask me for: my engineering problem set, one problem per
line; each known answer or planned check; input data, units and sources;
assumptions; acceptance criteria; permission to use the data; the models
and providers to compare; available usage and budget; and the date.

I will send this prompt, unchanged, to each model under test myself:
"Here is my problem set, one problem per line, each with its known answer
or the check I will apply. Solve each problem and state every assumption
you make. Mark each problem you cannot solve, or solve only by a
simplification, and name the simplification."

Then I give you all the answers. Keep the same prompt and set for every
model and provider. Compare each answer with its known answer or planned check. Record the model, provider,
date, settings, tools, prompt, answer, assumptions and checking result.
Check the inputs and units. Include a case where a tempting shortcut
breaks, such as the article's shallow fill, alongside one where it agrees.
Keep unsolved problems for the next model and raise complexity step by
step as models solve more. Where useful, split a problem into smaller
steps, each with a known answer. Report which checks still need a hand check.
Remind me to send only problems I may share with that provider, and for
confidential work to use an account where training on my data is off.
Where a step needs a paid standard or book I do not hold, cite it and
mark it "to be checked".
```

Public data set: AceEngineer publishes the illustrative tank inputs and the two schematics on the article page, 4 Nov 2026. These are illustrations rather than measured data; no CSV is required. The page states no separate data licence. The adapted data set is your own problem set.
