# Public capability page — layout & brand standard

Canonical standard for the designed pages under `docs/api/**` (GitHub Pages).
Ref: digitalmodel#1471 (epic) · #1473 (this doc). Reference implementation:
[`hydro/short-horizon-motion-forecast.html`](https://vamseeachanta.github.io/digitalmodel/hydro/short-horizon-motion-forecast.html).

> **Publish-path note.** Pages publishes `docs/api/` as the **site root**:
> `docs/api/hydro/x.html` → `/digitalmodel/hydro/x.html`, and
> `docs/api/_assets/brand.css` → `/digitalmodel/_assets/brand.css`. Use paths
> relative to the page's own depth (`../_assets/brand.css`, `../capabilities/`).

---

## 1. Brand tokens — never hard-code

Link the canonical token source; do **not** paste a brand `:root{}` block:

```html
<link rel="stylesheet" href="../_assets/brand.css">
```

Consume the vars (`var(--brand)`, `var(--accent)`, `var(--ink)`, status
`var(--go|--caution|--nogo)`, `var(--wrap)`, …). Only genuinely **page-specific**
tokens (e.g. per-series data hues, a page's own shading) may stay in the page's
inline `:root{}`, and must define their own light / `prefers-color-scheme:dark`
/ `[data-theme]` variants. The CI guard (#1476) fails pages that redefine brand
tokens inline. `--accent` is **interactive chrome only** — pick distinct hues for
data series.

## 2. Container width

One canonical width via the token: `.wrap{ max-width:var(--wrap); margin:0 auto }`
(`--wrap` = 1240px for consoles). Use a narrower reading width for prose-heavy
pages; do not invent new `max-width` px values.

## 3. Logo routes home

Every page's logo / brand kicker must link to the capabilities index using the
shared `brandmark` primitive from `brand.css`:

```html
<a class="brandmark" href="../capabilities/" aria-label="Back to capabilities index">
  <!-- existing logo / kicker markup -->
</a>
```

Adjust `../` for the page's depth.

## 4. Single-screen fit (interactive consoles)

The primary interactive content — controls **and** the payoff (e.g. the go/no-go
strip) — must be visible **without scrolling from 1366×768 up**. Verify with
headless Chrome:

```bash
google-chrome --headless=new --hide-scrollbars --virtual-time-budget=1800 \
  --window-size=1536,864 --screenshot=out.png "file://$PWD/page.html"
# repeat at 1366,768 and 1920,1080
```

### 4a. Measure the budget — don't guess

Size the plot canvases to the **measured** remaining viewport, not fixed px, so
it self-corrects on any screen. Run once after a first `render()` (so canvases
have real heights) and on `resize`:

```js
function sizePanels(){
  const traces=[el("cvA"),el("cvB"),el("cvC")];
  const curTrace=traces.reduce((s,c)=>s+c.getBoundingClientRect().height,0);
  const tp=document.querySelector(".transport");            // last element that must stay on-screen
  if(!curTrace||!tp) return;
  const nonTrace=tp.getBoundingClientRect().bottom - curTrace;  // all chrome above/between/below
  let avail=window.innerHeight - nonTrace - 10;             // 10px breathing room
  avail=Math.max(150, Math.min(avail, 560));
  /* distribute avail across the canvases, set each via setAttribute("height", …) */
}
// init: build(); render(); sizePanels(); render();
// window.addEventListener("resize", ()=>{ sizePanels(); render(); });
```

### 4b. Density on short viewports

Compress chrome so the payoff fits: tight panel padding, right-sized status/hero,
and drop non-essential hero prose under a short-viewport query:

```css
@media (max-height:820px){ .tag{display:none} header.top{margin-bottom:8px} .wrap{padding-top:10px} }
```

### 4c. Shared axis across stacked panels

Stacked time-domain panels must share **one** `t0..t1 → x` mapping so a vertical
scan lines up (a "now" plumb-line hits the same x in every panel). Factor the
axis math into one helper both drawers call; don't let a strip start at a
different origin than the traces above it.

## 5. Accessibility baseline (see #1475)

- Every `<canvas>` plot: `role="img"` + a `render()`-updated `aria-label`.
- Every interactive control has `:focus-visible` (buttons built with `all:unset`
  lose the ring — add it back).
- Range inputs get `aria-label` + spoken units.
- Status must carry a **colour-independent** channel (distinct dashes, labels,
  or a position marker) — never colour alone (WCAG 1.4.1).
- Announce verdict/countdown changes via a debounced `aria-live` region (on
  *change*, not every animation frame). Sensible heading order; footer text ≥ AA.

## 6. Page author checklist

- [ ] `<link>`s `../_assets/brand.css`; no brand hex in the page's `:root{}`.
- [ ] `.wrap` uses `var(--wrap)` (or a justified narrower prose width).
- [ ] Logo wrapped in `<a class="brandmark" href="../capabilities/">`.
- [ ] (console) Fits 1366×768 → 1920×1080, screenshot-verified; canvases measured, not fixed.
- [ ] (console) Stacked panels share one time axis.
- [ ] A11y baseline (§5) satisfied.
