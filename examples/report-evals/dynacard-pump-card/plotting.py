"""Render the physical cards and dimensionless reference shapes without inference."""
from pathlib import Path
import textwrap

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import numpy as np

BLUE = '#0072B2'
ORANGE = '#D55E00'
GREEN = '#009E73'
GREY = '#8a8a8a'
INK = '#333333'
MARK = '#1a1a1a'


def _closed(card, shift=0.0):
    x = np.asarray(card['position_in'], dtype=float) - shift
    y = np.asarray(card['load_lbf'], dtype=float)
    return np.r_[x, x[0]], np.r_[y, y[0]]


def _number(value, decimals=0):
    """One number form: thousands separator and the true minus sign."""
    return f'{value:,.{decimals}f}'.replace('-', '−')


def _dimension(ax, start, stop, label, *, offset=(0, 5), va='bottom', ha='center'):
    ax.annotate('', xy=stop, xytext=start,
                arrowprops={'arrowstyle': '<->', 'lw': .8, 'color': INK,
                            'shrinkA': 0, 'shrinkB': 0})
    midpoint = tuple((a + b) / 2 for a, b in zip(start, stop))
    ax.annotate(label, xy=midpoint, xytext=offset, textcoords='offset points',
                ha=ha, va=va, fontsize=9, color=INK)


def _draw_cards(ax, data):
    """Pump cards share one datum: position is measured from the bottom of each stroke."""
    cards = data['cards']
    key = data['key_results']
    shifts = {name: (min(card['position_in']) if name != 'surface' else 0.0)
              for name, card in cards.items() if card is not None}
    for name, color, width, style, order in (
        ('reference', GREY, 3.2, '-', 2), ('surface', BLUE, 2.0, '-', 3),
        ('default', ORANGE, 2.0, '-', 4), ('gibbs', GREEN, 1.2, '--', 5),
    ):
        if cards.get(name) is not None:
            ax.plot(*_closed(cards[name], shifts[name]), color=color, lw=width,
                    ls=style, zorder=order, solid_joinstyle='round',
                    alpha=.55 if name == 'reference' else 1)
    surface_x, surface_y = _closed(cards['surface'])
    pump_x, pump_y = _closed(cards['default'], shifts['default'])
    y_low = min(pump_y.min(), 0.0)
    y_span = surface_y.max() - y_low
    x_right = max(surface_x.max(), pump_x.max())
    ax.set(xlim=(-.07 * x_right, 1.34 * x_right),
           ylim=(y_low - .2 * y_span, surface_y.max() + .12 * y_span),
           xlabel='Position (in), each card from the bottom of its stroke',
           ylabel='Load (lbf)')
    ax.xaxis.set_major_formatter(FuncFormatter(lambda value, _: _number(value)))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda value, _: _number(value)))
    ax.set_yticks([tick for tick in ax.get_yticks() if 0 <= tick <= surface_y.max() + .12 * y_span])
    ax.grid(color='#e3e3e3', lw=.5, zorder=0)
    ax.axhline(0, color='#b0b0b0', lw=.7, zorder=1)
    ax.spines[['top', 'right']].set_visible(False)

    label_x = 1.03 * x_right
    ax.text(label_x, float(np.interp(.5, [0, 1], [surface_y.min(), surface_y.max()])),
            'Surface card\nmeasured at the\npolished rod', color=BLUE, fontsize=10,
            fontweight='bold', va='center', ha='left')
    ax.text(label_x, pump_y.max() + .035 * y_span, 'Pump card\ncomputed by the\nwave equation',
            color=ORANGE, fontsize=10, fontweight='bold', va='bottom', ha='left')
    notes = ['orange: finite-difference solver', 'green dashes: second solver,\nfrequency domain']
    if cards.get('reference') is not None:
        notes.append('grey band: card stored in\nthe well record')
    ax.text(label_x, pump_y.max() + .02 * y_span, '\n'.join(notes), color='#555555',
            fontsize=7.5, va='top', ha='left')

    top = surface_y.max() + .05 * y_span
    _dimension(ax, (surface_x.min(), top), (surface_x.max(), top),
               f"surface stroke {_number(key['surface_stroke_in'], 1)} in")
    for x in (surface_x.min(), surface_x.max()):
        ax.plot([x, x], [surface_y[np.argmin(np.abs(surface_x - x))], top],
                color='#777777', lw=.5, ls=':')
    low = y_low - .13 * y_span
    _dimension(ax, (pump_x.min(), low), (pump_x.max(), low),
               f"pump stroke {_number(key['pump_stroke_in'], 1)} in",
               offset=(0, -5), va='top')
    for x in (pump_x.min(), pump_x.max()):
        ax.plot([x, x], [pump_y[np.argmin(np.abs(pump_x - x))], low],
                color='#777777', lw=.5, ls=':')
    travel = key['effective_travel_in']
    mid = y_low - .055 * y_span
    _dimension(ax, (pump_x.min(), mid), (pump_x.min() + travel, mid),
               f"effective plunger travel {_number(travel, 1)} in: "
               f"fillage {_number(key['fillage_pct'], 0)} %",
               offset=(0, 3))
    levels = data['fluid_load_levels']
    x = pump_x.max() + .035 * x_right
    for level in (levels['downstroke_lbf'], levels['upstroke_lbf']):
        ax.plot([pump_x.max() - .25 * x_right, x + .01 * x_right], [level, level],
                color='#777777', lw=.5, ls=':')
    ax.annotate('', xy=(x, levels['upstroke_lbf']), xytext=(x, levels['downstroke_lbf']),
                arrowprops={'arrowstyle': '<->', 'lw': .8, 'color': INK,
                            'shrinkA': 0, 'shrinkB': 0})
    ax.text(x + .012 * x_right, (levels['upstroke_lbf'] + levels['downstroke_lbf']) / 2,
            f"fluid load\n{_number(key['fluid_load_lbf'])} lbf", fontsize=9, color=INK,
            va='center', ha='left')


def _draw_plate(fig, grid, data):
    ranks = {row['label']: row['rank']
             for row in data['shape_hypothesis']['nearest_three']}
    subgrid = grid.subgridspec(5, 2, wspace=.12, hspace=.62)
    for index, shape in enumerate(data['plate_shapes']):
        ax = fig.add_subplot(subgrid[index // 2, index % 2])
        x = np.asarray(shape['position'], dtype=float)
        y = np.asarray(shape['load'], dtype=float)
        x = (x - x.min()) / np.ptp(x)
        y = (y - y.min()) / np.ptp(y)
        rank = ranks.get(shape['label'])
        ax.plot(np.r_[x, x[0]], np.r_[y, y[0]], color=MARK if rank else '#8a8a8a',
                lw=1.8 if rank else 1.0)
        title = textwrap.fill(shape['label'].replace('Traveling', 'Travelling'), width=24)
        if shape['crosswalk_status'] == 'ASSUMED':
            title += '\n(label mapping assumed)'
        ax.set_title(title, fontsize=7.5, pad=3, color=INK)
        ax.set(xlim=(-.1, 1.1), ylim=(-.12, 1.12), xticks=[], yticks=[])
        ax.set_box_aspect(.55)
        for spine in ax.spines.values():
            spine.set_color(MARK if rank else '#d9d9d9')
            spine.set_linewidth(1.8 if rank else .6)
        if rank:
            ax.text(.04, .93, str(rank), transform=ax.transAxes, va='top',
                    color=MARK, fontsize=11, fontweight='bold')


def render_figure(data: dict, path: Path) -> None:
    """Export a PNG and companion SVG from the same unsimplified card arrays."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    style = {'font.family': 'sans-serif', 'font.size': 9, 'axes.unicode_minus': True,
             'svg.fonttype': 'none', 'figure.facecolor': 'white',
             'axes.facecolor': 'white', 'savefig.facecolor': 'white'}
    with plt.rc_context(style):
        fig = plt.figure(figsize=(12, 6.5))
        grid = fig.add_gridspec(1, 2, width_ratios=(2.55, 1), left=.07,
                               right=.985, bottom=.115, top=.83, wspace=.07)
        try:
            _draw_cards(fig.add_subplot(grid[0]), data)
            _draw_plate(fig, grid[1], data)
            condition = data.get('condition_name', 'reader card')
            fig.suptitle('Surface card and computed pump card, ' + condition,
                         x=.07, y=.965, ha='left', fontsize=14, fontweight='bold')
            fig.text(.07, .915, 'The wave equation removes rod weight, rod stretch and rod '
                     'dynamics from the surface card.\nThe pump card rests on the assumed '
                     'inputs listed with the results.', ha='left', va='top', fontsize=9,
                     color='#555555')
            fig.text(.862, .93, 'Public reference card shapes',
                     ha='center', fontsize=10, color=INK)
            fig.text(.862, .895, '1, 2, 3: nearest by shape distance',
                     ha='center', color=MARK, fontsize=9)
            fig.text(.862, .045, 'Shapes are normalised. Rank is shape distance only;\n'
                     'the fault they suggest is a hypothesis.',
                     ha='center', va='center', fontsize=7.5, color='#555555')
            fig.savefig(path, dpi=200)
            fig.savefig(path.with_suffix('.svg'), metadata={'Date': None})
        finally:
            plt.close(fig)
