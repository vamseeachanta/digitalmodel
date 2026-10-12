"""Two defects the code-stage review found in the deck basis reader.

Both are silent wrong answers on decks already committed to this repository,
which is the failure class the module exists to remove.

**A multi-structure deck was assembled from more than one body.** Mass came from
the first ``MATE`` card, inertia and the mass node from the *last* ``PMAS``
card, and the coordinate lookup matched the node column without checking the
structure column. On the FPSO-and-turret deck that returned the hull's mass
beside the turret's inertia, giving radii of gyration of 1.22 m for a 45,225 t
vessel, with an empty warning list.

**The number regex crossed fixed-column boundaries.** AQWA records are column
positional. When a value fills its ten-column field the fields abut, and a
free-form float regex fuses them: a node number and the first inertia term come
back as one number.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_deck_basis import (
    AmbiguousDeck,
    read_aqwa_basis,
    structures_in_deck,
)

REPO = Path(__file__).resolve().parents[3]
SHIP = (REPO / "docs" / "domains" / "aqwa" / "examples" / "03_dat"
        / "001_ship_raos" / "001_ship_raos.dat")
FPSO_DIR = (REPO / "docs" / "domains" / "aqwa" / "examples" / "03_dat"
            / "003_FPSO_Turret")

ship_only = pytest.mark.skipif(not SHIP.exists(), reason="ship deck absent")


def _fpso_decks():
    if not FPSO_DIR.is_dir():
        return []
    return sorted(p for p in FPSO_DIR.iterdir() if p.suffix.lower() == ".dat")


fpso_only = pytest.mark.skipif(not _fpso_decks(), reason="FPSO deck absent")


class TestMultiStructureDecks:
    @fpso_only
    @pytest.mark.parametrize("deck", _fpso_decks(), ids=lambda p: p.name)
    def test_more_than_one_structure_is_refused_rather_than_mixed(self, deck):
        """Silence is the defect. A mixed basis must not be returned."""
        with pytest.raises(AmbiguousDeck) as exc:
            read_aqwa_basis(deck)
        message = str(exc.value)
        assert "structure" in message.lower()
        # The message has to be actionable: it names what to pass.
        assert "structure=" in message

    @fpso_only
    @pytest.mark.parametrize("deck", _fpso_decks(), ids=lambda p: p.name)
    def test_structures_can_be_enumerated_before_choosing(self, deck):
        found = structures_in_deck(deck)
        assert len(found) >= 2
        assert all(isinstance(s, int) for s in found)
        assert found == tuple(sorted(found))

    @fpso_only
    @pytest.mark.parametrize("deck", _fpso_decks(), ids=lambda p: p.name)
    def test_each_structure_reads_as_a_self_consistent_body(self, deck):
        """Mass, inertia and node must come from the same structure.

        The defect produced radii of gyration of about 1.2 m for a 45,000 t
        body by pairing one structure's mass with another's inertia. A radius
        of gyration far below a metre on a vessel-scale mass is the signature,
        so it is asserted against directly.
        """
        for s in structures_in_deck(deck):
            b = read_aqwa_basis(deck, structure=s)
            if b.mass is None or b.inertia is None:
                continue
            radii = b.radii_of_gyration
            assert radii is not None
            if b.mass > 1.0e6:
                assert min(radii) > 1.0, (
                    f"structure {s}: mass {b.mass:,.0f} kg with radii {radii}, "
                    "which is the signature of inertia taken from another body"
                )

    @ship_only
    def test_a_single_structure_deck_still_reads_without_an_argument(self):
        b = read_aqwa_basis(SHIP)
        assert b.mass == pytest.approx(45184268.0)
        assert b.structure == 1

    @ship_only
    def test_asking_for_a_structure_the_deck_does_not_have_is_an_error(self):
        with pytest.raises(AmbiguousDeck):
            read_aqwa_basis(SHIP, structure=7)


def _pmas(structure, node, values):
    return (f"{structure:>6d}PMAS{node:>10d}"
            + "".join(f"{v:10.3e}" for v in values))


def _two_body_deck(tmp_path, b44_one, b44_two, stray_fidp=False):
    """Two structures, each with its own Deck 7 block and its own B44.

    FIDP cards are written the way ``fidp_row_card`` writes them -- structure
    field blank -- because AQWA takes the structure from the enclosing block
    header (``WFS1``, ``WFS2``), not from the record.
    """
    from digitalmodel.hydrodynamics.diffraction.roll_damping import fidp_row_card

    def fidp(b44):
        return fidp_row_card(4, [0.0, 0.0, 0.0, b44, 0.0, 0.0])

    lines = [
        "* Hydrodynamic Solver Unit System : Metric: kg, m [N]",
        "JOB AQWA  LINE",
        "OPTIONS REST END",
        "          COOR",
        # Structure in columns 1-6, node in 7-11, coordinates from column 21.
        f"{1:>6d}{98000:>5d}" + " " * 9 + f"{100.0:>10.4f}{0.0:>10.4f}{5.0:>10.4f}",
        f"{2:>6d}{98001:>5d}" + " " * 9 + f"{20.0:>10.4f}{0.0:>10.4f}{2.0:>10.4f}",
        " END",
        "          ELM1",
        "     1PMAS        18(1)(98000)(98000)(98000)",
        " END",
        "          ELM2",
        "     2PMAS        18(1)(98001)(98001)(98001)",
        " END",
        "          MATE",
        "     1         98000  45000000",
        "     2         98001   1000000",
        " END",
        "          GEOM",
        _pmas(1, 98000, (8.0e9, 0.0, 0.0, 1.1e11, 0.0, 1.2e11)),
        _pmas(2, 98001, (4.0e7, 0.0, 0.0, 5.0e8, 0.0, 6.0e8)),
    ]
    if stray_fidp:
        # A blank-owner card in a block that names no structure.
        lines.append(fidp(9.999e3))
    lines += [
        " END",
        "          WFS1",
        fidp(b44_one),
        " END",
        "          WFS2",
        fidp(b44_two),
        " END",
    ]
    p = tmp_path / "two_body.dat"
    p.write_text("\n".join(lines) + "\n", encoding="ascii")
    return p


class TestPerStructureBlocks:
    """A blank structure field means the block's structure, not the caller's.

    Review found ``owner in (None, structure)`` credited every blank-owner record
    to whichever body was asked for, so in a two-body deck with B44 1,234 in
    body 1's block and 5,678 in body 2's, body 1 read 5,678.
    """

    def test_each_body_reads_its_own_damping(self, tmp_path):
        deck = _two_body_deck(tmp_path, 1.234e3, 5.678e3)
        one = read_aqwa_basis(deck, structure=1)
        two = read_aqwa_basis(deck, structure=2)
        assert one.additional_damping[3][3] == pytest.approx(1.234e3, rel=1e-12)
        assert two.additional_damping[3][3] == pytest.approx(5.678e3, rel=1e-12)

    def test_each_body_reads_its_own_inertia(self, tmp_path):
        deck = _two_body_deck(tmp_path, 1.234e3, 5.678e3)
        assert read_aqwa_basis(deck, structure=1).inertia[3] == pytest.approx(1.1e11)
        assert read_aqwa_basis(deck, structure=2).inertia[3] == pytest.approx(5.0e8)

    def test_a_titled_block_header_still_assigns_its_structure(self, tmp_path):
        """Real decks title their blocks: ``          ELM1      ShipHull``."""
        deck = _two_body_deck(tmp_path, 1.234e3, 5.678e3)
        text = deck.read_text(encoding="ascii")
        text = (text.replace("          WFS1\n", "          WFS1      Hull\n")
                    .replace("          WFS2\n", "          WFS2      Turret\n"))
        deck.write_text(text, encoding="ascii")
        assert read_aqwa_basis(deck, structure=1).additional_damping[3][3] \
            == pytest.approx(1.234e3, rel=1e-12)
        assert read_aqwa_basis(deck, structure=2).additional_damping[3][3] \
            == pytest.approx(5.678e3, rel=1e-12)

    def test_a_blank_owner_record_outside_any_numbered_block_is_refused(
        self, tmp_path
    ):
        """With two bodies and nothing to say whose it is, guessing is the defect."""
        deck = _two_body_deck(tmp_path, 1.234e3, 5.678e3, stray_fidp=True)
        with pytest.raises(AmbiguousDeck):
            read_aqwa_basis(deck, structure=1)


class TestFixedColumnFields:
    """Records are read by column, not by the spaces between numbers."""

    def _deck(self, tmp_path, pmas_line):
        text = (
            "* Hydrodynamic Solver Unit System : Metric: kg, m [N]\n"
            "JOB AQWA  LINE\n"
            "OPTIONS REST END\n"
            "          COOR\n"
            "     1     98000              100.0000    0.0000    5.0000\n"
            " END\n"
            "          MATE\n"
            "     1         98000  45000000\n"
            " END\n"
            "          GEOM\n"
            f"{pmas_line}\n"
            " END\n"
        )
        p = tmp_path / "columns.dat"
        p.write_text(text, encoding="ascii")
        return p

    def test_values_that_fill_their_field_are_still_read_separately(
        self, tmp_path
    ):
        """Ten-column fields abut when a value fills one. A real deck does this.

        `F_FST1_L00_DAMPAD.dat` carries `0.000e+00-9.332e+10` on one record, so
        a reader that relies on whitespace loses a field.
        """
        values = (1000000.0, 12345.6789, 98765.4321, 1000000.0, 0.0, 1000000.0)
        fields = "".join(f"{v:10.4f}"[:10] for v in values)
        line = "     1PMAS" + f"{98000:>10d}" + fields
        assert len(line) == 80
        assert " " not in line[20:30] + line[30:40], "fields must abut here"

        basis = read_aqwa_basis(self._deck(tmp_path, line))
        assert basis.inertia is not None
        assert basis.inertia == pytest.approx(values, rel=1e-9)
        assert basis.cog_node == 98000

    def test_the_shipped_layout_is_unchanged(self, tmp_path):
        """The ordinary spaced form must keep reading as it did."""
        line = ("     1PMAS     98000 8.35728e9        0.        0."
                " 1.1296e11        0. 1.2218e11")
        basis = read_aqwa_basis(self._deck(tmp_path, line))
        assert basis.inertia == pytest.approx(
            (8.35728e9, 0.0, 0.0, 1.1296e11, 0.0, 1.2218e11), rel=1e-9)

    def test_a_negative_value_abutting_the_field_before_it(self, tmp_path):
        values = (0.0, 0.0, 0.0, 0.0, -9.332e10, 0.0)
        fields = "".join(f"{v:10.3e}" for v in values)
        line = "     1PMAS" + f"{98000:>10d}" + fields
        assert "0.000e+00-9.332e+10" in line
        basis = read_aqwa_basis(self._deck(tmp_path, line))
        assert basis.inertia == pytest.approx(values, rel=1e-9)
