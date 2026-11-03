# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand-calculated geometry invariants for issue #1196.

Numerical basis: synthetic bands A=[1, 1.5], B=[1.5, 2.5], C=[2, 3] m,
a 2 m fender face, and a 0.5 m search step. These are constructed test
dimensions, not measured geometry or engineering design recommendations.

Independent interval-intersection table (all entries in metres):

    Centre  Fender interval  Intersection A  Intersection B  Intersection C
    1.0     [0.0, 2.0]       [1.0, 1.5]     [1.5, 2.0]     {2.0}
    1.5     [0.5, 2.5]       [1.0, 1.5]     [1.5, 2.5]     [2.0, 2.5]
    2.0     [1.0, 3.0]       [1.0, 1.5]     [1.5, 2.5]     [2.0, 3.0]
    2.5     [1.5, 3.5]       {1.5}           [1.5, 2.5]     [2.0, 3.0]
    3.0     [2.0, 4.0]       empty           [2.0, 2.5]     [2.0, 3.0]

The literal lengths below transcribe that table; a singleton has zero length.
Their sums are 1, 2, 2.5, 2, 1.5 m, so 2 m is the unique maximizing centre.
There the mean fraction is (0.5 + 1 + 1) / (3 * 2) = 5/12 and the worst
fraction is 0.5/2 = 1/4, uniquely for A. Translating by -0.5 or +1.5 m
(respectively -1 or +3 search steps) preserves every intersection length.
All coordinates use binary-exact halves, allowing exact equality assertions.
"""

from itertools import permutations

import pytest

from digitalmodel.tug.fendering import AssistedVessel, optimize_fender_height


# Source for every dimension and expected value: the hand calculation above.
CONTACT_BANDS = (
    AssistedVessel("band-a", 1.0, 1.5),
    AssistedVessel("band-b", 1.5, 2.5),
    AssistedVessel("band-c", 2.0, 3.0),
)
FACE_HEIGHT_M = 2.0
SEARCH_STEP_M = 0.5
INTERSECTION_LENGTHS_M = {
    1.0: (0.5, 0.5, 0.0),
    1.5: (0.5, 1.0, 0.5),
    2.0: (0.5, 1.0, 1.0),
    2.5: (0.0, 1.0, 1.0),
    3.0: (0.0, 0.5, 1.0),
}
EXPECTED_CENTRE_M = 2.0


def _assert_hand_calculated_optimum(result, translation_m=0.0):
    """Check the independently tabulated unique maximum and its fractions."""
    lengths = INTERSECTION_LENGTHS_M[EXPECTED_CENTRE_M]
    assert all(
        sum(other_lengths) < sum(lengths)
        for centre, other_lengths in INTERSECTION_LENGTHS_M.items()
        if centre != EXPECTED_CENTRE_M
    )
    assert result.fender_height_m == EXPECTED_CENTRE_M + translation_m
    assert result.fender_face_height_m == FACE_HEIGHT_M
    assert result.mean_overlap_fraction == sum(lengths) / (
        len(CONTACT_BANDS) * FACE_HEIGHT_M
    )
    assert result.worst_overlap_fraction == min(lengths) / FACE_HEIGHT_M
    assert result.worst_vessel == "band-a"
    assert result.all_vessels_contact is True


@pytest.mark.parametrize("translation_m", [-0.5, 1.5])
def test_fender_height_translates_with_contact_bands(translation_m):
    """Whole-step translations move the optimum and preserve overlap fractions."""
    original = optimize_fender_height(CONTACT_BANDS, FACE_HEIGHT_M, SEARCH_STEP_M)
    translated_bands = [
        AssistedVessel(
            band.name,
            band.contact_low_m + translation_m,
            band.contact_high_m + translation_m,
        )
        for band in CONTACT_BANDS
    ]
    translated = optimize_fender_height(
        translated_bands, FACE_HEIGHT_M, SEARCH_STEP_M
    )

    _assert_hand_calculated_optimum(original)
    _assert_hand_calculated_optimum(translated, translation_m)
    assert translated.fender_height_m == original.fender_height_m + translation_m
    assert translated.mean_overlap_fraction == original.mean_overlap_fraction
    assert translated.worst_overlap_fraction == original.worst_overlap_fraction


@pytest.mark.parametrize(
    "contact_bands",
    list(permutations(CONTACT_BANDS)),
    ids=lambda bands: "_".join(band.name for band in bands),
)
def test_fender_objective_is_independent_of_input_order(contact_bands):
    """Every ordering retains the unique optimum and hand-calculated objective."""
    result = optimize_fender_height(contact_bands, FACE_HEIGHT_M, SEARCH_STEP_M)

    _assert_hand_calculated_optimum(result)
