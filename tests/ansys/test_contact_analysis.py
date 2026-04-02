# ABOUTME: Tests for ContactAnalysisGenerator — contact pairs, interference, seals
# ABOUTME: Verifies CONTA174/TARGE170 command generation and contact settings

"""Tests for contact_analysis — ContactAnalysisGenerator methods."""

import pytest

from digitalmodel.ansys.contact_analysis import (
    ContactAnalysisGenerator,
    ContactPairConfig,
    InterferenceFitConfig,
    SealConfig,
)


def _gen() -> ContactAnalysisGenerator:
    return ContactAnalysisGenerator()


# ---------------------------------------------------------------------------
# Contact pair generation
# ---------------------------------------------------------------------------

class TestContactPair:
    def test_generates_conta174(self):
        config = ContactPairConfig(pair_id=1)
        text = _gen().generate_contact_pair(config)
        assert "CONTA174" in text

    def test_generates_targe170(self):
        config = ContactPairConfig(pair_id=1)
        text = _gen().generate_contact_pair(config)
        assert "TARGE170" in text

    def test_augmented_lagrangian_keyopt(self):
        config = ContactPairConfig(pair_id=1, algorithm="augmented_lagrangian")
        text = _gen().generate_contact_pair(config)
        # Algorithm keyopt 2 = 1 for augmented Lagrangian
        assert "KEYOPT,10,2,1" in text

    def test_penalty_algorithm(self):
        config = ContactPairConfig(pair_id=1, algorithm="penalty")
        text = _gen().generate_contact_pair(config)
        assert "KEYOPT,10,2,0" in text

    def test_friction_coefficient_set(self):
        config = ContactPairConfig(pair_id=1, friction_coefficient=0.3)
        text = _gen().generate_contact_pair(config)
        assert "MP,MU,1,0.3" in text

    def test_node_to_surface_uses_conta175(self):
        config = ContactPairConfig(pair_id=1, contact_type="node_to_surface")
        text = _gen().generate_contact_pair(config)
        assert "CONTA175" in text

    def test_esurf_commands_generated(self):
        config = ContactPairConfig(pair_id=1)
        text = _gen().generate_contact_pair(config)
        assert text.count("ESURF") == 2  # one for contact, one for target

    def test_allsel_at_end(self):
        config = ContactPairConfig(pair_id=1)
        text = _gen().generate_contact_pair(config)
        assert "ALLSEL" in text


# ---------------------------------------------------------------------------
# Interference fit
# ---------------------------------------------------------------------------

class TestInterferenceFit:
    def test_generates_interference_commands(self):
        config = InterferenceFitConfig(
            pair_id=2, radial_interference_mm=0.05
        )
        text = _gen().generate_interference_fit(config)
        assert "Interference" in text

    def test_includes_solve(self):
        config = InterferenceFitConfig(pair_id=2)
        text = _gen().generate_interference_fit(config)
        assert "SOLVE" in text

    def test_nlgeom_enabled(self):
        config = InterferenceFitConfig(pair_id=2)
        text = _gen().generate_interference_fit(config)
        assert "NLGEOM,ON" in text

    def test_includes_initial_interference_keyopt(self):
        config = InterferenceFitConfig(pair_id=2)
        text = _gen().generate_interference_fit(config)
        # KEYOPT for include initial interference
        assert "KEYOPT," in text


# ---------------------------------------------------------------------------
# Seal contact
# ---------------------------------------------------------------------------

class TestSealContact:
    def test_generates_hyperelastic_material(self):
        seal = SealConfig(material_id=5, shore_hardness=70.0)
        text = _gen().generate_seal_contact(seal)
        assert "TB,HYPER," in text
        assert "MOON" in text

    def test_generates_contact_pair(self):
        seal = SealConfig()
        text = _gen().generate_seal_contact(seal)
        assert "CONTA174" in text
        assert "TARGE170" in text

    def test_includes_seal_type_description(self):
        seal = SealConfig(seal_type="o_ring")
        text = _gen().generate_seal_contact(seal)
        assert "o_ring" in text


# ---------------------------------------------------------------------------
# Contact results extraction
# ---------------------------------------------------------------------------

class TestContactResultsExtraction:
    def test_generates_etable_for_pressure(self):
        text = _gen().generate_contact_results_extraction([1])
        assert "ETABLE,CPRES_1,CONT,PRES" in text

    def test_generates_etable_for_penetration(self):
        text = _gen().generate_contact_results_extraction([1])
        assert "ETABLE,CPEN_1,CONT,PENE" in text

    def test_generates_contact_status(self):
        text = _gen().generate_contact_results_extraction([1])
        assert "ETABLE,CSTAT_1,CONT,STAT" in text

    def test_multiple_pairs(self):
        text = _gen().generate_contact_results_extraction([1, 2, 3])
        assert "CPRES_1" in text
        assert "CPRES_2" in text
        assert "CPRES_3" in text


# ---------------------------------------------------------------------------
# Frictional contact solution
# ---------------------------------------------------------------------------

class TestFrictionalContactSolution:
    def test_generates_solu_block(self):
        config = ContactPairConfig()
        text = _gen().generate_frictional_contact_solution(config)
        assert "/SOLU" in text
        assert "SOLVE" in text

    def test_unsym_for_friction(self):
        config = ContactPairConfig(friction_coefficient=0.3)
        text = _gen().generate_frictional_contact_solution(config)
        assert "NROPT,UNSYM" in text

    def test_no_unsym_for_frictionless(self):
        config = ContactPairConfig(friction_coefficient=0.0)
        text = _gen().generate_frictional_contact_solution(config)
        assert "NROPT,UNSYM" not in text
