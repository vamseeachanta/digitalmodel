"""Offline checks of the research-note transcription and database access."""
from copy import deepcopy
import socket
from dataclasses import FrozenInstanceError
from pathlib import Path

import pytest
import yaml

from digitalmodel.units import Q_, ureg

DATA = Path(__file__).resolve().parents[2] / "data/geotechnical/geotech_database.yaml"
FIELDS = ("row_id", "soil_unit", "parameter", "value_or_range", "units_as_printed",
          "test_or_derivation_method", "site_or_region", "depth_range", "source_id",
          "locator", "licence", "confidence_grade")
# Each line is the complete printed row, in column order, from the research note.
TRANSCRIPTION = """P01	Unit I, weathered clay crust	Total unit weight	17.5	kN/m3	Water content, core logging, direct measurement	Onsøy, Norway	approx. 0-1 m	S01	Sec 4.2	BY	A
P02	Unit II clay (and top of Unit IV)	Total unit weight	16.2	kN/m3	As P01	Onsøy	approx. 1 m to 8 m (south-central) or 10.5 m (south-east corner)	S01	Sec 4.2	BY	A
P03	Unit III clay	Total unit weight	around 17.8	kN/m3	As P01	Onsøy	below Unit II; about 5.5 m thick (south-central), about 9 m (south-east)	S01	Sec 4.2	BY	A
P04	All units	Unit weight of solid particles	27.1 (best estimate)	kN/m3	Laboratory	Onsøy	whole profile	S01	Sec 4.3	BY	A
P05	Unit II clay	Plasticity index	typically around 44	%	Atterberg limits	Onsøy	as P02	S01	Sec 3 (stratigraphy text, Fig. 8)	BY	A
P06	Unit II clay	Water content	typically 70	%	Laboratory	Onsøy	as P02	S01	Sec 3	BY	A
P07	Unit III clay	Plasticity index	typically about 27	%	Atterberg limits	Onsøy	as P03	S01	Sec 3	BY	A
P08	Unit III clay	Water content	about 45	%	Laboratory	Onsøy	as P03	S01	Sec 3	BY	A
P09	Soft clay profile	Overconsolidation ratio	largely 1.3 to 1.8	-	Oedometer tests on block samples, best-estimate profile	Onsøy	3-20 m	S01	Sec 5.2, Eq 3	BY	A
P10	Soft clay profile	Apparent preconsolidation stress	6.9 (z + 4.54), z in m	kPa	Best-estimate fit to oedometer data	Onsøy	3-20 m	S01	Eq 3	BY	A
P11	Soft clay, upper	Undrained shear strength, triaxial compression	15.8	kPa	Anisotropically consolidated triaxial compression, emphasis on block samples	Onsøy	2-5 m	S01	Eq 9a	BY	A
P12	Soft clay, lower	Strength ratio in triaxial compression, su/σ'v0	0.43	-	As P11	Onsøy	5-20 m	S01	Eq 9b	BY	A
P13	Soft clay	Strength anisotropy, simple shear / triaxial compression	0.74	-	Direct simple shear versus triaxial compression	Onsøy	profile	S01	Sec 5.4.2	BY	A
P14	Soft clay	Strength anisotropy, triaxial extension / compression	0.52	-	Triaxial extension versus compression	Onsøy	profile	S01	Sec 5.4.2	BY	A
P15	Soft clay	Sensitivity	generally 5 to 8	-	Fall cone and field vane	Onsøy	down to 14 m	S01	Sec 5.4.3	BY	A
P16	Soft clay	Constant-volume friction angle	30	degrees	Effective-stress interpretation of triaxial tests (with attraction 5 kPa)	Onsøy	profile	S01	Sec 5.4	BY	A
P17	Soft clay	Cone factor Nkt (site-specific)	9	-	Piezocone calibrated to laboratory triaxial compression strength	Onsøy	profile	S01	Sec 5.4, triaxial compression strength text	BY	A
P18	Soft clay	Pore-pressure cone factor NΔu (site-specific)	7.2	-	As P17	Onsøy	profile	S01	Sec 5.4, triaxial compression strength text	BY	A
P19	Deep-water soft clay	Cone factor Nkt, recommended	12.5	-	Reported from an earlier study	Luva field, Norwegian Sea	not stated	S01	Table 2	BY	B
P20	Deep-water soft clay	T-bar factor, recommended	9.5	-	As P19	Luva field	not stated	S01	Table 2	BY	B
P21	Soft clay	OCR from normalised cone resistance, factor k in OCR = k Qt	0.36 (site); 0.3 (general, plots low)	-	Piezocone versus oedometer	Onsøy	profile	S01	Sec 5.2	BY	A
P22	Soft clay	Vertical hydraulic conductivity at in-situ state	best estimate 1e-9; usual soft-clay range 5e-10 to 5e-9	m/s	Constant-rate-of-strain oedometer	Onsøy	profile	S01	Sec 5.6	BY	A
P23	Sandy-silty deltaic soil	Total unit weight	18 to 20; representative 19	kN/m3	Laboratory and water content	Øysand, Norway	5-20 m	S02	Sec 6, unit weight text (before 6.4)	BY	A
P24	Unit II sand	Unit weight of solid particles	26.6 to 26.9; average 26.7	kN/m3	Laboratory	Øysand	Unit II	S02	Sec 6, solid-particle unit weight text	BY	A
P25	Sandy-silty soil	Relative density	average around 60; locally 30	%	Indirect estimate from in-situ and laboratory data	Øysand	investigated depth	S02	Sec 6.4	BY	A
P26	Unit II sandy silty soil	Peak friction angle	37 to 40	degrees	Anisotropically consolidated drained triaxial compression (K = 0.5), push-piston samples	Øysand	Unit II	S02	Sec 8.1	BY	A
P27	Unit II sand	Critical-state friction angle	about 34	degrees	Large-strain end of the same tests	Øysand	Unit II	S02	Sec 8.1	BY	A
P28	Sandy-silty soil	Hydraulic conductivity	1e-7 to 1e-3	m/s	Hazen's formula on gradings (silt content 0 to 70 %)	Øysand	profile	S02	Hydraulic conductivity discussion (Hazen and slug tests)	BY	B
P29	Sand	Ageing coefficient used for K0 estimate	0.005 (sand deposited hundreds to thousands of years ago); 0.002 (freshly deposited)	-	Literature value adopted in the paper	Øysand	profile	S02	Sec 9.2	BY	B
P30	Generic normally consolidated clay	Undrained strength at mudline (intercept)	80	psf	Generic design profile for a deepwater study	Gulf of Mexico (generic)	mudline	S03	Fig. 3	NS	C
P31	Generic normally consolidated clay	Undrained strength gradient	10	psf/ft	As P30	Gulf of Mexico (generic)	below mudline	S03	Fig. 3 and text	NS	C
P32	Generic normally consolidated clay	Plastic limit	25	%	As P30	Gulf of Mexico (generic)	profile	S03	Fig. 3	NS	C
P33	Generic normally consolidated clay	Liquid limit	70	%	As P30	Gulf of Mexico (generic)	profile	S03	Fig. 3	NS	C
P34	Normally consolidated clay, suction caisson wall	Side-shear adhesion factor, recommended with P35	on the order of 0.8	-	Calibration against 25 pullout tests	Generic	n/a	S03	"Calibration of Suction Caisson Design Method"	NS	B
P35	Normally consolidated clay, caisson tip	Reverse end-bearing factor, recommended with P34	12	-	As P34	Generic	n/a	S03	Same section	NS	B
P36	Normally consolidated clay	Adhesion factor range used by designers for suction caissons	0.6 to 0.8	-	Reported practice	Generic	n/a	S03	"Capacity for Axial (Uplift) Loading"	NS	B
P37	Uniform sand, loose	Dry / wet unit weight	90 / 118	pcf	Typical values, natural state (after Peck, Hanson and Thornburn)	Generic	n/a	S04	Table 1	NS	B
P38	Uniform sand, dense	Dry / wet unit weight	109 / 130	pcf	As P37	Generic	n/a	S04	Table 1	NS	B
P39	Well-graded sand, loose	Dry / wet unit weight	99 / 124	pcf	As P37	Generic	n/a	S04	Table 1	NS	B
P40	Well-graded sand, dense	Dry / wet unit weight	116 / 135	pcf	As P37	Generic	n/a	S04	Table 1	NS	B
P41	Glacial till	Dry / wet unit weight	132 / 145	pcf	As P37	Generic	n/a	S04	Table 1	NS	B
P42	Soft glacial clay	Dry / wet unit weight	76 / 110	pcf	As P37	Generic	n/a	S04	Table 1	NS	B
P43	Stiff glacial clay	Dry / wet unit weight	106 / 129	pcf	As P37	Generic	n/a	S04	Table 1	NS	B
P44	Soft slightly organic clay	Dry / wet unit weight	58 / 98	pcf	As P37	Generic	n/a	S04	Table 1	NS	B
P45	Cohesionless, very loose	Relative density; SPT N; friction angle	<20; <4; <30	%; blows/ft; degrees	Correlation (after Meyerhof), relatively clean sands	Generic	n/a	S04	Table 2	NS	B
P46	Cohesionless, loose	Relative density; SPT N; friction angle	20-40; 4-10; 30-35	%; blows/ft; degrees	As P45	Generic	n/a	S04	Table 2	NS	B
P47	Cohesionless, compact	Relative density; SPT N; friction angle	40-60; 10-30; 35-40	%; blows/ft; degrees	As P45	Generic	n/a	S04	Table 2	NS	B
P48	Cohesionless, dense	Relative density; SPT N; friction angle	60-80; 30-50; 40-45	%; blows/ft; degrees	As P45	Generic	n/a	S04	Table 2	NS	B
P49	Cohesionless, very dense	Relative density; SPT N; friction angle	>80; >50; >45	%; blows/ft; degrees	As P45	Generic	n/a	S04	Table 2	NS	B
P50	Sand, loose	Equivalent elastic modulus	10,000 to 30,000	kPa	Typical values (modified after FHWA circular)	Generic	n/a	S04	Table 3	NS	B
P51	Sand, medium dense	Equivalent elastic modulus	30,000 to 50,000	kPa	As P50	Generic	n/a	S04	Table 3	NS	B
P52	Sand, dense	Equivalent elastic modulus	50,000 to 80,000	kPa	As P50	Generic	n/a	S04	Table 3	NS	B
P53	Inorganic clay (sensitivity up to 4, liquid limit below 100)	Compression index	0.009 × (LL − 10), LL in %	-	Correlation attributed to Terzaghi and Peck	Generic	n/a	S04	Eq 200E-1_1	NS	B
P54	Clay	Recompression index as a fraction of compression index	approximately 10 to 20	%	Attributed to Ladd	Generic	n/a	S04	"Recompression Index"	NS	B
P55	Soil at reference monopile	Shear modulus	140E6	Pa	Assumed summary value, "representative of dense sand or gravel soils"	Generic reference site	n/a	S05	Table 4-1	NS	C
P56	Soil at reference monopile	Poisson's ratio	0.4	-	As P55	Generic reference site	n/a	S05	Table 4-1	NS	C
P57	Silica sand against steel pile surface	Constant-volume interface friction angle	29 (mean measured)	degrees	Ring-shear interface tests with fast pre-shearing; used "in the absence of site specific ring shear interface tests"	Generic silica sands	n/a	S06	"Interface friction angle"	CO	B
P58	Sand, driven pile shaft	Tension-to-compression coefficient on shaft friction	1.0 compression; 0.75 tension	-	Method coefficient	Generic silica sands	n/a	S06	Eq 1 and following text	CO	B
P59	Clay	Cone factor Nkt, general	10 to 18; 14 as an average	-	Empirical; rises with plasticity, falls with sensitivity	Generic	n/a	S07	"Undrained shear strength" section	CO	B
P60	Soft clays	Ratio of horizontal to vertical permeability	1 to 1.5 (essentially homogeneous); 2 to 4 (well-developed macrofabric); 3 to 10 (varved, continuous permeable layers)	-	Field values (modified from Jamiolkowski et al.)	Generic	n/a	S07	Table 7	CO	B"""
SOURCE_TRANSCRIPTION = """S01	Gundersen et al., "Characterization and engineering properties of the NGTS Onsøy soft clay site", AIMS Geosciences 5(3), 665-703 (2019)	https://aimspress.com/aimspress-data/aimsgeo/2019/3/PDF/geosci-05-03-665.pdf	"open access article distributed under the terms of the Creative Commons Attribution License (http://creativecommons.org/licenses/by/4.0)"
S02	Quinteros et al., "Øysand research site: Geotechnical characterisation of deltaic sandy-silty soils", AIMS Geosciences 5(4), 750-783 (2019), doi 10.3934/geosci.2019.4.750	https://www.aimspress.com/aimspress-data/aimsgeo/2019/4/PDF/geosci-05-04-750.pdf	Same CC BY 4.0 statement
S03	"Suction Caissons: Seafloor Characterization for Deepwater Foundation Systems" (R. Gilbert, principal investigator), OTRC Library Number 6/06B171, MMS Project 362 (PDF dated June 2006)	https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program/362aa6.pdf	Terms not stated (US-agency-funded research report; carries only a views disclaimer)
S04	Iowa DOT Design Manual, Chapter 200 Geotechnical Design, Section 200E-1 "Engineering Properties of Soil and Rock" (originally issued 01-15-14, revised 05-19-15)	https://documents.iowa.gov/home/download/9593050	Terms not stated (public state-agency manual)
S05	"Definition of the IEA 15-Megawatt Offshore Reference Wind Turbine", NREL/TP-5000-75698	https://www.osti.gov/servlets/purl/1603478 (redirects to the laboratory document server)	Terms not checked in the extracted text: treat as "terms not stated"
S06	Lehane et al., "A New 'Unified' CPT-Based Axial Pile Capacity Design Method for Driven Piles in Sand", Proc. 4th Int. Symp. Frontiers in Offshore Geotechnics, pp. 462-477 (2020)	https://repository.tudelft.nl/file/File_4a46e02f-190c-4e7e-8f1e-c6724a8b55c5	Repository cover: "Other than for strictly personal use, it is not permitted to download, forward or distribute the text or part of it, without the consent of the author(s) and/or copyright holder(s), unless the work is under an open content license such as Creative Commons." Cite as fact; do not copy text
S07	Robertson and Cabal, "Guide to Cone Penetration Testing", Seventh Edition, December 2022 (Gregg Drilling)	Free PDF; the copy read was a third-party mirror, so link the publisher's page in the build	"Copyright © 2022 Gregg Drilling LLC. All rights reserved." Cite as fact; do not copy tables
S08	Brødbæk, Møller, Sørensen and Augustesen, "Review of p-y relationships in cohesionless soil", Aalborg University DCE Technical Report No. 57 (2009)	https://vbn.aau.dk/ws/files/16545021/Review_of_p-y_relationships_in_cohesionless_soil	"You may not further distribute the material or use it for any profit-making activity or commercial gain"; "You may freely distribute the URL". Cite as fact; link only
S09	PISA Cowden field-test paper, Géotechnique 70(11), pp. 970-985, publisher id jgeot.18.PISA.003 (title page did not extract)	https://www.vliz.be/imisdocs/publications/361397.pdf	"Published with permission by the ICE under the CC-BY 4.0 license"
S10	Oberhollenzer et al., "Cone penetration test dataset Premstaller Geotechnik", Data in Brief 34:106618 (2020), doi 10.1016/j.dib.2020.106618	https://pmc.ncbi.nlm.nih.gov/articles/PMC7744689/	"open access article under the CC BY license (http://creativecommons.org/licenses/by/4.0/)"
S11	Bennett, Holzer, Noce and Grant, "An Open Repository of U.S. Cone Penetration Test (CPT) data", USGS data release (2025), doi 10.5066/P13M3WVY	https://www.usgs.gov/data/open-repository-us-cone-penetration-test-cpt-data	Marked CC0 1.0 Universal
S12	Buczkowski et al., "usSEABED: Offshore surficial-sediment database for samples collected within the United States Exclusive Economic Zone", USGS data release (2020), doi 10.5066/P9H3LGWM	usgs.gov data-release page	"This work is marked with CC0 1.0 Universal"
S13	FHWA Deep Foundation Load Test Database v.2 user manual (file 17034.pdf) and the FHWA software page	https://www.fhwa.dot.gov/publications/research/infrastructure/structures/bridge/dfltd/17034.pdf	Manual: "No restrictions. This document is available to the U.S. public through the National Technical Information Service". Database page states no licence; download is behind "Register and Download"
S14	"Suction Caissons: Finite Element Studies", MMS Project 362 (only the header and scope were read)	https://bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program/362aa1.pdf	Terms not stated
S15	Marine Data Exchange (UK offshore wind survey data), terms-of-use page	https://www.marinedataexchange.co.uk/content/info/terms-of-use	Bespoke: "non-exclusive non-transferable licence (without the right to sublicense)"; digital copies must not be provided to third parties. Not redistributable
S16	EMODnet Geology landing page	https://emodnet.ec.europa.eu/en/geology	Terms page could not be opened (HTTP 403): "terms not stated" until someone reads it
S17	WSDOT Geotechnical Design Manual M 46-03, Chapter 5	wsdot.wa.gov	Read; no typical-value tables were found in the extracted text, so it supplies no rows"""
PRINTED_UNITS = {"kN/m3", "%", "-", "kPa", "degrees", "m/s", "psf", "psf/ft",
                 "pcf", "%; blows/ft; degrees", "Pa"}
# Formulas, qualified text, or alternatives cannot receive SI values.
NO_SI_ROWS = {"P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P15",
              "P21", "P22", "P23", "P24", "P25", "P27", "P29", "P34", "P45",
              "P49", "P53", "P54", "P57", "P58", "P59"}
# Blows/ft is not defined by the repository unit facility.
NO_SI_QUANTITIES = {("P46", "SPT N"), ("P47", "SPT N"), ("P48", "SPT N")}


@pytest.fixture(autouse=True)
def forbid_network(monkeypatch):
    def blocked(*args, **kwargs):
        raise AssertionError("network access is forbidden in database tests")
    monkeypatch.setattr(socket.socket, "connect", blocked)
    monkeypatch.setattr(socket.socket, "connect_ex", blocked)
    monkeypatch.setattr(socket, "create_connection", blocked)


def raw_data():
    return yaml.safe_load(DATA.read_text(encoding="utf-8"))


def database(data=None):
    from digitalmodel.geotechnical.database import load_database
    return load_database(DATA if data is None else data)


def test_file_and_transcription():
    data = raw_data()
    expected = [line.split("\t") for line in TRANSCRIPTION.splitlines()]
    assert len(data["parameters"]) == 60
    assert [r["row_id"] for r in data["parameters"]] == [f"P{i:02d}" for i in range(1, 61)]
    assert [[r[f] for f in FIELDS] for r in data["parameters"]] == expected
    assert all(isinstance(r[f], str) and r[f].strip() for r in data["parameters"] for f in FIELDS)
    assert {r["units_as_printed"] for r in data["parameters"]} == PRINTED_UNITS
    source_fields = ("source_id", "citation", "access", "reuse_terms")
    assert [[s[f] for f in source_fields] for s in data["sources"]] == [
        line.split("\t") for line in SOURCE_TRANSCRIPTION.splitlines()]
    assert [s["source_id"] for s in data["sources"]] == [f"S{i:02d}" for i in range(1, 18)]


def test_validation_provenance_and_cite_only():
    db = database()
    assert set(r.source_id for r in db.rows.values()) == {f"S{i:02d}" for i in range(1, 8)}
    pairs = {"S01": "BY", "S02": "BY", "S03": "NS", "S04": "NS", "S05": "NS",
             "S06": "CO", "S07": "CO"}
    for row in db.rows.values():
        assert row.licence == pairs[row.source_id]
        assert row.confidence_grade in {"A", "B", "C"}
        assert row.source.read_status == "read"
        assert row.source.reuse_terms and row.locator and row.source.citation
        assert row.source is db.sources[row.source_id]
    for row in raw_data()["parameters"]:
        assert "quotation" not in row and "excerpt" not in row
        if row["licence"] == "CO":
            assert all(len(row[f]) < 200 for f in FIELDS)
    assert db.sources["S06"].redistribution == "cite-only"
    assert db.sources["S07"].redistribution == "cite-only"
    assert all(db.sources[s].redistribution == "not-stated" for s in ("S03", "S04", "S05"))


def test_immutable_lookups_and_gaps():
    db = database()
    row = db.rows["P01"]
    for field in ("soil_unit", "parameter", "site_or_region", "source_id", "confidence_grade"):
        assert row in db.lookup(field, getattr(row, field))
        assert db.lookup(field, "absent") == ()
    with pytest.raises(ValueError, match="lookup"):
        db.lookup("value_or_range", "17.5")
    with pytest.raises(TypeError):
        db.rows["P01"] = row
    with pytest.raises(FrozenInstanceError):
        row.locator = "changed"
    assert isinstance(db.gaps, tuple) and isinstance(db.excluded, tuple)
    assert len(db.gaps) == 16  # Twelve section-8 rows plus four section-2 omissions.
    assert all(set(g) == {"topic", "where_commonly_defined"} for g in db.gaps)
    # Numbers in standard designations are identifiers, not numerical design values.
    assert all(not any(c.isdigit() for c in g["topic"]) for g in db.gaps)
    assert db.meta["counts"] == {"sources_opened_and_read": 17, "record_pages": 2,
                                 "located_but_not_read": 21, "rows_proposed": 60}
    assert db.meta["source_statement"] == "research note on the issue, 2026-10-12"
    with pytest.raises(TypeError):
        db.meta["counts"]["rows_proposed"] = 0
    with pytest.raises(TypeError):
        db.gaps[0]["topic"] = "changed"
    original = raw_data()
    loaded = database(original)
    original["parameters"][0]["value_or_range"] = "999"
    assert loaded.rows["P01"].value_or_range == "17.5"


def test_quantity_splits_and_units():
    db = database()
    assert len(db.quantities) == 80
    for n in range(37, 45):
        qs = db.quantities_for(f"P{n:02d}")
        assert [q.name for q in qs] == ["Dry unit weight", "wet unit weight"]
        assert len(qs) == 2
    for n in range(45, 50):
        assert [q.name for q in db.quantities_for(f"P{n:02d}")] == [
            "Relative density", "SPT N", "friction angle"]
    assert len(db.quantities_for("P60")) == 3
    for q in db.quantities:
        assert q.row is db.rows[q.parent_row_id]
        assert q.source is q.row.source
        if q.parent_row_id in NO_SI_ROWS or (q.parent_row_id, q.name) in NO_SI_QUANTITIES:
            with pytest.raises(ValueError, match="SI unavailable"):
                q.in_si()
            assert q.si is None and q.unavailable_reason
        else:
            value = q.in_si()
            assert value.unit and q.in_printed_units().unit == q.printed_units
    assert db.quantities_for("P03")[0].qualifier == "around"
    assert db.quantities_for("P04")[0].qualifier == "(best estimate)"
    q = db.quantities_for("P45")[0]
    assert q.qualifier == "<" and q.in_printed_units().maximum == 20
    assert q.in_printed_units().value is None
    with pytest.raises(ValueError, match="numeric access unavailable"):
        db.quantities_for("P10")[0].in_printed_units()


def test_hand_checkable_conversions():
    db = database()
    def si(row):
        return db.quantities_for(row)[0].in_si()
    assert si("P01").value == pytest.approx(Q_(17.5, "kN/m**3").to("N/m**3").magnitude)
    assert si("P30").value == pytest.approx(Q_(80, "force_pound/foot**2").to("kPa").magnitude)
    assert si("P30").unit == "kPa"
    assert si("P31").value == pytest.approx(Q_(10, "force_pound/foot**3").to("kPa/m").magnitude)
    expected = (Q_(90, "pcf") * ureg.standard_gravity).to("kN/m**3").magnitude
    assert si("P37").value == pytest.approx(expected)
    assert si("P37").unit == "kN/m**3"
    q = si("P50")
    assert q.minimum == pytest.approx(Q_(10000, "kPa").to("Pa").magnitude)
    assert q.maximum == pytest.approx(Q_(30000, "kPa").to("Pa").magnitude)


@pytest.mark.parametrize("field", FIELDS)
def test_reject_empty_required_field(field):
    data = raw_data()
    data["parameters"][0][field] = ""
    with pytest.raises(ValueError, match="required"):
        database(data)


@pytest.mark.parametrize("field,value,message", [
    ("source_id", "S99", "unknown source"),
    ("licence", "OTHER", "licence"),
    ("confidence_grade", "D", "grade"),
])
def test_reject_invalid_reference_or_code(field, value, message):
    data = raw_data()
    data["parameters"][0][field] = value
    with pytest.raises(ValueError, match=message):
        database(data)


@pytest.mark.parametrize("section", ["parameters", "sources"])
def test_reject_duplicate_id(section):
    data = raw_data()
    data[section].append(deepcopy(data[section][0]))
    with pytest.raises(ValueError, match="duplicate"):
        database(data)


@pytest.mark.parametrize("status", ["located", "unverified", "not read"])
def test_reject_source_not_read(status):
    data = raw_data()
    data["sources"][0]["read_status"] = status
    with pytest.raises(ValueError, match="not read"):
        database(data)


def test_missing_field_has_no_default():
    data = raw_data()
    del data["parameters"][0]["locator"]
    with pytest.raises(ValueError, match="required"):
        database(data)


def test_default_database_path_and_source_immutability():
    from digitalmodel.geotechnical.database import load_database
    db = load_database()
    assert len(db.rows) == 60
    with pytest.raises(TypeError):
        db.sources["S01"] = db.sources["S02"]
    with pytest.raises(FrozenInstanceError):
        db.sources["S01"].reuse_terms = "changed"


@pytest.mark.parametrize("field", ["citation", "access", "reuse_terms", "read_status"])
def test_reject_missing_source_field(field):
    data = raw_data()
    del data["sources"][0][field]
    with pytest.raises(ValueError, match="required"):
        database(data)
