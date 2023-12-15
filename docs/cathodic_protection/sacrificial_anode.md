
# Codes

## DNV

## ABS

See docs\cathodic_protection\codes\cathodic-protection-gn-dec17.pdf

## Other References

## Calculation

### Design Current, Section 4

Design current for each sub area of the ship is to be used for the CP design.

- Structure subdivision
  - Port side (Shore-side)
  - Starboard side (Carrier side)
- Surface Area calculations

- Any marine growth prevension systems (MGPS) or anti-fouling system can impact the CP system design and the associated efffectiveness.

- Internal surfaces
  - All internal surfaces of the ship are to be protected by CP system.

- From typical design current below, V <= 1 m/s (as vessel is stationary) is used

<img src="cp_ship_typical_design_currents.PNG" width=auto, height=auto/>

- Evaluate Design current density for coated steeel, See section 4.4

- Evaluate current demand, See section 4.5

### Circuit Resistance, Section 5

- The objective of the CP system is to provide a protection potential over the whole steel surface.

- The number and location of anodes is critical for this

### Anode Resistance, Section 6

- Assume slender anodes at 0.3m offset from steel, Section 6.1 (Implement in Spreadsheet)

- For other anode types, see Section 6.2 and Section 6.3 (Implement in Python)

### Anode current output and Operating Life, Section 7

- Service temperature
  - Assume 40 def F (5 deg C)

- Minimum Net Weight of Anode (Main Output). If calculation is started in this Section, Rest of it will be chain reaction to the above and below evaluations.
  - See Section 7.3

- Anode Life: 25 yrs Design life

- Anode resistance at End of Life, Section 7.5
