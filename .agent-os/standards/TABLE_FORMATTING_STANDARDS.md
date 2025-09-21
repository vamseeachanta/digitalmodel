# Table Formatting Standards

## 🚨 MANDATORY: Units in Table Headers

### Critical Rule
**ALWAYS include units in table column headers, NEVER in cell values**

### Why This Is Mandatory
Special symbols (°, ², ³, µ, Ω, etc.) in markdown table cells can cause:
- Rendering issues in different markdown processors
- Encoding errors in various systems
- Parsing failures for AI agents and bots
- Copy-paste problems across platforms
- Git diff readability issues

## Correct Format ✅

### Good Example - Units in Headers
```markdown
| Wind Speed (m/s) | Wave Height (m) | Direction (deg) | Pressure (kPa) |
|------------------|-----------------|-----------------|----------------|
| 10               | 0.5             | 45              | 101.3          |
| 15               | 0.75            | 90              | 99.8           |
```

### Good Example - Complex Units
```markdown
| Force (kN) | Area (m²) | Stress (N/mm²) | Angle (degrees) | Temperature (°C) |
|------------|-----------|----------------|-----------------|------------------|
| 250.5      | 12.3      | 20.4           | 30              | 25               |
| 180.2      | 8.7       | 20.7           | 45              | 18               |
```

## Incorrect Format ❌

### Bad Example - Units in Cells
```markdown
| Wind Speed | Wave Height | Direction | Pressure |
|------------|-------------|-----------|----------|
| 10 m/s     | 0.5 m       | 45°       | 101.3 kPa |  ❌ Units in cells
| 15 m/s     | 0.75 m      | 90°       | 99.8 kPa  |  ❌ Special symbols
```

### Bad Example - Mixed Format
```markdown
| Parameter | Value     |
|-----------|-----------|
| Wind      | 10 m/s    |  ❌ Unit in cell
| Wave      | 0.5m Hs   |  ❌ Unit in cell
| Angle     | 45°       |  ❌ Degree symbol in cell
```

## Standard Unit Representations

### Use These in Headers:
- **Degrees**: `(deg)` or `(degrees)` NOT `(°)`
- **Square/Cubic**: `(m2)` or `(m^2)` NOT `(m²)`
- **Micro**: `(um)` or `(micro-m)` NOT `(µm)`
- **Ohm**: `(ohm)` NOT `(Ω)`
- **Plus/Minus**: `+/-` NOT `±`

### Common Engineering Units for Headers:
```markdown
| Distance (m) | Area (m2) | Volume (m3) | Speed (m/s) | Acceleration (m/s2) |
| Force (N) | Pressure (Pa) | Stress (MPa) | Power (W) | Energy (J) |
| Temperature (degC) | Angle (deg) | Frequency (Hz) | Time (s) | Mass (kg) |
```

## Benefits of This Standard

1. **Universal Compatibility**: Works across all markdown processors
2. **AI/Bot Friendly**: Clean parsing for automated systems
3. **Version Control**: Clean diffs in Git
4. **Copy-Paste Safe**: No encoding issues
5. **International**: Works across all locales and systems
6. **Searchable**: Easier to grep/search for values

## Implementation Examples

### Environmental Conditions Table
```markdown
| Sea State | Wind Speed (m/s) | Wave Hs (m) | Wave Tp (s) | Wind Dir (deg) |
|-----------|------------------|-------------|-------------|----------------|
| SS001     | 15               | 0.75        | 4.0         | 0              |
| SS002     | 10               | 0.50        | 2.7         | 0              |
```

### Scaling Factors Table
```markdown
| Condition | Wind Scale (ratio) | Wave Scale (ratio) | Tension Range (kN) |
|-----------|-------------------|-------------------|-------------------|
| Test      | 2.25              | 1.50              | 912.2 - 2148.3    |
| Reference | 1.00              | 1.00              | 431.5 - 1035.9    |
```

### Material Properties Table
```markdown
| Material | Density (kg/m3) | E-Modulus (GPa) | Yield Strength (MPa) |
|----------|-----------------|-----------------|---------------------|
| Steel    | 7850            | 210             | 355                 |
| Aluminum | 2700            | 70              | 270                 |
```

## Enforcement

- **Code Reviews**: Tables with units in cells should be rejected
- **Documentation**: All new tables must follow this standard
- **Migration**: Update existing tables when modified
- **Templates**: Use header-unit format in all templates
- **Validation**: Automated checks for special characters in cells

## Quick Checklist

Before committing any table:
- [ ] All units are in column headers
- [ ] No special symbols in cell values
- [ ] Degree symbol (°) replaced with (deg)
- [ ] Superscripts (²,³) replaced with (2,3) or (^2,^3)
- [ ] Greek letters spelled out or use standard abbreviations
- [ ] Values are plain numbers or text only

This standard is MANDATORY for all tables in documentation, code comments, and data files.