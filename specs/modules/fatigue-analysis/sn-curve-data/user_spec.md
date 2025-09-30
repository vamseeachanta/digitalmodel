The program add or modifies or removes SN curve data for fatigue analysis. The SN curve data is saved in a CSV file in the repository.


## Data Sources
  - ABS: https://pub-rm20.apps.eagle.org/r/2/2020-06-01/Section-4-Fatigue-Design-Factors
  - DNV: https://rules.dnv.com/docs/pdf/DNV/OS/2019-07/DNV-OS-C401.pdf
  - GL: https://www.dnv.com/oilgas/download/standards/GL%20Rules%20for%20Offshore%20Units,%20Part%20C,%20Chapter%205,%20Fatigue%20Design%20of%20Offshore%20Structures.pdf
  - Norsok: https://www.standard.no/en/sectors/energy/oil-and-gas/standards/norsok-standards/norsok-standards-list/norsok-m-120--fatigue-design-of-steel-structures/
  - API: https://www.api.org/~/media/Files/Publications/Whats%20New/2014/14-Fatigue-Design-Of-Offshore-Structures.pdf
  - BS 7608: https://shop.bsigroup.com/ProductDetail/?pid=000000000030251205
  - ISO 19902: https://www.iso.org/standard/65082.html

Data should contain the fatigue curve name, material, environment, thickness correction, slope, intercept, etc.
- use SN Curve of ABS E curve, in air from above reference

thickness correction parameters:
  - reference thickness : 22 mm
  - tk: thickness exponent (check ABS reference for value)

## Output 
  - a summary of all SN curves saved to .csv.
