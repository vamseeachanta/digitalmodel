## Introduction

Summarizes the tutorials worked

## Common Libraries

### Geometry

| Model | Description | Run Status | Notes |
| --- | --- | --- | --- |
| ./geoemtry/ship.iges | Ship geometry in IGES format | :heavy_check_mark: | n/a |
| [ship_wo_wp_cut.scdoc](./geoemtry/ship_wo_wp_cut.scdoc) | ANSYS tutorial ship geometry without waterplane cut | :heavy_check_mark: | n/a |
| [02_ship_with_pier.scdoc](./geoemtry/02_ship_with_pier.scdoc) | ANSYS tutorial ship geometry with pier | :heavy_check_mark: | n/a |

### Workbench

### Tutorial 1: Ship RAO analysis

| Model | Description | Run Status | Notes |
| --- | --- | --- | --- |
| 01_ship_raos.wbpj | For ship RAO analysis | :heavy_check_mark: | .HYD file created |

**Handling ship draft**

**TODO:**

- [ ] Perform analysis with and without waterplane cut and compare the results
- [ ] Perform analysis by changing the water plane and not change vessel location and compare the results

### Tutorial 2: Ship With Pier analysis

| Model | Description |Run Status | Notes |
| --- | --- | --- | --- |
| 02_s01_hd_pier.wbpj | Hydrodynamic analysis for Ship with pier analysis | :heavy_check_mark: | .HYD file created |
| 02_s02_hr_add_fenders.wbpj | Stability Analysis for Ship with Piers, mooorings, fenders | :heavy_check_mark: | Stability analysis ran |
| 02_s03_hr_wave.wbpj | Time Domain analysis using regular wave | &cross; | Convolution Error. <br> Contacted support |
| 02_s04_hr_ext_force.wbpj | External force on a fender | &cross; | Determine implementation strategy |
| 101_s01_external_force.wbpj | External force example in utils | &cross; | Determine implementation strategy |

