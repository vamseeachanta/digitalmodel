# Step 5 Scaling Comparison

## Test Conditions and Results

| Condition   |   Wind (m/s) |   Hs (m) |   Wind Scale |   Wave Scale | Combined Range (kN)   | Wind %   | Wave %   |
|:------------|-------------:|---------:|-------------:|-------------:|:----------------------|:---------|:---------|
| FC001       |           15 |     0.75 |         2.25 |          1.5 | 912.2 - 2148.3        | 78.8%    | 21.2%    |
| FC002       |           10 |     0.5  |         1    |          1   | 431.5 - 1035.9        | 71.3%    | 28.7%    |
| FC003       |            5 |     0.25 |         0.25 |          0.5 | 127.5 - 319.8         | 55.3%    | 44.7%    |
| FC004       |           20 |     1    |         4    |          2   | 1481.1 - 3657.0       | 83.2%    | 16.8%    |

## Key Observations

- Wind scaling follows quadratic relationship: (V/10)²
- Wave scaling is linear: Hs/0.5
- Wind contribution dominates at higher wind speeds
- Combined tension is simple addition of scaled components
- All outputs maintain 1000 samples (100 seconds at 10 Hz)
