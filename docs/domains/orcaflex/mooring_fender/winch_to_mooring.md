## Introduction


To make a multiple mooring-fender model converge and get the right tensions

## Summary


tests:
 -  tests\modules\orcaflex\orcaflex_analysis\ofx_mooring_analysis.yml



### Detailed Procedure

a/ Define approximate fixed length on 1 mooring. This single moooring should be picked in the middle of the ship. This will be the reference mooring.

![Picking a mooring](image.png)

- Stage 1 analysis:
    - Run the model with the 2 stabilizing mooring  with set length. This will help stabilize the vessel in translation and rotation (moment).
    - This will set reference tensions in other lines via "Specified Tension".

- Stage 2 analysis:

    - Set all other moorings to payout 0.
    - Set the 2 stabilizing moorings to target constant tension.
    - Iterate to get as close to target tension in the stabilizing moorings across all stages. 

- Stage 3 main analysis: Run with environment.