A typical setting when convergence not acheived. Example of WLNG Orcaflex model with 2 FSTs. This setting was required for only a few runs

**Configuration - Basic**
<code>
    BaseFile: ../rev_a03/fst1_l_fst2_l_hwl.yml
    includefile: ../rev_a03/env_100yr_030deg.yml
    General:
    StaticsTolerance: 1e-6
    StaticsMinDamping: 200
    StaticsMaxDamping: 2000
    StaticsMaxIterations: 26000
</code>


**Configuration - For Convergence**
<code>
    BaseFile: ../rev_a03/fst1_l_fst2_l_hwl.yml
    includefile: ../rev_a03/env_100yr_030deg.yml
    General:
    StaticsTolerance: 0.01
    StaticsMinDamping: 50
    StaticsMaxDamping: 2000
    StaticsMaxIterations: 26000
</code>

