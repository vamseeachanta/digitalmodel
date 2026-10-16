# SLWR post-processing

This folder holds SLWR post-process configurations and references to their input models.
The post-process configurations read results of SLWR input models that are not kept in the tree.
From the repository root, restore the inputs with:

```sh
python scripts/data/fetch.py 'docs/domains/orcaflex/risers/production/slwr/*'
```

This command restores 40 store entries; the configurations read results of 39 of them
(52 references to 39 distinct result paths across 16 configurations).
Set `ACE_DATA_ROOT` or `ACE_DATA_REMOTE` as described by the fetch script's help.
A restored input model must be run to produce its `.sim` results before the configurations are used.
