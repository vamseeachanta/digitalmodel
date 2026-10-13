# finance

> Part of `digitalmodel.specialized`

## Overview

TODO: Module description

## Usage

```python
from digitalmodel.specialized.finance import ...
```

## API Reference

TODO: Document public API

## Environment variables

| Variable | Use | Requirement |
| --- | --- | --- |
| `TIINGO_API_KEY` | Tiingo market-data access through `pandas_datareader.get_data_tiingo`. | Required for functions that fetch Tiingo market data; an unset or empty variable raises an error. |

The credential is read from the environment when Tiingo data is requested. No
credential value is stored in the repository or supplied as a default.
