# CMEMS synthetic offline fixture contract

`OfflineCMEMSFixture` is a synthetic contract exercise for the existing wave
scatter reducer. It provides no Copernicus Marine connectivity and accepts no
provider observations. The fixture is hand-authored, contains no third-party
data and does not represent an official product schema or captured response.

Import the class from
`digitalmodel.data_systems.data_procurement.metocean.api_clients.cmems_offline`.
Pass an already decoded mapping to its constructor, then call `records()`.
The adapter performs no file, network, authentication or cache operations.
It is deliberately absent from the package's public re-exports.

The mapping has exactly `provenance` and `records`. Provenance must match
`tests/data_systems/data_procurement/metocean/fixtures/cmems_synthetic.json`:
origin `synthetic`, schema version `cmems-synthetic-v1`, purpose
`offline-contract-exercise`, boolean `third_party_data: false`, and explicit
units `significant_wave_height: m` and `peak_period: s`. Unknown fields are
refused at every level, including credential fields, URLs and provider metadata.

Each record requires a timestamp with seconds and an explicit UTC offset,
significant wave height in metres and peak period in seconds. Output timestamps
are UTC datetimes; wave fields become `wave_height` and `wave_period`. Heights
must be nonnegative and periods positive. Nulls remain missing; booleans, numeric
strings, non-finite numbers, missing keys and invalid timestamps raise
`ValueError` without echoing input contents. There is no unit conversion or
substitution of mean period for peak period. Each output has `synthetic: true`
and an independent provenance copy. Caller mappings remain unchanged.

Pass `records()` to `reduce_to_scatter` with explicit caller-selected Hs and Tp
upper-edge bins, then use `scatter_to_seastates` for occupied-cell midpoints.
The existing reducer skips null and out-of-range samples. An explicitly empty
record list returns an empty list; no result is cached. This seam exercises no
envelope engine or solver and does not qualify synthetic values for design use.

A future real-data adapter must require the exact attribution
“E.U. Copernicus Marine Service Information”, a verified product DOI and licence
credit on every derived record and fixture. Missing real-data provenance must
cause refusal; synthetic provenance cannot satisfy that requirement. Real
toolbox integration requires a separate pinned-package and licence review,
environment-only credentials and a redistributable recorded example stripped
of signed URLs and authentication material. NetCDF/OPeNDAP decoding, current
profiles and live queries remain outside this contract.
