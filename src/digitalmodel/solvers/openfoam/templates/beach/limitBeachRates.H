// Insertion fragment for codeAddSupRho, OpenFOAM v2312 (2023).
// Include <cmath> in codeInclude. Input: mesh-sized scalarField beachRate
// in inverse seconds, with the spatial blend already applied, and cells_.
// Output: beachEffectiveRate, zero outside cells_. Use this SAME rate in
// the implicit coefficient and explicit tangential add-back, BEFORE rho.
// Policy: issue #2046 (2026), s_eff = min(s, c*r). The caller supplies c.
const scalar beachCap = @MAX_DAMPING_FRACTION@;
// Match the density-weighted momentum operator, including field overrides.
// Source: v2312 fvmDdt.C, ddt(rho, vf), lines 90-100.
const word beachScheme
(
    mesh().ddtScheme("ddt(" + rho.name() + ',' + eqn.psi().name() + ')')
);
if (beachScheme != "Euler" && beachScheme != "localEuler")
{
    FatalErrorInFunction
        << "Beach limiter supports only Euler or localEuler"
        << exit(FatalError);
}
const volScalarField* beachRDeltaT = nullptr;
scalar beachGlobalRDeltaT = 0;
if (beachScheme == "localEuler")
{
    // Source: v2312 localEulerDdt.C, localRDeltaT(), lines 47-55.
    // Momentum callback only: alpha subcycling is outside this contract.
    beachRDeltaT = mesh().lookupObjectPtr<volScalarField>("rDeltaT");
    if (!beachRDeltaT)
    {
        FatalErrorInFunction
            << "localEuler beach requires registered rDeltaT"
            << exit(FatalError);
    }
    if (beachRDeltaT->dimensions() != dimless/dimTime)
    {
        FatalErrorInFunction
            << "Beach rDeltaT must have inverse-time dimensions"
            << exit(FatalError);
    }
}
else
{
    const scalar beachDeltaT = mesh().time().deltaTValue();
    if (!std::isfinite(beachDeltaT) || beachDeltaT <= 0)
    {
        FatalErrorInFunction << "Invalid beach deltaT" << exit(FatalError);
    }
    beachGlobalRDeltaT = 1/beachDeltaT;
}
if (beachRate.size() != mesh().nCells())
{
    FatalErrorInFunction
        << "beachRate must contain one raw rate per mesh cell"
        << exit(FatalError);
}
scalarField beachEffectiveRate(mesh().nCells(), 0);
label beachClipped = 0;
label beachCount = cells_.size();
scalar beachMaxFraction = 0;
forAll(cells_, beachI)
{
    const label beachCell = cells_[beachI];
    const scalar beachRaw = beachRate[beachCell];
    const scalar beachR = beachRDeltaT
        ? (*beachRDeltaT)[beachCell] : beachGlobalRDeltaT;
    // Zero is the nonnegative-rate boundary; unity defines reciprocal time.
    // No fitted physical coefficient or timestep tolerance is introduced.
    if (!std::isfinite(beachRaw) || beachRaw < 0
     || !std::isfinite(beachR) || beachR <= 0)
    {
        FatalErrorInFunction
            << "Beach requires finite nonnegative rates and positive reciprocal steps"
            << exit(FatalError);
    }
    const scalar beachEffective = min(beachRaw, beachCap*beachR);
    beachEffectiveRate[beachCell] = beachEffective;
    beachClipped += (beachEffective < beachRaw);
    beachMaxFraction = max(beachMaxFraction, beachEffective/beachR);
}
// Count-weighted diagnostic over all partitions, including empty partitions.
reduce(beachClipped, sumOp<label>());
reduce(beachCount, sumOp<label>());
reduce(beachMaxFraction, maxOp<scalar>());
if (Pstream::master())
{
    Info<< "beachDamping: clipped=" << beachClipped << " cells=" << beachCount;
    if (beachCount)
    {
        Info<< " fraction=" << scalar(beachClipped)/scalar(beachCount)
            << " maxEffectiveFraction=" << beachMaxFraction << nl;
    }
    else
    {
        Info<< " fraction=NOT_EVALUATED maxEffectiveFraction=NOT_EVALUATED"
            << " (empty zone)" << nl;
    }
}
