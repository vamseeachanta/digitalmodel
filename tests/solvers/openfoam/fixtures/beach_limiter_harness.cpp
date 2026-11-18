// Arithmetic harness only. These stubs do not implement OpenFOAM or MPI.
#include <algorithm>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <string>
#include <vector>

using scalar = double;
using label = int;
using word = std::string;
using scalarField = std::vector<scalar>;

struct Dimensions
{
    int timeExponent;
    bool operator!=(Dimensions other) const
    {
        return timeExponent != other.timeExponent;
    }
};
Dimensions operator/(Dimensions left, Dimensions right)
{
    return {left.timeExponent - right.timeExponent};
}
// SI time exponents: dimensionless=0 and time=1, by dimensional definition.
const Dimensions dimless{0}, dimTime{1};
struct volScalarField : scalarField
{
    Dimensions units{-1};
    Dimensions dimensions() const { return units; }
};
struct Time
{
    scalar step;
    scalar deltaTValue() const { return step; }
};
struct Mesh
{
    word scheme;
    Time clock;
    volScalarField reciprocal;
    label size;
    bool hasField;
    word ddtScheme(const word& key) const
    {
        // Deliberately incompatible default: the fragment must query momentum.
        return key == "ddt(rho,U)" ? scheme : "steadyState";
    }
    Time time() const { return clock; }
    std::size_t nCells() const { return size; }
    template<class T> const T* lookupObjectPtr(const word&) const
    {
        return hasField ? &reciprocal : nullptr;
    }
};
template<class T> struct sumOp {};
template<class T> struct maxOp {};
template<class T, class Op> void reduce(T&, Op) {}
struct Pstream { static bool master() { return true; } };
enum Error { FatalError };
int exit(Error) { throw std::runtime_error("invalid limiter input"); }
#define FatalErrorInFunction std::cerr
#define forAll(values, i) for (std::size_t i = 0; i < (values).size(); ++i)
using std::min;
using std::max;
std::ostream& Info = std::cout;
const char nl = '\n';

struct Source
{
    Mesh data;
    std::vector<label> cells_;
    const Mesh& mesh() const { return data; }
    void run(const scalarField& beachRate)
    {
        struct NamedField { word name() const { return "rho"; } } rho;
        struct Equation
        {
            struct Velocity { word name() const { return "U"; } };
            Velocity psi() const { return {}; }
        } eqn;
        #include "limited.H"
        Info << "effective:";
        for (scalar rate : beachEffectiveRate) Info << ' ' << rate;
        Info << nl;
    }
};

scalar readScalar()
{
    std::string token;
    std::cin >> token;
    return std::stod(token);
}
int main()
{
    try
    {
        Source source;
        int hasField, wrongDimensions, nRates, nZone;
        std::cin >> source.data.scheme;
        source.data.clock.step = readScalar();
        std::cin >> hasField >> wrongDimensions >> source.data.size >> nRates >> nZone;
        source.data.hasField = hasField;
        source.data.reciprocal.units = wrongDimensions ? dimless : dimless/dimTime;
        scalarField rates(nRates);
        for (scalar& value : rates) value = readScalar();
        source.data.reciprocal.resize(source.data.size);
        for (scalar& value : source.data.reciprocal) value = readScalar();
        source.cells_.resize(nZone);
        for (label& cell : source.cells_) std::cin >> cell;
        std::cout << std::setprecision(std::numeric_limits<scalar>::max_digits10);
        source.run(rates);
    }
    catch (const std::runtime_error&)
    {
        std::cout << "REJECTED\n";
    }
}
