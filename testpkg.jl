(:)(a::Real, b::Real) = (:)(promote(a,b)...)

(:)(start::T, stop::T) where {T<:Real} = UnitRange{T}(start, stop)

(:)(start::T, stop::T) where {T} = (:)(start, oftype(stop-start, 1), stop)

# promote start and stop, leaving step alone
(:)(start::A, step, stop::C) where {A<:Real,C<:Real} =
    (:)(convert(promote_type(A,C),start), step, convert(promote_type(A,C),stop))




oftype(30 - 22.3, 1)

# AbstractFloat specializations
(:)(a::T, b::T) where {T<:AbstractFloat} = (:)(a, T(1), b)

(:)(a::T, b::AbstractFloat, c::T) where {T<:Real} = (:)(promote(a,b,c)...)
(:)(a::T, b::AbstractFloat, c::T) where {T<:AbstractFloat} = (:)(promote(a,b,c)...)
(:)(a::T, b::Real, c::T) where {T<:AbstractFloat} = (:)(promote(a,b,c)...)

(:)(start::T, step::T, stop::T) where {T<:AbstractFloat} =
    _colon(OrderStyle(T), ArithmeticStyle(T), start, step, stop)
(:)(start::T, step::T, stop::T) where {T<:Real} =
    _colon(OrderStyle(T), ArithmeticStyle(T), start, step, stop)
_colon(::Ordered, ::Any, start::T, step, stop::T) where {T} = StepRange(start, step, stop)
# for T<:Union{Float16,Float32,Float64} see twiceprecision.jl
_colon(::Ordered, ::ArithmeticRounds, start::T, step, stop::T) where {T} =
    StepRangeLen(start, step, floor(Int, (stop-start)/step)+1)
_colon(::Any, ::Any, start::T, step, stop::T) where {T} =
    StepRangeLen(start, step, floor(Int, (stop-start)/step)+1)

    """
        (:)(start, [step], stop)
    Range operator. `a:b` constructs a range from `a` to `b` with a step size of 1 (a [`UnitRange`](@ref))
    , and `a:s:b` is similar but uses a step size of `s` (a [`StepRange`](@ref)).
    `:` is also used in indexing to select whole dimensions
     and for [`Symbol`](@ref) literals, as in e.g. `:hello`.
    """
    (:)(start::T, step, stop::T) where {T} = _colon(start, step, stop)
    (:)(start::T, step, stop::T) where {T<:Real} = _colon(start, step, stop)
    # without the second method above, the first method above is ambiguous with
    # (:)(start::A, step, stop::C) where {A<:Real,C<:Real}
    function _colon(start::T, step, stop::T) where T
        T′ = typeof(start+zero(step))
        StepRange(convert(T′,start), step, convert(T′,stop))
    end

    """
        range(start[, stop]; length, stop, step=1)
    Given a starting value, construct a range either by length or from `start` to `stop`,
    optionally with a given step (defaults to 1, a [`UnitRange`](@ref)).
    One of `length` or `stop` is required.  If `length`, `stop`, and `step` are all specified, they must agree.
    If `length` and `stop` are provided and `step` is not, the step size will be computed
    automatically such that there are `length` linearly spaced elements in the range (a [`LinRange`](@ref)).
    If `step` and `stop` are provided and `length` is not, the overall range length will be computed
    automatically such that the elements are `step` spaced (a [`StepRange`](@ref)).
    `stop` may be specified as either a positional or keyword argument.
    !!! compat "Julia 1.1"
        `stop` as a positional argument requires at least Julia 1.1.
    # Examples
    ```jldoctest
    julia> range(1, length=100)
    1:100
    julia> range(1, stop=100)
    1:100
    julia> range(1, step=5, length=100)
    1:5:496
    julia> range(1, step=5, stop=100)
    1:5:96
    julia> range(1, 10, length=101)
    1.0:0.09:10.0
    julia> range(1, 100, step=5)
    1:5:96
    ```
    """
    range(start; length::Union{Integer,Nothing}=nothing, stop=nothing, step=nothing) =
        _range(start, step, stop, length)

    range(start, stop; length::Union{Integer,Nothing}=nothing, step=nothing) =
        _range2(start, step, stop, length)

    _range2(start, ::Nothing, stop, ::Nothing) =
        throw(ArgumentError("At least one of `length` or `step` must be specified"))

    _range2(start, step, stop, length) = _range(start, step, stop, length)

    # Range from start to stop: range(a, [step=s,] stop=b), no length
    _range(start, step,      stop, ::Nothing) = (:)(start, step, stop)
    _range(start, ::Nothing, stop, ::Nothing) = (:)(start, stop)

    # Range of a given length: range(a, [step=s,] length=l), no stop
    _range(a::Real,          ::Nothing,         ::Nothing, len::Integer) = UnitRange{typeof(a)}(a, oftype(a, a+len-1))
    _range(a::AbstractFloat, ::Nothing,         ::Nothing, len::Integer) = _range(a, oftype(a, 1),   nothing, len)
    _range(a::AbstractFloat, st::AbstractFloat, ::Nothing, len::Integer) = _range(promote(a, st)..., nothing, len)
    _range(a::Real,          st::AbstractFloat, ::Nothing, len::Integer) = _range(float(a), st,      nothing, len)
    _range(a::AbstractFloat, st::Real,          ::Nothing, len::Integer) = _range(a, float(st),      nothing, len)
    _range(a,                ::Nothing,         ::Nothing, len::Integer) = _range(a, oftype(a-a, 1), nothing, len)

    _range(a::T, step::T, ::Nothing, len::Integer) where {T <: AbstractFloat} =
        _rangestyle(OrderStyle(T), ArithmeticStyle(T), a, step, len)
    _range(a::T, step, ::Nothing, len::Integer) where {T} =
        _rangestyle(OrderStyle(T), ArithmeticStyle(T), a, step, len)
    _rangestyle(::Ordered, ::ArithmeticWraps, a::T, step::S, len::Integer) where {T,S} =
        StepRange{T,S}(a, step, convert(T, a+step*(len-1)))
    _rangestyle(::Any, ::Any, a::T, step::S, len::Integer) where {T,S} =
        StepRangeLen{typeof(a+0*step),T,S}(a, step, len)

    # Malformed calls
    _range(start,     step,      ::Nothing, ::Nothing) = # range(a, step=s)
        throw(ArgumentError("At least one of `length` or `stop` must be specified"))
    _range(start,     ::Nothing, ::Nothing, ::Nothing) = # range(a)
        throw(ArgumentError("At least one of `length` or `stop` must be specified"))
    _range(::Nothing, ::Nothing, ::Nothing, ::Nothing) = # range(nothing)
        throw(ArgumentError("At least one of `length` or `stop` must be specified"))
    _range(start::Real, step::Real, stop::Real, length::Integer) = # range(a, step=s, stop=b, length=l)
        throw(ArgumentError("Too many arguments specified; try passing only one of `stop` or `length`"))
    _range(::Nothing, ::Nothing, ::Nothing, ::Integer) = # range(nothing, length=l)
        throw(ArgumentError("Can't start a range at `nothing`"))

    ## 1-dimensional ranges ##

    """
        AbstractRange{T}
    Supertype for ranges with elements of type `T`.
    [`UnitRange`](@ref) and other types are subtypes of this.
    """
    abstract type AbstractRange{T} <: AbstractArray{T,1} end

    RangeStepStyle(::Type{<:AbstractRange}) = RangeStepIrregular()
    RangeStepStyle(::Type{<:AbstractRange{<:Integer}}) = RangeStepRegular()

    convert(::Type{T}, r::AbstractRange) where {T<:AbstractRange} = r isa T ? r : T(r)

    ## ordinal ranges

    """
        OrdinalRange{T, S} <: AbstractRange{T}
    Supertype for ordinal ranges with elements of type `T` with
    spacing(s) of type `S`. The steps should be always-exact
    multiples of [`oneunit`](@ref), and `T` should be a "discrete"
    type, which cannot have values smaller than `oneunit`. For example,
    `Integer` or `Date` types would qualify, whereas `Float64` would not (since this
    type can represent values smaller than `oneunit(Float64)`.
    [`UnitRange`](@ref), [`StepRange`](@ref), and other types are subtypes of this.
    """
    abstract type OrdinalRange{T,S} <: AbstractRange{T} end

    """
        AbstractUnitRange{T} <: OrdinalRange{T, T}
    Supertype for ranges with a step size of [`oneunit(T)`](@ref) with elements of type `T`.
    [`UnitRange`](@ref) and other types are subtypes of this.
    """
    abstract type AbstractUnitRange{T} <: OrdinalRange{T,T} end

    """
        StepRange{T, S} <: OrdinalRange{T, S}
    Ranges with elements of type `T` with spacing of type `S`. The step
    between each element is constant, and the range is defined in terms
    of a `start` and `stop` of type `T` and a `step` of type `S`. Neither
    `T` nor `S` should be floating point types. The syntax `a:b:c` with `b > 1`
    and `a`, `b`, and `c` all integers creates a `StepRange`.
    # Examples
    ```jldoctest
    julia> collect(StepRange(1, Int8(2), 10))
    5-element Array{Int64,1}:
     1
     3
     5
     7
     9
    julia> typeof(StepRange(1, Int8(2), 10))
    StepRange{Int64,Int8}
    julia> typeof(1:3:6)
    StepRange{Int64,Int64}
    ```
    """
    struct StepRange{T,S} <: OrdinalRange{T,S}
        start::T
        step::S
        stop::T

        function StepRange{T,S}(start::T, step::S, stop::T) where {T,S}
            new(start, step, steprange_last(start,step,stop))
        end
    end

    # to make StepRange constructor inlineable, so optimizer can see `step` value
    function steprange_last(start::T, step, stop) where T
        if isa(start,AbstractFloat) || isa(step,AbstractFloat)
            throw(ArgumentError("StepRange should not be used with floating point"))
        end
        z = zero(step)
        step == z && throw(ArgumentError("step cannot be zero"))

        if stop == start
            last = stop
        else
            if (step > z) != (stop > start)
                last = steprange_last_empty(start, step, stop)
            else
                # Compute absolute value of difference between `start` and `stop`
                # (to simplify handling both signed and unsigned T and checking for signed overflow):
                absdiff, absstep = stop > start ? (stop - start, step) : (start - stop, -step)

                # Compute remainder as a nonnegative number:
                if T <: Signed && absdiff < zero(absdiff)
                    # handle signed overflow with unsigned rem
                    remain = convert(T, unsigned(absdiff) % absstep)
                else
                    remain = absdiff % absstep
                end
                # Move `stop` closer to `start` if there is a remainder:
                last = stop > start ? stop - remain : stop + remain
            end
        end
        last
    end

    function steprange_last_empty(start::Integer, step, stop)
        # empty range has a special representation where stop = start-1
        # this is needed to avoid the wrap-around that can happen computing
        # start - step, which leads to a range that looks very large instead
        # of empty.
        if step > zero(step)
            last = start - oneunit(stop-start)
        else
            last = start + oneunit(stop-start)
        end
        last
    end
    # For types where x+oneunit(x) may not be well-defined
    steprange_last_empty(start, step, stop) = start - step

    StepRange(start::T, step::S, stop::T) where {T,S} = StepRange{T,S}(start, step, stop)

    """
        UnitRange{T<:Real}
    A range parameterized by a `start` and `stop` of type `T`, filled
    with elements spaced by `1` from `start` until `stop` is exceeded.
    The syntax `a:b` with `a` and `b` both `Integer`s creates a `UnitRange`.
    # Examples
    ```jldoctest
    julia> collect(UnitRange(2.3, 5.2))
    3-element Array{Float64,1}:
     2.3
     3.3
     4.3
    julia> typeof(1:10)
    UnitRange{Int64}
    ```
    """
    struct UnitRange{T<:Real} <: AbstractUnitRange{T}
        start::T
        stop::T
        UnitRange{T}(start, stop) where {T<:Real} = new(start, unitrange_last(start,stop))
    end
    UnitRange(start::T, stop::T) where {T<:Real} = UnitRange{T}(start, stop)

    unitrange_last(::Bool, stop::Bool) = stop
    unitrange_last(start::T, stop::T) where {T<:Integer} =
        ifelse(stop >= start, stop, convert(T,start-oneunit(stop-start)))
    unitrange_last(start::T, stop::T) where {T} =
        ifelse(stop >= start, convert(T,start+floor(stop-start)),
                              convert(T,start-oneunit(stop-start)))

    if isdefined(Main, :Base)
        function getindex(t::Tuple, r::AbstractUnitRange{<:Real})
            n = length(r)
            n == 0 && return ()
            a = Vector{eltype(t)}(undef, n)
            o = first(r) - 1
            for i = 1:n
                el = t[o + i]
                @inbounds a[i] = el
            end
            (a...,)
        end
    end

    """
        Base.OneTo(n)
    Define an `AbstractUnitRange` that behaves like `1:n`, with the added
    distinction that the lower limit is guaranteed (by the type system) to
    be 1.
    """
    struct OneTo{T<:Integer} <: AbstractUnitRange{T}
        stop::T
        OneTo{T}(stop) where {T<:Integer} = new(max(zero(T), stop))
        function OneTo{T}(r::AbstractRange) where {T<:Integer}
            throwstart(r) = (@_noinline_meta; throw(ArgumentError("first element must be 1, got $(first(r))")))
            throwstep(r)  = (@_noinline_meta; throw(ArgumentError("step must be 1, got $(step(r))")))
            first(r) == 1 || throwstart(r)
            step(r)  == 1 || throwstep(r)
            return new(max(zero(T), last(r)))
        end
    end
    OneTo(stop::T) where {T<:Integer} = OneTo{T}(stop)
    OneTo(r::AbstractRange{T}) where {T<:Integer} = OneTo{T}(r)

    ## Step ranges parameterized by length

    """
        StepRangeLen{T,R,S}(ref::R, step::S, len, [offset=1]) where {T,R,S}
        StepRangeLen(       ref::R, step::S, len, [offset=1]) where {  R,S}
    A range `r` where `r[i]` produces values of type `T` (in the second
    form, `T` is deduced automatically), parameterized by a `ref`erence
    value, a `step`, and the `len`gth. By default `ref` is the starting
    value `r[1]`, but alternatively you can supply it as the value of
    `r[offset]` for some other index `1 <= offset <= len`. In conjunction
    with `TwicePrecision` this can be used to implement ranges that are
    free of roundoff error.
    """
    struct StepRangeLen{T,R,S} <: AbstractRange{T}
        ref::R       # reference value (might be smallest-magnitude value in the range)
        step::S      # step value
        len::Int     # length of the range
        offset::Int  # the index of ref

        function StepRangeLen{T,R,S}(ref::R, step::S, len::Integer, offset::Integer = 1) where {T,R,S}
            len >= 0 || throw(ArgumentError("length cannot be negative, got $len"))
            1 <= offset <= max(1,len) || throw(ArgumentError("StepRangeLen: offset must be in [1,$len], got $offset"))
            new(ref, step, len, offset)
        end
    end

    StepRangeLen(ref::R, step::S, len::Integer, offset::Integer = 1) where {R,S} =
        StepRangeLen{typeof(ref+0*step),R,S}(ref, step, len, offset)
    StepRangeLen{T}(ref::R, step::S, len::Integer, offset::Integer = 1) where {T,R,S} =
        StepRangeLen{T,R,S}(ref, step, len, offset)

    ## range with computed step

    """
        LinRange{T}
    A range with `len` linearly spaced elements between its `start` and `stop`.
    The size of the spacing is controlled by `len`, which must
    be an `Int`.
    # Examples
    ```jldoctest
    julia> LinRange(1.5, 5.5, 9)
    9-element LinRange{Float64}:
     1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5
    ```
    """
    struct LinRange{T} <: AbstractRange{T}
        start::T
        stop::T
        len::Int
        lendiv::Int

        function LinRange{T}(start,stop,len) where T
            len >= 0 || throw(ArgumentError("range($start, stop=$stop, length=$len): negative length"))
            if len == 1
                start == stop || throw(ArgumentError("range($start, stop=$stop, length=$len): endpoints differ"))
                return new(start, stop, 1, 1)
            end
            new(start,stop,len,max(len-1,1))
        end
    end

    function LinRange(start, stop, len::Integer)
        T = typeof((stop-start)/len)
        LinRange{T}(start, stop, len)
    end

    function _range(start::T, ::Nothing, stop::S, len::Integer) where {T,S}
        a, b = promote(start, stop)
        _range(a, nothing, b, len)
    end
    _range(start::T, ::Nothing, stop::T, len::Integer) where {T<:Real} = LinRange{T}(start, stop, len)
    _range(start::T, ::Nothing, stop::T, len::Integer) where {T} = LinRange{T}(start, stop, len)
    _range(start::T, ::Nothing, stop::T, len::Integer) where {T<:Integer} =
        _linspace(float(T), start, stop, len)
    ## for Float16, Float32, and Float64 we hit twiceprecision.jl to lift to higher precision StepRangeLen
    # for all other types we fall back to a plain old LinRange
    _linspace(::Type{T}, start::Integer, stop::Integer, len::Integer) where T = LinRange{T}(start, stop, len)

    function show(io::IO, r::LinRange)
        print(io, "range(")
        show(io, first(r))
        print(io, ", stop=")
        show(io, last(r))
        print(io, ", length=")
        show(io, length(r))
        print(io, ')')
    end

    """
    `print_range(io, r)` prints out a nice looking range r in terms of its elements
    as if it were `collect(r)`, dependent on the size of the
    terminal, and taking into account whether compact numbers should be shown.
    It figures out the width in characters of each element, and if they
    end up too wide, it shows the first and last elements separated by a
    horizontal ellipsis. Typical output will look like `1.0,2.0,3.0,…,4.0,5.0,6.0`.
    `print_range(io, r, pre, sep, post, hdots)` uses optional
    parameters `pre` and `post` characters for each printed row,
    `sep` separator string between printed elements,
    `hdots` string for the horizontal ellipsis.
    """
    function print_range(io::IO, r::AbstractRange,
                         pre::AbstractString = " ",
                         sep::AbstractString = ",",
                         post::Ab
