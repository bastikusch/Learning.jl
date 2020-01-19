module MultiArmedBandit

using AbstractEnvironment
using Distributions

export MultiArmedBandit

#= MultiArmedBandit(k, n = 100; σ = 1)
k: number of available arms (reward for each arm N(0,1)
n: max steps for each episode.
σ: std for all Normal distributions =#
mutable struct MultiArmedBandit{K,D<:Vector} <: AbstractEnvironment
  arms::D
  n::Int
  r::Float64
end

function MultiArmedBandit(k::Int, n::Int = 1000; σ::Real = 1)
  k ≤ 0 && throw(ArgumentError("k must > 0"))
  arms = map(i -> Normal(rand(Uniform(-1, 1)), σ), 1:k)
  MultiArmedBandit{k,typeof(arms)}(arms, n, 0)
end

# change reward distribution of arms
function MultiArmedBandit(x::Vararg{Distribution,N}) where {N}
  y = collect(x)
  MultiArmedBandit{N,typeof(y)}(y, N, 0)
end

AbstractEnvironment.state(::MultiArmedBandit) = nothing
AbstractEnvironment.reward(env::MultiArmedBandit) = env.r
AbstractEnvironment.reset!(env::MultiArmedBandit) = (env.r = 0; env)
AbstractEnvironment.actions(::MultiArmedBandit{K}, s) where {K} = Base.OneTo(K)
AbstractEnvironment.step!(env::MultiArmedBandit, s, a::Int) = (env.r = rand(env.arms[a]); (env.r, nothing))
AbstractEnvironment.maxsteps(env::MultiArmedBandit) = env.n

end  # module MultiArmedBanditEnv
