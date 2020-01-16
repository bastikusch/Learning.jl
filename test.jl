# trying out some stuff as of now


mutable struct Episode{E<:AbstractEnvironment,P<:AbstractPolicy,F<:AbstractFloat}
  env::E
  policy::P
  total_reward::F # total reward of the episode
  last_reward::F
  niter::Int      # current step in this episode
  freq::Int       # number of steps between choosing actions
  maxn::Int       # max steps in an episode - should be constant during an episode
end
