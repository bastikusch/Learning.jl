module AbstractEnvironment

export
  AbstractEnvironment,
  reset!,
  step!,
  reward,
  state,
  finished,
  actions,
  maxsteps

# Abstract environment class
abstract type AbstractEnvironment end

# Functions.........................................................

# Reset an environment.
function reset! end

# r, s′ = step!(env, s, a) (get reward and new state)
function step! end

# check if finished
finished(env::AbstractEnvironment, s′) = false

# A = actions(env, s), return a list of valid actions from state
function actions end

# s = state(env), return the current state of the environment
state(env::AbstractEnvironment) = env.state

# r = reward(env), return the current reward of the environment.
reward(env::AbstractEnvironment) = env.reward

# maxsteps(env)::Int, return max steps in single episode, default (unlmtd).
maxsteps(env::AbstractEnvironment) = 0

#..............................................................


# Implement this interface for a new policy

abstract type AbstractPolicy end

#= a = action(policy, r, s, A)
r: last reward
s: current state
A = actions(env, s): set of valid actions =#
function action end

end
