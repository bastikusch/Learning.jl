

#...............................................................

# Basic class for any kind of cost, eg. Loss and Penalty
abstract type Cost end

# Loss:  L(features, targets, outputs), outputs = f(features)
abstract type Loss <: Cost end

# Supervised loss: L(features, targets, outputs) = L(targets, outputs)
abstract type SupervisedLoss <: Loss end

#= margin-based supervised loss: targets are in {-1, 1} and
L(targets, outputs) = L(targets * outputs) =#
abstract type MarginLoss <: SupervisedLoss end

# distance-based supervised loss: L(targets, outputs) = L(targets - outputs)
abstract type DistanceLoss <: SupervisedLoss end

# unsupervised: L(features, targets, outputs) = L(features, outputs)
abstract type UnsupervisedLoss <: Loss end

# alternative cost type to Loss
abstract type Penalty <: Cost end

#....................................................................

# Return the gradient of the learnable parameters w.r.t. some objective
function grad end
# Updating the gradients of learnable parameters and/or inputs
function grad! end

function isminimizable end
function isdifferentiable end
function istwicedifferentiable end
function isconvex end
function ismarginbased end
function isdistancebased end

#......................................................................

abstract type Transformation end
abstract type Learnable <: Transformation end

#= Baseclass for any prediction model that can be minimized.
Object of subclass contains all information to compute own current loss. =#
abstract type Minimizable <: Learnable end

function update end
function update! end
function learn end
function learn! end
