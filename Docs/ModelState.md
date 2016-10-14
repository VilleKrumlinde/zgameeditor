# ModelState {#ModelState}

Represents a model state. States are used to separate different behaviors of a model. Examples of states for an enemy in a action game could be Attacking, Idling, TakeDamage and Dying.

This component can only be used in the @ref ModelStates "Model.States" property.

Example usage: The states for "BoidModel" in @ref Steering sample project

See also: @ref Model, @ref SetModelState

## List Properties

@dl

@dt Definitions
@dd Local data for this state.

@dt OnStart
@dd Called when this state is started.

@dt OnUpdate
@dd Called between every frame when this state is active.

@dt @anchor ModelStateOnRender OnRender
@dd A list of render commands for this state.

@dt OnLeave
@dd Called when the model leaves this state, i.e. when SetModelState sets another state.

@dt OnCollision
@dd Called when the model has collided with another model. See Model.OnCollision.

@dlx
