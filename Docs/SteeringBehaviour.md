# SteeringBehaviour {#SteeringBehaviour}

Represents a steering behaviour.

This component can only be used in the Behaviours property on the @ref SteeringController component.

Example usage: @ref Steering and @ref TripleE sample projects.

See also: @ref SteeringController

External links: [Craig Reynolds: Steering Behaviors](http://www.red3d.com/cwr/steer/)

## Properties

@dl

@dt Kind
@dd Allowed values:

* SeekModel - Seek towards the model set in TargetModel.
* SeparationCategory - Keep separation from models in TargetCategory.
* Noise - A smooth random movement.
* FleeModel - Flee from the model set in TargetModel.
* Expression - Takes a force from an expression.
* WallAvoidance2D - Steer away from walls defined by the models in TargetCategory. The walls boundaries are defined with CollisionBounds of the models.

@dt TargetModel
@dd When kind is set to "Seek" or "Flee", this is the model to seek towards or flee from.

@dt TargetCategory
@dd When kind is "Separation": the category of models that the current model is being separated from. When kind is "WallAvoidance": the category of models that are considered being "walls".

@dt Weight
@dd A value giving this behaviour a weight. The force of each behaviour is multiplied with this value. Use for fine-tuning the effect of each behaviour.

@dt Expression
@dd When Kind is "Expression", this expression calculates a steer vector. The special property "OutVector" is to be set by the expression. The keyword "this" refers to this SteeringBehaviour component. Example:

    this.OutVector.X = 0.5; this.OutVector.Y=0.5;

@dt OutVector
@dd Result steer vector. It is used only in the Expression property to "record" the steering expression result.

@dlx
