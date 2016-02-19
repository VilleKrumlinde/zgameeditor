# SteeringController {#SteeringController}

Add a SteeringController to the Model Update command list to make it controlled using "steering". This is a powerful feature for making "flocking" style movement without using scripting.

Example usage: @ref Steering and @ref TripleE sample projects.

See also: @ref SteeringBehaviour

External links: [Craig Reynolds: Steering Behaviors](http://www.red3d.com/cwr/steer/)

## Properties

@dl

@dt Mass
@dd The models mass. Values smaller than one makes the model change direction more quickly. A value higher than one makes it "heavy" and slow reacting.

@dt MaxSpeed
@dd Maximum speed (velocity) of the model.

@dt MaxForce
@dd Max "force" that can be applied to the model during a evaluation of the steering behaviours.

@dt AdjustHeading
@dd If this property is set then the models heading is ajdusted to match the direction it is moving.

@dt Radius
@dd The radius of the model, used when calculating separation using the separation steering behaviour.

@dlx

## List Properties

@dl

@dt Behaviours
@dd A list of @subpage SteeringBehaviour components.

@dlx
