# DefineCollision {#DefineCollision}

Defines a collision detection test. Collision detection will occur between models with their category set to Cat1 and Cat2. Collision detection only need to be defined once, so a good place to put this component is in @ref ZApplicationOnLoaded "ZApplication.OnLoaded".

For example: PlayerModel has its category set to "1". EnemyShotModel has category "2". Add a DefineCollision component with Cat1 set to "1" and Cat2 set to "2" for making collision detection between player and enemy shots.

Example usage: @ref ZPong sample project.

## Properties

@dl

@dt Cat1
@dd The first model category in this collision test.

@dt Cat2
@dd The second model category in this collision test.

@dt Action
@dd The action performed when models collided. Possible values are:

* Collision - @ref ModelOnCollision "Model.OnCollision" is called on both models when models intersect.
* Stop - one model stops its movement and @ref ModelOnCollision "Model.OnCollision" is called on both models when models intersect.

@dlx
