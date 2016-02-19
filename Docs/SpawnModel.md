# SpawnModel {#SpawnModel}

Creates an instance of a model.

See also: @ref Model, @ref createModel "createModel() function"

## Properties

@dl

@dt Model
@dd A reference to the model that will be spawned.

@dt Position
@dd If not set to zero then this property is the position that the model will have when it has been spawned.

@dt Scale
@dd If not set to identity (1,1,1) then this property is the scale that the model will have when it has been spawned.

@dt Rotate
@dd If not set to zero then this property is the rotation that the model will have when it has been spawned.

@dt @anchor SpawnModelSpawnStyle SpawnStyle
@dd Can be set to either 'Clone' or 'Reference'. If set to 'Clone' the model and all its children will be cloned for each instantiation. If set to 'Reference' it will spawn the actual model it refers to. A model spawned with 'Reference' can only be spawned once, then it has to be removed before it can be respawned.

@dt UseSpawnerPosition
@dd If this property is set then the spawned model will use the same position as the model that issues the SpawnModel-command. Useful when a model should emit other models, like a model firing missiles for instance. Note that if the Position-property also is set, then this value will be added to the current model to get the final position. This can be used to offset the spawn position (i.e. shots are emitted from the front of the ship).

@dt SpawnerIsParent
@dd Sets CurrentModel as 'Parent' to the spawned model. If a model has a parent then it will automatically be removed when its parent is removed.

@dlx
