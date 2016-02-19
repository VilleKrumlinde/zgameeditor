# RemoveModel {#RemoveModel}

Removes a model instance. The are two options:

* Either this component is called from a model instance which is being removed. In this case the RemoveModel.Model property is not specified. Usually, such a RemoveModel is placed in OnUpdate or OnCollision list property of a @ref Model or @ref ModelState.
* The RemoveModel can also be called from outside a Model and in this case it specifies which model instance to remove in the RemoveModel.Model property. If the removed model instance was spawned by reference (@ref SpawnModelSpawnStyle "SpawnModel.SpawnStyle" is Reference), the Model itself can be specified as value of RemoveModel.Model. If the the model instance was spawned as a clone (@ref SpawnModelSpawnStyle "SpawnModel.SpawnStyle" is Clone) or was created by calling the @ref createModel "createModel()" function, the RemoveModel.Model must be set to this instance. It is possible to do it only in expression, see the following example:

      model myModelInstance = createModel(MyModel);
      ...
      @RemoveModel(Model: myModelInstance);

See also: @ref SpawnModel, @ref createModel "createModel()"

## Properties

@dl

@dt Model
@dd The model instance to be removed. See the description above for details.

@dlx
