# Model {#Model}

Defines a moving graphical object. Contains information on how to render and move the object.

To create a instance of a model use the @ref SpawnModel component or @ref createModel "createModel()" function. The created model instance takes all properties from its Model, as they are at the instantiation time, and can change them in runtime. It is also possible to change properties of Model itself in runtime, which will influence all its instances in future.

Example usage: See "BallModel" in @ref ZPong sample project

See also: @ref SpawnModel, @ref createModel "createModel() function"

## Properties

@dl

@dt BaseModel
@dd A model representing "parent" of this model. The current model inherits all properties from its parent; including list properties that are executed before model's list properties. If the child model leaves the default values of properties, they are taken from parent. The child model can override parent's properties by changing default values. This mechanism represents a kind of object-oriented inheritance. More details see in [this Forum post](http://www.emix8.org/forum/viewtopic.php?p=6841#p6841).

@dt Position
@dd The model's current position in 3D-space.

@dt Rotation
@dd The model's current rotation in 3D-space in cycles (one cycle = 360 degrees = 2PI).

@dt Velocity
@dd Current velocity in 3D-space. Assign a velocity (by setting a value in the editor, or using a ZExpression) to make your model move. The unit of Model.Velocity is the amount that the model will move in one second. So for example if Velocity.X is set to 5, then the model will move 5 units per second in the X-axis, which is about half the screen width using the default camera settings.

@dt Scale
@dd Current scale of model. Default value is 1 for the scale in x,y,z directions. A model can be scaled up and down, for instance 0.5 is half size and 2.0 doubles the size.

@dt RotationVelocity
@dd Set a value to this property to animate the models rotation in each axis. For example: Setting RotationVelocity.Y to 1 will rotate the model one full circle per second around the Y axis.

@dt Category
@dd Use a separate category for every kind of model for which a separate collision detection should be used. See @ref DefineCollision. Category can also be used for deleting several model instances at once, see @ref RemoveAllModels component for details.

@dt CollisionBounds
@dd The size of this model when it is tested for collision. This value is interpreted differently depending on the value of the CollisionStyle property:

* CollisionStyle is Rect2D or Rect2D_OBB: The first value represents the width, and the second value is the height.
* CollisionStyle is Sphere3D or Circle2D: The first value is the radius of the sphere.
* CollisionStyle is Box3D: The first value represents the width, the second is height, and the third value is the depth (x,y,z).

@dt CollisionOffset
@dd Offset-value for the models position when it is tested for collision. This can be used for moving the center point of collision from the center point of the geometry, for instance a "weak spot" on a enemy boss. First value is offset in X axis, second is Y.

@dt CollisionStyle
@dd How to determine collision of this model:

* Rect2D - 2-dimensional rectangular collision.
* Sphere3D - 3-dimensional spherical collision.
* Box3D - 3-dimensional rectangular collision.
* Rect2D_OBB - 2-dimensional rectangular collision taking the scale and rotation of the model into account. If your model does not rotate or scale then use Rect2D instead for improved performance.
* Circle2D - 2-dimensional circle collision.

@dt RenderOrder
@dd Order of rendering instances of this model. It can be one of the following:

* Normal - order is given by instantiation time - older instances are rendered first.
* Depthsorted - farther objects are rendered first. This can be used for transparent geometry in 3D scenes.

@dt @anchor ModelCollidedWith CollidedWith
@dd Read only. Reference to a model which collided with this model. This property should be read from the @ref ModelOnCollision "OnCollision" section.

@dt @anchor ModelPersonality Personality
@dd Read only. A value that is automatically assigned a random value between 0 and 1 when a model is spawned. Use this value in expressions for making every instance of a model slightly unique. For example see BoidModel in @ref Steering sample project, it uses the value of Personality for modifying the locally defined mesh to give every boid a separate appearance.

@dlx

## List Properties

@dl

@dt Definitions
@dd List of private data for this model. Use the @ref Variable component to define private variables. Every instance of a model will have their own copy of the components in this list. This allows creating models that have unique geometry for every instance. Other components such as Mesh or Bitmap can be placed here as well. However, the data is created during spawning, so only use very small meshes or bitmaps, otherwise there will be noticeable delays in your game when the models are spawned. See "MonkeyModel" in @ref TripleE sample project

@dt @anchor ModelStates States
@dd A list of @subpage ModelState components for this model.

@dt @anchor ModelOnSpawn OnSpawn
@dd Called once when this model is spawned (created). Use components that initialize your models local variables and model properties here.

@dt @anchor ModelOnUpdate OnUpdate
@dd Called between every frame update. Here you should put components that update your models position.

@dt @anchor ModelOnCollision OnCollision
@dd Called when this model has collided with another model. Collision detection must have been enabled using a @ref DefineCollision component. A @ref ZExpression defined here can examine which category the model has collided with using the @ref ModelCollidedWith "CollidedWith" property.

@dt @anchor ModelOnRender OnRender
@dd This component list is called when it's time to render this model to the screen. Use render command components such as @ref RenderMesh to render your models.

@dt OnRemove
@dd Called when the model is removed.

@dlx
