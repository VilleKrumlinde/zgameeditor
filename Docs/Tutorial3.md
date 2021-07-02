# Tutorial 3: The "OneButton" Game {#Tutorial3}

In this tutorial we will make a very simple game: OneButton. This is a game where a single button is used for control.

It is best if you have read the first two tutorials before starting this one.

Here is a picture illustrating the gameplay:

![Gameplay illustration](tut3-gameplay.png)

First we will create the main active components in our game: Player and Walls. In ZGameEditor a moving graphical entity and its behavior is called a @ref Model. So we need a @ref CreatingPlayerModel "PlayerModel" and a @ref CreatingWallModel "WallModel".

# Creating the Player Model {#CreatingPlayerModel}

Start with defining the player model.

1. Start ZGameEditor.

2. To start a new project, select "File / New project / (empty project)" menu.

A model needs 3D-geometry for it to display itself. This is called a Mesh. The first component we create is the PlayerMesh.

1. Add (Ctrl-A) a @ref Mesh component to the __Content__ list property.

2. Set the following property on Mesh:

  * __Name__ = `PlayerMesh`
  
3. Now expand the PlayerMesh tree node to expose the __Producers__ list property. This is a list of component that is used to generate (or produce) the mesh.

4. Add a @ref MeshSphere component to the Producers tree node.

5. Set the following properties:

  * __Scale__ = `0.5 0.5 0.5`
  * __ZSamples__ = `3`
  * __RadialSamples__ = `4`

  If you click on the PlayerMesh node you will see that we now have created a small diamond shaped mesh.

  ![PlayerMesh node preview](tut3-player-preview.png)


In this game we want to use flat-shaded geometry. So the next component we need is a Material.

1. Add a @ref Material component to the __Content__ list property.

2. Set the following properties:

  * __Name__ = `SpriteMaterial`
  * __Shading__ = Flat

And create another material that we will use later on in this tutorial - the TextMaterial.

1. Add a @ref Material component to the __Content__ list property.

2. Set the following properties:

  * __Name__ = `TextMaterial`
  * __Color__ = click the color and set Red: 255, Green: 255, Blue: 0 in the color selector dialog
  * __Blend__ = Alpha/OneMinusSourceAlpha

Now we are ready to create the model representing the player.

1. Add a @ref Model component to the __Content__ list property.

2. Set the following properties:

  * __Name__ = `PlayerModel`
  * __CollisionBounds__ = `0.5 0.5`

  Notice that __CollisionBounds__ have four edit fields, but we only set the first two. This is because the default collision style in ZGameEditor is 2D rectangular collision. The first value is width and the second is height.
  
Next we will control how the PlayerModel will be rendered on screen.

1. Expand PlayerModel node to display the model's component lists.

2. Add a @ref UseMaterial component to the __OnRender__ list.

3. Set the following properties:

  * __Material__ = SpriteMaterial

  We will use the same material for both the player and walls. In order to display them in different color we use a @ref RenderSetColor component.

4. Add a @ref RenderSetColor component to __OnRender__.

5. Set the following property:

  * __Color__ = click the color and set Red: 217, Green: 252, Blue: 245 in the color selector dialog

Now we can render the mesh.

1. Add a @ref RenderMesh component to __OnRender__.

2. Set the following property:

  * __Mesh__ = PlayerMesh

![Component tree so far](tut3-tree1.png)

That's it for displaying the player model. Now let's add some logic for how the player will move. This is done in the __OnUpdate__ component list of the model.

1. Add a @ref KeyPress component to __OnUpdate__.

2. Set the following properties:

  * __Comment__ = `Test mouse-click or up-key`
  * __Keys__ = `^{`

  The KeyPress component tests for user input. __Comment__ property is used for typing comments that will help remembering how the application works without looking at all the property values. __Keys__ property is set to which input to test, in this case we test for Arrow-Up key (represented by special character "^") and left mouse button click ("{").

3. Expand the __KeyPress__ component to make the __OnPressed__ list property visible.

4. Add a @ref ZExpression component to __OnPressed__ on __KeyPress__.

5. Click the __Expression__ property on the __ZExpression__ component. This will make the expression custom text editor appear.

6. Write the following code in the expression custom text editor:

        CurrentModel.Velocity.Y+=10.5*App.DeltaTime;

7. Click the "Compile" button to save the expression.

So what did we just do? When the user clicks the mouse or press the arrow Up-key, the ZExpression will be executed. The expression adds a value to the Y (vertical) velocity. This makes the model accelerate upwards.

Next, add some movement logic and constraints to the player model:

1. Add a @ref ZExpression component to __OnUpdate__ on PlayerModel.

2. Click the __Expression__ property on the __ZExpression__ component. This will make the expression custom text editor appear.

3. Write the following code in the expression custom text editor:

        CurrentModel.Velocity.Y-=5.5*App.DeltaTime;
        CurrentModel.Velocity.Y=clamp(CurrentModel.Velocity.Y,-6,5);

        if(CurrentModel.Position.Y<-4) {
          CurrentModel.Position.Y=-4;
          CurrentModel.Velocity.Y=0;
        }

        if(CurrentModel.Position.Y>4) {
          CurrentModel.Position.Y=4;
          CurrentModel.Velocity.Y=0;
        }

        CurrentModel.Rotation.X+=0.25 * App.DeltaTime;

4. Click the "Compile" button to save the expression.

This is what the expression does:

  1. Decrease the Y-velocity: This gives a downward acceleration which simulates gravitation.
  2. Test that velocity do not get too high.
  3. Test that the player do not move outside the screen.
  4. Rotates the model over the X-axis.

# Delta-time Explained {#DeltaTimeExplained}

Why the multiply with @ref ZApplicationDeltaTime "App.DeltaTime"? This is to make sure your game works in the same speed independent of the frame update interval.

Imagine a very slow computer that is only able to display 10 frames per second. __OnUpdate__ is called between each frame. DeltaTime is set to the time-difference between last frame. So on the slow computer this will be 1/10 = 0.1 seconds. After one second of game time, velocity will be 0.1 * 10 frames = 1.0.

On the other hand, a fast computer may display 100 frames per second. Then DeltaTime becomes 1/100 = 0.01 seconds. So after one seconds we have velocity 0.01 * 100 = which is again 1.0. Thanks to multiplying with DeltaTime we have the same effect on both the slow and the fast computer after one second.

Without DeltaTime, velocity would be 10 times higher on the faster computer. That would make the game too slow and boring on the slow computer, and too fast and impossible to play on the fast computer!

So that is why DeltaTime is important to use!

# Defining Game Variables {#DefiningGameVariables}

Variables are components that are used in expressions for holding values that are important for the application.

In our game, we will have two such values to keep track of:

* GameTime: this will hold the time in seconds that the current game have been played. We will use this for increasing difficulty in the game.

* Score: for keeping track of player score.

Here is how you create the variables:

1. Add a @ref Variable component to __OnLoaded__ on the application.

2. Set the following property:

  * __Name__ = `GameTime`
  
3. Add a @ref Variable component to __OnLoaded__ on the application.

4. Set the following property:

  * __Name__ = `Score`

![Component tree after defining variables](tut3-tree2.png)

# Creating the Wall Model {#CreatingWallModel}

Next is the Wall model. Walls are the obstacles that the player have to dodge.

We create the wall in the same way we created the player. Start with the geometry.

1. Add (Ctrl-A) a @ref Mesh component to the __Content__ list property on App.

2. Set the following property on Mesh:

  * __Name__ = `WallMesh`

3. Expand the WallMesh tree node to expose the __Producers__ list property.

4. Add a @ref MeshBox component to the __Producers__ list property.

5. Set the following property:

  * __Scale__ = `0.25 1.2 3`

  Click the WallMesh component for preview. The wall is a vertical standing box.

  ![WallMesh preview](tut3-wall.png)


Continue to create the Model for the wall.

1. Add a @ref Model component to the __Content__ tree node.

2. Set the following properties:

  * __Name__ = `WallModel`
  * __Category__ = `1`
  * __CollisionBounds__ = `0.25 2.2`

  Category and CollisionBounds are used for collision detection.

Add rendering of the wall:

1. Expand WallModel to display the models component lists.

2. Add a @ref UseMaterial component to __OnRender__ on WallModel.

3. Set the following property:

  * __Material__ = SpriteMaterial

Use an orange color for the walls:

1. Add a @ref RenderSetColor component to __OnRender__ on WallModel.

2. Set the following property:

  * __Color__ = click the color and set Red: 255, Green: 130, Blue: 0 in the color selector dialog

Render wall geometry:

1. Add a @ref RenderMesh component to __OnRender__ on WallModel.

2. Set the following property:

  * __Mesh__ = WallMesh

Now for the "update logic" for the wall. The wall will move to the left. We need to check if the wall have reached outside the screen and if so, remove it.

1. Add a @ref Condition component to __OnUpdate__.

2. Set the following properties:

  * __Comment__ = `Remove wall if outside screen`
  * __Expression__ = `return CurrentModel.Position.X<-7.5;`

  The expression will return true when the model's X (horizontal) position is lower than -7.5. The default camera in ZGameEditor has origin in the center of the screen with X rising from a negative value in the far left, to a positive value in the far right of the screen.

  In this case -7.5 is a position outside the left edge of the screen, including some extra space to make sure no part of the wall is visible when it is removed.

3. Expand Condition to display __OnTrue__ and __OnFalse__ list properties.

4. Add a @ref RemoveModel component to __OnTrue__.

  No properties are needed for RemoveModel, it just removes the current model from the screen.

We want to initiate every wall with some unique property values on creation. A @ref Model instance is created when the @ref SpawnModel component is executed. The first thing a newly spawned model do is running the commands in the OnSpawn list property.

1. Add a @ref ZExpression component to __OnSpawn__ on WallModel.

2. Click the __Expression__ property on the ZExpression component.

3. Write the following code in the expression custom text editor:

        CurrentModel.Position.X=8;
        CurrentModel.Position.Y=random(0,4);
        CurrentModel.Velocity.X=-3 - clamp(GameTime*0.1,0,2.5);

4. Click the "Compile" button to save the expression.

The expression does the following:

  1. Set the X position to 8, this just outside the right side of the screen.
  2. Sets the Y (vertical position) to a random value between -4 and 4.
  3. Set the X velocity to a negative value. This makes the wall move left. The speed of the wall will increase with the `GameTime` variable, this makes the game more difficult over time.

This is how the finished wall model should look like on your screen:

![WallModel's component tree](tut3-tree3.png)

We have created our player and wall objects so far. Let's move on to define the application logic.

# Application States {#ApplicationStates}

An application state is a part of your application that has a distinct behavior, such as waiting for a keypress or displaying a message. Only one state can be active at once. This is called the __current application state__.

Some examples of typical states for a game application:

* Game playing
* Game paused
* Game over
* Player loses a life
* Restart current level

In our game we will have two different application states:

* The title screen: display a message and wait for a keypress.
* The PlayingState: the state when the game is being played.

The ZGameEditor component that defines a application state is called @ref AppState.

@note Note: It is not required to use AppState components. Simple applications may not need them at all, and different application behavior can be achieved with using Condition components in OnUpdate and OnRender. But using AppStates can make your project cleaner and require less effort when you want to make changes.

First we create the PlayingState.

1. Add an @ref AppState component to __States__ on App.

2. Set the following property:

  * __Name__ = `PlayingState`
  
Now define what will happen when when the player wants to start a new game. This is done in __PlayingState.OnStart__.

1. Expand PlayingState to display the list properties.

2. Add a @ref SpawnModel component to __OnStart__ on PlayingState.

3. Set the following properties:

  * __Model__ = PlayerModel
  * __Position__ = `-2 0 0`

  This will spawn the player model at the X=-2 position, a bit left of center.

Next for some variable initialization.

1. Add a @ref ZExpression component to __OnStart__ on PlayingState.

2. Set the following properties:

  * __Comment__ = `Init game vars`

3. Click on the __Expression__ property.

4. Write the following code in the expression custom text editor:

        GameTime=0;
        Score=0;

5. Click the "Compile" button to save the expression.

The expression do just what it looks like: set the GameTime and Score variables to zero when the game starts.

@ref AppStateOnUpdate "AppState.OnUpdate" is called once between every frame update, just like @ref ModelOnUpdate "Model.OnUpdate". On our PlayingState we will add logic to update the GameTime variable and spawn new walls.

1. Add a @ref ZExpression component to __OnUpdate__ on PlayingState.

2. Click on the __Expression__ property.

3. Write the following code in the expression custom text editor:

        GameTime+=App.DeltaTime;

4. Click the "Compile" button to save the expression.

The expression just increase our __GameTime__ variable with the amount of time that has elapsed since last frame update.

5. Add a @ref Timer component to __OnUpdate__ on PlayingState.

6. Set the following properties:

  * __Name__ = WallTimer
  * __Comment__ = Emit walls
  * __Interval__ = `1`

  The Timer component executes the __OnTimer__ list on a set interval, in this case 1 second.

7. Expand WallTimer to display the __OnTimer__ list.

8. Add a @ref SpawnModel component to __OnTimer__.

9. Set the following property:

  * __Model__ = WallModel

10. Add a @ref ZExpression component to __OnTimer__.

11. Click on the __Expression__ property.
Write the following code in the expression custom text editor:

        Score+=100;
        WallTimer.Interval=1.5 - clamp(GameTime*0.02,0,1);

12. Click the "Compile" button to save the expression.

The first line of expression increase the score with 100. The second line sets a new interval for the next time the __OnTimer__ list will be executed. The interval is decreased with GameTime to make walls appear faster over time.

![PlayingState's component tree](tut3-tree4.png)

The PlayerState is now finished. Let's move on and define TitleState.

1. Add an @ref AppState component to __States__ on App.

2. Set the following property:

  * __Name__ = `TitleState`

Now for the "update behavior". When the user press space on the keyboard, the game will begin.

1. Expand TitleState to display the list properties.

2. Add a @ref KeyPress component to __OnUpdate__ on TitleState.

3. Set the following property:

  * __Keys__ = press spacebar to type a single " " character

4. Expand KeyPress to display the list properties.

5. Add a @ref SetAppState component to __OnPressed__.

6. Set the following property:

  * __State__ = PlayingState

The title state will display a message.

1. Add a @ref UseMaterial component to __OnRender__ on TitleState.

2. Set the following property:

  * __Material__ = TextMaterial

3. Add a @ref RenderText component to __OnRender__ on TitleState.

4. Set the following properties:

  * __Text__ = `PRESS SPACE TO START`
  * __Scale__ = 0.75

![TitleState's component tree](tut3-tree5.png)
  
Time to save the project if you haven't already done so. Select "File / Save Project" from the main menu. Name the project "OneButton".

_Note: There is no global undo functionality in ZGameEditor, except of "Undo delete" if you remove a component, so it is a good idea to save your work often!_

We need to tell ZGameEditor that the initial application state is the TitleState.

1. Add a @ref SetAppState component to __OnLoaded__ on App.

2. Set the following property:

  * __State__ = TitleState

# Collision Detection and Displaying the Score {#CollisionDetection}

Almost done now. Three more things are needed:

  1. Define that we want to have collision detection between player and walls.
  2. Display the current score.
  3. Define what will happen when the player collides with a wall.

First the collision:

1. Add a @ref DefineCollision component to __OnLoaded__ on App.

2. Set the following properties:

  * __Comment__ = `Player vs Walls`
  * __Cat2__ = `1`

Then the score display. We render the text from App.OnRender instead of PlayingState.OnRender because we want the score to be displayed in both state. That way, the last score will be visible on title screen.

1. Add a @ref UseMaterial component to __OnRender__ on App.

2. Set the following property:

  * __Material__ = TextMaterial

3. Add a @ref RenderText component to __OnRender__ on App.

4. Set the following properties:

  * __Comment__ = `Display score`
  * __TextFloatRef__ = `Score.Value`
  * __X__ = `0.62`
  * __Y__ = `-0.87`

  The __TextFloatRef__ property allows the value of a property on another component to be displayed as text.

Now for the definition of behavior when the player collides with a wall. Clear screen of all models and return to the title screen:

1. Add a @ref RemoveAllModels component to __OnCollision__ on PlayerModel.

2. Add a @ref SetAppState component to __OnCollision__ on PlayerModel.

3. Set the following property:

  * __State__ = TitleState

# The Finished Game {#FinishedGame}

That's it! First save your project again, and then try pressing F9 to start the game.

![Running game](tut3-run.png)

The finished tutorial source file can be downloaded here: [OneButton.zip (external link)](http://zgameeditor.org/files/zzdc/OneButton.zip). The final version also have some simple sound effects added.

Some suggestions for improvements:

* modify the difficulty to make it easier or harder,
* give the walls different heights, and
* use a separate color for each wall.