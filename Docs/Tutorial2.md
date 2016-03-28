# Tutorial 2: The "Hello World" Application {#Tutorial2}

This tutorial will guide you through making the "Hello World" application, which is simply a program that displays the text "Hello World" on the screen.

# Displaying Text {#DisplayingText}

Here is how we will begin:

1. Start ZGameEditor.

2. To start a new project, select "File / New project / (empty project)" menu.

  You will now have a blank project with a single @ref ZApplication component in the component tree.

Add rendering of text to the project:

1. Click on the __OnRender__ node in the tree.

  This is the command list that is executed each time the application will be rendered (displayed) on the screen.

2. Add a @ref RenderText component from @ref ComponentSelector "Component selector", or press the "+" icon in the toolbar, or press Ctrl+A and select RenderText. Click OK to close the dialog, or double click the RenderText component.

3. In the @ref PropertyEditor, click the edit box next to the __Text__ property and type `HELLO WORLD`.

4. Press "Tab" to exit the edit box and commit the value.

Now save your project:

1. Click on the "Save" icon in the toolbar, or by select "File / Save Project" menu, or press Ctrl+S key. Name your project `HelloWorld`.

  This is how it should look on your screen now:
  
  ![ ](tut2-tree1.png)


Run the project:

1. Press F9 key, or select the "Run" icon in the toolbar. This is what you will see:

  ![ ](tut2-run1.png)

2. Press Escape key to quit the HelloWorld program and return to the editor.

3. Open a Windows Explorer window and look into the folder where you saved your project. You will find the program "HelloWorld.exe". This was created when you selected the "Run" action. Try starting it if you like. The program is independent from ZGameEditor and can be executed on any Windows computer with OpenGL support.

  ![ ](tut2-folder.png)

  Wait, why is the file larger than 64kb? This is because when you run a game from inside the tool (using F9), they are uncompressed. The size is around 100kb before compression. When you select "Build release" from the menu you will get a small exe file (thanks to upx compressor external tool).

4. Return to the ZGameEditor window.

# Animation {#Animation}

Now we will add some simple animation.

1. Give the @ref RenderText component a name by setting the __Name__ property to `MyText`.

2. Click on the __OnUpdate__ node.

3. Add a @ref AnimatorSimple component from @ref ComponentSelector "Component selector", or press the "+" icon in the toolbar, or press Ctrl+A and select AnimatorSimple.

4. Set the following properties on AnimatorSimple:

  * __Duration__ = `2`
  * __AutoStart__ = checked
  * __Target__ = `MyText.Y` (the value is an @ref ScriptingLanguage "expression" specified in @ref CodeEditor "Code editor")
  * __FromValue__ = `-0.5`
  * __ToValue__ = `0.5`
  * __Smooth__ = checked
  * __AutoReverse__ = checked
  * __RepeatCount__ = `100`

  You have created an animator that will smoothly animate the value of our RenderText components __Y__ property from -0.5 to 0.5. The coordinate system for text is in the range -1 to 1 with 0 being center.

5. Run the application. The text should now smoothly move up and down.

6. Press Escape key to return to the editor.

# User Interaction {#UserInteraction}

Time to add some interactivity.

1. Select the @ref RenderText component again

2. Make the text slightly larger by setting the __Scale__ property to `1.5`.

3. Click on the edit box of the __RenderCharExpression__ property.

4. The @ref CodeEditor "Code editor" appears. You can make the editor larger by click and drag the borders with the mouse.

5. Add a new line after the lines that start with "//" (comments) and type:

        CharY = 1 + sin(App.MousePosition.Y*CharI);

  Click the "Compile" button.

  ![ ](tut2-code.png)
  
  The expression will assign a separate "Y"-value (vertical position) for each letter in the displayed text. The position is dependent on the position of the mouse.

6. Run the application by pressing F9.

  When you move the mouse up and down the letters follows the mouse.

# Adding Transparency {#Transparency}

You will notice there are ugly borders where the characters overlap. This is because by default graphics in ZGameEditor are not drawn with transparency. Here are the steps to fix that.

Add a @ref Material component:

1. Click the __Content__ node in the component tree.

2. Add a @ref Material component from @ref ComponentSelector "Component selector", or press the "+" icon in the toolbar, or press Ctrl+A and select Material.

3. Set the following properties on Material:

  * __Name__ = `TextMaterial`
  * __Blend__ = `Alpha/OneMinusSourceAlpha`

Add a component that will use the material:

1. Click the __OnRender__ node in the @ref ProjectTree.

2. Add a @ref UseMaterial component from @ref ComponentSelector "Component selector", or press the "+" icon in the toolbar, or press Ctrl+A and select UseMaterial.

3. Set the following properties on UseMaterial:

  * __Material__ = TextMaterial (select from the dropdown menu)
  
4. Click the "Move up" icon (up arrow) in the toolbar, or right click and select "Move up" from the context menu. This will move the UseMaterial component before RenderText.

  The component tree should now look something like this:

  ![ ](tut2-tree2.png)

5. Press F9 to run the application. The text is now rendered with transparency.

  ![ ](tut2-run2.png)

6. Press Escape key to return to the editor.

# Adding Sound {#AddingSound}

Just for fun, we will now add a simple sound effect.

Create the sound component:

1. Click the __Content__ node in the component tree.

2. Add a @ref Sound component from @ref ComponentSelector "Component selector", or press the "+" icon in the toolbar, or press Ctrl+A and select Sound.

3. Set the following properties on Sound:

  * __Name__ = MySound
  * __Length__ = `0.2`
  * __UseFilter__ = check
  * __FilterQ__ = `0.7`

Next, add components that will play our sound.
  
1. Click the __OnUpdate__ node in the component tree.

2. Add a @ref PlaySound component from @ref ComponentSelector "Component selector", or press the "+" icon in the toolbar, or press Ctrl+A and select PlaySound.

3. Set the following properties on PlaySound:

  * __Sound__ = MySound
  * __ReplayDelay__ = `0.25`

And an expression to change the sound over time:

1. Add a @ref ZExpression component from @ref ComponentSelector "Component selector", or press the "+" icon in the toolbar, or press Ctrl+A and select ZExpression.

2. Type in the @ref CodeEditor "Code editor":

        MySound.BaseNoteNr = 24+frac(App.Time)*18;
        MySound.FilterCutoff = 0.5 + App.MousePosition.X*0.48;

3. Click the "Compile" button.

4. Press F9 to run the application.

  You will now here a sound playing. When you move the mouse in the horizontal axis the sound will be sharper or more muted. This is because the expression assigns a value to the sound filter depending on the position of the mouse.

That was it! Your first application using ZGameEditor.

The tutorial source file can be downloaded here: [HelloWorld.zip (external link)](http://zgameeditor.org/files/zzdc/HelloWorld.zip).
