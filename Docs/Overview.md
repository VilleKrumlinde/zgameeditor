# Overview {#Overview}

__ZGameEditor__ is a tool for rapid development of games and multimedia applications. It consists of two main components:
* an integrated developer environment (IDE) for developing applications running on Windows, and
* a multi-platform runtime OpenGL-based game engine.

With ZGameEditor you can do:
* small retro style games like Space invaders or Pacman,
* screensavers, or
* animations to use in other applications (see the About-box in ZGameEditor for an example).

Take a look at the included sample projects for an idea of what is possible. Other demo projects come with extensions to demonstrate the provided functionality.

The applications you make with ZGameEditor can be as small as 64 kilobytes or less, and become stand-alone programs that can be distributed royalty-free. 

Of course 64 kb is not a limit of ZGameEditor, you can make games that are much larger. It depends on how much content you import from external files.

# Application Development

Application project is stored as an XML-based file with the .zgeproj extension. The project file is created in and can be loaded to the ZGameEditor IDE which allows to modify its content. When ready, i.e., there are no syntax/compilation errors, the application can be exported to an executable file for the selected platform. The exported file contains both the ZGameEditor game engine and the compiled binary code of the application itself.

To quickly try, debug or fix the application in a more dynamic way, the IDE allows to execute it in an integrated view. You can change the "code" of the application  during its runnig and the execution immediately reflects the performed changes.

![Steps of creating an application](over-development.png)

# Component-based Development

Projects in ZGameEditor are component based. Each component is a GUI representation of a functional code block. This means that your project is built by joining components to form an application.

![Components of application](over-application-components.png)

For example, the @ref KeyPress component tests for user input from the keyboard.

Components have properties. These properties can be set to different values. There are two different kinds of properties in ZGameEditor:

1. value properties, and
2. list properties. 

Value properties are simple values that you plug in, such as "Keys" for the @ref KeyPress component that controls which keys will be tested for key presses.

List properties are lists that hold other components, and/or links to connect components. For the KeyPress component there is a list property called OnPressed. This list accepts command components that will be called when the KeyPress components detects a key press.

# Application Structure and Execution Flow

An ZGameEditor-application is controlled by a component called @ref ZApplication. When a new application is created in the editor, the ZApplication is already created as a root in the component tree.

Here is what this look like within the editor:

![Component tree of an empty application](over-empty-app.png)

A moving graphical object is called a "Model" in ZGameEditor and is represented by the @ref Model component. The model has value properties such as Position, Scale and Rotation, which modify its appearance on screen.

Both ZApplication and Model have list properties. The two most important are OnUpdate and OnRender.

 __OnUpdate__: Components in this list are called between every rendered frame. This is used for application logic and updating model movement.

 __OnRender__: Components in this list are called when the model or application is being rendered on screen. For the model this can be commands for rendering a 3d-mesh or 2d-sprite. The application can use this list for displaying text such as "player score" in a game.

There are also other list properties used in application life-cycle. For more information see the following components: @ref ZApplication, @ref AppState, @ref Model, and @ref ModelState.
 
![Application execution flow](over-execution-flow.png)

# Scripting Language

One of the design philosophies behind ZGameEditor is that users should be able to create applications without requiring lots of programming-knowledge. Simple applications can be created without any programming at all. Just add components and set the properties to fit your needs.

For more advanced applications and fine-tuning, ZGameEditor uses a minimal C-style scripting language. This is mainly used with the @ref ZExpression component. More info about the scripting can be found in @ref ScriptingLanguage.