# Tutorial 1: Getting Started Using the Editor {#Tutorial1}

This tutorial will guide you through the basic structure and functionality of ZGameEditor IDE.

After starting the ZGameEditor, the the project you had open when you quit the last time is automatically opened in editor.

To start a new project, select _File / New project_ menu item. This is how the editor looks like:

![ZGameEditor window](tut1-editor-structure.png)

The main window is split into the following panels:

1. @ref ProjectTree
2. @ref PropertyEditor
3. @ref PreviewWindow
4. @ref CustomPropertyEditor
5. @ref LogWindow
6. @ref ToolbarAndMenu

# Project Tree {#ProjectTree}

This panel displays a tree of the components that the current project contains. This works similar to a folder-tree in a file explorer: you can create folders (@ref Group) and arrange your components in sub-folders.

The root of the tree is always the @ref ZApplication component. This represents your application. "App" is the default name used to refer to this component in expressions.

Right mouse button click brings up the context menu with useful functions that can be performed on the currently selected component(s).

Mouse drag-and-drop, Ctrl+Up and Ctrl+Down are enabled to reorder components. For full list of keyboard shortcuts see @ref KeyboardShortcuts "here".

# Property Editor {#PropertyEditor}

When you select a component in the component tree (by clicking to highlight it), the properties of that component are displayed in the property editor panel.

The properties are displayed in a vertical list. When you click on a property, you can edit its value.

![Property editor](tut1-property-editor.png)

1. The name of the property
2. The value of the property. Click to edit the value
3. When the name of the property is displayed in __bold__ font, it means that the value is changed from the default value. This is useful for remembering what values you have changed.
4. Grey text color means that the property is read-only and cannot be changed. 

The different types of properties:

* String property: A text value. Just type the new text here.
* Floating point number: Type the new value. The "." character is used as a decimal separator.
* Boolean property: this is a checkbox indicating on or off.
* Vertex property: three separate floating point input fields for the x, y and z values.
* Set property: a drop-down with the different allowed values is displayed.
* Color property: the current color is shown - left/right-click on the color to open a color select dialog or options.
* Component reference: a drop-down with the names of suitable components in the project are shown, for example "PlayerModelBitmap".
* Property reference: edit box where you type the name of a floating point property in another component, example: "PlayerModel.Position.X". When you start to type in a numeric floating point value the edit box changes color to indicate that the value is being changed.

Press "Tab" or click outside the property entry area to commit the new value.

@anchor ComponentSelector When a list property (e.g. OnLoaded, OnUpdate, or Content properties/sections of the @ref ZApplication component) is selected in the @ref ProjectTree, the Property Editor shows a list of all components:

![Component selector](tut1-component-list.png)

By left mouse clicking on a component, this will be added as a child of the selected list property. This is a very effective way of adding (possibly multiple) new components to your project.

# Preview Window {#PreviewWindow}

This window displays different information depending on the currently selected component. The following preview windows can be shown:

* Model/Mesh preview: Displayed when a Model or Mesh component is selected. The 3D-model is displayed. Click and drag in the window to change camera position.
* Bitmap preview: Displayed when a Bitmap component is selected.
* Audio preview/editor: Displayed when a Sound component is selected.
* Application preview: Displayed when the ZApplication component is selected. 

The preview window allows you to instantly see the effect of changed values of properties. An example:

1. Select "File" - "Open Project" and choose "ZPong" sample project
2. In the component tree: select the "BatModel" component. It is located under Content / Models / BatModel.
3. The player bat is displayed in the preview window.
4. Now try changing the value of the "Scale" property, either by typing in a new value "1.5" or using the "Edit value" slider in the Custom property editor.
5. You'll see the models scale changed in the preview window. 

![Preview window](tut1-preview-window.png)

Now try another feature __Lock Preview__:

Lock Preview allows you to preview one component while selecting and editing other components that affect the previewed component. It is activated either by the padlock icon in the toolbar, by pressing Ctrl+L, or by context menu of the selected project component.

An example:

1. Select the "BallMesh" component. The ball is displayed in the mesh preview window.
2. Right-click the component and select "Lock preview".
3. Now select the "MeshSphere" component which you will find under "Producers" on the "BallMesh" component.
4. Try changing the Scale property and watch the scale of the ball change. 

Another example:

1. Select the "App" component at the top of the component tree.
2. Right-click the "App" component and select "Lock preview".
3. Select "MeshSphere" component under Content / Models / BallMesh / Producers.
4. Click the "Start" button to start the ZPong game in the preview window.
5. Now try changing the Scale property and watch the scale of the ball change while the game is playing. 

![Lock preview](tut1-lock-preview.png)

There are two ways to test your project:

* You can use the preview window "Start" and "Stop" buttons to run (to toggle running by ALT+Enter) your project in the editor. Its possible to lock the top @ref ZApplication component and then modify the component properties in real time to fix problems or to tune application's behavior. This is hugely faster than trial and error compiling like other game making programs. Please note that the preview window runs with the overhead of the editor, so results are sometimes slow.

* Alternatively, you may compile and run application's executable (.EXE file). Just click the "Run" button in the toolbar or press F9.

# Custom Property Editor {#CustomPropertyEditor}

This panel displays different content depending on the type of property that is currently selected in the @ref PropertyEditor.

The following custom editors are available:

__Floating point value slider__: A horizontal slide-control is displayed when a floating point number property is selected. Drag the slider to change the value of the property.

![Floating point value slider](tut1-float-slider.png)

@anchor CodeEditor __Code editor__: This editor is displayed when an expression type property of a component is selected. Allows you to edit an expression using a syntax highlighted text editor providing content assistant functionality.

See the available keyboard shortcuts @ref KeyboardShortcuts "here". For more information about the syntax and usage of expression/scripting language see @ref ScriptingLanguage "here".

![Code editor](tut1-code-editor.png)

@anchor ShaderEditor __GLSL Shader editor__: This editor is displayed when a shader source property of the @ref Shader component is clicked.

For more info see [OpenGL Shading Language on Wikipedia (external link)](https://en.wikipedia.org/wiki/OpenGL_Shading_Language).

Click "Compile" button to commit and validate the expression. In the case of syntax error this is displayed in a error line below and is also written to the @ref LogWindow.

# Log Window {#LogWindow}

This panel displays internal messages from the ZGameEditor-engine. If the editor doesn't behave as you expect, check out the log window.

# Toolbar and Menu {#ToolbarAndMenu}

A standard toolbar for actions that also can be accessed from the menus above. Hover the mouse pointer over a button for displaying a tool-tip that explains the action of that button.

# Customizing the Editor {#CustomizingEditor}

To customize the editor, you can perform the following actions:

__Change style (skin)__: ZGameEditor supports Delphi XE8 style files to change visual appearance of the editor. Just select one style from the "Style" menu and appearance changes immediately.

You can download additional style files from [here (external link)](http://www.zgameeditor.org/files/zge%5Fstyles.zip), copy the unpacked files to the `<install dir>\Styles` directory, and restart ZGameEditor.

__Change position of Property Editor__: Select the "Tools / Settings..." menu. In the opened "Settings" dialog, change "Property editor position" to: Left or Middle and press the "OK" button. After restarting, position of the @ref PropertyEditor panel changes accordingly.

__Detaching editors__: @ref CustomPropertyEditor "Custom property editors" for scripts, shaders and float sliders and @ref PropertyEditor "Property editors" for bitmaps and meshes can be detached. This means you can have multiple editors opened at the same time in separate windows. You can, for instance, to have the @ref ZLibrary source visible at the same time you edit a @ref ZExpression. Just hit the "Detach" button to detach an editor.

![Detached editors](tut1-detached-editors.png)