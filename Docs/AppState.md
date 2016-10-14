# AppState {#AppState}

Represents an application state. A game does not need to use this component, but it is useful when your application have several distinct states such as "Title screen", "Playing" or "Game over".

Example usage: @ref TripleE sample project.

See also: @ref SetAppState

## Properties

@dl

@dt ModelUpdatesEnabled
@dd If checked (default) components in the OnUpdate sections of @ref Model and AppState components are executed while application is in this AppState. If unchecked, updates are not performed.

@dt CollisionEnabled
@dd If checked (defaut) the built-in mechanism of collision detection is enabled while application is in this AppState. If unchecked, collision detection is disabled.

@dlx

## List Properties

@dl

@dt OnStart
@dd Called when this state is started.

@dt @anchor AppStateOnUpdate OnUpdate
@dd Called between every frame when this state is active.

@dt @anchor AppStateOnRender OnRender
@dd A list of render commands for this state.

@dt OnLeave
@dd Called when the model leaves this state, i.e. when SetAppState sets another state.

@dt Definitions
@dd Local data for this state. Even if more component types can be placed here, usually, local variables are defined at this place.

@dlx
