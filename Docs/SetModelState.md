# SetModelState {#SetModelState}

Switches state for the model issuing this command.

See also: @ref ModelState, @ref Model

## Properties

@dl

@dt State
@dd The new state for the model. If it is different from the current state then OnLeave will be called for the old state, and OnStart called for the new state.

@dlx
