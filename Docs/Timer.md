# Timer {#Timer}

Executes a component list on a timed interval.

This component is used in the OnUpdate-list.
This component can only be used in the OnUpdate property of @ref Model, @ref ModelState, @ref ZApplication or @ref AppState components.

## Properties

@dl

@dt Interval
@dd An interval in seconds between the calls to OnTimer. If set to zero then the timer stops.

@dt RepeatCount
@dd A integer value indicating how many times to call OnTimer before timer stops. The default value of -1 means repeat forever. 0 means call once, 1 call twice etc.

@dt CurrentRelativeTime
@dd Counter of the timer in seconds. If set to some number, the timer will be triggered in CurrentRelativeTime + Interval seconds. For instance setting to 0 resets the timer. This property is read-only in properties editor, but can be set in expressions.

@dlx

## List Properties

@dl

@dt OnTimer
@dd The list of components that will be executed.

@dlx
