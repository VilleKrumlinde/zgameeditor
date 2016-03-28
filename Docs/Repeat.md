# Repeat {#Repeat}

Repeat execution of a component list.

## Properties

@dl

@dt Count
@dd Number of times the components in OnIteration will be executed. This value is not used if WhileExp-property is set.

@dt WhileExp
@dd An @ref ScriptingLanguage "expression" that will be executed after each loop-iteration. Example expression:

    return this.Iteration < 5;

Value of this property is specified in @ref CodeEditor "Code editor".
    
@dt Iteration
@dd Read-only property that contains the identity value of the current iteration. Return a false value to end the repeat-loop. It can be accessed from expressions of the WhileExp or OnIteration properties.

@dlx

## List Properties

@dl

@dt OnIteration
@dd The list of components that will be executed repeatedly.

@dlx
