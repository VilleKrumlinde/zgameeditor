# Condition {#Condition}

Execute a component list depending on the return value of an expression.

## Properties

@dl

@dt Expression
@dd The expression that will be evaluated. If a true value is returned then the "OnTrue" component list will be executed, otherwise the "OnFalse" will be executed. Example expressions:

    return 1; // true

or 

    return CurrentModel.Position.X > 5;

@dlx

## List Properties

@dl

@dt OnTrue
@dd List of components that will be executed when Expression evaluates to true.

@dt OnFalse
@dd List of components that will be executed when Expression evaluates to false.

@dlx
