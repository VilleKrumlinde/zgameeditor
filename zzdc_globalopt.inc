{$ifndef fpc}

{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFDEF CPUX64}
  {$EXCESSPRECISION OFF}
{$ENDIF}

{$endif}

{$ifdef zdesigner}
  {$define CALLSTACK}  //Show callstack if error in zexpression runtime
{$endif}
