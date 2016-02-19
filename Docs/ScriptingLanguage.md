# Scripting Language {#ScriptingLanguage}

ZGameEditor has a scripting language with syntax based upon C language dialects. The scripting language allows to specify expressions to compute values for component properties but also enables to specify more complex code to control the application behavior. Components and their properties defined in project tree can be fully reflected (accessed) by scripting and in this way you can achieve the desired dynamics of objects in application.

The following sections define syntax of the scripting language.

# Statements {#Statements}

Expression consists of zero or more statements and/or @ref Comments "comments". Comments and white space characters (space, tab, new line, etc.) are ignored while they are not included in @ref stringType "strings".

A __statement__ represents either declaration of variable or function, or execution of a command. It is terminated by semicolon (`;`). Statements are written each after other and are executed in the same order.

@syn{<statement>; <statement>; ...}

@anchor Block Several statements can be grouped to a __block__, a set of statements enclosed in curly brackets (`{` and `}`) which do not need to be terminated with semicolon.

@syn{\{ <statement>; <statement>; ... <statement> \} <statement>; ...}

Blocks are usually parts of compound commands that control program flow (see section @ref ControlFlow) or definitions of functions (see section @ref Functions).

# Comments {#Comments}

Comments are portions of the code ignored by the compiler which allow the user to make simple notes in the relevant areas of the source code. Comments come either in block form or as single lines.

Single-line comment starts with `//` and continue until the end of the line.

@syn{// this is a single line comment}

Multi-line comment starts with `/*` and end with `*/`.

@syn
/*
   this is a
   multi-line comment
*/
@synx

# Variables {#Variables}

Variable is a named value of a specified type. After defining a variable, it is possible to set value of variable and to read this value in expressions. There are two kinds of variables:

_Global variables_ are visible in all project's components. They are defined by means of @ref Variable component.

_Local variables_ are defined in expressions (for instance in @ref ZExpression component) and are valid only in these expressions. A variable defined within a function is also a local variable and can be accessed only from definition of this function. Another kind of local variables can be iterators of @ref For looping statements that can be accessed only from bodies of the for statement.

Local variable is defined in expressions as follows:

@syn{<type> <name>;}

See the list of @subpage Types "available types".

It is also possible to initialize value of variable at the place of its definition by the syntax:

@syn{<type> <name> = <value>;}

Value can be any expression valid at this place returning a value of the required type. It can use also variables defined before this variable.
    
Several variables of the same type can be defined at once with syntax:

@syn{<type> <name1> = <value>, <name2> = <value>, ... <nameN> = <value>;}

Specifying default values is optional for all variables in list.

Examples:

    int status = 1;
    float positionX = 3, positionY, positionZ;

If a variable definition is placed to @ref ZLibrary or @ref ZExternalLibrary, it becomes global.

It is possible to assign values to variables (see the section @ref Assignments for details) and also to use variables as values used in other expressions.

Example:

    int a;                  // declaration
    a = 23;                 // assignment
    a++;                    // updating value
    trace(intToStr(a));     // usage - print "24" to Log window

# Constants {#Constants}

__Constant__ is named value of basic type.

_Global constants_ can be defined either in @ref Constant component or in @ref ZLibrary or @ref ZExternalLibrary as expressions with the following syntax:

@syn{const <type> <name> = <value>;}

It is also possible to define several constants of the same type:

@syn{const <type> <name> = <value>, <name> = <value>, ...;}

The type of a constant can only be @ref byteType, @ref intType, @ref floatType, or @ref stringType. Value is an expression that can use either type literals or other already defined constants.

Not so usual, but it is possible to define also _local constants_ visible only for expressions they are defined in. Local constants can be placed to the content of expression-based component properties, e.g., Expression property of @ref ZExpression, @ref Condition, @ref BitmapExpression, WhileExp property of @ref Repeat, etc.

Constants are used in expressions by referring to their names.

Example:

    // defining constants
    const int FALSE = 0, TRUE = 1;
    const float PI2 = PI * 2;

    // usage of constants
    if(x > 22) flag = FALSE;
    a = sin(CurrentModel.Rotation.Z * PI2);

ZGameEditor provides several @subpage BuiltinConstants "built-in constants" automatically set by system. They are either mathematical constants or system constants you can use to obtain an information about the operating system the application is running on.

# Accessing Component Properties {#AccessingProperties}

Named components and values of their properties can be accessed from expressions in the following way:

@syn{<component name>}

@syn{<component name>.<property>}

Component properties are used to obtain their current values, and if not read-only, also to set their values dynamically in runtime. Assignments to component properties are usually done either during component initialization or during component updating.

If you change properties, local variables and properties of contained components of a @ref Model component outside that Model, you change properties of components in the project. The changes will be applied for all next instances of that Model. For example, to change initial position and shape (local variable) of all next instances of "Object" Model and use:

    Object.Position = vector3(2,3,4);
    Object.Shape = BOX; // or just Shape = BOX;

If you change properties, local variables and properties of contained components of a Model component from expressions belonging to that component, you change properties of the current __component instance__.

Example: Model "A" has local integer variable M.

    // expression outside A
    M = 1;      // global M is changed to 1
    create(A);  // A.M is 1
    
    // expression in A
    M = 2;      // instance's M is 2
    create(A);  // create new instance; its M is 1
    
    // expression in A (executed in new instance)
    M = 3;    // new instance's M is 3
    create(A);  // create new instance; its M is 1

To access the current model instance from model, you can use the variable `CurrentModel`. For example, to set position of the current model instance you can write:

    CurrentModel.Position.X = 0;

Example: A @ref Model called "Bullet" should initially be spawn at position of another Model called "Player" and its velocity is set to direction of Player. This is done in @ref ZExpression put to @ref ModelOnSpawn "OnSpawn" section of Bullet model:

    CurrentModel.Position = Player.Position;
    CurrentModel.Velocity.X = cos(Player.Rotation.Z * PI * 2) * BULLET_SPEED;
    CurrentModel.Velocity.X = sin(Player.Rotation.Z * PI * 2) * BULLET_SPEED;

To change color of bullet during its existence, update color of "BulletColor" component of type @ref RenderSetColor in @ref ModelOnUpdate "OnUpdate" section:

    BulletColor.Color.R = abs(sin(CurrentModel.Personality + App.Time * 10));

@ref ModelPersonality "Personality" property of the current model is used to have different colors for multiple instantiated bullets at the same time.

# Assignments {#Assignments}

_Simple assignment_ of a value to variable or component property is done with the following syntax:

@syn{<variable> = <value>;}

@syn{<variable>.<index property> = <value>;}

@syn{<component>.<property> = <value>;}

@syn{<component>.<property>.<index property> = <value>;}

Variable, component and property are given by name. If the variable or property is a vector, its component can be addressed by index property; see @ref Vectors for details. Value of assignment refers here to an expression returning a value of the required type. 

Example:

    // definition of variables
    int i, j;
    float f;
    vec4 v;
    vec2[5] arr;
    
    // assignment of:
    i = 22;                  // literal
    j = 14 + i;              // result of operation
    vec4 = vector4(1,2,3,4); // function call
    vec4.X = 8f;             // literal to index property
    arr[1].Y = 14.0;         // literal to index property of array item

To simplify assignments to numeric (or string) variables which are based on the previous value of variable you can use the following assignments:

Operator | Name | Equivalent
---------|------|-----------
a += b | Addition assignment (also used for String concatenation) | a = a + b
a -= b | Subtraction assignment | a = a - b
a *= b | Multiplication assignment | a = a * b
a /= b | Division assignment | a = a / b
a++ | Post-increment | a = a + 1
++a | Pre-increment | a = a + 1
a-- | Post-decrement | a = a - 1
--a | Pre-decrement | a = a - 1

_Note: If you are using prefix form then increment or decrement will be done before rest of the expression, and if you are using postfix form, then increment or decrement will be done after the complete expression is evaluated._

Example:

    int a = 1, b = 3;
    string s = "aaa";
    
    a += b;     // a is 4
    a *= b;     // a is 12
    s += "bbb"; // s is "aaabbb"

# Operators {#Operators}

In addition to @ref Assignments "assignment operators", the scripting language offers also other types of operators described in this section.

Operators use usual precedence known from other programming languages, such as C, C++ or Java. Operator precedence can be overridden by round brackets - `(` and `)`.

## Arithmetic Operators {#ArithmeticOperators}

Numeric operators work on @ref intType, @ref floatType,  and @ref byteType operands. If type of operands is not the same, _implicit conversion_ applies.

Operator | Semantics | Example
---------|-----------|--------
+ | Addition (also used for @ref stringType concatenation) | 1 + 2 // 3<br>"aaa" + "bbb" // "aaabbb"
- | Subtraction | 2.1 - 2 // 0.1
* | Multiplication (also used for matrix multiplication of @ref mat4Type operands) | 3 * 4.5 // 13.5 
/ | Division | 15 / 2 // 7.5
% | Modulo (remainder after dividing the first operand by second) | 26 % 10 // 6

## Boolean Operators {#BooleanOperators}

Boolean operators work on numbers. 0 represents "false" and a number other than 0 represents "true". If the result of boolean expression is "true", the expression returns integer 1.

Operator | Semantics | Example
---------|-----------|--------
&& | Logical AND | 0 && 1 // 0
\|\| | Logical OR | 0 \|\| 1 // 1
! | Logical negation | ! 0 // 1

## Relational Operators {#RelationalOperators}

Similarly to @ref BooleanOperators "boolean operators" also relational operators return values: 0 for "false", and 1 for "true".

Operator | Semantics | Example
---------|-----------|--------
== | Equal | a == a // 1
!= | Not equal | 12 != 0 // 1
&gt; | Greater than | 22 &gt; 21 // 1
&lt; | Less than | 2 &lt; 2 // 0
&gt;= | Greater than or equal | 22 &gt;= 22 // 1
&lt;= | Less than or equal | -100 &lt;= -99 // 1

## Bit Manipulation Operators {#BitOperators}

Operator | Semantics | Example
---------|-----------|--------
& | Bitwise AND | 0xff & 0x0f; // 0xf (15)
\| | Bitwise OR | 1 \| 2 \| 4 // 7
^ | Bitwise XOR | 1 ^ 2 ^ 4 ^ 4 //3
&lt;&lt; | Binary shift left | 1 &lt;&lt; 1 // 2
&gt;&gt; | Binary shift right | 2 &gt;&gt; 1 // 1

## Other Operators {#OtherOperators}

Operator | Semantics | Example
---------|-----------|--------
? : | Ternary if | a &gt; b ? x : y;<br>is equal to:<br>if(a &gt; b) x else y;
    
# Control Flow Statements {#ControlFlow}

Scripts allow to define conditional execution of alternative flows, looping of statement blocks, and continuations at different statements using the following statements.

## if-then-else {#IfThenElse}

The `if-then` and `if-then-else` statements allow conditional execution of statements, depending on th evalue of a condition. Their syntax is folllowing:

@syn
if( @<condition@> )
  @<true expression@>
@synx

or

@syn
if( @<condition@> )
  @<true expression@>
else
  @<false expression@>
@synx

Condition is an expression which returns a boolean value - 0 for "false" and other than 0 for "true". Depending on the returned value, either @<true expression@> or @<false expression@> is executed. These expressions are usually @ref Block "blocks" containing several statements. However they can be just a single statement. It is not necessary to terminate the `if-then(-else)` statement by semicolon.

Examples:

    if(PlayerScore > 0 && PlayerScore % 5000 == 0)
      PlayerLives += 1;

    if( (App.Time > 30) && (PlayerLives < 2) ) {
      Difficulty *= 1.2;
      Speed *= 1.2;  
    }

    if(CurrentModel.Position.Y > -30) {
      CurrentModel.Scale = 1.2 + sin(App.Time);
      ObjectColor.Color.R = rnd();
    } else
      @RemoveModel();

## switch {#Switch}

The `switch` statement executes one of several groups of statements, depending on the value of an expression.

@syn
switch ( @<value expression@> ) {

  case @<value1@>:
    @<expression@>
    break;

  case @<value2@>:
  case @<value3@>:
    @<expression@>
    break;

  default:
    @<expression@>
}
@synx

The @<value expression@> returns a value which is compared to the values specified for particular cases. If matches, the following sequence of statements is executed until the `break;` command occurs. If a sequence of statements does not finish with `break;` the checking of value continues with the following cases. If none of the cases was appplied, the `default:` section is applied, if exists. The default sequence of statements does not need to finish with `break;`. Sequences of statements are not enclosed in @ref Block "blocks". It is not necessary to terminate the `switch` statement by semicolon.

Example:

    switch(status) {
      case 0 :
      case 1 :
      case 2 :
        // when i is 0, 1 or 2
        doSomething();
        break;
      case 3 :
        // when i is 3
        doSomethingElse();
        break;
      default :
        //The default case when i is none of the above
    }

## while {#While}

The simplest kind of loop is the while-loop. Its syntax is:

@syn
while( @<condition@> )
  @<expression@>
@synx

Wile the @<condition@> is "true" (other than 0), the body @<expression@> is repeated. The body expression can be either a single statement, but usually it is a @ref Block "block" of statements.

Example:

  int i = 0, j = 0;
  while(i<100) {
       j += i;
       i++;    
  }
  trace(intToStr(j));

## for {#For}

The `for` loop is designed to iterate a number of times. Its syntax is:

@syn
for( @<initialization@>; @<condition@>; @<step@> )
  @<expression@>
@synx

Like the @ref While "while loop", this loop repeats @<expression@> while @<condition@> is "true" (other than 0). But, in addition, the `for` loop provides specific sections to contain an @<initialization@> and a @<step@> expressions, executed before the loop begins the first time, and after each iteration, respectively. Therefore, it is especially useful to declare iterator variable(s) in @<initialization@>, check it in @<condition@>, and update it in @<step@>.

It works in the following way: the @<initialization@> expression is executed, then @<condition@> is checked, if it's "true" (other than 0), then the loop body @<expression@> is executed. After the first iteration, the @<step@> expression is executed, @<condition@>  is checked again, if it's true, the @<expression@> is executed again, etc.

Examples:

You can use already defined variable as iterator:

    int j = 22;
    for(j = 0; j < 10; ++j)
      trace(intToStr(j));

In case you want to use a local variable (only exists within the scope that it is defined in), initializing the variable as you need it is more convenient:

    for(int i = 0; i < 10; ++i) {
      updatePosition(CurrentModel);
      updateRotation(CurrentModel);
    }

It is also possible to use more complex expressions for @<initialization@> and/or @<step@>:

    for(float i = 10, j = 0; i < 100; j+=1.5, i+=j)
      trace(intToStr(i*10));

## break and continue {#BreakContinue}

Use the `break` statement to break out of a loop before it has finished execution. For example:

    while(i < 42) {
       if(i == 10)
         break;  //exit loop
       i++;    
    }

Use the `continue` statement to jump directly to the next loop iteration. For example:

    int sum = 0;

    for(int i=0; i<10; i++) {
       if(i >= 5)
         continue;  //keep looping and skip the rest of the loop body
       sum += i;  //this line only executes when i<5
    }

# Functions {#Functions}

## User-defined Functions {#UserFunctions}

You can define functions in @ref ZLibrary component. Syntax for defining a function is the following:

@syn
@<return type@> @<function name@> (@<parameters@>) {
  @<body expression@>
}
@synx

Parameters represent a list of type-name pairs.

@syn{<type> <name>, <type> <name>, ...}

@<return type@> is a type of value returned by function. It can be `void` if the function returns no value.

@<parameters@> represent a list of comma-separated type-name pairs.

@syn{<type> <name>, <type> <name>, ...}

@<body expression@> is an expression which computes the function result or/and executes the desired commands. Returning a value from function with a return type different than "void" is done by command:

@syn{return <value>;}

Once the return command is executed, execution of function body is stopped and the value is returned to the context of function call.

A function can be called from expression by command:

@syn{<function name>(<arguments>)}

@<arguments@> is a comma-separated list of values/expressions corresponding to the function parameters at the same position in function definition. Values of arguments are passed to the function execution.

Examples:

Definitions of functions:

    float max(float v1, float v2) {
      return v1 >= v2 ? v1 : v2;
    }
    
    float max3(float v1, float v2, float v3) {
      return max(max(v1, v2), v3);
    }

Calling of functions:

    float var1 = max(2, 4);     // 4
    float var2 = max3(8,14,-5); // 14

## External Functions {#ExternalFunctions}

External function is a function implemented in a linked library (DLL for Windows or shared SO library for Android) and called from ZGameEditor expressions. Declaration of external functions is done in @ref ZExternalLibrary components.

External libraries provide the extensibility mechanism providing access to graphical libraries (e.g., direct calls of OpenGL), sound and music systems, 2D/3D physic engines, character animation libraries, image processing systems, APIs of operating systems (e.g., Windows API, Android sensors, Android game controllers, or OUYA API), and more.

## Built-in Functions {#BFunctions}

ZGameEditor defines a number of built-in functions providing the basic functionality which is not provided by @ref Components "components".

See the list of all built-in functions in @subpage BuiltinFunctions "here".

# Advanced Scripting {#AdvancedScripting}

## Script-based Component Invocation {#ComponentInvocation}

Component can be defined, its properties set to the desired values, and executed in script with the following syntax:

@syn{@<component> ( <property> : <value> , ... );}

It's possible to set component properties of types compatible with @ref Types "types in scripting language". Values of properties edited by drop-down lists in the @ref PropertyEditor are mapped to integer values corresponding to their order in the list; starring with 0. List properties, vector-based or binary properties cannot be set with this statement.

_Note: After typing "@" and pressing Ctrl+Space in_ @ref CodeEditor "Code Editor" _the content assistant offers names of all component types, so you do not need to remember them._

Examples:

    // A component that does not need any properties
    @RemoveModel();

    // Component with a single property
    @SetAppState(State : TitleScreenAppState);

    // Several properties
    @RenderText(
      Text : "Hello" + intToStr(i),
      X : -0.5,
      Y : -1 + (0.1*i)
    );

Reading an ASCII text file from script:

    // declare array
    string[] lines;

    // read from text file into array
    @FileAction(File: @File(FileName: "file.txt", TargetArray: lines));

    // print out the lines
    for(int i = 0; i < lines.SizeDim1; i++)
      trace(lines[i]);

It makes much more functionality available from scripting so it's up to the user to choose between Component, code-based, or a combination approach, based on personal preference.

## Cross Component Referencing {#CrossComponentRef}

If a component has reference to another component, it's possible to address properties of the referred component. 

Example: Um1 is name of a @ref UseMaterial component. It has property Material which refers to the used @ref Material component. You can write this expression to set the red channel of the material's color:

    Um1.Material.Color.R = 1;

You can also assign component references to another component. For instance, use different materials:

    if(something)
      Um1.Material = Material1;
    else
      Um1.Material = Material2;

## Pass Reference Arguments to Functions {#References}

Function parameter can be a reference to the value holder, e.g., a variable or property of component. Using the `ref` keyword before the parameter type, the parameter becomes a reference:

@syn{<return type> <function name> ( ref <type> <name>, ...) ...}

Function than can set the value of a reference which changes the value of the holder in the scope of function call. This mechanism allows you to return more than one value from a function.

Examples:

    // Separate r,g,b from color and return back to caller.
    // Notice the "ref" word in front of the arguments that are passed by reference.
    void splitColor(int color,ref int r, ref int g, ref int b) {
      r = color >> 16;
      g = color >> 8 & 0xFF;
      b = color & 0xFF;
    }

    // call this function
    int r,g,b;
    splitColor(0xff8020, r, g, b); // r == 255, g == 128, b == 32

References can also be used for external functions, for instance, the function from ZgeBullet physical library with ZGameEditor signature:

    void zbtGetPositionXYZ (xptr obj,
              ref float outX, ref float outY, ref float outZ) 

has in C++ the following signature:

    void zbtGetPositionXYZ(btCollisionObject* obj,
              float &outX, float &outY, float &outZ)

## reinterpret_cast {#ReinterpretCast}

Type unsafe conversion of pointer types is allowed by means of `reinterpret_cast` command. It allows any pointer to be converted into any other pointer type. Its syntax is:

@syn{reinterpret_cast< <type> >( <expression> )}

Example: To call OpenGL `glVertexAttribPointer` function, you need to specify a pointer to memory offset where the interleaved vertex attribute data start. In C/C++ you would specify the pointer to 12th char in the data array as:

    glVertexAttribPointer(ColorAttrib, 3, GL_FLOAT, GL_FALSE, MESH_STRIDE, (char*)NULL + (12));
    
The same call in ZGameEditor which imported OpenGL external library can be rewritten to:

    glVertexAttribPointer(ColorAttrib, 3, GL_FLOAT, GL_FALSE, MESH_STRIDE, reinterpret_cast<xptr>(12));

Example: Even if the scripting language does not support variables of type "arbitrary component" this functionality can be achieved by @ref xptrType variable and casting during assignment. See the following code, where Comp1 and Comp2 are names of components to be executed from script:

    xptr p;

    p = reinterpret_cast<xptr>(Comp1);
    @CallComponent(Component : p);

    p = reinterpret_cast<xptr>(Comp2);
    @CallComponent(Component : p);

# Garbage Collecting {#GarbageCollecting}

Local variables are at end of expression automatically garbage collected. You do not need specially care about deallocation of temporal vector, matrix, object, array or component variables. However, because garbage collecting takes some time, it is recommended to use as less as possible local variables and to reduce their size, if possible.