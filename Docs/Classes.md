# Classes {#Classes}

Scripting comes with handy basic object-oriented features allowing users to
define and use classes. The syntax is inspired by C# and Java.

More discussion about object-oriented features and examples see in
[the related Forum topic](http://www.emix8.org/forum/viewtopic.php?t=1559).

## Class

A class is a user-defined type with a set of fields and methods. It represents a
blueprint from which objects are created. Classes are defined in @ref ZLibrary
with the following syntax:

<!-- prettier-ignore -->
@syn
class @<class name@> {
  @<field@>
  @<field@>
  ...
  @<method@>
  @<method@>
  ...
}
@synx

One ZLibrary component can define one or several classes and they can be mixed
with other functions, variables or constants.

Fields and methods are commonly referred to as **members**.

Example:

    class Player {
      private int score, lives;
      private int scoreRemainingToNextExtraLife = 10000;
      model theModel;

      Player(int initialLives) {
        lives = initialLives;
      }

      Player() {
        lives = 3;
      }

      void giveScore(int amount) {
        score += amount;
        scoreRemainingToNextExtraLife -= amount;
        if(scoreRemainingToNextExtraLife < 0) {
          lives ++;
          scoreRemainingToNextExtraLife += 10000;
        }
      }

      inline int getLives() {
        return lives;
      }
    }

Within class declaration, the keyword `this` can be optionally used to refer to
"this object" when accessing class members. See the following example:

    class Rectangle {
      int width = 10;
      int height = this.width;

      int area() {
        return this.width * this.height;
      }

      void info() {
        trace("Rectangle; area=" + intToStr(this.area()));
      }
    }

is identical to:

    class Rectangle {
      int width = 10;
      int height = width;

      int area() {
        return width * height;
      }

      void info() {
        trace("Rectangle; area=" + intToStr(area()));
      }
    }

However, use `this` to differentiate method/constructor parameters from fields
if named identically. For example:

@anchor squareExample

    class Square {
      float width;

      Square(float width) {
        this.width = width;
      }
    }

## Objects

Objects are created from classes by instantiation with the following syntax:

@syn{new <class>(<arguments>)}

The `new` keyword followed by class name actually calls a @ref Constructors
"constructor" with none or more parameters. Constructor returns an object which
is usually assigned to a variable or is used as an argument of function call.

It is possible to access public members of object by:

- reading field: @syn{<object>.<field>}
- assigning a value to field: @syn{<object>.<field> = <value>;}
- calling method: @syn{<object>.<method>(<arguments>)}

Example of @ref ZExpression with object instantiation and accessing public
members:

    Player p = new Player(4);
    p.theModel = null;
    p.giveScore(100);
    trace(intToStr(p.getLives()));

## Field

Class fields represent structure of objects. Each field is a variables scoped to
object. Field is defined as:

@syn{<visibility> <type> <name>;}

Alternatively, it is possible to specify initial value assigned to field on
object instantiation:

@syn{<visibility> <type> <name> = <value>;}

**Visibility** determines where the field is visible. There are the following
options:

- **private**: field is visible only in the class definition and in subclasses.
  Private field cannot be accessed from class instances. It is determined by the
  `private` keyword.

  @note The `private` visibility in ZGE scripting language is more like
  `protected` in Java or C#.

- **public**: field is visible everywhere. It can be accesses as public property
  of object. It is determined by omitting any visibility keyword from field
  definition.

## Method

Method is like a function which is scoped to a class. It can access all class
members and public variables, constants, components, etc. Method declaration
uses the following syntax:

<!-- prettier-ignore -->
@syn
  @<modifiers@> @<return type@> @<function name@> (@<parameters@>) {
    @<body expression@>
  }
@synx

See the section @ref Functions for description of @<return type@>,
@<parameters@>, and @<body expression@>.

Methods can use the following modifiers specified in arbitrary order before
method signature:

- `private` - private method used only within its class; if not specified, the
  method is to be considered to be public
- `inline` - inline method; analogous to @ref InlineFunction "inline function"
- `virtual` - virtual method declared in the base class; cannot be used together
  with `override` modifier; for more information see the @ref Inheritance
  section
- `override` - a method overriding a virtual method from base class; cannot be
  used together with `virtual` modifier; for more information see the @ref
  Inheritance section

**Method overloading** allows you to have several methods of one class with the
same name but different number of parameters. For example:

    class Math {
      float sum(float a, float b) { return a + b; }
      float sum(float a, float b, float c) { return a + b + c; }
    }

## Constructors {#Constructors}

A **constructor** is a special method that is used to initialize objects. The
constructor is called when an object of a class is created. It can be used to
set initial values for object attributes.

Constructor has no return type and its name is identical to the name of class.
Constructors can be overloaded.

Constructor can have any number of parmeters parameters.

    class Bullet {
      private int power;
      model theModel;

        Bullet() {
          theModel = BulletModel;
          power = 10;
        }

        Bullet(int power) {
          Bullet(); // calling another constructor
          this.power = power;
        }
    }

Creation of instances:

    Bullet b1 = new Bullet(); // power = 10
    Bullet b2 = new Bullet(80); // power = 80

Non-parametric constructor is always available even if not defined explicitly in
class. It can be, of course, overridden by user's implementation. For instance,
the class Square defined @ref squareExample "here" can be instantiated as:

    Square s1 = new Square(); // s1.width = 0

## Inheritance {#Inheritance}

It is possible to inherit fields and methods from one class--called
**superclass**--to another--called **subclass**. To inherit from a class, use
the following syntax:

@syn{class <subclass> : <superclass> { ... }}

Only one superclass is allowed. Subclass can use members of superclass, but can
define additional ones.

If a method is marked by keyword `virtual` in superclass, it can be overridden
in subclass by the method with the same signature but marked by keyword
`override`.

Example:

    class Vehicle {
      virtual void move() {}
    }

    class Car : Vehicle {
      Engine engine;

      override void move() {
        this.engine.start();
      }
    }

## Absence of Destructors

There are no destructors. The reason for this is that the lifespan of a class
instance is determined by the garbage collector and as such it may be destroyed
after the code has finished running.
