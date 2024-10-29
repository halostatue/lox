defmodule Lox do
  @moduledoc """
  Documentation for `Lox`, the language implemented (twice!) in Crafting Interpreters.

  This is an implementation in Elixir, because I'm not interested in using Java.

  ## The Lox Language

  [Lox][] is a high-level dynamically typed language with automatic memory management.

  ### Data Types (3.3)

  There are four data types in Lox: Booleans (`true`, `false`), Numbers (double-precision
  floating point numbers; `1234`, `12.34`), Strings, Nil, Functions, and Classes.

  ### Expressions (3.4)

  These produce a *value*.

  **Arithmetic operators** (3.4.1)

  ```
  add + me;
  subtract - me;
  multiply * me;
  divide / me;
  -negateMe;

  "add" + "me";
  ```

  **Comparison and equality operators** (3.4.2)

  ```
  less < than;
  lessThan <= orEqual;
  greater > than;
  greaterThan >= orEqual;
  1 == 2;                 // false
  "cat" != "dog";         // true
  314 == "pi";            // false
  123 == "123";           // false
  ```

  **Logical operators** (3.4.3)

  ```
  !true;                  // false
  !false;                 // true
  true and false;         // false
  true and true;          // true
  false or false;         // false
  true or false;          // true
  ```

  **Precedence and grouping** (3.4.4)

  Precedence and associativity matches that of C.

  ```
  var average = (min + max) / 2;
  ```

  ## Statements (3.5)

  Statements produce an **effect**. Expressions followed by semicolon (`;`) is
  a statement. Statements can be grouped into blocks with braces (`{}`).

  ```
  {
    print "One statement.";
    print "Two statements.";
  }
  ```

  ### Variables (3.6)

  Variables must be declared with `var`.

  ```
  var imAVariable = "here is my value";
  var iAmNil;

  var breakfast = "bagels";
  print breakfast; // "bagels"
  breakfast = "beignets";
  print breakfast; // "beignets"
  ```

  ### Control Flow (3.7)

  **if/else**

  ```
  if (condition) {
    print "yes";
  } else {
    print "no";
  }
  ```

  **while**

  ```
  var a = 1;

  while (a < 10) {
    print a;
    a = a + 1;
  }
  ```

  **for**

  The classic C `for` loops:

  ```
  for (var a = 1; a < 10; a = a + 1) {
    print a;
  }
  ```

  ### Functions (3.8)

  Function call expressions must have parentheses (`()`). They are defined with `fun`:

  ```
  fun printSum(a, b) {
    print a + b;
  }

  printSum(1, 3);
  ```

  Functions may return values with `return`:

  ```
  fun returnSum(a, b) {
    return a + b;
  }
  ```

  If there is no `return` in the function, there is an implicit return of `nil`.

  **Closures** (3.8.1)

  Functions are _first class_ values.

  ```
  fun addPair(a, b) {
    return a + b;
  }

  fun identity(a) {
    return a;
  }

  print identity(addPair)(1, 2); // prints 3
  ```

  Functions may be declared inside other functions:

  ```
  fun outer() {
    fun inner() {
      print "I'm local!";
    }

    inner();
  }
  ```

  Local functions, first-class functions, and block scoping result in closures:

  ```
  fun outer() {
    var outside = "outside";

    fun inner() {
      print outside;
    }

    return inner;
  }

  var fn = outer();
  fn();
  ```

  ### Classes (3.9)

  ```
  class Breakfast {
    cook() {
      print "Eggs a-fryin'!";
    }

    serve(who) {
      print "Enjoy your breakfast, " + who + ".";
    }
  }

  var someVariable = Breakfast;
  someFunction(Breakfast);

  var breakfast = Breakfast();
  print breakfast;
  ```

  **Instantiation and initialization** (3.9.5)

  Object fields can be added directly to instances:

  ```
  breakfast.meat = "sausage";
  breakfast.bread = "sourdough";
  ```

  Fields can be accessed within methods using `this`:

  ```
  class Breakfast {
    serve(who) {
      print "Enjoy your " + this.meat + " and " + this.bread + ", " + who + ".";
    }
  }
  ```

  Object initialization is done with the `init()` method:

  ```
  class Breakfast {
    init(meat, bread) {
      this.meat = meat;
      this.bread = bread;
    }

    // ...
  }

  var baconAndToast = Breakfast("bacon", "toast");
  baconAndToast.serve("Dear Reader");
  // "Enjoy your bacon and toast, Dear Reader."
  ```

  **Inheritance** (3.9.6)

  ```
  class Brunch < Breakfast {
    drink() {
      print "How about a Bloody Mary?";
    }
  }

  var benedict = Brunch("ham", "English muffin")
  benedict.serve("Noble Reader")
  ```

  You can call superclass methods with `super`:

  ```
  class Brunch < Breakfast {
    init(meat, bread, drink) {
      super.init(meat, bread);
      this.drink = drink;
    }
  }
  ```

  ### The Standard Library (3.10)

  There is basically none, defining one function `clock()` that returns seconds since the
  program started.

  [Lox]: https://craftinginterpreters.com/the-lox-language.html
  """
end
