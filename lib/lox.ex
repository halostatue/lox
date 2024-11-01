defmodule Lox do
  @moduledoc """
  This is an implementation of the [Lox][1] language implemented (twice) in [Crafting
  Interpreters][2] by Robert Nystrom.

  There are two versions that will eventually be present in this repo, both implemented in
  Elixir. The first version (Ilox) is based on `jlox`, the Java implementation using
  a [tree-walk interpreter][3]. The second version (VmLox) is based on `clox`, the
  C implementation using a [bytecode virtual machine][4].

  ## The Lox Language

  > This is a short summary of the [language design][1] described in chapter 3. Full
  > details are found there.

  Lox is a high-level dynamically typed language with automatic memory management.

  Lox is statement-oriented with required statement terminal semicolons (`;`). Only line
  comments are supported (`//`).

  ### Dynamic Typing (3.2.1)

  Variables may store values of any type, and may change their stored data type at any
  time. Performing operations on values of the wrong type will result in runtime errors.

  ### Automatic Memory Management (3.2.2)

  Lox has a simple garbage collector (this may be skipped on the Elixir versions as there
  is no direct way to manage memory).

  ### Data Types (3.3)

  There are four primitive data types in Lox:

  - Boolean (`true`, `false`)
  - Numbers (double-precision floating point numbers)
  - Strings (UTF-8 character strings)
  - Nil (`nil`)

  Additionally, there are two higher-order data types available:

  - Functions are first class values that can be assigned to variables and passed around.
  - Classes can be created to collect functions related to data. Properties are not
    declared, but assigned when required.

  ### Expressions (3.4)

  Expressions produce a *value*.

  #### Arithmetic operators (3.4.1)

  The basic arithmetic operators (`+-*/`) exist as expected, and `-` may be used for
  negation.

  ```
  add + me;
  subtract - me;
  multiply * me;
  divide / me;
  -negateMe;
  ```

  The `+` operator may also be used to concatenate strings:

  ```
  "add" + "me";
  ```

  #### Comparison and equality operators (3.4.2)

  Equality and inequality comparisons may be made between any types, but there is no value
  coercion:

  ```
  1 == 2;                 // false
  "cat" != "dog";         // true
  314 == "pi";            // false
  123 == "123";           // false
  ```

  Relative comparisons may only be performed between number values.

  ```
  less < than;
  lessThan <= orEqual;
  greater > than;
  greaterThan >= orEqual;
  ```

  #### Logical operators (3.4.3)

  There are three logical operators: not (spelled `!`), `and`, and `or`. These are used
  only with Boolean values.

  ```
  !true;                  // false
  !false;                 // true
  true and false;         // false
  true and true;          // true
  false or false;         // false
  true or false;          // true
  ```

  #### Precedence and grouping (3.4.4)

  Precedence and associativity matches that of C.

  ```
  var average = (min + max) / 2;
  ```

  ### Statements (3.5)

  Where expressions produce a value, statements produce an **effect**. Expressions may be
  promoted to statements with the terminal semicolon (`;`). Statements can be grouped into
  blocks with braces (`{}`).

  ```
  {
    print "One statement.";
    print "Two statements.";
  }
  ```

  Blocks affect variable scoping.

  ### Variables (3.6)

  Variables must be declared before use with `var` statements, which have an optional
  initializer assignment. Uninitialized variables default to `nil`.

  ```
  var imAVariable = "here is my value";
  var iAmNil;

  var breakfast = "bagels";
  print breakfast; // "bagels"
  breakfast = "beignets";
  print breakfast; // "beignets"
  ```

  ### Control Flow (3.7)

  There are three constructs for control flow:

  - `if/else`: The `else` branch is optional, and both branch bodies may be single
    statements or blocks. There is no support for `else if`, so a nested `if` will be
    inside the `else` block. Declarations in a branch must be inside of a block.

    ```
    if (expr) {
      print "yes";
    } else {
      print "no";
    }
    ```


  - `while`

    ```
    var a = 1;

    while (a < 10) {
      print a;
      a = a + 1;
    }
    ```

  - `for`: The C `for` loop with initializer statement, comparison expression, and next
    statement. A single statement or block must follow the `for` loop conditions.
    Declarations in the loop must be inside of a block.

    ```
    for (var a = 1; a < 10; a = a + 1) {
      print a;
    }
    ```

  ### Functions (3.8)

  Functions are defined with the `fun` keyword and always return a value. If there is no
  `return` statement before the end of the function, there is an implicit `return nil`
  statement added to the end of the function.

  Function call expressions must have parentheses (`()`), or the value refers to the
  function.

  ```
  fun printSum(a, b) {
    print a + b;
  }

  fun returnSum(a, b) {
    return a + b;
  }

  printSum(1, 3);   // prints 4 and returns nil
  returnSum(1, 3);  // returns 4
  ```

  #### Closures (3.8.1)

  Functions are _first class_ values that can be referenced, stored, and passed to other
  functions:

  ```
  fun addPair(a, b) {
    return a + b;
  }

  fun identity(a) {
    return a;
  }

  print identity(addPair)(1, 2); // prints 3
  ```

  Function declarations are statements, allowing local functions to be declared inside
  another function:

  ```
  fun outer() {
    fun inner() {
      print "I'm local!";
    }

    inner();
  }
  ```

  Functions are closures around their declaration scope, permitting interesting
  behaviours:

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

  Lox implements class-based OOP with dynamic properties, methods, and inheritance. In
  classes, methods are declared like functions, but the `fun` keyword is omitted. Objects
  instances are created by calling the class name as if it were a function.

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

  #### Instantiation and initialization (3.9.5)

  Unlike normal variables (which must be declared with a `var` statement), object
  properties are added or referenced dynamically. (Accessing an unknown property results
  in a `nil` value.) Properties are accessed within methods using the `this` keyword.

  ```
  class Breakfast {
    serve(who) {
      print "Enjoy your " + this.meat + " and " + this.bread + ", " + who + ".";
    }
  }

  breakfast = Breakfast()
  breakfast.meat = "sausage";
  breakfast.bread = "sourdough";
  breakfast.serve("Anonymous Eater")
  ```

  Properties may be initialized on creation if there is an `init` method on the class.
  Parameters on the class are forwarded to the `init` method.

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

  #### Inheritance (3.9.6)

  Lox supports single inheritance trees and does not support mixins using the `<`
  operator on class declarations. All methods in the superclasses are exposed on the
  subclasses, including `init`. The subclass may want to call the superclass
  implementations, and this is done with `super`.

  ```
  class Brunch < Breakfast {
    init(meat, bread, drink) {
      super.init(meat, bread);
      this.drink = drink;
    }

    drink() {
      print "How about a Bloody Mary?";
    }
  }

  var benedict = Brunch("ham", "English muffin")
  benedict.serve("Noble Reader")
  ```

  ### The Standard Library (3.10)

  There is basically none, defining one function `clock()` that returns seconds since the
  program started.

  [1]: https://craftinginterpreters.com/the-lox-language.html
  [2]: https://craftinginterpreters.com
  [3]: https://craftinginterpreters.com/a-tree-walk-interpreter.html
  [4]: https://craftinginterpreters.com/a-bytecode-virtual-machine.html
  """
end
