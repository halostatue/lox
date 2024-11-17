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
    if (expression) {
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

    `for` statements are transformed ("desugared") into appropriately-scoped `while`
    loops.

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

  The standard library offers two functions:

  - `clock/0`, which returns a monotonic time in seconds (this is defined in _Crafting
    Interpreters_)
  - `env/0`, which returns a string representation of the current environment. In Ilox,
    this is the *complete* operating environment, not the current scope.

  ## Grammar

  Ilox uses a context free grammar for expressions and adapts that into a parsing grammar
  encoding precedence rules.

  ### Context-Free Expression Grammar

  ```
  expression  → binary | grouping | literal | unary ;
  binary      → expression binaryOp expression ;
  binaryOp    → "=="  | "!="
              | "<"   | "<=" | ">" | ">="
              | "+"   | "-"  | "*" | "/"
              | "and" | "or" ;
  grouping    → "(" expression ")" ;
  literal     → NUMBER | STRING | "true" | "false" | "nil" ;
  unary       → unaryOp expression ;
  unaryOp     → "-" | "!" ;
  ```

  ### Parsing Grammar

  This grammar rearranges the context-free expression grammar into one that encodes
  precedence and adds *statements* for side effects.

  ```
  program     → declaration* EOF ;

  declaration → classDecl
              | funDecl
              | varDecl
              | statement ;

  classDecl   → "class" IDENTIFIER ( "<" IDENTIFIER )?
                "{" function* "}" ;
  funDecl     → "fun" function ;
  varDecl     → "var" IDENTIFIER ( "=" expression )? ";" ;

  statement   → exprStmt
              | forStmt
              | ifStmt
              | printStmt
              | returnStmt
              | whileStmt
              | block ;

  exprStmt    → expression ";" ;
  forStmt     → "for" "(" ( varDecl | exprStmt | ";" )
                          expression? ";"
                          expression? ")" statement ;
  ifStmt      → "if" "(" expression ")" statement
                ( "else" statement )? ;
  printStmt   → "print" expression ";" ;
  returnStmt  → "return" expression? ";" ;
  whileStmt   → "while" "(" expression ")" statement ;
  block       → "{" declaration* "}" ;

  expression  → assignment ;

  assignment  → ( call "." )? IDENTIFIER "=" assignment
              | logical ;

  logical     → logicAnd ( "or" logicAnd )* ;
  logicAnd    → equality ( "and" equality )* ;
  equality    → comparison ( ( "!=" | "==" ) comparison )* ;
  comparison  → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  term        → factor ( ( "-" | "+" ) factor )* ;
  factor      → unary ( ( "/" | "*" ) unary )* ;

  unary       → ( ( "!" | "-" ) unary ) | call ;
  call        → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
  primary     → "true" | "false" | "nil" | "this"
              | NUMBER | STRING | IDENTIFIER |  "(" expression ")"
              | "super" "." IDENTIFIER ;

  function    → IDENTIFIER "(" parameters? ")" block ;
  parameters  → IDENTIFIER ( "," IDENTIFIER )* ;
  arguments   → expression ( "," expression )* ;
  ```

  [1]: https://craftinginterpreters.com/the-lox-language.html
  [2]: https://craftinginterpreters.com
  [3]: https://craftinginterpreters.com/a-tree-walk-interpreter.html
  [4]: https://craftinginterpreters.com/a-bytecode-virtual-machine.html
  """

  alias Ilox.Token

  @typedoc """
  The list of declarations representing the Lox program.

  ```
  program     → declaration* EOF ;
  ```
  """
  @type program :: list(declaration)

  @typedoc """
  A class, function, or variable declaration, or a statement.
  """
  @type declaration :: class_decl | fun_decl | var_decl | statement

  @typedoc """
  A class declaration.
  """
  @type class_decl :: {:class_decl, name :: Token.t(), methods :: list(fun_decl)}

  @typedoc """
  A function definition.
  """
  @type fun_decl ::
          {
            :fun_decl,
            name :: Token.t(),
            params :: list(Token.t()),
            arity :: non_neg_integer(),
            body :: list(statement)
          }

  @typedoc """
  A variable declaration with an optional initialization expression.
  """
  @type var_decl :: {:var_decl, name :: Token.t(), initializer :: Lox.expr() | nil}

  @typedoc """
  Supported program statements. `for` statements are transformed into `while` blocks
  statements during parsing.
  """
  @type statement :: expr_stmt | if_stmt | print_stmt | return_stmt | while_stmt | block

  @typedoc """
  An expression statement. The expression is evaluated, but is essentially side-effect
  free.
  """
  @type expr_stmt :: {:expr_stmt, expr :: Lox.expr()}

  @typedoc """
  A conditional flow control statement made with an expression and a statement executed
  when the expression is truthy. It may optionally have a else statement executed when the
  expression is falsy.
  """
  @type if_stmt :: {:if_stmt, expr :: Lox.expr(), truthy :: statement, falsy :: nil | statement}

  @typedoc """
  A statement that displays the result of its expression to standard output.
  """
  @type print_stmt :: {:print_stmt, expr :: Lox.expr()}

  @typedoc """
  Breaks the execution of a function and returns the result of the expression, or `nil` if
  it is omitted.

  If a function terminates execution without a `return` statement, `nil` is implicitly
  returned.
  """
  @type return_stmt :: {:return_stmt, keyword :: Token.t(), value :: nil | Lox.expr()}

  @typedoc """
  A looping flow control statement that only executes if the expression is truthy.
  """
  @type while_stmt :: {:while_stmt, expr :: Lox.expr(), body :: statement}

  @typedoc """
  A list of statements or declarations wrapped in curly braces.
  """
  @type block :: {:block, list(declaration)}

  # expression  → assignment ;

  # assignment  → IDENTIFIER "=" assignment | logical ;

  # logical     → logicAnd ( "or" logicAnd )* ;
  # logicAnd    → equality ( "and" equality )* ;
  # equality    → comparison ( ( "!=" | "==" ) comparison )* ;
  # comparison  → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  # term        → factor ( ( "-" | "+" ) factor )* ;
  # factor      → unary ( ( "/" | "*" ) unary )* ;

  # unary       → ( ( "!" | "-" ) unary ) | call ;
  # call        → primary  ( "(" arguments? ")" | "." IDENTIFIER )* ;
  # primary     → "true" | "false" | "nil"
  #             | NUMBER | STRING | IDENTIFIER |  "(" expression ")" ;

  # function    → IDENTIFIER "(" parameters? ")" block ;
  # parameters  → IDENTIFIER ( "," IDENTIFIER )* ;
  # arguments   → expression ( "," expression )* ;
  # ```

  @typedoc """
  Any expression that evaluates to a value.
  """
  @type expr ::
          assignment
          | logical
          | binary_expr
          | unary
          | call
          | get
          | literal
          | set
          | group
          | this
          | variable

  @typedoc """
  A variable assignment expression.
  """
  @type assignment :: {:assignment, name :: Token.t(), value :: expr}

  @typedoc """
  A field assignment expression.
  """
  @type set :: {:set, object :: expr, name :: Token.t(), value :: expr}

  @typedoc """
  Two expressions with a logical operator ("and", "or") between them.
  """
  @type logical :: {:logical, left :: expr, operator :: Token.t(), right :: expr}

  @typedoc """
  Two expressions with a binary operator (one of "==", "!=", "<", "<=", ">", ">=", "+"
  , "-" , "*", or "/") between them.
  """
  @type binary_expr :: {:binary, left :: expr, operator :: Token.t(), right :: expr}

  @typedoc """
  A unary operator ("-", "!") followed by an expression.
  """
  @type unary :: {:unary, operator :: Token.t(), right :: expr}

  @typedoc """
  A function call expression.

  This stores the closing parenthesis token to use the location to report errors on
  function call.
  """
  @type call ::
          {:call, callee :: Lox.expr(), arguments :: list(Lox.expr()), argc :: non_neg_integer(),
           closing :: Token.t()}

  @typedoc """
  A property access expression.
  """
  @type get :: {:get, object :: Lox.expr(), name :: Token.t()}

  @typedoc """
  A reference to the current instance.
  """
  @type this :: {:this, keyword :: Token.t()}

  @typedoc """
  A literal value. One of `nil`, `true`, `false`, any floating point number, any integer,
  or any UTF-8 string.
  """
  @type literal :: {:literal, value :: number() | binary() | true | false | nil}

  @typedoc "An expression surrounded by parentheses."
  @type group :: {:group, expr}

  @typedoc "A named reference to a variable."
  @type variable :: {:variable, name :: Token.t()}
end
