# Excelent
## Haskell application for spreadsheet calculations

Authors:  
Curtis Chin Jen Sem - 5601118  
Daniel Kamphorst    - ?  
Noud   Savenije     - ?

# Usage: 
The environment the program start with contains a chain of integer references, a chain referencing an empty cell, and a chain referencing a cycle. 

- Arrow keys to move  
- Switching modes is done using the Enter key, there are 2 modes
  - Edit mode where it is possible to edit the expression in the current cell
  - View mode where navigation is possible
- The Escape key exits the program
- Expression notation:
  - Integers: `1`, `-2`, `123213` 
  - Double: `1.0`, `2.01`, `1.0e-12` 
  - Absolute references: `(1, 3)`, `(91, 1212)`
  - Relative references: `$(0, 0)`, `$(1, 0)`
  - Plus: `$(0, 0) + 3`, `1.0 + $(1, 0)`

# Supported functionality
- Expressions
  - Empty
  - Literals (Integers, Doubles)
  - References (Relative, Absolute)
  - Operators (+)
- Seperate layers for expressions and values
  - Ensures that expressions are evaluated at most once
  - The moment a cell is evaluated, the value is inserted into the view layer of the environment and reused
  - Any expression containing a reference first checks if that reference has been calculated before
- Graph evaluation
  - Only update the necessary cells when inserting
  - Cycle detection
- Type errors, errors and warnings.
  - We differentiate between a cycle itself, and referencing cycles. Both are treated as errors.
  - Referencing an empty cell is treated as a warning due to an empty cell maybe being useful in some way.
  - However, using that empty reference in an operation results in a type error. Same with using a reference to a cell containing an error.
- Multiple types
  -  Type checker implemented which inserts type errors where relevant into the view
  -  Unable to do operations on expressions of different types
  -  Types currently implemented: Int, Float, Empty, Error
  -  We decided against int-to-float coercion to demonstrate the type checkers ability to handle the possible future extension of  currencies

# Folder structure
- app/
  - Main.hs: Contains the brick application which makes using the necessary functions from the library
- src/
  - Excelent/
    - Definition.hs: Contains the data definitions used in the rest of the application. Also calls the necessary template haskell functions from the relevant libraries
    - Parser.hs: Contains the expression parser
    - Print.hs: Contains usefull functions for printing the content of cells
    - Eval/
      - Eval: Contains the expression evaluating function
      - Graph: Contains functions for generating graphs from expressions. Also contains the code for complete, partial updates and error detection.
      - Checker: Contains the type checking function