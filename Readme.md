# Imperative Language Interpreter

This project is an interpreter for a custom imperative programming language, which is similar to Latte and written in Haskell. The language supports a subset of features typically found in imperative languages, with a focus on simplicity and demonstrating core programming language concepts.

## Features

The interpreter currently supports the following features, with their corresponding point values as per the project's scoring system:

### Basic Features (15 Points)
- **01 (Three Types)**: The language supports three basic types: `int` for integers, `bool` for boolean values, `string` for string literals, and `void` for functions that do not return a value.
- **02 (Literals, Arithmetic, Comparisons)**: It includes integer and string literals, basic arithmetic operations, and comparison operators.
- **03 (Variables, Assignment)**: Users can declare variables and assign values to them.
- **04 (Print)**: The language provides a print function to output values.
- **05 (While, If)**: It includes control flow constructs such as `while` loops and `if` statements.
- **06 (Functions or Procedures, Recursion)**: Functions or procedures can be defined and called, including recursive function calls.
- **07 (Pass by Variable / Value / In/Out)**: The language supports passing arguments by variable, by value, and in/out parameters.

### Advanced Features (20 Points)
- **09 (Shadowing and Static Binding)**: Variable shadowing is supported, and the language uses static binding.
- **10 (Runtime Error Handling)**: The interpreter can handle runtime errors gracefully.
- **11 (Value-Returning Functions)**: Functions can return values.

### Extended Features (30 Points)
- **12 (Static Typing)**: The language enforces static typing, ensuring type safety at compile-time.
- **16 (Break, Continue)**: The `break` and `continue` statements are implemented for loop control.
