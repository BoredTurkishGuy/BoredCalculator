# BoredCalculator

A scuffed Command Line Interface calculator, made by one man in Haskell.

---

## DISCLAIMER

I do not know how many problems this code can cause on your system, especially if you are not careful with whatever you are doing. Please, be careful while editing the code, distributing it, or making any changes since my code is scuffed and hard to read.

---

## Features

- **Basic Arithmetic**: Supports `+`, `-`, `*`, `/`, `%`, `//`, `^` operators.
- **Variable Assignment**: Assign values to variables (e.g., `x = 10`).
- **Mathematical Functions**:
  - `sin(x)`, `cos(x)`, `tan(x)`
  - `log(x)`, `exp(x)`, `sqrt(x)`
  - `factorial(x)`, `gcd(x, y)`, `lcm(x, y)`
  - `fibonacci(x)`, `prime(x)`, `nthprime(x)`
  - `min(x, y)`, `max(x, y)`
- **Command Support**:
  - `:help`: Display available commands and examples.
  - `:clear`: Clear the screen.
  - `:vars`: Show all defined variables and their values.
  - `:quit`: Exit the calculator.

---

## Compiling Guide

Follow these instructions to compile and run the calculator on your system.

### Prerequisites

1. **Haskell Platform** (includes GHC and Cabal):
   - [Download and install GHCup](https://www.haskell.org/ghcup/) to manage GHC and Cabal.
   - Alternatively, install the Haskell Platform directly from [haskell.org](https://www.haskell.org/platform/).

2. **Stack (optional but recommended)**:
   - [Download and install Stack](https://haskellstack.org/).

3. **Git**:
   - [Download and install Git](https://git-scm.com/downloads).


### Cloning the Repository

1. Open a terminal or command prompt.
2. Clone the repository using Git:
   ```bash
   git clone https://github.com/yourusername/BoredCalculator.git
   cd BoredCalculator
   ```

---

### Using Cabal

1. **Install Dependencies**:
   ```bash
   cabal install --lib
   ```

2. **Build the Project**:
   ```bash
   cabal build
   ```

3. **Run the Calculator**:
   ```bash
   cabal run
   ```

---

### Using Stack

1. **Build the Project**:
   ```bash
   stack build
   ```

2. **Run the Calculator**:
   ```bash
   stack exec BoredCalculator
   ```

---

### Compiling Manually with GHC

If you prefer to compile manually:

1. **Install Dependencies**:
   ```bash
   cabal install ansi-terminal parsec containers mtl --lib
   ```

2. **Compile the Program**:
   ```bash
   ghc BoredCalculator.hs -o BoredCalculator
   ```

3. **Run the Calculator**:
   ```bash
   ./BoredCalculator
   ```

---

## Notes

1. **Editing the Code**:
   - If you modify the code, you’ll need to recompile the project using the steps above.

2. **Common Issues**:
   - If dependencies are missing, ensure they are installed using:
     ```bash
     cabal install --lib
     ```
     or
     ```bash
     stack build
     ```

3. **Platform Compatibility**:
   - The program is tested on Windows but should work on Linux and macOS with appropriate Haskell setup.

---

## Examples

```text
>> x = 5
= 5.0

>> y = factorial(x)
= 120.0

>> gcd(10, 15)
= 5.0

>> nthprime(10)
= 29.0
```

---

## Contributing

Feel free to fork this repository, make changes, and submit a pull request. Contributions are welcome, but please ensure your changes are tested and maintain the spirit of this scuffed project.

---

## License

This project is licensed under the [Apache 2.0 LICENSE](https://github.com/BoredTurkishGuy/BoredCalculator/blob/main/LICENSE). See the LICENSE file for details.
