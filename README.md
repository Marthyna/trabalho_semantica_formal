# L1 (extended) interpreter

## Project Setup

Follow the steps below to set up the project environment:

### Prerequisites

Ensure you have the following installed on your system:

- [OCaml](https://ocaml.org/) (version 4.12 or later recommended)
- [Opam](https://opam.ocaml.org/) (OCaml package manager)
- [Dune](https://dune.build/) (build system for OCaml projects)

### Installation

1. **Initialize Opam:**

   ```sh
   opam init
   eval $(opam config env)
   ```

2. **Install Dependencies:**

   ```sh
   opam install ocamlformat
   opam install ocaml-lsp-server
   opam install dune
   ```

3. **Clone the Repository:**

   ```sh
   git clone <repository-url>
   cd trabalho_semantica
   ```

4. **Build the Project:**
   Use `dune` to build the project:

   ```sh
   dune build
   ```

   For continuous rebuilding during development:

   ```sh
   dune build --watch --terminal-persistence=clear-on-rebuild
   ```

5. **Test:**
   Run the test suite using `dune`:
   ```sh
   dune clean
   dune runtest
   ```

### Additional Notes

- Use `ocamlformat` to format the code:

  ```sh
  dune build @fmt
  dune promote
  ```

- For editing, use an OCaml-compatible IDE or editor with LSP support (e.g., Visual Studio Code with the OCaml plugin).
