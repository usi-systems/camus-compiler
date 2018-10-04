# Install Dependencies

1. OCaml Package Manager (OPAM)

On Ubuntu 16.04, simply issue the following command:
```
sudo apt-get install opam
```

Otherwise, follow the instructions at the OPAM [web page](https://opam.ocaml.org/doc/Install.html).

After the install completes, be sure to execute the command
```
opam init
eval `opam config env`
```
to set up your environment variables to use OPAM-installed packages. You may want to add this to your shell's initialization script (e.g., `.bashrc`).

2. OCaml 4.04.0

Upgrade the OCaml compiler to version 4.04.0 using OPAM:
```
opam switch 4.04.0
```

3. OCaml Packages

Install OCaml dependencies using OPAM:
```
opam install async core core_extended oasis menhir ocamlgraph jbuilder ocaml-migrate-parsetree bignum mparser ppx_deriving ipaddr
```

# Build

```
make
```

# Running

Compile the ITCH example:

    ./camus.exe -rules examples/itch_rules.txt -rt-out generated -prog-out generated.p4 -dot-out generated.dot examples/itch.p4
    
You can vizualize the BDD that's generated: 

    dot -Tpng generated.dot > bdd.png

And inspect the generatd P4 program, as well as the table entries:

    cat generated.p4
    cat generated*
