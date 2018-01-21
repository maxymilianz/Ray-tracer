Tested on Lubuntu and Cygwin.

To compile, type:
`ocamlbuild -pkg graphics -pkg str 'main.native'`.
To run the built executable, type:
`./main.native`.
To delete files necessary to run, except `main.native`, type:
`ocamlbuild -clean`.

Ray-tracer either displays the generated image (not on Cygwin), or saves it to file in ppm format.

To compile, You need OCaml and Linux or Cygwin. Installation instructions:
https://ocaml.org/docs/install.html