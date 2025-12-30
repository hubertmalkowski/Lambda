(lang dune 3.18)

(name lambda-monorepo)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.0)

(package
 (name lambda-utils)
 (depends
  (ocaml (>= 4.14))
  dune))

(package
 (name lambda-let-poly)
 (synopsis "Let-polymorphic lambda calculus with Hindley-Milner type inference")
 (description "Complete implementation of lambda calculus with let-polymorphism")
 (depends
  (ocaml (>= 4.14))
  lambda-utils
  menhir
  dune))


(package
 (name lambda-system-f)
 (synopsis "Lambda system f calculus")
 (description "Complete implementation of lambda calculus with system f type system")
 (depends
  (ocaml (>= 4.14))
  lambda-utils
  menhir
  dune))
