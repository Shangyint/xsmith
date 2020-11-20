#lang scribble/manual

@title{Version String With Git Hash}
@defmodule[version-string-with-git-hash]
@author[(@author+email "William Hatch" "william@hatch.uno")
        (@author+email "Pierce Darragh" "pierce.darragh@gmail.com")
        (@author+email "Eric Eide" "eeide@cs.utah.edu")]

If you want to put a git commit hash in your version info, you might like this package.

@defform[(define-version-strings-with-git-hash
           package-name
           #:just-git-hash just-git-hash-name
           #:with-package-name named-version-string-name
           #:no-package-name no-name-version-string-name
           )]{

Defines @racket[just-git-hash-name], @racket[named-version-string-name], and/or @racket[no-name-version-string-name] as strings.
But each of those keyword arguments is optional.

Example:
@racketblock[
             (define-version-strings-with-git-hash "version-string-with-git-hash"
               #:with-package-name wpn
               #:no-package-name npn
               #:just-git-hash jgh)
             (displayln wpn)
             (displayln npn)
             (displayln jgh)
]

Will display something like:

@verbatim{
version-string-with-git-hash 2.0.3 (7a0927c)
2.0.3 (7a0927c)
7a0927c
}


}
