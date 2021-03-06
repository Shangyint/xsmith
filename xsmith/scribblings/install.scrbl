#lang scribble/manual
@; -*- mode: Scribble -*-
@;
@; Copyright (c) 2019-2020 The University of Utah
@; All rights reserved.
@;
@; This file is part of Xsmith, a generator of highly effective fuzz testers.
@;
@; Redistribution and use in source and binary forms, with or without
@; modification, are permitted provided that the following conditions are met:
@;
@;   * Redistributions of source code must retain the above copyright notice,
@;     this list of conditions and the following disclaimer.
@;
@;   * Redistributions in binary form must reproduce the above copyright
@;     notice, this list of conditions and the following disclaimer in the
@;     documentation and/or other materials provided with the distribution.
@;
@; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
@; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
@; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
@; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
@; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
@; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
@; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
@; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
@; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
@; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
@; POSSIBILITY OF SUCH DAMAGE.

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@(require
"util.rkt"
)

@title{How to Install Xsmith}

First, install Racket.  If your operating system's package manager doesn't have a package or you want a fresher version, @hyperlink["https://download.racket-lang.org/"]{download it}. Make sure that @exec{racket} and @exec{raco} is in your @link["https://github.com/racket/racket/wiki/Configure-Command-Line-for-Racket"]{@envvar{PATH}}.

Then run @exec{raco pkg install xsmith} if you want to install the current release version.

Alternatively, run @exec{raco pkg install https://gitlab.flux.utah.edu/xsmith/xsmith.git\?path\=xsmith} if you want to install the latest development version.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@; End of file.
