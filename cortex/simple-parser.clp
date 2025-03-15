;maya
;Copyright (c) 2012-2025, Joshua Scoggins
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; A simple function based recursive descent parser of lisp code
(include cortex/has-parent.clp)
(include cortex/container.clp)

(defclass atomic-value
  (is-a has-parent)
  (slot kind
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE))
  )

(defclass file-container
  (is-a container)
  (slot file-name
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE)))

(defgeneric MAIN::parse-file
            "Parse the given CLIPS file and return the result as a file-container instance")

(defmethod MAIN::parse-file
  ((?path LEXEME))
  (bind ?result
        FALSE)
  (bind ?id
        (gensym*))
  (with-open-file (?path ?id "r")
    (bind ?result
          (make-instance of file-container
                         (file-name ?path)))
    (while (neq (bind ?value
                 (next-token ?id)) 
  ?result)

(defmethod MAIN::parse-file
  ((?container container)
   (?file-handle SYMBOL))
  (while 


