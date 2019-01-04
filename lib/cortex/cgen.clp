; maya
; Copyright (c) 2012-2019, Joshua Scoggins
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; cgen.clp - helps streamline process of dynamic code generation
(deffunction paren-wrap
             (?str)
             (str-cat "(" ?str ")"))

(defclass has-title
          (is-a USER)
          (slot title
                (type SYMBOL)
                (visibility public)
                (storage local)
                (default ?NONE)))
(defclass has-doc-string
          (is-a USER)
          (slot documentation
                (type STRING
                      SYMBOL)
                (visibility public)
                (storage local)
                (allowed-symbols FALSE)
                (default-dynamic FALSE)))
(defclass defgeneric
          (is-a has-title
                has-doc-string))
(defclass defmethod-argument
          (is-a has-title)
          (slot title-prefix
                (type LEXEME)
                (storage shared)
                (visibility public)
                (access read-only)
                (default ERROR-NOT-OVERWRITTEN-IN-CHILD)))
                
(defclass defmethod-singlefield-argument
          (is-a defmethod-argument)
          (slot title-prefix
                (source composite)
                (default "?"))
          (multislot conditional-elements
                     (visibility public)
                     (storage local)))
(defclass defmethod-multifield-argument
          (is-a defmethod-argument)
          (slot title-prefix
                (source composite)
                (default "$?")))
          
(defclass defmethod
          (is-a has-title
                has-doc-string)
          (multislot arguments
                     (type INSTANCE)
                     (visibility public)
                     (storage local))
          (multislot body
                     (visibility public)
                     (storage local)))

