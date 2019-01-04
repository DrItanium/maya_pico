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
             ($?str)
             (str-cat "(" (expand$ ?str) ")"))
(deffunction space-concat
             ($?elements)
             (bind ?str
              "")
             (progn$ (?element ?elements)
                     (bind ?str
                           (str-cat ?str
                                    " "
                                    (send ?element
                                          codegen))))
             ?str)

(defmessage-handler LEXEME codegen primary () ?self)
(defmessage-handler NUMBER codegen primary () ?self)
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
(defclass declaration
  (is-a has-title
        has-doc-string)
  (slot decl-title
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (access read-only)
        (default PLEASE-OVERRIDE-IN-CHILD-CLASS))
  (message-handler codegen primary))
(defmessage-handler declaration codegen primary
                    ()
                    (str-cat (dynamic-get decl-title) " "
                             (dynamic-get title)
                             " \""
                             (if (bind ?k 
                                       (dynamic-get documentation)) then
                               ?k
                               else
                               "")
                             "\""))
(defclass defgeneric
  (is-a declaration)
  (slot decl-title
        (source composite)
        (default defgeneric))

  (message-handler codegen primary))
(defmessage-handler defgeneric codegen primary
                    ()
                    (paren-wrap (call-next-handler)))



(defclass argument
  (is-a has-title)
  (slot title-prefix
        (type LEXEME)
        (storage shared)
        (visibility public)
        (access read-only)
        (default ERROR-NOT-OVERWRITTEN-IN-CHILD))
  (message-handler codegen primary))

(defmessage-handler argument codegen primary
                    ()
                    (format nil
                            "%s%s"
                            (dynamic-get title-prefix)
                            (dynamic-get title)))

(defclass singlefield-argument
          (is-a argument)
          (slot title-prefix
                (source composite)
                (default "?")))
(defclass multifield-argument
          (is-a argument)
          (slot title-prefix
                (source composite)
                (default "$?")))

(defclass defmethod-argument
  (is-a argument))



(defclass defmethod-singlefield-argument
  (is-a defmethod-argument
        singlefield-argument)
  (multislot conditional-elements
             (visibility public)
             (storage local))
  (message-handler codegen around))
(defmessage-handler defmethod-singlefield-argument codegen around
                    ()
                    (bind ?output 
                          (call-next-handler))
                    (if (not (empty$ (dynamic-get conditional-elements))) then
                      ; now we must generate the conditional elements one after another
                      (paren-wrap ?output 
                                  (space-concat (dynamic-get conditional-elements)))
                      else
                      ?output))
(defclass deffunction-argument
 (is-a argument))
(defclass deffunction-singlefield-argument
  (is-a deffunction-argument
        singlefield-argument))
(defclass deffunction-multifield-argument
  (is-a deffunction-argument
        multifield-argument))



(defclass defmethod-multifield-argument
  (is-a defmethod-argument
        multifield-argument))

(defclass has-arguments
  (is-a USER)
  (multislot arguments
             (type INSTANCE)
             (visibility public)
             (storage local)))

(defclass has-body
  (is-a USER)
  (multislot body
             (visibility public)
             (storage local)))

(defclass subroutine
          (is-a declaration
                has-arguments
                has-body)
          (message-handler codegen primary))

(defmessage-handler subroutine codegen primary
                    ()
                    (paren-wrap (call-next-handler)
                                " "
                                (paren-wrap (space-concat (dynamic-get arguments)))
                                " "
                                (space-concat (dynamic-get body))))
        
(defclass defmethod
  (is-a subroutine)
  (slot decl-title
        (source composite)
        (default defmethod))
  (multislot arguments
             (source composite)
             (allowed-classes defmethod-argument)))


(defclass deffunction
  (is-a subroutine)
  (slot decl-title
        (source composite)
        (default deffunction))
  (multislot arguments
             (source composite)
             (allowed-classes deffunction-argument)))

