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

(defmessage-handler USER build primary 
                    "anything that is not buildable should still respond to the build message"
                    () 
                    ; does nothing
                    )
(defmessage-handler LEXEME codegen primary () ?self)
(defmessage-handler NUMBER codegen primary () ?self)
(defclass buildable
  (is-a USER)
  (message-handler build primary)
  (message-handler codegen primary))
(defmessage-handler buildable build primary
                    ()
                    (build (send ?self
                                 codegen)))
(defclass has-title
  (is-a USER)
  (slot title
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE)))
(defclass has-module-declaration
  (is-a USER)
  (slot module-declaration
        (type SYMBOL)
        (visibility public)
        (storage local)
        (allowed-symbols FALSE)
        (default-dynamic FALSE)))
(defclass simple-declaration
  (is-a has-title
        has-module-declaration
        buildable)
  (slot decl-title
        (type SYMBOL)
        (storage shared)
        (visibility public)
        (access read-only)
        (default PLEASE-OVERRIDE-IN-CHILD-CLASS))
  (slot module-separator
        (type LEXEME)
        (storage shared)
        (visibility public)
        (access read-only)
        (default "::"))
  (message-handler codegen primary))

(defmessage-handler simple-declaration codegen primary
                    ()
                    (str-cat (dynamic-get decl-title) 
                             " "
                             (if (bind ?m
                                       (dynamic-get module-declaration)) then
                               (str-cat ?m 
                                        (dynamic-get module-separator))
                               else
                               "")
                             (dynamic-get title)))
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
  (is-a simple-declaration
        has-doc-string)
  (message-handler codegen primary))

(defmessage-handler declaration codegen primary
                    ()
                    (str-cat (call-next-handler) 
                             " "
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
(defclass defglobal-entry
  (is-a has-title)
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler codegen primary))

(defmessage-handler defglobal-entry codegen primary
                    ()
                    (format nil
                            "?*%s* = %s"
                            (dynamic-get title)
                            (send (dynamic-get value)
                                  codegen)))

(defclass defglobal
  (is-a simple-declaration
        has-body)
  (slot decl-title
        (source composite)
        (default defglobal))
  (slot module-separator
        (source composite)
        (default " "))
  (multislot body
             (source composite)
             (allowed-classes defglobal-entry))
  (message-handler codegen primary))
(defmessage-handler defglobal codegen primary
                    ()
                    (paren-wrap (call-next-handler)
                                " "
                                (space-concat (dynamic-get body))))
; TODO define constraint classes
(defclass conditional-element
          (is-a USER))
(defclass non-pattern-ce
          (is-a conditional-element)
          (slot action-kind
                (storage shared)
                (visibility public)
                (access read-only)
                (default OVERRIDE))
          (message-handler codegen primary))
(defmessage-handler non-pattern-ce codegen primary 
                    ()
                    (dynamic-get action-kind))
(defclass non-pattern-singlefield-ce
          (is-a non-pattern-ce)
          (slot conditional-element
                (storage local)
                (visibility public)
                (default ?NONE))
          (message-handler codegen primary))
(defmessage-handler non-pattern-singlefield-ce codegen primary
                    ()
                    (paren-wrap (call-next-handler)
                                " "
                                (send (dynamic-get conditional-element)
                                      codegen)))

(defclass not-ce 
  (is-a non-pattern-singlefield-ce)
  (slot action-kind
        (source composite)
        (default not)))
(defclass test-ce 
  (is-a non-pattern-singlefield-ce)
  (slot action-kind
        (source composite)
        (default test)))
(defclass non-pattern-multifield-ce
 (is-a non-pattern-ce)
 (multislot conditional-elements
            (storage local)
            (visibility public)
            (default ?NONE))
 (message-handler codegen primary))
(defmessage-handler non-pattern-multifield-ce codegen primary
                    ()
                    (paren-wrap (call-next-handler)
                                " "
                                (space-concat (dynamic-get conditional-element))))
(defclass and-ce 
  (is-a non-pattern-multifield-ce)
  (slot action-kind
        (source composite)
        (default and)))
(defclass or-ce 
  (is-a non-pattern-multifield-ce)
  (slot action-kind
        (source composite)
        (default or)))
(defclass logical-ce 
  (is-a non-pattern-multifield-ce)
  (slot action-kind
        (source composite)
        (default logical)))
(defclass exists-ce 
  (is-a non-pattern-multifield-ce)
  (slot action-kind
        (source composite)
        (default exists)))
(defclass forall-ce 
  (is-a non-pattern-multifield-ce)
  (slot action-kind
        (source composite)
        (default forall)))
   
          
(defclass assignable-pattern-conditional-element
          (is-a conditional-element)
          (slot bind-name
                (type SYMBOL)
                (storage local)
                (visibility public)
                (default-dynamic FALSE))
          (message-handler codegen around))
(defmessage-handler assignable-pattern-conditional-element codegen around
                    ()
                    (bind ?ce
                          (call-next-handler))
                    (if (bind ?k
                              (dynamic-get bind-name)) then
                      (format nil
                              "?%s <- %s"
                              ?k ?ce)
                      else
                      ?ce))

(defclass ordered-pattern-ce
          (is-a assignable-pattern-conditional-element)
          (slot symbol
                (type SYMBOL)
                (storage local)
                (visibility public)
                (default ?NONE))
          (multislot constraints
                (storage local)
                (visibility public))
          (message-handler codegen primary))
(defmessage-handler ordered-pattern-ce codegen primary 
                    ()
                    (paren-wrap (dynamic-get symbol)
                                " "
                                (space-concat (dynamic-get constraints))))
(defclass lhs-slot
          (is-a USER)
          (slot slot-name
                (type SYMBOL)
                (storage local)
                (visibility public)
                (default ?NONE))
          (message-handler codegen primary))
(defclass singlefield-lhs-slot
  (is-a lhs-slot)
  (slot constraint
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler codegen primary))
(defmessage-handler singlefield-lhs-slot codegen primary
                    ()
                    (paren-wrap (dynamic-get slot-name)
                                " "
                                (send (dynamic-get constraint)
                                      codegen)))
(defclass multifield-lhs-slot
  (is-a lhs-slot)
  (multislot constraints
        (storage local)
        (visibility public))
  (message-handler codegen primary))
(defmessage-handler multifield-lhs-slot codegen primary
                    ()
                    (paren-wrap (dynamic-get slot-name)
                                " "
                                (space-concat (dynamic-get constraints))))
(defclass template-pattern-ce 
  (is-a assignable-pattern-conditional-element)
  (slot deftemplate-name
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot lhs-slots
             (allowed-classes lhs-slot)
             (storage local)
             (visibility public))
  (message-handler codegen primary))

(defmessage-handler template-pattern-ce codegen primary 
                    ()
                    (paren-wrap (dynamic-get deftemplate-name)
                                " "
                                (space-concat (dynamic-get lhs-slots))))

(defclass basic-attribute-constraint
          (is-a USER)
          (slot slot-name
                (type SYMBOL)
                (storage local)
                (visibility public)
                (default ?NONE))
          (message-handler codegen primary))

(defclass generic-attribute-constraint
  (is-a basic-attribute-constraint)
  (multislot constraints
             (storage local)
             (visibility public))
  (message-handler codegen primary))

(defmessage-handler generic-attribute-constraint codegen primary
                    ()
                    (paren-wrap (dynamic-get slot-name)
                                " "
                                (space-concat (dynamic-get constraints))))


(defclass fixed-attribute-constraint
          (is-a basic-attribute-constraint)
          (slot slot-name
                (source composite)
                (storage shared)
                (access read-only)
                (create-accessor read)
                (default OVERRIDE))
          (slot constraint
                (storage local)
                (visibility public)
                (default ?NONE))
          (message-handler codegen primary))

(defmessage-handler fixed-attribute-constraint codegen primary 
                    ()
                    (paren-wrap (dynamic-get slot-name)
                                " "
                                (send (dynamic-get constraint)
                                      codegen)))

(defclass isa-attribute-constraint
          (is-a fixed-attribute-constraint)
          (slot slot-name
                (source composite)
                (default is-a)))

(defclass name-attribute-constraint
          (is-a fixed-attribute-constraint)
          (slot slot-name
                (source composite)
                (default name)))

(defclass object-pattern-ce 
  (is-a assignable-pattern-conditional-element)
  (multislot attribute-constraints 
             (allowed-classes attribute-constraint)
             (storage local)
             (visibility public))
  (message-handler codegen primary))

(defmessage-handler template-pattern-ce codegen primary 
                    ()
                    (paren-wrap "object "
                                (space-concat (dynamic-get attribute-constraints))))
                
(defclass defrule
          (is-a declaration
                has-body)
          (multislot conditional-elements
                     (allowed-classes conditional-element)
                     (storage local)
                     (visibility public))
          (slot decl-title 
                (source composite)
                (default defrule))
          (message-handler codegen primary))

(defmessage-handler defrule codegen primary
                    ()
                    (paren-wrap (call-next-handler)
                                " "
                                (space-concat (dynamic-get conditional-elements))
                                " => "
                                (space-concat (dynamic-get body))))
