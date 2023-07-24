;maya
;Copyright (c) 2012-2023, Joshua Scoggins
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

(defclass container
  (is-a USER)
  (role concrete)
  (multislot contents
             (storage local)
             (visibility public))
  (slot parent
        (type SYMBOL
              INSTANCE)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE)))
(defclass atomic-value
  (is-a USER)
  (role concrete)
  (slot kind
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot parent
        (type SYMBOL
              INSTANCE)
        (storage local)
        (visibility public)
        (allowed-symbols FALSE)))

(defclass file-container
  (is-a container)
  (role concrete)
  (slot file-name
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE)))

(deffunction display-tokenize
             (?file ?out)
             (bind ?depth
                   0)
             (while (neq (nth$ 1 
                               (bind ?result 
                                     (next-token ?file)))
                         STOP) do
                    (bind ?first
                          (nth$ 1 
                                ?result))
                    (if (eq ?first 
                            RIGHT_PARENTHESIS) then
                      (bind ?depth 
                            (- ?depth 1)))
                    (loop-for-count (?i 1 ?depth) do
                                    (printout ?out tab))
                    (if (eq ?first 
                            LEFT_PARENTHESIS) then
                      (bind ?depth 
                            (+ ?depth 1)))
                    (printout ?out
                              ?result crlf)
                    )
             )
(deffunction inner-container
             "keep collecting until we hit a right paren and return this object"
             (?parent ?id)
             (bind ?top 
                   (make-instance of container
                                  (parent ?parent)))
             (bind ?children
                   (create$))
             (while (neq (nth$ 1 
                               (bind ?result
                                     (next-token ?id))) STOP) do
                    (bind ?kind
                          (nth$ 1 ?result))
                    (bind ?value
                          (nth$ 2 ?result))
                    (switch ?kind
                            (case LEFT_PARENTHESIS then
                              (bind ?children
                                    ?children
                                    (inner-container (instance-name ?top)
                                                     ?id)))
                            (case RIGHT_PARENTHESIS then
                              (modify-instance ?top
                                               (contents ?children))
                              (return (instance-name ?top)))
                            (default (bind ?children
                                           ?children
                                           (make-instance of atomic-value
                                                          (parent (instance-name ?top))
                                                          (kind ?kind)
                                                          (value ?value)))))
                    )
             (printout stderr 
                       "Hit end of file before finishing expression!" crlf)
             ; if we get here then an error has occurred and we need to display it as such
             FALSE)


(deffunction contain-file
             (?file-name)
             (if (not (open ?file-name 
                            (bind ?file-id 
                                  (gensym*))
                            "r")) then
               FALSE
               else
               (bind ?top
                     (make-instance of file-container
                                    (parent FALSE)
                                    (file-name ?file-name)))
               (bind ?children
                     (create$))
               (while (neq (nth$ 1 
                                 (bind ?result
                                       (next-token ?file-id)))
                           STOP) do
                      (bind ?kind
                            (nth$ 1 ?result))
                      (bind ?value
                            (nth$ 2 ?result))
                      (switch ?kind
                              (case LEFT_PARENTHESIS then
                                (bind ?out
                                      (inner-container (instance-name ?top)
                                                       ?file-id))
                                (if ?out then
                                  (bind ?children
                                        ?children
                                        ?out)
                                  else
                                  (printout stderr 
                                            "Terminating early!" crlf)
                                  (close ?file-id)
                                  (return FALSE)))
                              (case RIGHT_PARENTHESIS then
                                (printout stderr
                                          "Random right paren found!" crlf)
                                (close ?file-id)
                                (return FALSE))
                              (default (bind ?children
                                      ?children
                                      (make-instance of atomic-value
                                                     (parent (instance-name ?top))
                                                     (kind ?kind)
                                                     (value ?value))))))
               (close ?file-id)
               (modify-instance ?top
                                (contents ?children))
               (return ?top))
             )
(defclass defrule-declaration
  (is-a USER)
  (role concrete)
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot description
        (type STRING)
        (storage local)
        (visibility public))
  (multislot left-hand-side
             (storage local)
             (visibility public))
  (multislot right-hand-side
             (storage local)
             (visibility public)))

(defrule generate-defrule-with-documentation
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents ?decl ?title ?doc-string $?lhs ?arrow $?rhs))
         ?f2 <- (object (is-a atomic-value)
                        (name ?decl)
                        (kind SYMBOL)
                        (value defrule))
         ?f3 <- (object (is-a atomic-value)
                        (name ?title)
                        (kind SYMBOL)
                        (value ?rule-name))
         ?f4 <- (object (is-a atomic-value)
                        (name ?doc-string)
                        (kind STRING)
                        (value ?docs))
         ?f5 <- (object (is-a atomic-value)
                        (name ?arrow)
                        (kind SYMBOL)
                        (value =>))
         =>
         (unmake-instance ?f ?f2 ?f3 ?f4 ?f5)
         (make-instance ?name of defrule-declaration
                        (title ?rule-name)
                        (description ?docs)
                        (left-hand-side ?lhs)
                        (right-hand-side ?rhs)))


(defrule generate-defrule-without-documentation
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents ?decl ?title ?doc-string $?lhs ?arrow $?rhs))
         ?f2 <- (object (is-a atomic-value)
                        (name ?decl)
                        (kind SYMBOL)
                        (value defrule))
         ?f3 <- (object (is-a atomic-value)
                        (name ?title)
                        (kind SYMBOL)
                        (value ?rule-name))
         ?f4 <- (object (is-a atomic-value)
                        (name ?doc-string)
                        (kind ~STRING))
         ?f5 <- (object (is-a atomic-value)
                        (name ?arrow)
                        (kind SYMBOL)
                        (value =>))
         =>
         (unmake-instance ?f ?f2 ?f3 ?f5)
         (make-instance ?name of defrule-declaration
                        (title ?rule-name)
                        (description "")
                        (left-hand-side ?doc-string 
                                        ?lhs)
                        (right-hand-side ?rhs)))
