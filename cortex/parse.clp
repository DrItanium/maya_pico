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

(defclass deffunction-declaration 
  (is-a USER)
  (slot title
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot description
        (type STRING)
        (storage local)
        (visibility public))
  (slot arguments
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot body
             (storage local)
             (visibility public)))

(defrule generate-defrule
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents defrule ?rule-name $?lhs => $?rhs))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defrule-declaration
                        (title ?rule-name)
                        (description "")
                        (left-hand-side ?lhs)
                        (right-hand-side ?rhs)))
(defrule move-doc-string-out
         ?f <- (object (is-a defrule-declaration)
                       (left-hand-side ?first $?rest))
         ?k <- (object (is-a atomic-value)
                       (name ?first)
                       (kind STRING)
                       (value ?doc-string))
         =>
         (modify-instance ?f
                          (left-hand-side ?rest)
                          (description ?doc-string))
         (unmake-instance ?k))



(defrule generate-deffunction-with-docstring
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents deffunction
                                 ?func-name
                                 ?doc-string
                                 ?args
                                 $?body))
         ?f4 <- (object (is-a atomic-value)
                        (name ?doc-string)
                        (kind STRING)
                        (value ?str))
         (object (is-a container)
                 (name ?args))
         =>
         (unmake-instance ?f ?f4)
         (make-instance ?name of deffunction-declaration
                        (title ?func-name)
                        (description ?str)
                        (arguments ?args)
                        (body $?body)))

(defrule generate-deffunction-without-docstring
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents deffunction
                                 ?func-name
                                 ?args
                                 $?body))
         (object (is-a container)
                 (name ?args))
         =>
         (unmake-instance ?f)
         (make-instance ?name of deffunction-declaration
                        (title ?func-name)
                        (description "")
                        (arguments ?args)
                        (body $?body)))

(defrule raise-symbols-out-of-atoms
         (declare (salience 10000))
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (contents $?a ?sym-ref $?b))
         ?k <- (object (is-a atomic-value)
                       (name ?sym-ref)
                       (kind SYMBOL)
                       (value ?sym))
         =>
         (unmake-instance ?k)
         (modify-instance ?f 
                          (contents ?a ?sym ?b)))
