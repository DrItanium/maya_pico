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

(deffunction audio-propertiesp
             "Check and see if we got audio properties back from the given file"
             (?path)
             (not (empty$ (get-audio-properties ?path))))
(deffunction basic-tag-infop 
             "Check and see if we have basic tag info in the given file"
             (?path)
             (not (empty$ (get-basic-tag-info ?path))))
(deffunction tag-propertiesp
             "Check and see if we have tag properties in the given file"
             (?path)
             (not (empty$ (get-tag-properties ?path))))
(defclass file
 (is-a USER)
  (slot path
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE)))
(defclass tag-property
  (is-a USER)
  (slot parent
        (type INSTANCE)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot key
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot value 
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE)))
        
(defclass basic-tag-data
  "Extract and save the basic tag data from an extractable file!"
  (is-a USER)
  (slot parent
        (type INSTANCE)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot title
        (type LEXEME)
        (storage local)
        (visibility public))
  (slot artist 
        (type LEXEME)
        (storage local)
        (visibility public))
  (slot album 
        (type LEXEME)
        (storage local)
        (visibility public))
  (slot year 
        (type NUMBER)
        (storage local)
        (visibility public))
  (slot track 
        (type NUMBER)
        (storage local)
        (visibility public))
  (slot genre 
        (type LEXEME)
        (storage local)
        (visibility public)))

(defclass audio-properties
 (is-a USER)
 (slot parent
       (type INSTANCE)
       (storage local)
       (visibility public)
       (default ?NONE))
 (slot bitrate 
       (type NUMBER)
       (storage local)
       (visibility public)
       (default ?NONE))
 (slot sample-rate 
       (type NUMBER)
       (storage local)
       (visibility public)
       (default ?NONE))
 (slot channels
       (type NUMBER)
       (storage local)
       (visibility public)
       (default ?NONE))
 (slot length
       (type NUMBER)
       (storage local)
       (visibility public)
       (default ?NONE))
 (slot mins 
       (type NUMBER)
       (storage local)
       (visibility public))
 (slot secs
       (type NUMBER)
       (storage local)
       (visibility public))
 (message-handler init after))
(defmessage-handler audio-properties init after
                    ()
                    (dynamic-put mins 
                                 (div (dynamic-get length)
                                      60))
                    (dynamic-put secs
                                 (mod (dynamic-get length)
                                      60)))

; testing routines
(deffunction assert-file-facts
             (?path)
             (progn$ (?p (get-recursive-directory-contents ?path))
                     (assert (file-check ?p))))
; testing rules
(defrule make-file-object
         ?f <- (file-check ?path)
         =>
         (retract ?f)
         (make-instance of file
          (path ?path)))
                        
(defrule make-basic-tag-data-fact
         (object (is-a file)
                 (path ?path)
                 (name ?parent))
         (not (basic-tag-info ?path
                              $?))
         (not (object (is-a basic-tag-data)
                      (parent ?parent)))
         (test (basic-tag-infop ?path))
         =>
         (assert (basic-tag-info ?path (get-basic-tag-info ?path))))
(defrule make-tag-property-fact
         (object (is-a file)
                 (path ?path)
                 (name ?parent))
         (test (tag-propertiesp ?path))
         =>
         (assert (tag-properties ?parent 
                                 (get-tag-properties ?path))))
(defrule construct-single-tag-property
         ?f <- (tag-properties ?parent
                               ?key ?value $?rest)
         =>
         (make-instance of tag-property
                        (parent ?parent)
                        (key ?key)
                        (value ?value))
         (retract ?f)
         (assert (tag-properties ?parent $?rest)))
(defrule done-with-tag-properties
         ?f <- (tag-properties ?)
         =>
         (retract ?f))

                                 
(defrule construct-basic-tag-data
         ?f <- (basic-tag-info ?path
                               ?title
                               ?artist
                               ?album
                               ?year
                               ?track
                               ?genre)
         (object (is-a file)
                 (path ?path)
                 (name ?parent))
         (not (object (is-a basic-tag-data)
                      (parent ?parent)))
         =>
         (retract ?f)
         (make-instance of basic-tag-data
                        (parent ?parent)
                        (title ?title)
                        (artist ?artist)
                        (album ?album)
                        (year ?year)
                        (track ?track)
                        (genre ?genre)))
(defrule eliminate-illegal-files
         (declare (salience -1))
         ?o <- (object (is-a file)
                       (name ?name))
         (not (object (is-a basic-tag-data)
                      (parent ?name)))
         (not (object (is-a tag-property)
                      (parent ?name)))
         =>
         (unmake-instance ?o))
