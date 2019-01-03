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
(deffacts stage-layout
          (stage (current generate)
                 (rest correlate
                       associate)))
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

(defclass property-correlation
  "Associate tag properties with the same key and value"
  (is-a USER)
  (slot key
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot value 
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot associated-files))
(defclass album
  (is-a USER)
  (slot title
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot files
             (storage local)
             (visibility public)
             (default ?NONE)))
(defclass album-artist
  (is-a USER)
  (slot title
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot files
             (storage local)
             (visibility public)
             (default ?NONE)))
(defclass artist
  (is-a USER)
  (slot title
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot files
             (storage local)
             (visibility public)
             (default ?NONE))
  (multislot albums
             (storage local)
             (visibility public)))
            
; testing routines
(deffunction assert-file-facts
             (?path)
             (progn$ (?p (get-recursive-directory-contents ?path))
                     (assert (file-check ?p))))
; testing rules
(defrule make-file-object
         (stage (current generate))
         ?f <- (file-check ?path)
         =>
         (retract ?f)
         (make-instance of file
                        (path ?path)))

(defrule make-basic-tag-data-fact
         (stage (current generate))
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
         (stage (current generate))
         (object (is-a file)
                 (path ?path)
                 (name ?parent))
         (test (tag-propertiesp ?path))
         =>
         (assert (tag-properties ?parent 
                                 (get-tag-properties ?path))))
(defrule construct-single-tag-property
         (stage (current generate))
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
         (stage (current generate))
         ?f <- (tag-properties ?)
         =>
         (retract ?f))


(defrule construct-basic-tag-data
         (stage (current generate))
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
(defrule make-audio-property-data-fact
         (stage (current generate))
         (object (is-a file)
                 (path ?path)
                 (name ?parent))
         (test (audio-propertiesp ?path))
         (not (audio-propery-data ?parent $?))
         (not (object (is-a audio-properties)
                      (parent ?parent)))
         =>
         (assert (audio-property-data ?parent
                                      (get-audio-properties ?path))))
(defrule construct-audio-properties
         (stage (current generate))
         ?f <- (audio-property-data ?parent
                                    ?bitrate
                                    ?sampleRate
                                    ?channels
                                    ?length)
         =>
         (retract ?f)
         (make-instance of audio-properties
                        (parent ?parent)
                        (bitrate ?bitrate)
                        (sample-rate ?sampleRate)
                        (channels ?channels)
                        (length ?length)))
(defrule eliminate-illegal-files
         (declare (salience -1))
         (stage (current generate))
         ?o <- (object (is-a file)
                       (name ?name))
         (not (object (is-a basic-tag-data)
                      (parent ?name)))
         (not (object (is-a tag-property)
                      (parent ?name)))
         (not (object (is-a audio-properties)
                      (parent ?name)))
         =>
         (unmake-instance ?o))


(defrule construct-property-correlation
         (stage (current generate))
         (object (is-a tag-property)
                 (key ?key)
                 (value ?value)
                 (parent ?parent))
         (not (object (is-a property-correlation)
                      (key ?key)
                      (value ?value)))
         =>
         (assert (associated ?parent with 
                             (make-instance of property-correlation 
                                            (associated-files ?parent)
                                            (key ?key)
                                            (value ?value)))))
(defrule update-property-correlation
         (stage (current generate))
         (object (is-a tag-property)
                 (key ?key)
                 (value ?value)
                 (parent ?parent))
         ?obj <- (object (is-a property-correlation)
                         (key ?key)
                         (value ?value)
                         (associated-files $?files)
                         (name ?pc))
         (not (associated ?parent with ?pc))
         =>
         (assert (associated ?parent with ?pc))
         (modify-instance ?obj
                          (associated-files $?files 
                                            ?parent)))

(deffacts high-level-prop-construction-facts
          (make-object album from property-correlation "ALBUM")
          (make-object album-artist from property-correlation "ALBUMARTIST")
          (make-object artist from property-correlation "ARTIST"))

(defrule translate-property-correlation
         "take a property-correlation and construct another object of it."
         (stage (current correlate))
         (make-object ?output-type from property-correlation ?key)
         (object (is-a property-correlation)
                 (key ?key)
                 (value ?title)
                 (associated-files $?files))
         =>
         (make-instance of ?output-type
                        (title ?title)
                        (files $?files)))
; neat data correlations we can perform now
(defrule album-by-a-single-artist
         (stage (current associate))
         (object (is-a album)
                 (title ?title)
                 (files $?files)
                 (name ?album))
         (object (is-a artist)
                 (title ?artist)
                 (files $?artist-files)
                 (name ?artist-name))
         (test (subsetp $?files
                        $?artist-files))
         =>
         ;(printout t "The album '" ?title "' has the single artist '" ?artist "'!" crlf)
         (assert (album ?album has single author ?artist-name)))
(defrule mark-artist-for-given-song
         (stage (current associate))
         (object (is-a album)
                 (title ?title)
                 (files $? ?file $?)
                 (name ?album))
         (object (is-a artist)
                 (title ?artist)
                 (files $? ?file $?)
                 (name ?artist-name))
         =>
         (assert (album ?album features artist ?artist-name)))
(defrule album-is-compilation-album
         (stage (current associate))
         (album ?album features artist ?artist-name)
         (object (is-a album)
                 (name ?album)
                 (title ?title)
                 (files $?files))
         (object (is-a artist)
                 (name ?artist-name)
                 (title ?artist)
                 (files $?artist-files))
         (test (not (subsetp $?files
                             $?artist-files)))
         ; since that check failed it means that there are files in the album which this artist did not make
         =>
         (assert (album ?album is compilation album)))

(defrule identify-songs-not-part-of-an-album
         (stage (current associate))
         (object (is-a file)
                 (name ?file))
         (not (object (is-a album)
                      (files $? ?file $?)))
         (object (is-a basic-tag-data)
                 (parent ?file)
                 (title ?title))
         =>
         (assert (song ?file has no album)))

(defrule identify-songs-with-only-audio-data
         (stage (current associate))
         (object (is-a file)
                 (name ?file))
         (not (object (is-a tag-property)
               (parent ?file)))
         (not (object (is-a basic-tag-data)
               (parent ?file)))
         (object (is-a audio-properties)
                 (parent ?file))
         =>
         (assert (file ?file is untagged)))

(defrule add-album-to-artist
         (stage (current associate))
         (album ?album features artist ?artist)
         ?art <- (object (is-a artist)
                         (name ?artist)
                         (albums $?albums))
         (not (added album ?album to artist ?artist))
         =>
         (assert (added album ?album to artist ?artist))
         (modify-instance ?art
                          (albums $?albums
                                  ?album)))


(deffunction generic-list-all
             (?router ?title ?kind)
             (printout ?router
                       ?title crlf)
             (do-for-all-instances ((?a ?kind))
                                   TRUE
                                   (printout ?router
                                             tab "- \"" ?a:title "\"" crlf)))

             
(deffunction list-all-albums
             (?router)
             (generic-list-all ?router
                               "List of Albums: "
                               album))
(deffunction list-all-artists
             (?router)
             (generic-list-all ?router
                               "List of Artists: "
                               artist))
(deffunction list-all-songs
             (?router)
             (generic-list-all ?router
                               "List of Songs: "
                               basic-tag-data))
