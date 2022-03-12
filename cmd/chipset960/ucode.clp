; @file
; Microcode entry point
; @copyright
; maya
; Copyright (c) 2012-2022, Joshua Scoggins
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

(defgeneric perform-read
            "Request by the i960 for a piece of data from some device (including ram)")
(defgeneric perform-write
            "Request by the i960 to store a value to some device (including ram)")
(defgeneric ucode-init
            "First function called after loading microcode, meant to set everything up")
(defclass MAIN::mapped-device
  (is-a USER)
  (role abstract)
  (pattern-match non-reactive)
  (slot start-address
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot end-address
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot kind
        (type LEXEME)
        (visibility public)
        (storage shared)
        (default UNDEFINED))
  (message-handler responds-to primary)
  (message-handler perform-read primary)
  (message-handler perform-write primary))
(defmessage-handler MAIN::mapped-device responds-to primary
                    (?address)
                    (and (< ?address ?self:end-address)
                         (>= ?address ?self:start-address)))
(defclass MAIN::ram-device
  (is-a mapped-device)
  (role concrete)
  (pattern-match reactive)
  (slot kind
        (source composite)
        (default ram))
  (message-handler perform-read primary)
  (message-handler perform-write primary))

(defmessage-handler MAIN::ram-device perform-read primary
                    (?address)
                    (ram:load ?address))
(defmessage-handler MAIN::ram-device perform-write primary
                    (?address ?value ?style)
                    (ram:store ?address
                               ?value
                               ?style))



(defclass MAIN::transaction-request
  (is-a USER)
  (slot address
        (type INTEGER)
        (range 0 ?VARIABLE)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot style
        (type SYMBOL)
        (allowed-symbols FALSE ; never set
                         none ; error state
                         full16 
                         lower8
                         upper8)
        (visibility public)
        (storage local)
        (default-dynamic FALSE))
  (slot kind
        (type SYMBOL)
        (visibility public)
        (storage local)
        (allowed-symbols READ 
                         WRITE)
        (default ?NONE))
  (slot value
        (type INTEGER)
        (visibility public)
        (storage local)
        (default-dynamic 0))
  (slot serviced
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (visibility public)
        (storage local)))

(defmethod perform-read
  ((?address INTEGER))
  ; kind of unsafe since we are relying that the transaction fact never gets 
  ; retracted during fact execution
  (bind ?the-transaction
        (make-instance of transaction-request
                       (address ?address)
                       (kind READ)
                       (style full16)))
  (run)
  (bind ?result 
        (send ?the-transaction
              get-value))
  (unmake-instance ?the-transaction)
  (format t 
          "(perform-read 0x%x) => 0x%x%n" 
          ?address 
          ?result)
  ?result)

(defmethod perform-write
  ((?address INTEGER)
   (?value INTEGER)
   (?style SYMBOL))
  (format t "(perform-write 0x%x 0x%x %d)%n" ?address ?value ?style)
  (bind ?the-transaction 
        (make-instance of transaction-request
                       (address ?address)
                       (style ?style)
                       (kind WRITE)
                       (value ?value)))
  (run)
  (unmake-instance ?the-transaction))


(defmethod ucode-init
  ()
  (bind ?boot-sys-handle
        (gensym*))
  (if (not (open "boot.sys" ?boot-sys-handle "r")) then
    (shutdown960 "Unable to open boot.sys!"))
  (printout t "Installing boot.sys!" crlf)
  (bind ?address
        0)
  (while (>= (bind ?current-character
                   (get-char ?boot-sys-handle)) 0) do
         (ram:store-byte ?address
                         ?current-character)
         (if (= (mod ?address 4096) 0) then
           (printout t "."))
         (bind ?address 
               (+ ?address 1)))
  (printout t crlf "Finished installing boot.sys!" crlf)
  (close ?boot-sys-handle)
  (bind ?init-phase-fact
        (assert (ucode-setup)))
  (run)
  (retract ?init-phase-fact)
  ; TODO: setup any other things necessary
  )
(definstances MAIN::devices
              (of ram-device 
                  (start-address 0)
                  (end-address (ram:size)))))
; glue logic for dispatching to different peripherals, this is just for the ones that make sense as objects

(defrule MAIN::device-responds-to-address
         (object (is-a transaction-request)
                 (name ?r)
                 (address ?addr0))
         (object (is-a mapped-device)
                 (start-address ?sa&:(>= ?addr0 ?sa))
                 (end-address ?ea&:(< ?addr0 ?ea))
                 (name ?d))
         =>
         (assert (handle-request ?d
                                 ?r)))
(defrule MAIN::address-is-unmapped
         "nothing matched!"
         (declare (salience -10000))
         ?tr <- (object (is-a transaction-request)
                        (serviced FALSE))
         =>
         (modify ?tr 
                 (serviced TRUE)))
(defrule MAIN::carry-out-read-request
         ?f <- (handle-request ?d ?r)
         ?tr <- (object (is-a transaction-request)
                        (name ?r)
                        (kind READ)
                        (serviced FALSE)
                        (address ?addr))
         ?md <- (object (is-a mapped-device) (name ?d))
         =>
         (retract ?f)
         (modify-instance ?tr 
                          (value (send ?md 
                                       perform-read
                                       ?addr))
                          (serviced TRUE)))



(defrule MAIN::carry-out-write-request
         ?f <- (handle-request ?d ?r)
         ?tr <- (object (is-a transaction-request)
                        (name ?r)
                        (kind WRITE)
                        (serviced FALSE)
                        (address ?addr)
                        (style ?style)
                        (value ?value))
         ?md <- (object (is-a mapped-device) (name ?d))
         =>
         (retract ?f)
         (send ?md 
               perform-write
               ?addr
               ?value
               ?style)
         (modify-instance ?tr 
                          (serviced TRUE)))
