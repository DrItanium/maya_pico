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

(defmethod perform-read
((?address INTEGER)
 (?style INTEGER))
 (format t "(perform-read 0x%x %d)%n" ?address ?style)
 0)
(defmethod perform-read
((?address INTEGER)
 (?style LEXEME))
 (format t "(perform-read 0x%x %s)%n" ?address ?style)
 0)

(defmethod perform-read
((?address INTEGER))
 (format t "(perform-read 0x%x)%n" ?address)
 0)

(defmethod perform-write
((?address INTEGER)
 (?value INTEGER)
 (?style INTEGER))
 (format t "(perform-write 0x%x 0x%x %d)%n" ?address ?value ?style)
 )

(defmethod perform-write
((?address INTEGER)
 (?value INTEGER)
 (?style LEXEME))
 (format t "(perform-write 0x%x 0x%x %s)%n" ?address ?value ?style)
 )

(defmethod ucode-init
()
; TODO: load boot.sys here
; TODO: setup any other things necessary
)