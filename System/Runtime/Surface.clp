;------------------------------------------------------------------------------
;Catharsis
;Copyright (c) 2013, Joshua Scoggins 
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;    * Neither the name of Joshua Scoggins nor the
;      names of its contributors may be used to endorse or promote products
;      derived from this software without specific prior written permission.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL Joshua Scoggins BE LIABLE FOR ANY
;DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;------------------------------------------------------------------------------
; Surface.clp - Defines a block of tiles to draw to the screen. Surfaces can't
; draw directly to the screen. That is up to the screen class.
; 
; Written by Joshua Scoggins 
; Started on 3/30/2013
;------------------------------------------------------------------------------
; Modify these global values to change tile size. 
; Only do this if you know what you're doing
;------------------------------------------------------------------------------
(defclass engine::Surface
  "Defines the screen to draw to. A screen is made up of one or more surfaces
  which contain tiles."
  (is-a DrawableObject)
  (role concrete)
  (pattern-match reactive)
  (slot width 
        (source composite)
        (create-accessor read-write)
        (range 0 ?VARIABLE))
  (slot height 
        (source composite)
        (create-accessor read-write)
        (range 0 ?VARIABLE))
  (slot position-x 
        (type INTEGER)
        (visibility public)
        (range 0 ?VARIABLE))
  (slot position-y 
        (type INTEGER)
        (visibility public)
        (range 0 ?VARIABLE))
  (slot visible
        (type SYMBOL)
        (allowed-symbols FALSE TRUE)
        (visibility public))
  (multislot tiles (type SYMBOL)))
;------------------------------------------------------------------------------
