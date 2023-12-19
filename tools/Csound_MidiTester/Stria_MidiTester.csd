<CsoundSynthesizer>
<CsOptions>
; amend device number accordingly
;-Q999
-Ma -odac -Q0

</CsOptions>
<CsInstruments>
ksmps = 32 ; no audio so sr and nchnls irrelevant

  instr 1

 xtratim 1 ;extra-time, i.e. release dur
; play a midi note
; read in values from p-fields
ichan     init      p4

; send continuous controller data
iCCnum    =         p5
kCCval    linseg      0, p3, 127.1, 0.2, 127, 0.1, 0  ; continuous controller data function ; contain to raise for one sec and then go to 0 for one
kCCval    =         int(kCCval)   ; convert data function to integers
ktrig     changed   kCCval      ; generate a trigger each time kCCval changes
 if ktrig=1 then                  ; if kCCval has changed...
          midiout   176, ichan, iCCnum, kCCval  ; ...send a controller message
 endif
 printk2 p6
  endin

</CsInstruments>
<CsScore>
;p1 p2 p3 p4(CH) p5(CC) p6(event_nr)
#include "stria_midiCC.sco"

</CsScore>
</CsoundSynthesizer>
;based on MIDI out example by Iain McCurdy
