; used Claude AI
#lang s-exp framework/keybinding-lang

(keybinding
 "c:s:z"
 (λ (ed evt)
   (send ed redo)))

