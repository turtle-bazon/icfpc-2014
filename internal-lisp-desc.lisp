
;; main ::
(lambda (world-initial-state unknown)
  ;; prepare AI
  ;; ...
  (cons initial-ai-state
        (lambda (current-ai-state current-world-state)
          ;; run AI
          ;; (current-ai-state, current-world-state) -> next-ai-state
          ;; 
          (cons next-ai-state lambdaman-move))))

;; initial-ai-state, current-ai-state ::
;; opaque

;; world-initial-state, current-world-state ::
(cons map (cons lambdaman-status (cons ghosts-statuses fruits-statuses)))

;; map ::
'(cons (cons cell-0-0 (cons cell-0-1 (cons cell-0... 0)))
  (cons (cons cell-1-0 (cons cell-1-1 (cons cell-1... 0)))
   (cons row-... 0)))

;; cell-?-? ::
(or 'wall 'empty 'pill 'power-pill 'fruit-location 'lambdaman-start-position 'ghost-starting-position)

;; wall :: 0
;; empty :: 1
;; pill :: 2
;; power-pill :: 3
;; fruit-location :: 4
;; lambdaman-start-position :: 5
;; ghost-starting-position :: 6

;; lambdaman-status ::
(cons lambdaman-vitality (cons location (cons direction (cons lives-remain score))))

;; lambdaman-vitality ::
(or 'standard-mode 'power-pill-mode)

;; standard-mode :: 0

;; power-pill-mode ::
game-ticks-remaining

;; game-ticks-remaining :: integer

;; location ::
(cons x y)

;; direction ::
(or 'up 'right 'down 'left)

;; up :: 0
;; right :: 1
;; down :: 2
;; left :: 3

;; lives-remain :: integer
;; score :: integer

;; ghosts-statuses ::
(cons ghost-0-status (cons ghost-1-status (cons ghost-...-status 0)))

;; ghost-?-status ::
(cons ghost-vitality (cons location (cons direction 0)))

;; ghost-vitality ::
(or 'standard 'fright-mode 'invisible)

;; standard :: 0
;; fright-mode :: 1
;; invisible :: 2

;; fruits-statuses ::

