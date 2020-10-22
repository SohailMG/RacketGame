#lang racket

(require racket/gui)

;;makeCode function takes two arguments number and maximum value
;;to create the 4 digit code player needs to guess
;;pre-condition: num and max must me real number > 0
;;  4 6 => 4 elemnts in a list with random numbers from 0 to 6 - 1
(define (makeCode lengthoflst maxval)
  (define (solution lengthoflst max l)
    (cond ((zero? lengthoflst) l)
          (else (solution (- lengthoflst 1) max (cons (random max) l)))))
  (solution lengthoflst maxval '()))

;;maximum value of numbers in a list i.e 0 1 2 3 4 5
;;definining solution to call makeCode and pass it values 4 and 6
(define max-val 6)
(define sol (makeCode 4 max-val))
;----------------------------------------------------------------------
;;GREENS takes two lists and accumlator
;;iterates through the lists to
;;check if each element in l1 is also in l2 and
;;they are in the same position
;;for every equal elements GREENS adds 1 to acc
;; GREENS returns number of elm that are in the correct pos
;;given '(1 2 3 4) AND '(5 2 7 7) ->> 1
(define (GREENS l1 l2)			
  (define (my-greens l1 l2 acc)
    (cond ((null? l1) acc)
          ((equal? (first l1) (first l2)) (my-greens(rest l1)
                                            (rest l2)
                                            (+ acc 1)))
          (else (my-greens (rest l1) (rest l2) acc))))
  (my-greens l1 l2 0))

;;sum takes a list and outputs the sum of all numbers in that list
;;pre-condition: l must be a list of numbers
;;post-condition: sumlst should add all numbers in the given list and return the sum
(define (sumlst l)
  (for/sum ((i l))i))

;;REDS returns the number of elements
;;that are equal in both l1 and l2
;;without looking at the positions
;;minim is used to filter out equal numbers
;;get the length of each l1 and l2
;;get the minimum out of them both
;;i.e (minim 3 '(1 2 3 4) '(1 2 4 4))=> 2
(define (REDS l1 l2)
  (define (minim x l1 l2)
    (min (length (filter (lambda (y) (equal? y x)) l1))
         (length (filter (lambda (y) (equal? y x)) l2))))
  (define (my-reds l1 l2 res)
    (sumlst (map (lambda (x) (minim x l1 l2)) 
              (build-list max-val values))))
  (my-reds l1 l2 0))

;;checkAns will take both
;;GREENS and REDS
;;so green is essentially (GREEENS = GREENS)
;;and red = (GREENS - REDS)
;;making a string for both greens and reds
;;and the remainig will show ? meaning that a number doesn't exist
(define (checkAns playerInput l2)
  (let* ((green (GREENS playerInput l2))
	 (red (- (REDS playerInput l2) green)))
    (string-append
     (make-string green #\G)
     (make-string red #\R)
     (make-string (- (length l2) green red) #\?))))

(define (comparelsts l)
  (checkAns l sol))
;------------------------------------------------------
;GUI FOR INITIAL SCREEN
;pan pan2 are used to allocate the buttons in the middle

(define init-frame (new frame%
                        [label "Crack The Code"]
                        [width 500]
                        [height 300]))
(define pan (new horizontal-panel%
                 [parent init-frame]))
             

(define msg1 (new message%
                  [parent init-frame]
                  [label "Welcome to Crack The Code"]))

(define startbutt (new button%
                       [parent init-frame]
                       [label "Start Game"]
                       [callback (lambda (o e)
                                   (soundeffect buttonsound #t)
                                   (send init-frame show #f)
                                   (send mainframe show #t))]))
(define helpbutt (new button%
                      [parent init-frame]
                      [label "How to play"]
                      [callback (lambda (o e) (soundeffect buttonsound #t)
                                              (send helpframe show #t)
                                              (send init-frame  show #f))]))
(define pan2 (new horizontal-panel%
                 [parent init-frame]))

;;how to play section;;
;;--------------------------------------------------------------------------

(define helpframe (new frame%
                       [label "How to play"]
                       [width 500]
                       [height 300]))
(define canvas (new canvas%
                    [parent helpframe]))
(define dc (send canvas get-dc))

(define go-back (new button%
                      [parent helpframe]
                      [label "Go Back"]
                      [callback (lambda (o e) (soundeffect buttonsound #t)
                                              (send helpframe show #f)
                                              (send init-frame  show #t))]))

(define startbutt2 (new button%
                       [parent helpframe]
                       [label "Start Game"]
                       [callback (lambda (o e)
                                   (soundeffect buttonsound #t)
                                   (send helpframe show #f)
                                   (send mainframe show #t))]))


(send helpframe show #f)

                        

;;---------------------------------------------------------------
;;Main frame for the game
(define mainframe (new frame%
                       [label "Game"]
                       [width 500]
                       [height 300]))
(define helptext (new message%
                  [parent mainframe]
                  [label "Press Crack after choosing all four digits"]))
(define msg2 (new message%
                  [parent mainframe]
                  [label "no choices yet..."]))
(define choicePanel (new horizontal-panel%
                  [parent mainframe]))
                  
;;makechoices will create a dropdown
;;choice menue from 0 to 5
;;it will also display the currently selected number
;;and display it on the msg2 function
(define (makechoices panel numberofchoices)
  (cond ((zero? numberofchoices) '()) 
        (else  
         (new choice% 
              [label ""]
              [parent panel]
              [choices '("0" "1" "2" "3" "4" "5")]
              [callback (lambda (c e) 
                          (send msg2 set-label(number->string(send c get-selection))))])
         (makechoices panel (- numberofchoices 1)))))
(makechoices choicePanel 4)

;;this function retrieves the currently selected
;;item from the choices
;;then displays them in a listbox
;;using get-children inorder to retrieve the elements in the same order
;;first half of the function will let playerinput to be
;;all choices player chose
;;second half of the code will store the choices in a listbox
;;and appends the cho0ices with the results of applying playerinput to comparelst
(define (results)
  (let ((playerInput (map (lambda (x) (number->string (send x get-selection)))
                          (send choicePanel get-children))))
    (send store-attempts append (format "~a => ~a" playerInput (comparelsts (map (lambda (x) (string->number x)) playerInput))))
    ;;this cond statement will check if the player input is
    ;;equal to the solution if #t then player wins
     (cond
      ((equal? (map string->number playerInput )sol)
                                              (sleep 0.5)
                                              (send winframe show #t)
                                              (send the-timer stop)
                                              (send mainframe show #f)
                                              (soundeffect winfile #t)))))

(define buttpan (new vertical-panel%
                     [parent mainframe]))
(define crackit (new button%
                     [label "Crack It"]
                     [parent buttpan]
                     [callback (lambda (o e) (soundeffect trysound #t)
                                             (results)
                                             (the-timer 1000))]))
                                          
;;this list box will be used to store in all the attempts
(define store-attempts (new list-box% 
                [parent mainframe] 
                [label " The order of these are random
                        \n G= correct number and position
                        \n R= correct number wrong position
                        \n ?= number doesn't exist"]
                [choices '()]))

(define finalpanel (new vertical-panel%
                        [parent mainframe]))
(define end (new button%
                     [parent finalpanel]
                     [label "End Game"]
                     [callback (lambda (o e)(send mainframe show #f)
                                             (send endframe show #t)
                                              (sleep 1)
                                              (send endframe show #f))]))
(define (new-game)
  (let [(new-sol (makeCode 4 6))]
    (set! sol new-sol)))
(define newgamebutt (new button%
                         [parent finalpanel]
                         [label "New Code"]
                         [callback (lambda (o e) (new-game)
                                                 (send store-attempts set '()))]))

;;this function will set the message label to the current solution or code
(define (show-Code) (send msg2 set-label
                          (format "~a" sol)))
(define cheatbutton (new button%
                           [parent finalpanel ]
                           [label "Cheat"]
                           [callback (lambda (o e) (show-Code))]))

;;exit game frame ;;
;;-----------------------------------------------------------------------
(define endframe (new frame%
                      [label "Code Cracker"]
                      [width 500 ]
                      [height 300]))
(define endmsg (new message%
                    [parent endframe]
                    [label " Thank you for playing "]))
;;----------------------------------------------------------------------
;;Game win frame

(define winframe (new frame%
                      [label "Code Cracker"]
                      [width 500]
                      [height 300]))
(define winmsg (new message%
                    [parent winframe]
                    [label "YOU WON!! Congratulations on wasting your time "]))

(define play-again (new button%
                      [parent winframe]
                      [label "Play again"]
                      [callback (lambda (o e) 
                                              (send winframe show #f)
                                              (send gameover show #f)
                                              (send mainframe  show #t)
                                              (new-game)
                                              (send store-attempts set '()))]))


;SOUNDS--------------------------------------------------------------
;;definining all sound files for background music
;;and button click sounds
;;as well as win and lose sound
(define winfile "tst.mp3")
(define losefile "gameover.mp3")
(define buttonsound "button.mp3")
(define trysound  "crackit.mp3")

;;this function takes two arguments
;; one for the path file which is defined above
;; the other one is a booleon value #t or #f
 (define (soundeffect sound n)
   (play-sound sound n))

;timer section ----------------------------------------

(define timermsg (new message%
                      [parent mainframe]
                      [label "Timer"]))
(define countdown 60)
(define (the-timer n)
  (new timer% [notify-callback
                (lambda ()
                  (set! countdown (- countdown 1))
                  (send timermsg set-label (number->string countdown))
                  (when (= countdown 0)
                    (send mainframe show #f)
                    (send gameover show #t)))]
              [interval n]))
(the-timer #f)

(define gameover (new frame%
                      [label "Game Over"]
                      [width 500]
                      [height 300]))
(define gameovermsg (new message%
                         [parent gameover]
                         [label "TIMES UP!! PC WINS"]))

(define play-again2 (new button%
                      [parent gameover]
                      [label "Play again"]
                      [callback (lambda (o e) 
                                              (send gameover show #f)
                                              (send mainframe  show #t)
                                              (new-game)
                                              (send store-attempts set '()))]))


         

(send init-frame show #t)
(sleep/yield 0.2)
;;This is all text used for the how to play screen
;;all texts are drawn on a canvas
(send dc draw-text "Welcome to Crack the Code" 10 10   )
(send dc draw-text "step-1 PC chooses a four digit code that you must crack" 10 30   )
(send dc draw-text "step-2 Choose four numbers from the choice menue"   10 50   )
(send dc draw-text "step-3 Press the crack button to check how many numbers are correct"   10 70   )
(send dc draw-text "step-4 Numbers chose will show in the log with symbols at the end"   10 90   )
(send dc draw-text "       G means a number is correct and in the right position"   10 110   )
(send dc draw-text "       R means a number is correct but in the wrong position"   10 130   )
(send dc draw-text "       In order to win you need to make all four GGGG"   10 150   )
