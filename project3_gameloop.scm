(load "error_handler.scm")

;; An elaborate game loop for Project 3
;; MUCH nicer than the provided one, since it keeps going, even on errors

(define (game-loop who)
  (newline)
  (let ((p (ask who 'place)))
    (print (ask p 'name))
    (display "exits: ") (print (ask p 'exits))
    (display "people: ") (print (map name (ask p 'people)))
    (display "things: ") (print (map name (ask p 'things)))
    (display "inventory: ") (print (map name (ask who 'possessions)))
    (when (ask p 'restaurant?)
          (display "restaurant menu: ") (print (ask p 'menu)))
    (display "=> ") (flush))
  (let* ((command (read)))
    (cond ((or (not (list? command))
               (null? command))
           (display "Command needs to be list.\n")
           (display "(<command> <arg1> <arg2> ...)\n")
           (game-loop who))
          ((memq (car command) '(quit stop bye exit die suicide))
           (newline))
          ((equal? (car command) 'do)
           (eval (cdr command)))
          (else
           (with-error-handler
            (lambda (x) (display (& "ERROR: " x "\n")))
            (unless
             (apply ask  who (car command) (map eval (cdr command)))
             (display "Can't do that\n")))
           (game-loop who)))))
