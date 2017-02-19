#lang racket
(require graphics/graphics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;INTERNAL LOGIC PART:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;The lists of vectors we'll work will be either:
;( (x0 y0) ... (xn yn) ) or
;( (x0 y0 w0) ... (xn yn w0) )
;simple accessors for a vector of the type (xk yk wk) or (xk yk) - get-w can't be used on the later
(define get-x car)
(define get-y cadr)
(define get-w caddr)

;inserts a 2d vector in an ordered set of 2d vectors (first ordered by x coord, 2nd by y)
(define (make-live x y G)
  (cond
    ((null? G) (list (list x y)))
    ((< (get-x (car G)) x) (cons (car G) (make-live x y (cdr G))))
    ((and (eq? (get-x (car G)) x) (< (get-y (car G)) y)) (cons (car G) (make-live x y (cdr G))))
    ((and (eq? (get-y (car G)) y) (eq? (get-x (car G)) x)) G)
    (else (cons (list x y) (cons (car G)  (cdr G))))
    )
  )

;removes a  2d vector in an ordered set of 2d vectors (first ordered by x coord, 2nd by y)
(define (make-dead x y G)
  (cond
    ((null? G) '())
    ((< (get-x (car G)) x) (cons (car G) (make-dead x y (cdr G))))
    ((and (eq? (get-x (car G)) x) (< (get-y (car G)) y)) (cons (car G) (make-dead x y (cdr G))))
    ((and (eq? (get-y (car G)) y) (eq? (get-x (car G)) x)) (cdr G))
    (else G)
    )
  )

;returns #t if the vector (x y) is in the set G (of ordered 2d vectors)
(define (live? x y G)
  (cond
    ((null? G) #f)
    ((< (get-x (car G)) x) (cons (car G) (live? x y (cdr G))))
    ((and (eq? (get-x (car G)) x) (< (get-y (car G)) y)) (cons (car G) (live? x y (cdr G))))
    ((and (eq? (get-y (car G)) y) (eq? (get-x (car G)) x)) #t)
    (else #f)
    )
  )

;inserts a 2d vector in an ordered set of 3d vectors (first ordered by x coord, 2nd by y), that have a 3rd component indicating how many times they were inserted in the set
(define (insert-semi-step x y Z)
  (cond
    ((null? Z) (list (list x y 1)))
    ((< (get-x (car Z)) x) (cons (car Z) (insert-semi-step x y (cdr Z))))
    ((and (eq? (get-x (car Z)) x) (< (get-y (car Z)) y)) (cons (car Z) (insert-semi-step x y (cdr Z))))
    ((and (eq? (get-y (car Z)) y) (eq? (get-x (car Z)) x)) (cons (list x y (+ 1 (get-w (car Z)))) (cdr Z)))
    (else (cons (list x y 1) (cons (car Z)  (cdr Z))))
    )
  )

;makes a list of the live neighbours of (x y)
(define (neighbours x y G)
  (cond
    ((null? G) '())
    ((< (get-x (car G)) (- x 1)) (neighbours x y (cdr G)))
    ((and (<= (- x 1) (get-x (car G))) (or (< (get-y (car G)) (- y 1)) (> (get-y (car G)) (+ y 1)))) (neighbours x y (cdr G)))
    ((and (eq? (get-x (car G)) x) (eq? (get-y (car G)) y)) (neighbours x y (cdr G)))
    ((<= (get-x (car G)) (+ x 1)) (cons (car G) (neighbours x y (cdr G))))
    (else '())
    )
  )
;generates the 8 neighbours around (x y) sorted by x first and y 2nd
(define (generate-neighbours x y) (list (list (- x 1) (- y 1))
                                     (list (- x 1) y)
                                     (list (- x 1) (+ y 1))
                                     (list x (- y 1))
                                     (list x (+ y 1))
                                     (list (+ x 1) (- y 1))
                                     (list (+ x 1) y)
                                     (list (+ x 1) (+ y 1)))
  )

;difference of a full neighbour set A and a partial neighbour set B
(define (neighbours-diff A B)
  (cond
    ((null? A) '())
    ((null? B) A)
    ((and (eq? (get-x (car B)) (get-x (car A))) (eq? (get-y (car B)) (get-y (car A)))) (neighbours-diff (cdr A) (cdr B)))
    (else (cons (car A) (neighbours-diff (cdr A) B)))
    )
  )

;A and B are ordered sets of 2d vectors(first by x, 2nd by y), merge A and B
(define (merge-vsets A B)
  (cond
    ((null? A) B)
    ((null? B) A)
    ((< (get-x (car A)) (get-x (car B))) (cons (car A) (merge-vsets (cdr A) B)))
    ((> (get-x (car A)) (get-x (car B))) (cons (car B) (merge-vsets A (cdr B))))
    ((and (eq? (get-x (car A)) (get-x (car B))) (<= (get-y (car A)) (get-y (car B)))) (cons (car A) (merge-vsets (cdr A) B)))
    (else (cons (car B) (merge-vsets A (cdr B))))
    )
  )

;given a set ( (x0 y0 w0) ... (xn yn w0) ) filters only the i1,...,ik for which wik==3, the result being ((xi1 yi1) ... (xik yik))
(define (filter-semi-step Z)
  (cond
    ((null? Z) '())
    ((eq? (get-w (car Z)) 3) (cons (list (get-x (car Z)) (get-y (car Z))) (filter-semi-step (cdr Z))))
    (else (filter-semi-step (cdr Z)))
    )
  )

;same as insert semi-step but inserts a whole list of vectors one by one
(define (insert-semi-stepL l Z)
  (if (null? l) Z
      (insert-semi-stepL (cdr l) (insert-semi-step (get-x (car l)) (get-y (car l)) Z))
      )
  )
;puts a at the end of l
(define (push-back a l)
  (if (null? l) (list a)
      (cons (car l) (push-back a (cdr l)))
      )
  )
;makes a list of the cells that will remain live on the next step and merges that list with a list of the cells that will become live on the next step
(define (make-step G)
  (define (helper T R Z)
    (if (null? T) (merge-vsets R (filter-semi-step Z))
        (let ((n (neighbours (get-x (car T)) (get-y (car T)) G)))
          (if (or (eq? (length n) 2) (eq? (length n) 3))
              (helper (cdr T) (push-back (car T) R) (insert-semi-stepL (neighbours-diff (generate-neighbours (get-x (car T)) (get-y (car T))) n) Z))
              (helper (cdr T) R (insert-semi-stepL (neighbours-diff (generate-neighbours (get-x (car T)) (get-y (car T))) n) Z))
              )
          )
        )
    )
  (helper G '() '())
  )

;makes n steps (sorry no backward steps)
(define (make-n-steps G n)
  (if (<= n 0) G
      (make-n-steps (make-step G) (- n 1))
      )
  )

;transposes a matrix
(define (transpose-matrix M) (apply map list M))

;makes a matrix from the rectangle of cells from x0,y0 to x1,y1
(define (make-mat-from-cells x0 y0 x1 y1 G)
  (define (helperX x Z)
    (define (helper0 y)
      (if (> y y1) '()
          (cons 0 (helper0 (+ 1 y)))
          )
      )
    (define (helperY y R Z)
      (cond
        ((> y y1) (list R Z))
        ((or (null? Z) (not (eq? (get-x (car Z)) x)) (< y (get-y (car Z))) (> y (get-y (car Z))) ) (helperY (+ 1 y) (push-back 0 R) Z))
        (else (helperY (+ 1 y) (push-back 1 R) (cdr Z)))
        )
      )
    (cond
      ((> x x1) '())
      ((or (null? Z) (< x (get-x (car Z)))) (cons (helper0 y0) (helperX (+ 1 x) Z)))
      (else (let ((tmp (helperY y0 '() Z))) (cons (car tmp) (helperX (+ 1 x) (cadr tmp)))))
      )
    )
  (transpose-matrix (helperX x0 G))
  )

;makes cells from a matrix with offset (ox,oy)
(define (make-cells-from-mat ox oy M)
  (define (helperX M x)
    (define (helper l T y)
       (cond
         ((null? l) T)
         ((eq? (car l) 1) (helper (cdr l) (push-back (list x y) T) (+ y 1)))
         (else (helper (cdr l) T (+ y 1)))
       )
      )
    (if (null? M) '()
      (append (helper (car M) '() oy) (helperX (cdr M) (+ x 1)))
      )
    )
  (helperX (transpose-matrix M) ox)
  )
;try this (2dGraphics 800 600 0 0 50 50 (let ((G glider-gun))  (make-cells-from-mat (get-x (bounding-box-cells G)) (get-y (bounding-box-cells G)) (make-mat-from-cells (get-x (bounding-box-cells G)) (get-y (bounding-box-cells G)) (get-w (bounding-box-cells G)) (cadddr (bounding-box-cells G)) G))))
;might be useful for (make-mat-from-cells (get-x (bounding-box-cells G)) (get-y (bounding-box-cells G)) (get-w (bounding-box-cells G)) (cadddr (bounding-box-cells G)) G)
;ex: (let ((G (make-n-steps glider-gun 60))) (make-mat-from-cells (get-x (bounding-box-cells G)) (get-y (bounding-box-cells G)) (get-w (bounding-box-cells G)) (cadddr (bounding-box-cells G)) G))
(define (bounding-box-cells G)
  (define (helper G ny my mx)
    (cond
      ((null? G) (list ny mx my))
      ((and (null? (cdr G)) (< (get-y (car G)) ny)) (helper (cdr G) (get-y (car G)) my (get-x (car G))))
      ((and (null? (cdr G)) (> (get-y (car G)) my)) (helper (cdr G) ny (get-y (car G)) (get-x (car G))))
      ((and (null? (cdr G)) (>= (get-y (car G)) ny) (<= (get-y (car G)) my)) (helper (cdr G) ny my (get-x (car G))))
      ((< (get-y (car G)) ny) (helper (cdr G) (get-y (car G)) my mx))
      ((> (get-y (car G)) my) (helper (cdr G) ny (get-y (car G)) mx))
      (else (helper (cdr G) ny my mx))
      )
    )
  (cons (get-x (car G)) (helper G (get-y (car G)) (get-x (car G)) (get-y (car G))))
  )

;translates all cells by ox oy
(define (offset-cells ox oy G)
  (if (null? G) '()
      (cons (list (+ ox (get-x (car G))) (+ oy (get-y (car G)))) (offset-cells ox oy (cdr G)))
      )
  )

;mirrors the config over the y=x line, works as rotation for symmetric cases
(define (rotate-cells G)
  (define (helper G T)
    (if (null? G) T
        (helper (cdr G) (make-live (get-y (car G)) (get-x (car G)) T))
        )
    )
  (helper G '())
  )
  

;Various configs
;still configs
(define cell-config-block '( (0 0) (0 1) (1 0) (1 1) ) )
(define cell-config-beehive '( (0 1) (1 0) (1 2) (2 0) (2 2) (3 1) ) )
(define cell-config-loaf '( (0 1) (1 0) (1 2) (2 0) (2 3) (3 1) (3 2) ) )
(define cell-config-boat '( (0 0) (0 1) (1 0) (1 2) (2 1) ) )

;oscillators
(define cell-config-blinker '( (0 0) (1 0) (2 0) ) )
(define cell-config-toad '( (0 1) (1 0) (1 1) (2 0) (2 1) (3 0) ) )
(define cell-config-beacon '( (0 0) (0 1) (1 0) (2 3) (3 2) (3 3) ) )
(define cell-config-pulsar '( (0 2) (0 3) (0 4) (0 8) (0 9) (0 10)
                                    (2 0) (2 5) (2 7) (2 12) (3 0) (3 5) (3 7) (3 12) (4 0) (4 5) (4 7) (4 12)
                                    (5 2) (5 3) (5 4) (5 8) (5 9) (5 10) (7 2) (7 3) (7 4) (7 8) (7 9) (7 10)
                                    (8 0) (8 5) (8 7) (8 12) (9 0) (9 5) (9 7) (9 12) (10 0) (10 5) (10 7) (10 12)
                                    (12 2) (12 3) (12 4) (12 8) (12 9) (12 10) ) )
(define cell-config-pentadecathlon '( (0 1) (1 1) (2 0) (2 2) (3 1) (4 1) (5 1) (6 1) (7 0) (7 2) (8 1) (9 1) ) )

;moving stuff
(define cell-config-glider '(  (0 2) (1 0) (1 2) (2 1) (2 2) ) )
(define cell-config-lwss '( (0 0) (0 2) (1 3) (2 3) (3 0) (3 3) (4 1) (4 2) (4 3) ) )

;methulases
(define cell-config-R-pentonimo '( (0 1) (1 0) (1 1) (1 2) (2 0) ) )
(define cell-config-Diehard '( (0 1) (1 1) (1 2) (5 2) (6 0) (6 2) (7 2) ) )
(define cell-config-acorn '( (0 2) (1 0) (1 2) (3 1) (4 2) (5 2) (6 2) ) )


;generators
(define cell-config-glider-gun '( (0 0) (0 1) (1 0) (1 1) (10 0) (10 1) (10 2) (11 -1) (11 3) (12 -2) (12 4) (13 -2) (13 4) (14 1)
                            (15 -1) (15 3) (16 0) (16 1) (16 2) (17 1) (20 -2) (20 -1) (20 0) (21 -2) (21 -1) (21 0)
                            (22 -3) (22 1) (24 -4) (24 -3) (24 1) (24 2) (34 -2) (34 -1) (35 -2) (35 -1)))
;infinite growth
(define cell-config-inf1 '( (0 5) (2 4) (2 5) (4 1) (4 2) (4 3) (6 0) (6 1) (6 2) (7 1) ) )
(define cell-config-inf2 '( (0 0) (0 1) (0 4) (1 0) (1 3) (2 0) (2 3) (2 4) (3 2) (4 0) (4 2) (4 3) (4 4) ) )
(define cell-config-inf3 '( (0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0) (9 0) (10 0) (11 0) (12 0) (13 0) (17 0) (18 0) (19 0) (26 0) (27 0) (28 0) (29 0) (30 0) (31 0) (32 0) (34 0) (35 0) (36 0) (37 0) (38 0) ) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2D GRAPHICS PART::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;draws a grid starting from x0 till x1 with delta dx on the x axis, and from y0 to y1 with a delta dy on the y axis
(define (drawGrid vp x0 y0 dx dy x1 y1)
  (define (helper kx)
    (if (> kx x1) (void)
        (begin ((draw-line vp) (make-posn kx 0) (make-posn kx y1) (make-rgb 0 0 0)) (helper (+ kx dx))) 
        )
    )

  (define (helper2 ky)
    (if (> ky y1) (void)
        (begin ((draw-line vp) (make-posn 0 ky) (make-posn x1 ky) (make-rgb 0 0 0)) (helper2 (+ ky dy))) 
        )
    )

  (begin (helper x0) (helper2 y0))
  )
;given a list of 2d vectors l, draws them depending on whether they are in the visible region
(define (drawCells vp x0 y0 dx dy x1 y1 l)
  (define (helper l)
    (if (null? l) (void)
        (if (and (<= x0 (+ 1 dx (* dx (get-x (car l))))) (< (* dx (get-x (car l))) x1) (<= y0 (+ 1 dy (* dy (get-y (car l))))) (< (* dy (get-y (car l))) y1)) (begin ((draw-solid-rectangle vp) (make-posn (+ 1 (- (* dx (get-x (car l))) x0)) (+ 1 (- (* dy (get-y (car l))) y0))) (- dx 1) (- dy 1) (make-rgb 1 0 0)) (helper (cdr l))) 
            (helper (cdr l))
            )
        )
    )
  (helper l)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SAVE/LOAD FUNCTIONS:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;saves a list of ordered 2d vectors to a file, (save-cells-to-file "someFile.txt" someListOfVectors)
(define (save-cells-to-file a Z)
  (define out (open-output-file a #:exists 'truncate)) 
  (define (helper T)
    (if (null? T) (close-output-port out)
        (begin (fprintf out "~s ~s\n" (get-x (car T))(get-y (car T))) (helper (cdr T)))
    )
  )
  (helper Z)
  )
;(load-cells-from-file "conwaysav.txt")
(define (load-cells-from-file a)
  (define in (open-input-file a))
  (define (helper r)
    (if (eof-object? r) (begin (close-input-port in) '())
        (cons (list r (read in)) (helper (read in)))
    )
    )
  (helper (read in))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MINI EXAMPLE:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;(2dGraphics 800 600 0 0 50 50 cell-config-glider-gun)
(define (2dGraphics screenX screenY spX spY zoomX zoomY G)
  (open-graphics)
  (define w (open-viewport "2dGraphics" screenX screenY))
  (define dx (+ 1 zoomX))
  (define dy (+ 1 zoomY))
  (define (posUpdate posX posY dx dy Z)
    (begin ((clear-viewport w))
    (drawGrid w (* -1 (remainder posX dx)) (* -1 (remainder posY dy)) dx dy screenX screenY)
    (drawCells w posX posY dx dy (+ posX screenX) (+ posY screenY) Z)
    )
    )

  ;escape-exit
  ;up,down,left,right to navigate
  ;w,s to zoom in/zoom out
  ;enter to go to the next step
  ;q to save to conwaysav.txt
  ;c to make a cell live at mouse position
  ;d to make a cell dead at mouse position
  (define (main-loop pX pY dx dy sx sy Z)
    (let ((c (key-value (get-key-press w))))
      (cond
        ((eq? c 'escape) (void))
        ((eq? c 'up) (begin (posUpdate pX (- pY sy) dx dy Z) (main-loop pX (- pY sy) dx dy sx sy Z)))
        ((eq? c 'down) (begin (posUpdate pX (+ pY sy) dx dy Z) (main-loop pX (+ pY sy) dx dy sx sy Z)))
        ((eq? c 'left) (begin (posUpdate (- pX sx)  pY dx dy Z) (main-loop (- pX sx) pY dx dy sx sy Z)))
        ((eq? c 'right) (begin (posUpdate (+ pX sx) pY dx dy Z) (main-loop (+ pX sx) pY dx dy sx sy Z)))
        ((eq? c #\w) (begin (posUpdate pX pY (+ dx 1) (+ dy 1) Z) (main-loop pX pY (+ dx 1) (+ dy 1) (- sx 1) (- sy 1) Z)))
        ((eq? c #\s) (if (and (< 2 dx) (< 2 dy)) (begin (posUpdate pX pY (- dx 1) (- dy 1) Z) (main-loop pX pY (- dx 1) (- dy 1) (+ sx 1) (+ sy 1) Z)) (main-loop pX pY dx dy sx sy Z)))
        ((eq? c #\return) (begin (let ((z (make-step Z))) (posUpdate pX pY dx dy z) (main-loop pX pY dx dy sx sy z))))
        ((eq? c #\q) (begin (save-cells-to-file "conwaysav.txt" Z) (main-loop pX pY dx dy sx sy Z)))
        ((eq? c #\c) (let* ((mp (query-mouse-posn w)) (rx (floor (/ (+ (posn-x mp) pX) dx))) (ry (floor (/ (+ (posn-y mp) pY) dy))) (z (make-live rx ry Z))) (posUpdate pX pY dx dy z) (main-loop pX pY dx dy sx sy z)))
        ((eq? c #\d) (let* ((mp (query-mouse-posn w)) (rx (floor (/ (+ (posn-x mp) pX) dx))) (ry (floor (/ (+ (posn-y mp) pY) dy))) (z (make-dead rx ry Z))) (posUpdate pX pY dx dy z) (main-loop pX pY dx dy sx sy z)))
        (else (main-loop pX pY dx dy sx sy Z))
        )
      )
    )
  (posUpdate spX spY dx dy G)
  (main-loop spX spY dx dy 30 30 G)
  (close-viewport w)
  (close-graphics)
  )

(define (start-it)
  (write-string "Would you like to start the program with the default glider gun config?")
  (write-string "\n")
  (if (string=? (read-line (current-input-port) 'any) "yes") (2dGraphics 800 600 0 0 50 50 cell-config-glider-gun)
      (begin
        (write-string "Enter the name of the file from which you'd like to load:")
        (write-string "\n")
        (2dGraphics 800 600 0 0 50 50 (load-cells-from-file (read-line (current-input-port) 'any)))
        )
      )
  )

(start-it)