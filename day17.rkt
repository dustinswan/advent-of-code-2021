#lang racket

(define target
  (let* (;[input "target area: x=20..30, y=-10..-5"]
         [input "target area: x=144..178, y=-100..-76"]
         [regex #rx"x=([^\\.]+)\\.\\.([^\\.]+), y=([^\\.]+)\\.\\.([^\\.]+)"]
         [matches (regexp-match regex input)]
         [coords (drop matches 1)])
    (map string->number coords)))

(define (point-in-target? point)
  (match-let* ([(list x y) point]
               [(list x1 x2 y1 y2) target])
    (and (>= x x1) (<= x x2)
         (>= y y1) (<= y y2))))

(define (point-past-target? point)
  (match-let* ([(list x y) point]
               [(list x1 x2 y1 y2) target])
    (or (> x x2) (< y y1))))

(define (step point velocity)
  (match-let ([(list x y) point]
              [(list dx dy) velocity])
    (let* ([new-point (list (+ x dx) (+ y dy))]
           [dx-op (if (= dx 0) identity (if (> dx 0) sub1 add1))]
           [new-velocity (list (dx-op dx) (sub1 dy))])
      (values new-point new-velocity))))

(define (good-velocity? point velocity)
  (if (point-in-target? point)
      #t
      (if (point-past-target? point)
          #f
          (let-values ([(p2 v2) (step point velocity)])
            (good-velocity? p2 v2)))))

; shoot it as high as you can
; such that it never goes *below* the bottom of the target
; bottom of target: -10 in the example
; by the time it gets back to 0, dy is equal to -initial dy
; so.. initial dy can't be > -10? set it to 9 and find a valid x

(define (find-max-velocity)
  (let* ([dy (sub1 (* -1 (third target)))])
    (let loop ([dx 0])
      (if (good-velocity? '(0 0) (list dx dy))
          (list dx dy)
          (loop (add1 dx))))))

(define (find-max-height velocity)
  (let loop ([p '(0 0)]
             [v velocity])
    (let-values ([(p2 v2) (step p v)])
      (if (= 0 (second v2))
          (second p2)
          (loop p2 v2)))))
    
(define part1
  (let* ([v (find-max-velocity)]
         [height (find-max-height v)])
    height))

part1

; PART 2

(define part2
  (let* ([dy-max (* -1 (third target))]
         [dy-min (* -1 dy-max)]
         [dx-max (* 2 (second target))]
         [all (apply append
                     (for/list ([dy (range dy-min dy-max)])
                       (for/list ([dx (range 0 dx-max)]) 
                         (list dx dy (good-velocity? '(0 0) (list dx dy))))))]
         [goods (filter (λ (x) (third x)) all)])
    (length goods)))

part2