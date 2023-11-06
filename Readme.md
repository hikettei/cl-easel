
# cl-easel (WIP)

A Common Lisp library dedicated to render 2d objects on terminal.

## Examples

```lisp
(let ((h 10)
      (w 18))
  (with-easel (x (1 h w))
    (curve-text x "Survey" 1 1 :vertical)
    (draw-horizontal! x 0)
    (draw-horizontal! x -1)
    (draw-vertical! x 0)
    (draw-vertical! x -1)
    (draw-horizontal! x -3)

    (flet ((draw-histgram-helper (name place height)
	     (curve-text x name -2 (+ 3 place))
	     (curve-text x (format nil "~a" height) (- h height 1) (+ 4 place))
	     (draw-vertical!   x (+ 3 place) (- h height) (- h 2))
	     (draw-vertical!   x (+ 4 place) (- h height) (- h 2))
	     (draw-horizontal! x (- h height) (+ 3 place) (+ 5 place))))
      
      (draw-histgram-helper "A" 1 7)
      (draw-histgram-helper "B" 5 4)
      (draw-histgram-helper "C" 9 6)
      )
    (realize x)
    (print x)))
```

```
┌────────────────┐
│S               │
│u   7           │
│r  ┌┐       6   │
│v  ││      ┌┐   │
│e  ││   4  ││   │
│y  ││  ┌┐  ││   │
├───┴┴──┴┴──┴┴───┤
│   A   B   C    │
└────────────────┘
```