
# cl-easel (WIP)

A Common Lisp library dedicated to render 2d objects on terminal.

## Examples

```lisp
(let ((h 9)
      (w 17))
  (with-easel (x (1 h w))
    (draw-horizontal! x 0)
    (draw-horizontal! x -1)
    (draw-vertical! x 0)
    (draw-vertical! x -1)
    (draw-horizontal! x -3)

    (flet ((draw-histgram-helper (place height)
	     (draw-vertical!   x (+ 3 place) (- h height) (- h 2))
	     (draw-vertical!   x (+ 4 place) (- h height) (- h 2))
	     (draw-horizontal! x (- h height) (+ 3 place) (+ 5 place))))
      
      (draw-histgram-helper 1 7)
      (draw-histgram-helper 4 4)
      (draw-histgram-helper 8 6))
    (realize x)
    (print x)))
```

```
┌───────────────┐
│               │
│   ┌┐          │
│   ││     ┌┐   │
│   ││     ││   │
│   ││ ┌┐  ││   │
├───┴┴─┴┴──┴┴───┤
│               │
└───────────────┘
```