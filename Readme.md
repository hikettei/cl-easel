
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

```lisp
(with-easel (x (1 14 49))
  (histogram
   x
   (wf/t:tensor-vec (wf/d:beta `(1000) 2 2))
   :direction :horizontal :title "[Be(2, 2)]")
  (realize x)
  (print x))

            ┬────────────────────────────────────
            │ [Be(2, 2)]                         
(0.01, 0.11]│████▏ 24                            
(0.11, 0.21]│███████████▍ 68                     
(0.21, 0.30]│██████████████████▏ 109             
(0.30, 0.40]│████████████████████████▋ 148       
(0.40, 0.50]│████████████████████████▎ 146       
(0.50, 0.60]│█████████████████████▏ 127          
(0.60, 0.69]│██████████████████████▏ 133         
(0.69, 0.79]│████████████████████▍ 122           
(0.79, 0.89]│███████████████▎ 91                 
(0.89, 0.98]│█████▎ 31                           
(0.98, 1.08]│▎ 1                                 
            ┴────────────────────────────────────
```

## The Goals

- High Performance 2D Rendering achieved by adopting Petalisp as a backend.

- Applying into scientific plotting for REPL developing (inspired in [UnicodePlots.jl](https://github.com/JuliaPlots/UnicodePlots.jl))

- Multiple Backend (e.g.: GNUPlot, Jupyter, Unicode...)

- Making the appearance of Common Lisp displayed on terminal much more elegant from the root!

