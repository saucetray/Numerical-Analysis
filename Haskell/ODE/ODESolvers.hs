-- forwardEuler.hs
-- author: Justin Sostre
-- Numerical Analysis ODE Solving
-- Can solve anything from dx/dt = f(x(t), t)
-- Dependencies: Chart and Chart-Cairo

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo


-- Using two-point formula to approximate the derivative at this point
f' :: (Float -> Float -> Float) -> Float -> Float -> Float -> Float
f' f x t h = (y2 - y1) / (2 * h)
             where y2 = f (x+h) t
                   y1 = f (x-h) t


-- newton's method for backwardEuler
newtonbe :: (Float -> Float -> Float) -> Float -> Float -> Float -> Float -> Float -> Float
newtonbe f x t g tol h = let eval = abs $ g - x - h * f g t in
                       if eval < tol
                          then g
                             else newtonbe f x t g0 tol h
                                where g0 = g - (g - x - h * f g t) / (1 - h * f' f g t h)


-- forwardEuler that is a good choice for simple ODEs
forwardEuler :: (Float -> Float -> Float) -> Float -> Float ->
                 Float -> Int -> [(Float, Float)]
forwardEuler f h x t s = if s > 0
                            then (t, x) : forwardEuler f h x0 t0 s0
                               else []
                                  where t0 = t + h
                                        x0 = x + h * f x t
                                        s0 = s - 1


-- backwardEuler that is much more stable for solving dx/dt = f(x(t), t)
backwardEuler :: (Float -> Float -> Float) -> Float -> Float ->
                  Float -> Int -> [(Float, Float)]
backwardEuler f h x t s =  if s > 0
                              then (t, x) : backwardEuler f h x0 t0 s0
                                 else []
                                    where t0 = t + h
                                          x0 = x + h * f (newtonbe f x t 4 h h) t
                                          s0 = s - 1


-- Make a graph!
backwardEulerGraph :: (Float -> Float -> Float) -> Float
                      -> Float -> Float -> Int -> String
                      -> String -> IO()
backwardEulerGraph f x t h s filename funcname = toFile def filename $ do
    layout_title .= "Solution to dx/dt = " ++ funcname
    plot (line "F(x)" [backwardEuler f h x t s])
