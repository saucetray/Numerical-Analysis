-- forwardEuler.hs
-- author: Justin Sostre
-- Numerical Analysis ODE Solving
-- Can solve anything from dx/dt = f(x(t), t)
-- Dependencies: Chart and Chart-Cairo

module ForwardEuler where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

-- Using two-point formula to approximate the derivative at this point
derivative :: (Float -> Float -> Float) -> Float -> Float -> Float -> Float
derivative f x t h = (y2 - y1) / (2 * h)
                     where y2 = f (x+h) t
                           y1 = f (x-h) t

-- Newton's method for functions of 1 variable but relies on a constant input
-- because of the nature of dx/dt = f(x(t), t)
newton :: (Float -> Float -> Float) -> Float -> Float -> Float -> Float -> Float
newton f x t tol h = let eval = f x t in
                       if eval < tol
                          then x
                               else newton f x0 t tol h
                                    where x0 = x - f x t / derivative f x t h


forwardEuler :: (Float -> Float -> Float) -> Float -> Float ->
                 Float -> Int -> [(Float, Float)]
forwardEuler func h t x s = if s > 0
                            then (t, x) : forwardEuler func h nextT nextX nextS
                            else []
                            where nextT = t + h
                                  nextX = x + h * func x t
                                  nextS = s - 1


-- backwardEuler that is much more stable for solving dx/dt = f(x(t), t)
backwardEuler :: (Float -> Float -> Float) -> Float -> Float ->
                  Float -> Int -> [(Float, Float)]
backwardEuler func h t x s = if s > 0
                             then (t, x) : backwardEuler func h nextT nextX nextS
                             else []
                             where nextT = t+h
                                   nextX = x + h * func x t
                                   nextS = s - 1
