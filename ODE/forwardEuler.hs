-- forwardEuler.hs
-- author: Justin Sostre
-- Numerical Analysis ODE Solving
-- Can solve anything from dx/dt = f(x(t), t)
-- Dependencies: Chart and Chart-Cairo

module ForwardEuler where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

f :: Float -> Float -> Float
f x t = 2 - x


-- Using two-point formula to approximate the derivative at this point
derivative :: (Float -> Float -> Float) -> Float -> Float -> Float
derivative func x h = y2 - y1 `div` (2 * h)
                      where y2 = func $ x-h
                            y1 = func $ x+h


-- Newton's method to solve f(x) = 0!
newtonMethod :: (Float -> Float -> Float) -> Float -> Float -> Float
newtonMethod func g tol = if x < tol
                          then g
                          else g - x `div` d
                          where x = func $ g
                                d = derivative func g .0001


-- forwardEuler method to use on a dx/dt = f(x(t), t)
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
